# Only the file argument of a direct base source call owns path completion.
# The native quote scanner supplies physical offsets even in an unfinished string.
source_path_context <- function(document, point, workspace) {
    if (!check_r_region(document, point)) return(NULL)
    line <- document$line0(point$row)
    key <- .Call("scan_string_completion_c", line, as.integer(point$col), PACKAGE = "languageserver")
    if (is.null(key) || (key$closed && point$col >= key$end)) return(NULL)
    quote <- substr(line, key$start + 1L, key$start + 1L)
    raw <- key$start > 0L && substr(line, key$start, key$start) %in% c("r", "R")
    literal_start <- key$start - as.integer(raw)
    call <- document$detect_call(list(row = point$row, col = literal_start), signature = TRUE)
    if (!length(call$token) || !call$token %in% c("source", "sys.source") ||
            !(identical(call$accessor, "") || (identical(call$accessor, "::") && identical(call$package, "base")))) {
        return(NULL)
    }
    parsed <- document$parse_data
    current <- identical(parsed$version, document$version)
    syntax <- parsed$syntax_data
    opening <- call$opening
    if (point$row - opening$row > 128L) return(NULL)
    direct <- FALSE
    if (current && !is.null(syntax)) {
        token <- member_navigation_token(parsed, list(row = point$row, col = literal_start))
        if (token && syntax$token[[token]] == "STR_CONST" &&
                syntax$line1[[token]] == point$row + 1L && syntax$col1[[token]] == literal_start + 1L) {
            argument <- syntax$parent_row[[token]]
            row <- syntax$parent_row[[argument]]
            if (!row) return(NULL)
            args <- syntax_call_arguments(syntax, row)
            if (!identical(source_file_syntax(syntax, row, args), argument)) return(NULL)
            # Qualification is a property of the whole parsed callee, even
            # when whitespace separates base, :: and the function name.
            call$package <- args$package
            direct <- TRUE
        }
    }
    if (!direct) {
        recovered <- source_path_partial_call(document, call, key, literal_start, point$row, current)
        if (is.null(recovered)) return(NULL)
        call$package <- recovered$package
    }
    if (is.null(call$package) && !source_base_binding(workspace, document, call$token,
            opening$row, opening$col, current)) return(NULL)
    raw_open <- raw_close <- ""
    if (raw) {
        size <- attr(regexpr("^-*[({\\[]", key$token, perl = TRUE), "match.length")
        if (size < 1L) return(NULL)
        raw_open <- substr(key$token, 1L, size)
        close <- switch(substr(raw_open, size, size), `(` = ")", `[` = "]", `{` = "}")
        raw_close <- paste0(close, substr(raw_open, 1L, size - 1L))
        key$token <- substring(key$token, size + 1L)
    }
    decode <- function(value) {
        if (raw) return(value)
        tryCatch(str2lang(paste0(quote, value, quote)), error = function(e) NULL)
    }
    decoded <- decode(key$token)
    if (!is.character(decoded) || length(decoded) != 1L || grepl("^[[:alpha:]][[:alnum:]+.-]*://", decoded)) return(NULL)
    # Leave the typed directory spelling and everything after the next slash
    # intact, including the user's quote and any following call arguments.
    directory <- sub("[^/]*$", "", decoded)
    prefix <- substring(decoded, nchar(directory) + 1L)
    spelling <- sub("[^/]*$", "", key$token)
    if (!identical(decode(spelling), directory)) return(NULL)
    end <- key$end - as.integer(key$closed) * (1L + nchar(raw_close))
    if (end < point$col) return(NULL)
    suffix <- substr(line, point$col + 1L, end)
    slash <- regexpr("/", suffix, fixed = TRUE)[[1L]]
    if (slash > 0L) end <- point$col + slash - 1L
    list(directory = directory, prefix = prefix, quote = quote, raw = raw, raw_close = raw_close, has_suffix = slash > 0L,
        range = range(document$to_lsp_position(point$row, key$start + 1L + nchar(raw_open) + nchar(spelling)),
            document$to_lsp_position(point$row, end)))
}

# Return the syntax row which actually supplies file, including named and
# partially matched arguments and the implicit first input of direct pipes.
source_file_syntax <- function(syntax, row, call = syntax_call_arguments(syntax, row)) {
    if (!isTRUE(call$name %in% c("source", "sys.source")) ||
            (!is.null(call$package) && !identical(call$package, "base"))) return(NULL)
    call <- syntax_pipe_arguments(syntax, row, call)
    fn <- if (call$name == "source") base::source else base::sys.source
    class_matched_arguments(call, names(formals(fn)))$file
}

# Test the source-site scope before interpreting a bare helper as a base call.
source_base_binding <- function(workspace, document, name, row, col, current = TRUE) {
    parsed <- document$parse_data
    if (current && length(navigation_definitions(parsed, row + 1L, col, name, last = TRUE))) return(FALSE)
    if (current && length(parsed$member_data$sources)) {
        view <- member_workspace_view(workspace, document)
        if (member_history_position(view$bindings[[name]], c(row, col))) return(FALSE)
    }
    index <- workspace$index
    if (isTRUE(index$enabled) && index$has_definition(name, uri = document$uri)) {
        peers <- setdiff(workspace_document_uris(workspace, document$uri), document$uri)
        if (any(vapply(peers, function(uri) name %in% names(index$summaries$get(uri, NULL)$definitions), logical(1L)))) {
            return(FALSE)
        }
    }
    if (!is.function(workspace$guess_namespace)) return(TRUE)
    owner <- workspace$guess_namespace(name, isf = TRUE, uri = document$uri)
    !length(owner) || identical(owner, "base")
}

# Keep the real callee and enclosing pipe when replacing an unfinished literal.
# Replacing only its argument list would also accept x $ source("...").
source_path_partial_call <- function(document, call, key, quote_start, row, current) {
    bounds <- document_r_bounds(document, call$opening$row)
    first <- bounds[[1L]]
    if (current) {
        syntax <- document$parse_data$syntax_data
        completed <- syntax$roots[syntax$line2[syntax$roots] < call$opening$row + 1L]
        if (length(completed)) first <- max(first, syntax$line2[utils::tail(completed, 1L)])
    }
    if (row - first > 128L) return(NULL)
    lines <- document$content[seq.int(first + 1L, row + 1L)]
    line <- lines[[length(lines)]]
    marker <- ".__languageserver_source_file__"
    lines[[length(lines)]] <- paste0(substr(line, 1L, quote_start), marker,
        if (key$closed) substring(line, key$end + 1L) else "")
    # Later named arguments can own file. Retain successive lines while the
    # original call is still open, then finish any enclosing function or block.
    if (key$closed && row < bounds[[2L]]) {
        following <- document$content[seq.int(row + 2L, min(bounds[[2L]], row + 128L) + 1L)]
        call_lines <- lines[seq.int(call$opening$row - first + 1L, length(lines))]
        call_lines[[1L]] <- substring(call_lines[[1L]], call$opening$col + 1L)
        for (next_line in following) {
            if (!nzchar(missing_closing_delimiters(call_lines))) break
            call_lines <- c(call_lines, next_line)
            lines <- c(lines, next_line)
            if (sum(nchar(lines, type = "bytes")) > 16384L) return(NULL)
        }
    }
    if (sum(nchar(lines, type = "bytes")) > 16384L) return(NULL)
    lines <- normalize_parse_content(lines)
    text <- paste0(paste(lines, collapse = "\n"), "\n", missing_closing_delimiters(lines))
    source <- srcfilecopy("<text>", text)
    parsed <- tryCatch(parse(text = text, srcfile = source, keep.source = TRUE), error = function(e) NULL)
    if (is.null(parsed)) return(NULL)
    syntax <- syntax_parse_data(utils::getParseData(source))
    marker_row <- which(syntax$text == marker & syntax$token == "SYMBOL" &
            syntax$line1 == row - first + 1L & syntax$col1 == quote_start + 1L)
    if (length(marker_row) != 1L) return(NULL)
    argument <- syntax$parent_row[[marker_row]]
    parent <- syntax$parent_row[[argument]]
    if (!parent) return(NULL)
    args <- syntax_call_arguments(syntax, parent)
    if (!identical(source_file_syntax(syntax, parent, args), argument)) return(NULL)
    p <- list(syntax_data = syntax, navigation_data = navigation_parse_data(syntax))
    if (is.null(args$package) &&
            length(navigation_definitions(p, call$opening$row - first + 1L, call$opening$col, call$token, last = TRUE))) return(NULL)
    args
}

source_path_completion <- function(document, point, workspace, context = source_path_context(document, point, workspace)) {
    if (is.null(context)) return(NULL)
    root <- get_root_path_for_uri(document$uri, workspace$root)
    from <- path_from_uri(document$uri)
    bases <- unique(c(root, if (nzchar(from)) dirname(from)))
    fragment <- context$directory
    directories <- if (grepl("^([/~]|[[:alpha:]]:/)", fragment)) path.expand(fragment) else file.path(bases, fragment)
    limit <- lsp_settings$get("max_completions")
    limit <- if (length(limit) && is.finite(limit) && limit > 0L) min(limit, 256L) else 256L
    state <- new.env(parent = emptyenv())
    state$seen <- new.env(hash = TRUE, parent = emptyenv())
    state$scanned <- 0L
    state$items <- list()
    state$incomplete <- FALSE
    for (directory in directories[dir.exists(directories)]) {
        # One level only. Limit inspected entries as well as emitted items so
        # large generated directories cannot create an unbounded completion list.
        withRestarts(fs::dir_walk(directory, function(path) {
            state$scanned <- state$scanned + 1L
            if (state$scanned > 4096L || length(state$items) >= limit) {
                state$incomplete <- TRUE
                invokeRestart("end_directory")
            }
            name <- basename(path)
            if (!startsWith(name, context$prefix) || exists(name, state$seen, inherits = FALSE)) return(invisible(NULL))
            folder <- dir.exists(path)
            if (context$has_suffix && !folder) return(invisible(NULL))
            label <- if (folder) paste0(name, "/") else name
            inserted <- if (context$has_suffix) name else label
            if (context$raw) {
                if (grepl(paste0(context$raw_close, context$quote), inserted, fixed = TRUE)) return(invisible(NULL))
            } else {
                escaped <- encodeString(inserted, quote = context$quote)
                inserted <- substr(escaped, 2L, nchar(escaped) - 1L)
            }
            state$seen[[name]] <- TRUE
            state$items[[length(state$items) + 1L]] <- list(label = label,
                kind = if (folder) CompletionItemKind$Folder else CompletionItemKind$File,
                detail = as.character(path), textEdit = text_edit(context$range, inserted))
            invisible(NULL)
        }, all = startsWith(context$prefix, "."), fail = FALSE), end_directory = function() NULL)
        if (state$incomplete) break
    }
    labels <- vapply(state$items, `[[`, character(1L), "label")
    list(isIncomplete = state$incomplete, items = state$items[order(labels)])
}
