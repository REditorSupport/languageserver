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
    function_formals <- if (call$token == "source") base::source else base::sys.source
    opening <- call$opening
    if (point$row - opening$row > 128L) return(NULL)
    direct <- FALSE
    if (current && !is.null(syntax)) {
        token <- navigation_token(parsed, point$row + 1L, key$start + 1L)
        if (length(token) && token && syntax$token[[token]] == "STR_CONST") {
            argument <- syntax$parent_row[[token]]
            parent <- syntax$parent_row[[argument]]
            if (!parent) return(NULL)
            args <- syntax_call_arguments(syntax, parent)
            if (!identical(args$name, call$token)) return(NULL)
            matched <- class_matched_arguments(args, names(formals(function_formals)))
            if (!identical(matched$file, argument)) return(NULL)
            direct <- TRUE
        }
    }
    # The argument under the cursor may lack both its closing quote and ')'.
    # Match a temporary literal to the real formals, retaining named arguments.
    if (!direct) {
        prefix <- get_range_text(document$content, opening$row + 1L, opening$col + 1L, point$row + 1L, literal_start)
        if (sum(nchar(prefix, type = "bytes")) > 16384L) return(NULL)
        text <- paste0("target", paste(prefix, collapse = "\n"), ".__path_cursor__")
        if (key$closed) {
            # A later file= argument can claim the first formal even while the
            # closing parenthesis is missing. Retain the rest of this call.
            text <- paste0(text, substring(line, key$end + 1L))
            bounds <- document_r_bounds(document, point$row)
            last <- min(bounds[[2L]], point$row + 128L)
            if (point$row < last && nzchar(missing_closing_delimiters(text))) {
                text <- c(text, document$content[seq.int(point$row + 2L, last + 1L)])
            }
            if (sum(nchar(text, type = "bytes")) > 16384L) return(NULL)
            text <- paste0(paste(text, collapse = "\n"), missing_closing_delimiters(text))
        } else {
            text <- paste0(text, ")")
        }
        matched <- tryCatch(match.call(function_formals, parse(text = text, n = 1L, keep.source = FALSE)[[1L]]),
            error = function(e) NULL)
        if (!identical(matched$file, as.name(".__path_cursor__"))) return(NULL)
    }
    if (is.null(call$package)) {
        if (current && length(navigation_definitions(parsed, opening$row + 1L, opening$col, call$token, last = TRUE))) return(NULL)
        at <- c(opening$row, opening$col)
        if (current && length(parsed$member_data$sources)) {
            view <- member_workspace_view(workspace, document)
            if (member_history_position(view$bindings[[call$token]], at)) return(NULL)
        }
        index <- workspace$index
        if (isTRUE(index$enabled) && index$has_definition(call$token, uri = document$uri)) {
            peers <- setdiff(workspace_document_uris(workspace, document$uri), document$uri)
            if (any(vapply(peers, function(uri) {
                call$token %in% names(index$summaries$get(uri, NULL)$definitions)
            }, logical(1L)))) return(NULL)
        }
        if (is.function(workspace$guess_namespace)) {
            owner <- workspace$guess_namespace(call$token, isf = TRUE, uri = document$uri)
            if (length(owner) && !identical(owner, "base")) return(NULL)
        }
        if (!direct && source_path_partial_shadow(document, call, literal_start, point$row, current)) return(NULL)
    }
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

# Recover a lexical scope that an unfinished source call prevents R from parsing.
# Accepted outer statements already have their own scope index.
source_path_partial_shadow <- function(document, call, quote_start, row, current) {
    bounds <- document_r_bounds(document, call$opening$row)
    first <- bounds[[1L]]
    if (current) {
        syntax <- document$parse_data$syntax_data
        completed <- syntax$roots[syntax$line2[syntax$roots] < call$opening$row + 1L]
        if (length(completed)) first <- max(first, syntax$line2[utils::tail(completed, 1L)])
    }
    if (row - first > 128L) return(TRUE)
    lines <- document$content[seq.int(first + 1L, row + 1L)]
    lines[[length(lines)]] <- paste0(substr(lines[[length(lines)]], 1L, quote_start), '""')
    if (sum(nchar(lines, type = "bytes")) > 16384L) return(TRUE)
    lines <- normalize_parse_content(lines)
    text <- paste0(paste(lines, collapse = "\n"), missing_closing_delimiters(lines))
    source <- srcfilecopy("<text>", text)
    parsed <- tryCatch(parse(text = text, srcfile = source, keep.source = TRUE), error = function(e) NULL)
    if (is.null(parsed)) return(TRUE)
    syntax <- syntax_parse_data(utils::getParseData(source))
    p <- list(syntax_data = syntax, navigation_data = navigation_parse_data(syntax))
    length(navigation_definitions(p, call$opening$row - first + 1L, call$opening$col, call$token, last = TRUE)) > 0L
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
            state$seen[[name]] <- TRUE
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
