# All member providers share receiver inference and method identity. Metadata
# contains syntax only; resolving a signature never calls the selected member.
member_symbol_info <- function(label, value, key, index) {
    if (!is.null(value$metadata)) {
        package_index <- member_namespace_index(index, value$metadata)
        if (!is.null(package_index)) index <- package_index
    }
    if (!is.null(value$function_key)) key <- value$function_key
    fn <- value$function_expr
    if (is.null(fn)) fn <- member_definition(index$definitions, key)
    if (!is.null(key) && key %in% index$native_factories) {
        fn <- member_returned_function(fn)$fn
    }
    if (isTRUE(value$s7_bound_name) && member_head(fn, "function")) {
        params <- as.list(fn[[2L]])
        keys <- names(params)
        # The injected name= uses the same exact/partial matching as a real R call.
        dots <- match("...", keys, nomatch = length(keys) + 1L)
        i <- match("name", keys, nomatch = 0L)
        if (!i) i <- pmatch("name", keys[seq_len(dots - 1L)], nomatch = 0L)
        if (i) fn[2L] <- list(as.pairlist(params[-i]))
    }
    display <- quote_completion_name(label)
    signature <- if (member_head(fn, "function")) get_signature(display, fn) else NULL
    delegated <- member_lookup(index$delegation, key)
    if (!is.null(delegated)) key <- delegated$original
    description <- member_r6_description(value)
    if (isTRUE(value$possible_receiver)) {
        description <- paste(c("Receiver type is uncertain; this member is shared by known method results.", description),
            collapse = "\n\n")
    }
    list(
        label = label, signature = signature, formals = if (!is.null(signature)) fn[[2L]],
        function_id = key,
        package = index$package, value = value, comments = member_callable_comments(fn),
        description = description
    )
}

# Source functions carry the physical lines kept by the document parser.
# Capture the small roxygen block before compacting the callable for resolve.
member_callable_comments <- function(fn) {
    if (!member_head(fn, "function") || length(fn) < 4L || !inherits(fn[[4L]], "srcref")) return(NULL)
    ref <- fn[[4L]]
    file <- attr(ref, "srcfile", exact = TRUE)
    if (!is.environment(file)) return(NULL)
    stored <- member_binding(file, "lines")
    if (!identical(stored[[1L]], "value")) return(NULL)
    lines <- stored[[2L]]
    ref <- unclass(ref)
    if (!is.integer(ref) || length(ref) < 7L || !is.character(lines) || is.object(lines)) return(NULL)
    row <- ref[[7L]]
    if (is.na(row) || row < 2L || row > length(lines) || !grepl("^\\s*#'", lines[[row - 1L]])) return(NULL)
    first <- detect_comments(lines, row - 1L) + 1L
    lines[seq.int(first, row - 1L)]
}

member_r6_description <- function(value) {
    role <- if (!is.null(value$r6_role)) switch(value$r6_role,
        instance = "R6 instance", private = "Private environment", super = "Superclass methods",
        generator = "R6 class generator", constructor = "Create an R6 instance",
        active = "Active bindings", enclosure = "Enclosing environment", clone = "Clone an R6 instance",
        NULL)
    describe <- function(role, class, member = FALSE) {
        if (is.null(role)) return(NULL)
        if (is.null(class$name)) return(role)
        description <- sprintf(if (member) "%s of R6 class `%s`" else "%s of `%s`", role, class$name)
        if (isTRUE(nzchar(class$package))) description <- sprintf("%s (package `%s`)", description, class$package)
        description
    }
    kind <- value$r6_member_kind
    if (!is.null(kind)) kind <- paste0(toupper(substr(kind, 1L, 1L)), substring(kind, 2L))
    descriptions <- c(describe(kind, value$r6_owner, TRUE), describe(role, value$r6_class))
    if (length(descriptions)) paste(descriptions, collapse = "\n\n") else NULL
}

member_symbol <- function(uri, workspace, document, location) {
    if (is.null(location)) {
        return(NULL)
    }
    resolved <- member_resolve_cursor(
        uri, workspace, document,
        location$point, location$cursor, location$cursor$token
    )
    if (is.null(resolved)) {
        return(NULL)
    }
    value <- resolved$value
    if (!is.null(value$s4_generator)) {
        constructor <- member_s4_constructor(value$s4_generator, resolved$index, resolved$bindings, resolved$budget)
        if (!is.null(constructor)) value <- constructor
    }
    member_symbol_info(location$cursor$token, value, value$function_key, resolved$index)
}

member_symbol_documentation <- function(workspace, symbol, uri) {
    key <- symbol$function_id
    doc <- if (length(symbol$comments)) {
        convert_comment_to_documentation(unlist(symbol$comments, use.names = FALSE))
    } else if (is.function(workspace$get_documentation) && isTRUE(nzchar(symbol$package)) &&
            is.character(key) && length(key) == 1L && !is.na(key)) {
        call_with_optional_uri(workspace$get_documentation, key, symbol$package, isf = TRUE, uri = uri)
    }
    if (is.null(symbol$description)) return(doc)
    if (is.list(doc)) {
        doc$description <- paste(c(symbol$description, doc$description), collapse = "\n\n")
        if (!is.null(doc$markdown)) doc$markdown <- paste(symbol$description, doc$markdown, sep = "\n\n")
        doc
    } else {
        paste(c(symbol$description, doc), collapse = "\n\n")
    }
}

# Find a complete member token, even when hovering inside a backtick name.
# Recovery confirms that the member operator belongs to R syntax, not a quote
# or comment. Columns stay in code points until the provider returns LSP ranges.
member_hover_location <- function(document, point) {
    if (point$row < 0L || point$row >= document$nline) {
        return(NULL)
    }
    if (substr(document$line0(point$row), point$col + 1L, point$col + 1L) == "`") {
        point$col <- point$col + 1L
    }
    cursor <- member_cursor(document, point)
    if (is.null(cursor) || point$col < cursor$start || cursor$end <= cursor$start) {
        return(NULL)
    }
    if (identical(cursor$accessor, "[[") && !member_bracket_key_closed(document, point$row, cursor$end)) {
        return(NULL)
    }
    token <- substr(document$line0(point$row), cursor$start + 1L, cursor$end)
    name <- tryCatch(member_name(str2lang(token)), error = function(e) NULL)
    if (is.null(name) || !nzchar(name)) {
        return(NULL)
    }
    cursor$token <- name
    list(
        point = list(row = point$row, col = cursor$end), cursor = cursor,
        range = list(
            start = list(row = point$row, col = cursor$start),
            end = list(row = point$row, col = cursor$end)
        )
    )
}

member_uncomment_line <- function(line) {
    .Call(
        "uncomment_lines_c",
        PACKAGE = "languageserver",
        as.character(line), 0L, length(line) - 1L, 0L
    )
}

document_r_bounds <- function(document, row) {
    if (is.null(row) || row < 0L || row >= document$nline) {
        return(NULL)
    }
    if (!isTRUE(document$is_rmarkdown)) {
        return(c(0L, document$nline - 1L))
    }
    run <- literate_r_run_at(document$regions, row)
    if (is.null(run)) NULL else c(run$start_line - 1L, run$end_line - 1L)
}

document_uncomment_lines <- function(document, start_row, end_row = start_row,
    bounds = document_r_bounds(document, start_row)) {
    if (is.null(bounds)) {
        return(character())
    }
    start_row <- max(as.integer(start_row), bounds[[1L]])
    end_row <- min(as.integer(end_row), bounds[[2L]])
    .Call(
        "uncomment_lines_c",
        PACKAGE = "languageserver",
        document$content, start_row, end_row, bounds[[1L]]
    )
}

# A hover on a string denotes one named element only when the key fills the
# entire subscript. Inspect continuations within the same R cell.
member_bracket_key_closed <- function(document, row, end) {
    rest <- substring(document$line0(row), end + 1L)
    closes <- "^\\s*\\]\\s*\\]"
    if (grepl(closes, rest, perl = TRUE)) return(TRUE)
    if (!grepl("^[ \t]*\\]?[ \t]*(#.*)?$", rest)) return(FALSE)
    bounds <- document_r_bounds(document, row)
    if (row >= bounds[[2L]]) return(FALSE)
    lines <- document_uncomment_lines(document, row, min(row + 128L, bounds[[2L]]), bounds)
    lines[[1L]] <- substring(lines[[1L]], end + 1L)
    grepl(closes, paste(lines, collapse = "\n"), perl = TRUE)
}

document_call_callee <- function(document, opening) {
    row <- opening$row
    bounds <- document_r_bounds(document, row)
    if (is.null(bounds)) {
        return(NULL)
    }
    col <- nchar(sub("[ \t]+$", "", substr(document$line0(row), 1L, opening$col)))
    if (col > 0L) {
        return(list(row = row, col = col))
    }
    start_row <- max(bounds[[1L]], row - 128L)
    if (start_row >= row) {
        return(NULL)
    }
    # R permits whitespace/newlines/comments between a callee and its call parenthesis.
    uncommented <- document_uncomment_lines(document, start_row, row - 1L, bounds)
    for (r in seq.int(row - 1L, start_row)) {
        col <- nchar(sub("[ \t]+$", "", uncommented[[r - start_row + 1L]]))
        if (col > 0L) {
            return(list(row = r, col = col))
        }
    }
    NULL
}

member_call_location <- function(document, point, call = document$detect_call(point), symbols = FALSE) {
    opening <- call$opening
    bounds <- document_r_bounds(document, point$row)
    if (is.null(opening) || is.null(bounds) ||
            opening$row < bounds[[1L]] || opening$row > bounds[[2L]]) {
        return(NULL)
    }
    callee <- if ("callee" %in% names(call)) call$callee else document_call_callee(document, opening)
    if (is.null(callee)) {
        return(NULL)
    }
    if (substr(document$line0(callee$row), callee$col, callee$col) == "]") {
        # The call scanner points just after the final ]. Walk past the two
        # delimiters to the literal, allowing comments and line breaks.
        for (i in seq_len(2L)) {
            if (is.null(callee) || substr(document$line0(callee$row), callee$col, callee$col) != "]") return(NULL)
            callee <- document_call_callee(document, list(row = callee$row, col = callee$col - 1L))
        }
        if (is.null(callee)) return(NULL)
        location <- member_hover_location(document, callee)
        if (identical(location$cursor$accessor, "[[") && location$cursor$end == callee$col) return(location)
        return(NULL)
    }
    location <- member_hover_location(document, callee)
    if (is.null(location) && symbols) location <- member_symbol_location(document, callee)
    location
}

# Reuse cursor recovery for class references, callable bindings and aliases.
# Providers accept this path only when inference identifies class metadata.
member_symbol_location <- function(document, point) {
    if (!check_scope(document$uri, document, point)) return(NULL)
    semantic <- document$parse_data$semantic_data
    col <- document$to_lsp_position(point$row, point$col)$character
    if (identical(document$version, document$parse_data$version) &&
            semantic_in_comment_or_string(semantic, point$row, col, strict_comment_start = TRUE)) {
        return(NULL)
    }
    token <- document$detect_token(point)
    if (!token$accessor %in% c("", "::") || !nzchar(token$token)) return(NULL)
    bounds <- token$range
    row <- bounds$end$row
    start <- bounds$start$col
    end <- bounds$end$col
    if (identical(token$accessor, "::")) {
        start <- start + nchar(token$package) + 2L
        bounds$start$col <- start
    }
    list(point = list(row = row, col = end), range = bounds,
        cursor = list(operator_row = row, operator = start + 1L, start = start, end = end,
            accessor = NULL, token = token$token, before = substr(document$line0(row), 1L, start),
            s4_new = token$token == "new" && (is.null(token$package) || identical(token$package, "methods"))))
}

member_constructor_symbol <- function(uri, workspace, document, location) {
    if (is.null(location)) return(NULL)
    symbol <- member_symbol(uri, workspace, document, location)
    if (is.null(symbol$value$s7_generator) && !isTRUE(symbol$value$s4_constructor) && !identical(symbol$value$r6_role, "constructor")) return(NULL)
    symbol
}

member_call_symbol <- function(uri, workspace, document, location) {
    if (is.null(location)) return(NULL)
    symbol <- member_symbol(uri, workspace, document, location)
    if (!is.null(location$cursor$accessor)) {
        # Keep unresolved member calls out of bare-name and S3 lookup.
        return(if (is.null(symbol)) list() else symbol)
    }
    if ((isTRUE(symbol$value$s7_bound_name) && !is.null(symbol$signature)) ||
            !is.null(symbol$value$s7_generator) || !is.null(symbol$value$s7_dispatch_args) ||
            isTRUE(symbol$value$s4_constructor) || identical(symbol$value$r6_role, "constructor") ||
            !is.null(symbol$value$r6_owner) || !is.null(symbol$value$refclass_instance) || isTRUE(symbol$value$extracted_callable)) return(symbol)
    NULL
}

# A tag in a custom initializer or constructor is a function parameter, even
# when it happens to share a property's spelling.
member_constructor_parameter_origin <- function(uri, workspace, document, point, name, call = document$detect_call(point)) {
    location <- member_call_location(document, point, call, symbols = TRUE)
    if (is.null(location)) return(NULL)
    symbol <- member_resolve_cursor(uri, workspace, document, location$point, location$cursor,
        location$cursor$token, origins = TRUE, origin_name = name)
    value <- symbol$value
    if (!is.null(value$s4_generator)) {
        value <- member_s4_constructor(value$s4_generator, symbol$index, symbol$bindings, symbol$budget)
    }
    if (isTRUE(value$s4_constructor)) return(value$constructor_origins[[name]])
    class <- value$s7_generator
    if (is.null(class) || !name %in% names(class$constructor[[2L]])) return(NULL)
    property <- class$properties[[name]]
    while (!is.null(class) && !isTRUE(class$custom)) {
        if (name %in% class$own) return(property$origin)
        class <- class$parent
    }
    NULL
}

member_call_arguments <- function(uri, workspace, document, point, token, call = document$detect_call(point),
    quoted = FALSE) {
    location <- member_call_location(document, point, call,
        symbols = member_symbol_required(workspace, document))
    symbol <- member_call_symbol(uri, workspace, document, location)
    if (is.null(symbol)) return(NULL)
    if (is.null(symbol$signature)) return(list())
    formals <- as.character(names(symbol$formals))
    slots <- setdiff(symbol$value$constructor_slots, formals)
    match_signature <- symbol$signature
    if (length(slots)) {
        # Slots are optional named arguments accepted through dots. Placing
        # them after dots gives them exact-only matching in completion.
        formals <- c(formals, slots)
        match_signature <- paste0("f(", paste(vapply(formals, quote_completion_name, ""), collapse = ", "), ")")
    }
    first <- 0L
    content <- document$content
    if (document$is_rmarkdown) {
        bounds <- document_r_bounds(document, point$row)
        first <- bounds[[1L]]
        content <- content[seq.int(first + 1L, bounds[[2L]] + 1L)]
    }
    match <- detect_active_parameter(content, call$opening$row - first, call$opening$col,
        point$row - first, point$col, match_signature, completion = TRUE)
    if (!is.list(match)) return(list())
    active <- formals[match[[1L]] + 1L]
    args <- formals[match[[2L]]]
    if (quoted || isTRUE(match[[3L]])) args <- character()
    args <- args[match_with(args, token)]
    identity <- list(signature = symbol$signature, package = symbol$package, function_id = symbol$function_id,
        comments = symbol$comments, context_uri = uri)
    tag_range <- NULL
    if (length(args) && nzchar(token)) {
        tag <- document$detect_token(point)$range
        rest <- substring(document$line0(tag$end$row), tag$end$col + 1L)
        if (grepl("^[ \t]*=(?!=)", rest, perl = TRUE)) {
            tag_range <- range(document$to_lsp_position(tag$start$row, tag$start$col),
                document$to_lsp_position(tag$end$row, tag$end$col))
        }
    }
    items <- lapply(seq_along(args), function(i) {
        name <- args[[i]]
        spelling <- quote_completion_name(name)
        item <- list(label = name, kind = CompletionItemKind$Variable,
            detail = symbol$signature, sortText = sprintf("%s%03d", sort_prefixes$arg, i),
            insertText = paste0(spelling, " = "),
            insertTextFormat = InsertTextFormat$PlainText,
            data = c(list(type = "member", parameter = name), identity))
        if (!is.null(tag_range)) item$textEdit <- text_edit(tag_range, spelling)
        if (!is.null(symbol$description)) item$documentation <- list(kind = "markdown", value = symbol$description)
        item
    })
    if (!length(active) || is.na(active) || !active %in% names(symbol$formals) || active == "...") return(items)
    line <- document$line0(point$row)
    if (quoted) {
        key <- .Call("scan_string_completion_c", line, as.integer(point$col), PACKAGE = "languageserver")
        if (is.null(key)) return(list())
        quote <- substr(line, key$start + 1L, key$start + 1L)
        token <- tryCatch(str2lang(paste0(quote, key$token, quote)), error = function(e) key$token)
    }
    choices <- argument_value_items(extract_default_values(symbol$formals[[active]]),
        symbol$label, symbol$package, active, token, uri)
    for (i in seq_along(choices)) {
        choices[[i]]$data <- c(list(type = "member_value", argument = active), identity)
        if (quoted) {
            spelling <- encodeString(choices[[i]]$label, quote = quote)
            choices[[i]]$insertText <- NULL
            replacement <- range(document$to_lsp_position(point$row, key$start + 1L),
                document$to_lsp_position(point$row, key$end - as.integer(key$closed)))
            choices[[i]]$textEdit <- text_edit(replacement, substr(spelling, 2L, nchar(spelling) - 1L))
        }
    }
    c(items, choices)
}

member_argument_location <- function(document, point, symbols = FALSE) {
    bounds <- document_r_bounds(document, point$row)
    if (is.null(bounds) || !check_scope(document$uri, document, point)) {
        return(NULL)
    }
    token <- document$detect_token(point)
    end <- token$range$end
    if (!nzchar(token$token) || !identical(token$accessor, "")) {
        return(NULL)
    }
    last_row <- min(bounds[[2L]], end$row + 128L)
    uncommented <- document_uncomment_lines(document, end$row, last_row, bounds)
    if (!length(uncommented) || end$col > nchar(uncommented[[1L]])) {
        return(NULL)
    }
    line <- uncommented[[1L]]
    named <- FALSE
    for (row in seq.int(end$row, last_row)) {
        rest <- if (row == end$row) substring(line, end$col + 1L) else uncommented[[row - end$row + 1L]]
        if (grepl("^[ \t]*$", rest)) next
        named <- grepl("^[ \t]*=(?!=)", rest, perl = TRUE)
        break
    }
    if (!named) {
        return(NULL)
    }
    location <- member_call_location(document, point, symbols = symbols)
    if (is.null(location)) {
        return(NULL)
    }
    location$parameter <- token$token
    location$range <- token$range
    location
}

member_hover_reply <- function(id, uri, workspace, document, location,
    symbol = member_symbol(uri, workspace, document, location)) {
    doc <- member_symbol_documentation(workspace, symbol, uri)
    signature <- symbol$signature
    contents <- NULL
    if (!is.null(location$parameter)) {
        if (is.list(doc)) contents <- argument_hover_contents(doc, signature, location$parameter)
        if (!is.null(symbol$description)) {
            if (is.null(contents) && !is.null(signature)) contents <- sprintf("```r\n%s\n```", signature)
            contents <- c(contents, symbol$description)
        }
    } else {
        description <- if (is.character(doc)) {
            doc
        } else if (is.list(doc)) {
            if (is.null(doc$markdown)) doc$description else doc$markdown
        }
        value <- symbol$value
        detail <- if (!is.null(signature)) {
            signature
        } else if (isTRUE(value$known_literal)) {
            deparse1(value$literal, collapse = "\n")
        } else if (length(value$type)) {
            paste(value$type, collapse = " | ")
        }
        contents <- c(if (!is.null(detail)) sprintf("```r\n%s\n```", detail), description)
    }
    if (is.null(contents)) {
        return(Response$new(id))
    }
    bounds <- location$range
    Response$new(id, result = list(contents = contents, range = range(
        document$to_lsp_position(bounds$start$row, bounds$start$col),
        document$to_lsp_position(bounds$end$row, bounds$end$col)
    )))
}
