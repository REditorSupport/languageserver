# All member providers share receiver inference and method identity. Metadata
# contains syntax only; resolving a signature never calls the selected member.
member_symbol_info <- function(label, value, key, index) {
    if (!is.null(value$metadata)) {
        package_index <- member_lookup(index$namespace_indices, value$metadata)
        if (!is.null(package_index)) index <- package_index
    }
    if (!is.null(value$function_key)) key <- value$function_key
    fn <- value$function_expr
    if (is.null(fn)) fn <- member_definition(index$definitions, key)
    if (!is.null(key) && key %in% index$native_factories) {
        fn <- member_returned_function(fn)$fn
    }
    display <- quote_completion_name(label)
    signature <- if (member_head(fn, "function")) get_signature(display, fn) else NULL
    delegated <- member_lookup(index$delegation, key)
    if (!is.null(delegated)) key <- delegated$original
    list(
        label = label, signature = signature, function_id = key,
        package = index$package, value = value,
        description = member_r6_description(value)
    )
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
    member_symbol_info(location$cursor$token, value, value$function_key, resolved$index)
}

member_symbol_documentation <- function(workspace, symbol, uri) {
    key <- symbol$function_id
    if (is.null(symbol) || is.null(workspace$get_documentation) || !nzchar(symbol$package) || !is.character(key) ||
        length(key) != 1L || is.na(key)) {
        return(symbol$description)
    }
    doc <- call_with_optional_uri(workspace$get_documentation,
        key, symbol$package,
        isf = TRUE, uri = uri
    )
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
    token <- substr(document$line0(point$row), cursor$start + 1L, cursor$end)
    name <- tryCatch(member_name(parse(text = token)[[1L]]), error = function(e) NULL)
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

member_call_location <- function(document, point, call = document$detect_call(point), symbols = FALSE) {
    opening <- call$opening
    if (is.null(opening) || !check_r_region(document, point)) {
        return(NULL)
    }
    row <- opening$row
    col <- opening$col
    # R permits whitespace/newlines between a member and its call parenthesis.
    for (i in seq_len(129L)) {
        prefix <- substr(document$line0(row), 1L, col)
        col <- nchar(sub("[ \\t]+$", "", prefix))
        if (col > 0L) {
            location <- member_hover_location(document, list(row = row, col = col))
            if (is.null(location) && symbols) location <- member_symbol_location(document, list(row = row, col = col))
            return(location)
        }
        row <- row - 1L
        if (row < 0L || !check_r_region(document, list(row = row, col = 0L))) break
        col <- nchar(document$line0(row))
    }
    NULL
}

# Reuse cursor recovery for class references, callable bindings and aliases.
# Providers accept this path only when inference identifies class metadata.
member_symbol_location <- function(document, point) {
    if (!check_scope(document$uri, document, point)) return(NULL)
    semantic <- document$parse_data$semantic_data
    col <- document$to_lsp_position(point$row, point$col)$character
    excluded <- semantic$lines == point$row & semantic$cols <= col &
        ((semantic$types == SemanticTokenTypes$comment & semantic$cols + semantic$lengths >= col) |
                (semantic$types == SemanticTokenTypes$string & semantic$cols + semantic$lengths > col))
    if (identical(document$version, document$parse_data$version) && any(excluded)) return(NULL)
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
            accessor = NULL, token = token$token, before = substr(document$line0(row), 1L, start)))
}

member_constructor_symbol <- function(uri, workspace, document, location) {
    if (is.null(location)) return(NULL)
    symbol <- member_symbol(uri, workspace, document, location)
    if (is.null(symbol$value$s7_generator) && !identical(symbol$value$r6_role, "constructor")) return(NULL)
    symbol
}

member_constructor_arguments <- function(uri, workspace, document, point, token) {
    location <- member_call_location(document, point, symbols = TRUE)
    symbol <- member_constructor_symbol(uri, workspace, document, location)
    if (is.null(symbol)) return(NULL)
    args <- names(symbol$value$function_expr[[2L]])
    args <- setdiff(args, "...")
    args <- args[match_with(args, token)]
    lapply(seq_along(args), function(i) {
        list(label = args[[i]], kind = CompletionItemKind$Variable,
            detail = symbol$signature, sortText = sprintf("%s%03d", sort_prefixes$arg, i),
            documentation = if (!is.null(symbol$description)) list(kind = "markdown", value = symbol$description),
            insertText = paste0(quote_completion_name(args[[i]]), " = "),
            insertTextFormat = InsertTextFormat$PlainText, data = list(type = "member", signature = symbol$signature))
    })
}

member_argument_location <- function(document, point, symbols = FALSE) {
    if (!check_scope(document$uri, document, point)) {
        return(NULL)
    }
    token <- document$detect_token(point)
    end <- token$range$end
    if (!nzchar(token$token) || !identical(token$accessor, "") ||
        !grepl("^[ \\t]*=(?!=)", substring(document$line0(end$row), end$col + 1L), perl = TRUE)) {
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
            paste(deparse(value$literal), collapse = "\n")
        } else if (length(value$type)) paste(value$type, collapse = " | ")
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
