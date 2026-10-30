#' Build shared, serializable syntax and navigation facts in the parse worker
#' @noRd
syntax_parse_data <- function(data) {
    columns <- c("id", "parent", "token", "text", "terminal", "line1", "col1", "line2", "col2")
    if (is.null(data) || !nrow(data)) {
        data <- list(id = integer(), parent = integer(), token = character(),
            text = character(), terminal = logical(), line1 = integer(),
            col1 = integer(), line2 = integer(), col2 = integer())
        ordered <- integer()
    } else {
        data <- as.list(data[columns])
        # Match xmlparsedata's opening-node order, including nested nodes with
        # identical ranges. The native builder also places detached comments
        # according to their ranges, as XML does, rather than their parent id.
        ordered <- order(data$line1, data$col1, -data$line2, -data$col2, data$terminal)
    }
    adjacency <- .Call("syntax_index_c", data, as.integer(ordered), PACKAGE = "languageserver")
    index <- c(data, adjacency)
    index$maxcol <- if (length(data$col1)) max(data$col1, data$col2) + 1L else 1L
    rows <- index$terminals
    index$tokens <- list(line1 = data$line1[rows], col1 = data$col1[rows],
        line2 = data$line2[rows], col2 = data$col2[rows], is_string = data$token[rows] == "STR_CONST")
    index
}

#' Serialize parser XML without allocating intermediate R tags and data frames
#' @noRd
syntax_xml <- function(syntax) {
    result <- .Call("syntax_xml_c", syntax, PACKAGE = "languageserver")
    if (!is.null(result)) return(result)
    columns <- c("token", "text", "terminal", "line1", "col1", "line2", "col2")
    data <- list2DF(syntax[columns])
    data$parent <- integer(nrow(data))
    xmlparsedata::xml_parse_data(data)
}

#' Direct children of a syntax row, in source order
#' @noRd
syntax_children <- function(index, row) {
    if (!row) return(index$roots)
    first <- index$child_start[[row]]
    last <- index$child_end[[row]]
    if (first > last) integer() else index$children[seq.int(first, last)]
}

#' Convert occurrences on each Unicode line in one batch
#' @noRd
syntax_unit_columns <- function(content, lines, start, end, non_ascii = NULL) {
    if (is.null(non_ascii)) {
        non_ascii <- nchar(content, type = "bytes") != nchar(content, type = "chars")
    }
    selected <- which(!is.na(lines) & non_ascii[lines])
    for (indices in split(selected, lines[selected])) {
        units <- code_point_to_unit(content[[lines[[indices[[1L]]]]]],
            c(start[indices], end[indices]))
        n <- length(indices)
        start[indices] <- units[seq_len(n)]
        end[indices] <- units[n + seq_len(n)]
    }
    list(start = start, end = end)
}

#' Index local declaration histories without changing legacy navigation scope
#' @noRd
navigation_parse_data <- function(syntax) {
    records <- .Call("navigation_index_c", syntax, PACKAGE = "languageserver")
    make_groups <- function(rows, names) {
        list2env(split(rows, names), hash = TRUE, parent = emptyenv())
    }
    records$by_name <- make_groups(seq_along(records$node), syntax$text[records$name])
    scope_names <- paste(records$scope, syntax$text[records$scope_name], sep = "\r")
    records$by_scope_name <- make_groups(seq_along(records$scope), scope_names)
    records$cache <- new.env(hash = TRUE, parent = emptyenv())
    records$bytes <- sum(vapply(list(records$by_name, records$by_scope_name), function(groups) {
        as.numeric(utils::object.size(as.list(groups)))
    }, numeric(1L)))
    records
}

#' Find a token using the shared terminal interval index
#' @noRd
navigation_token <- function(parse_data, line, col) {
    syntax <- parse_data$syntax_data
    if (is.null(syntax)) return(NULL)
    rows <- syntax$terminals
    selected <- .Call("navigation_find_token_c", PACKAGE = "languageserver",
        syntax$tokens$line1, syntax$tokens$col1, syntax$tokens$line2, syntax$tokens$col2,
        syntax$tokens$is_string, as.integer(c(line, col)))
    if (selected) rows[[selected]] else 0L
}

#' Query declaration histories in the same order as the legacy XPath union
#' @noRd
navigation_definitions <- function(parse_data, line, col, name,
    mode = "definition", token = 0L, last = FALSE) {
    syntax <- parse_data$syntax_data
    index <- parse_data$navigation_data
    if (is.null(syntax) || is.null(index)) return(NULL)
    candidates <- get0(name, envir = index$by_name, inherits = FALSE, ifnotfound = integer())
    result <- .Call("navigation_select_c", syntax, index, candidates,
        as.integer(c(line, col, token)), match(mode, c("definition", "hover", "signature")),
        as.logical(last), PACKAGE = "languageserver")
    if (mode == "signature") return(result)

    scopes <- .Call("syntax_enclosing_c", syntax, as.integer(c(line, col)), PACKAGE = "languageserver")
    for (scope in scopes) {
        selected <- get0(paste(scope, name, sep = "\r"), envir = index$by_scope_name,
            inherits = FALSE, ifnotfound = integer())
        selected <- selected[syntax$line1[index$scope_name[selected]] <= line]
        if (length(selected)) {
            nodes <- if (mode == "hover") index$scope_hover[selected] else index$scope_name[selected]
            result <- if (last) utils::tail(nodes, 1L) else c(result, nodes)
        }
    }
    if (last) result else unique(result)
}

#' Syntax ranges use code points and inclusive parser endpoints
#' @noRd
navigation_range <- function(document, syntax, row) {
    range(start = document$to_lsp_position(syntax$line1[[row]] - 1L, syntax$col1[[row]] - 1L),
        end = document$to_lsp_position(syntax$line2[[row]] - 1L, syntax$col2[[row]]))
}

#' Format and memoize a local function's header without allocating its body
#' @noRd
navigation_signature <- function(document, parse_data, row, symbol) {
    index <- parse_data$navigation_data
    key <- paste0("signature:", row, ":", symbol)
    found <- get0(key, envir = index$cache, inherits = FALSE)
    if (!is.null(found)) return(found)
    syntax <- parse_data$syntax_data
    closing <- syntax$function_close[[row]]
    text <- get_range_text(document$content,
        syntax$line1[[row]], syntax$col1[[row]], syntax$line2[[closing]], syntax$col2[[closing]])
    found <- analysis_function_header(document, text, symbol)$signature
    assign(key, found, envir = index$cache)
    found
}

#' Documentation is immutable for the lifetime of an accepted parse
#' @noRd
navigation_documentation <- function(document, parse_data, line) {
    cache <- parse_data$navigation_data$cache
    key <- paste0("documentation:", line)
    if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache))
    first <- detect_comments(document$content, line - 1L) + 1L
    comments <- if (first < line) document$content[seq.int(first, line - 1L)] else character()
    doc <- analysis_documentation(document, comments)
    assign(key, doc, envir = cache)
    doc
}

#' Resolve preceding call tokens for named-argument hover
#' @noRd
navigation_argument_call <- function(syntax, row) {
    parent <- syntax$parent_row[[row]]
    children <- syntax_children(syntax, parent)
    children <- children[seq_len(match(row, children) - 1L)]
    expressions <- children[syntax$token[children] == "expr"]
    for (expression in expressions) {
        tokens <- syntax_children(syntax, expression)
        calls <- tokens[syntax$token[tokens] == "SYMBOL_FUNCTION_CALL"]
        if (length(calls)) {
            packages <- tokens[syntax$token[tokens] == "SYMBOL_PACKAGE"]
            return(list(name = syntax$text[[calls[[1L]]]],
                    package = if (length(packages)) syntax$text[[packages[[1L]]]]))
        }
    }
    NULL
}

#' Whether a symbol follows a dollar operator amongst its direct siblings
#' @noRd
navigation_after_dollar <- function(syntax, row) {
    children <- syntax_children(syntax, syntax$parent_row[[row]])
    children <- children[seq_len(match(row, children) - 1L)]
    # getParseData uses a quoted operator; XML uses its named equivalent.
    any(syntax$token[children] %in% c("'$'", "OP-DOLLAR"))
}

#' Indexed local hover, retaining package and member fallbacks
#' @noRd
navigation_hover <- function(uri, workspace, document, parse_data, line, col) {
    syntax <- parse_data$syntax_data
    row <- navigation_token(parse_data, line, col)
    empty <- list(contents = NULL, resolved = FALSE)
    if (!row) return(empty)
    kind <- syntax$token[[row]]
    name <- syntax$text[[row]]
    if (kind %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
        if (navigation_after_dollar(syntax, row)) return(empty)
        found <- navigation_definitions(parse_data, line, col, name, "hover", row, last = TRUE)
        if (!length(found)) return(empty)
        declaration <- found[[1L]]
        children <- syntax_children(syntax, declaration)
        assignment <- any(syntax$token[children] %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN"))
        functions <- if (assignment) children[syntax$function_close[children] > 0L] else integer()
        if (length(functions)) {
            first <- syntax$line1[[functions[[1L]]]]
            text <- navigation_signature(document, parse_data, functions[[1L]], name)
        } else {
            first <- syntax$line1[[declaration]]
            text <- trimws(document$content[[first]])
        }
        doc <- navigation_documentation(document, parse_data, first)
        doc_string <- if (is.character(doc)) doc else if (is.list(doc)) {
            if (is.null(doc$markdown)) doc$description else doc$markdown
        }
        return(list(contents = c(sprintf("```r\n%s\n```", text), doc_string), resolved = TRUE))
    }
    if (kind == "SYMBOL_SUB") {
        call <- navigation_argument_call(syntax, row)
        if (is.null(call)) return(empty)
        if (is.null(call$package)) {
            found <- navigation_definitions(parse_data, line, col, call$name, "signature", last = TRUE)
            if (length(found)) {
                declaration <- found[[1L]]
                doc <- navigation_documentation(document, parse_data, syntax$line1[[declaration]])
                contents <- NULL
                if (is.list(doc)) {
                    description <- doc$arguments[[name]]
                    if (!is.null(description)) contents <- c(
                        sprintf("```r\n%s\n```", navigation_signature(document, parse_data, declaration, call$name)),
                        sprintf("`%s` - %s", name, description))
                }
                return(list(contents = contents, resolved = TRUE))
            }
        }
        return(list(contents = function_argument_hover_contents(workspace, call$name, call$package, name, uri), resolved = TRUE))
    }
    if (kind == "SYMBOL_FORMALS") {
        declaration <- syntax$parent_row[[row]]
        doc <- navigation_documentation(document, parse_data, syntax$line1[[declaration]])
        contents <- if (is.list(doc) && !is.null(doc$arguments[[name]])) sprintf("`%s` - %s", name, doc$arguments[[name]])
        return(list(contents = contents, resolved = TRUE))
    }
    if (kind == "SYMBOL_PACKAGE") {
        contents <- if (length(find.package(name, quiet = TRUE))) {
            desc <- utils::packageDescription(name, fields = c("Title", "Description"))
            sprintf("**%s**\n\n%s", desc$Title, gsub("\\s*\n\\s*", " ", desc$Description))
        } else {
            sprintf("Package `%s` is not installed.", name)
        }
        return(list(contents = contents, resolved = TRUE))
    }
    operators <- substr(kind, 1L, 1L) == "'" || startsWith(kind, "OP-")
    list(contents = NULL, resolved = kind %in% c("SLOT", "STR_CONST", "COMMENT") || operators)
}
