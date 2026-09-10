#' Test whether an LSP position is inside a range
#' @noRd
lsp_range_contains <- function(target_range, point) {
    after_start <- point$line > target_range$start$line ||
        (point$line == target_range$start$line &&
            point$character >= target_range$start$character)
    before_end <- point$line < target_range$end$line ||
        (point$line == target_range$end$line &&
            point$character <= target_range$end$character)
    after_start && before_end
}

#' Extract documented parameter ranges from a contiguous roxygen block
#' @noRd
roxygen_parameter_ranges <- function(document, definition_row) {
    rows <- integer()
    row <- definition_row - 1L
    while (row >= 0L && grepl("^\\s*#'", document$line0(row))) {
        rows <- c(row, rows)
        row <- row - 1L
    }

    result <- list()
    pattern <- "^\\s*#'\\s*@param\\s+([^[:space:]]+)"
    for (row in rows) {
        line <- document$line0(row)
        match <- regexec(pattern, line, perl = TRUE)[[1]]
        if (match[[1L]] == -1L) next

        lengths <- attr(match, "match.length")
        capture_start <- match[[2L]]
        capture <- substr(
            line,
            capture_start,
            capture_start + lengths[[2L]] - 1L
        )
        pieces <- strsplit(capture, ",", fixed = TRUE)[[1]]
        offset <- 0L
        for (piece in pieces) {
            name <- trimws(piece)
            if (!nzchar(name)) next
            local_start <- regexpr(name, piece, fixed = TRUE)[[1L]] - 1L
            start_col <- capture_start - 1L + offset + local_start
            result[[name]] <- c(result[[name]], list(range(
                document$to_lsp_position(row, start_col),
                document$to_lsp_position(row, start_col + nchar(name))
            )))
            offset <- offset + nchar(piece) + 1L
        }
    }
    result
}

#' Index definition and documentation intervals once per document version
#' @noRd
linked_editing_index <- function(parse_data, document) {
    cached <- parse_data$linked_editing_cache
    definitions <- parse_data$definitions
    if (!is.null(cached) && identical(cached$content, document$content) &&
            identical(cached$definitions, definitions)) return(cached)

    functions <- which(vapply(definitions, function(definition) {
        identical(definition$type, "function")
    }, logical(1L)))
    starts <- vapply(definitions[functions], function(definition) {
        as.integer(definition$range$start$line)
    }, integer(1L))
    ends <- vapply(definitions[functions], function(definition) {
        as.integer(definition$range$end$line)
    }, integer(1L))
    roxygen <- grepl("^\\s*#'", document$content)
    previous_non_roxygen <- cummax(ifelse(roxygen, 0L, seq_along(roxygen)))
    documented_starts <- starts
    preceding <- which(starts > 0L & starts <= length(document$content))
    documented_starts[preceding] <- previous_non_roxygen[starts[preceding]]
    cached <- list(
        content = document$content,
        definitions = definitions,
        symbols = names(definitions)[functions],
        starts = documented_starts,
        ends = ends,
        ranges = new.env(parent = emptyenv())
    )
    if (is.environment(parse_data)) parse_data$linked_editing_cache <- cached
    cached
}

#' The response to a textDocument/linkedEditingRange request
#'
#' Links roxygen @param names with the corresponding R function formal. This
#' makes correcting a parameter name update its documentation at the same time.
#' @noRd
linked_editing_range_reply <- function(id, uri, workspace, document, point) {
    internal_point <- document$from_lsp_position(point)
    if (!check_r_region(document, internal_point)) {
        return(Response$new(id, result = NULL))
    }

    parse_data <- current_parse_data(uri, workspace, document)
    if (is.null(parse_data)) return(NULL)
    xdoc <- parse_data$xml_doc
    if (is.null(xdoc)) return(Response$new(id, result = NULL))

    definitions <- parse_data$definitions
    indexed <- linked_editing_index(parse_data, document)
    candidates <- which(indexed$starts <= point$line & indexed$ends >= point$line)
    for (symbol in indexed$symbols[candidates]) {
        if (exists(symbol, envir = indexed$ranges, inherits = FALSE)) {
            linked <- get(symbol, envir = indexed$ranges, inherits = FALSE)
            for (ranges in linked) {
                if (any(vapply(ranges, lsp_range_contains, logical(1L), point = point))) {
                    return(Response$new(id, result = list(ranges = ranges)))
                }
            }
            next
        }
        definition <- definitions[[symbol]]
        definition_row <- definition$range$start$line
        documented <- roxygen_parameter_ranges(document, definition_row)
        if (!length(documented)) {
            assign(symbol, list(), envir = indexed$ranges)
            next
        }
        xpath <- glue(
            signature_xpath,
            row = definition_row + 1L,
            token_quote = xml_single_quote(symbol)
        )
        scopes <- xdoc_find_enclosing_scopes(xdoc, definition_row + 1L,
            definition$range$start$character + 1L)
        context <- if (length(scopes)) scopes[[1L]] else xdoc
        function_nodes <- xml_find_all(context, xpath)
        if (!length(function_nodes)) next
        function_node <- function_nodes[[length(function_nodes)]]
        formal_nodes <- xml_find_all(function_node, "SYMBOL_FORMALS")
        if (!length(formal_nodes)) next

        linked <- list()
        for (formal_node in formal_nodes) {
            name <- xml_text(formal_node)
            documentation_ranges <- documented[[name]]
            if (!length(documentation_ranges)) next

            line1 <- as.integer(xml_attr(formal_node, "line1"))
            col1 <- as.integer(xml_attr(formal_node, "col1"))
            line2 <- as.integer(xml_attr(formal_node, "line2"))
            col2 <- as.integer(xml_attr(formal_node, "col2"))
            formal_range <- range(
                document$to_lsp_position(line1 - 1L, col1 - 1L),
                document$to_lsp_position(line2 - 1L, col2)
            )
            ranges <- c(list(formal_range), documentation_ranges)
            linked[[length(linked) + 1L]] <- ranges
        }
        assign(symbol, linked, envir = indexed$ranges)
        for (ranges in linked) {
            if (any(vapply(ranges, lsp_range_contains, logical(1L), point = point))) {
                return(Response$new(id, result = list(ranges = ranges)))
            }
        }
    }

    Response$new(id, result = NULL)
}
