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
    for (symbol in names(definitions)) {
        definition <- definitions[[symbol]]
        if (!identical(definition$type, "function")) next

        definition_row <- definition$range$start$line
        xpath <- glue(
            signature_xpath,
            row = definition_row + 1L,
            token_quote = xml_single_quote(symbol)
        )
        function_nodes <- xml_find_all(xdoc, xpath)
        if (!length(function_nodes)) next
        function_node <- function_nodes[[length(function_nodes)]]
        formal_nodes <- xml_find_all(function_node, "SYMBOL_FORMALS")
        if (!length(formal_nodes)) next

        documented <- roxygen_parameter_ranges(document, definition_row)
        if (!length(documented)) next

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

            if (any(vapply(ranges, lsp_range_contains, logical(1L), point = point))) {
                return(Response$new(id, result = list(ranges = ranges)))
            }
        }
    }

    Response$new(id, result = NULL)
}
