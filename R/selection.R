#' The response to a textDocument/selectionRange Request
#'
#' @noRd
selection_range_reply <- function(id, uri, workspace, document, points) {
    result <- NULL

    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(Response$new(id))
    }

    syntax <- parse_data$syntax_data
    cursor_range <- function(point) {
        pos <- document$to_lsp_position(point$row, point$col)
        list(range = range(pos, pos), parent = NULL)
    }
    if (!is.null(syntax)) {
        result <- lapply(points, function(point) {
            if (!check_r_region(document, point)) return(cursor_range(point))
            row <- navigation_token(parse_data, point$row + 1L, point$col + 1L)
            if (!row) return(cursor_range(point))
            if (syntax$line2[[row]] == point$row + 1L && syntax$col2[[row]] == point$col) {
                next_row <- navigation_token(parse_data, point$row + 1L, point$col + 2L)
                if (next_row && syntax$line1[[next_row]] == point$row + 1L &&
                        syntax$col1[[next_row]] == point$col + 1L) row <- next_row
            }
            ancestors <- collections::stack()
            while (row) {
                ancestors$push(row)
                row <- syntax$parent_row[[row]]
            }
            result <- NULL
            while (ancestors$size()) {
                row <- ancestors$pop()
                result <- list(range = navigation_range(document, syntax, row), parent = result)
            }
            result
        })
        return(Response$new(id, result = result))
    }

    xdoc <- parse_data_xml(parse_data)
    if (!is.null(xdoc)) {
        result <- lapply(points, function(point) {
            if (!check_r_region(document, point)) return(cursor_range(point))
            row <- point$row + 1
            col <- point$col + 1
            token <- xdoc_find_token(xdoc, row, col)
            if (!inherits(token, "xml_missing") && as.integer(xml_attr(token, "line2")) == row &&
                    as.integer(xml_attr(token, "col2")) == col - 1L) {
                following <- xdoc_find_token(xdoc, row, col + 1L)
                if (!inherits(following, "xml_missing") && as.integer(xml_attr(following, "line1")) == row &&
                        as.integer(xml_attr(following, "col1")) == col) token <- following
            }
            nodes <- xml_find_all_(token, "ancestor-or-self::*[@line1]")
            if (!length(nodes)) return(cursor_range(point))
            ranges <- lapply(nodes, function(token) {
                range(
                    start = document$to_lsp_position(
                        row = as.integer(xml_attr(token, "line1")) - 1,
                        col = as.integer(xml_attr(token, "col1")) - 1),
                    end = document$to_lsp_position(
                        row = as.integer(xml_attr(token, "line2")) - 1,
                        col = as.integer(xml_attr(token, "col2")))
                )
            })
            selection_range <- NULL
            for (item in ranges) {
                selection_range <- list(
                    range = item,
                    parent = selection_range
                )
            }
            selection_range
        })
    }

    Response$new(id, result = result)
}
