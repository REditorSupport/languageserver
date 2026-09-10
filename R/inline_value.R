#' Compare an internal code-point position with an LSP range
#' @noRd
position_is_in_lsp_range <- function(document, row, col, request_range) {
    point <- document$to_lsp_position(row, col)
    start <- request_range$start
    end <- request_range$end

    after_start <- point$line > start$line ||
        (point$line == start$line && point$character >= start$character)
    before_end <- point$line < end$line ||
        (point$line == end$line && point$character < end$character)
    after_start && before_end
}

#' The response to a textDocument/inlineValue request
#' @noRd
inline_value_reply <- function(id, uri, workspace, document, request_range) {
    parse_data <- current_parse_data(uri, workspace, document)
    if (is.null(parse_data)) return(NULL)
    xdoc <- parse_data$xml_doc
    if (is.null(xdoc)) return(Response$new(id, result = list()))

    indexed <- parse_data$range_data$variables
    if (!is.null(indexed)) {
        selected <- range_line_indices(
            indexed$line, request_range$start$line, request_range$end$line
        )
        selected <- selected[range_position_selected(
            indexed$line[selected], indexed$col[selected], request_range
        )]
        selected <- selected[nzchar(indexed$name[selected]) & indexed$name[selected] != "..."]
        # Construct the deduplication keys only for visible occurrences.
        selected <- selected[!duplicated(paste(
            indexed$line[selected], indexed$name[selected], sep = ":"
        ))]
        return(Response$new(id, result = lapply(selected, function(i) {
            list(
                range = range(
                    position(indexed$line[[i]], indexed$col[[i]]),
                    position(indexed$end_line[[i]], indexed$end_col[[i]])
                ),
                variableName = indexed$name[[i]],
                caseSensitiveLookup = TRUE
            )
        })))
    }

    start_line <- request_range$start$line + 1L
    end_line <- request_range$end$line + 1L
    if (request_range$end$character == 0L && end_line > start_line) {
        end_line <- end_line - 1L
    }
    nodes <- xml_find_all(
        xdoc,
        paste0(
            "//*[(self::SYMBOL or self::SYMBOL_FORMALS) and ",
            "@line1 >= ", start_line, " and @line1 <= ", end_line, "]"
        )
    )
    if (!length(nodes)) return(Response$new(id, result = list()))

    line1 <- as.integer(xml_attr(nodes, "line1"))
    col1 <- as.integer(xml_attr(nodes, "col1"))
    line2 <- as.integer(xml_attr(nodes, "line2"))
    col2 <- as.integer(xml_attr(nodes, "col2"))
    names <- xml_text(nodes)

    values <- list()
    seen <- new.env(parent = emptyenv())
    for (i in seq_along(nodes)) {
        row <- line1[[i]] - 1L
        col <- col1[[i]] - 1L
        if (!position_is_in_lsp_range(document, row, col, request_range)) next

        name <- names[[i]]
        if (!nzchar(name) || identical(name, "...")) next

        # Debug adapters render lookup results at the end of the source line;
        # one lookup per variable and line avoids noisy duplicate values.
        key <- paste(row, name, sep = ":")
        if (exists(key, envir = seen, inherits = FALSE)) next
        assign(key, TRUE, envir = seen)

        values[[length(values) + 1L]] <- list(
            range = range(
                document$to_lsp_position(row, col),
                document$to_lsp_position(line2[[i]] - 1L, col2[[i]])
            ),
            variableName = name,
            caseSensitiveLookup = TRUE
        )
    }

    Response$new(id, result = values)
}
