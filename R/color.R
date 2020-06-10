#' The response to a textDocument/documentColor Request
#'
#' @keywords internal
document_color_reply <- function(id, uri, workspace, document) {
    result <- NULL

    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
        (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    xdoc <- parse_data$xml_doc
    if (!is.null(xdoc)) {
        str_tokens <- xml_find_all(xdoc, "//STR_CONST[@line1=@line2 and @col2 > @col1 + 1]")
        str_line1 <- as.integer(xml_attr(str_tokens, "line1"))
        str_col1 <- as.integer(xml_attr(str_tokens, "col1"))
        str_col2 <- as.integer(xml_attr(str_tokens, "col2"))
        str_expr <- substr(document$content[str_line1], str_col1, str_col2)
        str_texts <- as.character(parse(text = str_expr, keep.source = FALSE))

        is_color <- !grepl("^[rR]", str_expr) &
            (grepl("^#[0-9a-fA-F]{6}([0-9a-fA-F]{2})?$", str_texts) |
                str_texts %in% grDevices::colors())
        color_tokens <- str_tokens[is_color]
        color_texts <- str_texts[is_color]
        color_line1 <- str_line1[is_color]
        color_col1 <- str_col1[is_color]
        color_col2 <- str_col2[is_color]
        color_rgb <- grDevices::col2rgb(color_texts, alpha = TRUE) / 255

        result <- .mapply(function(line, col1, col2, i) {
            list(
                range = range(
                    start = document$to_lsp_position(line - 1, col1),
                    end = document$to_lsp_position(line - 1, col2 - 1)
                ),
                color = as.list(color_rgb[, i])
            )
        }, list(color_line1, color_col1, color_col2, seq_along(color_texts)), NULL)
    }

    if (is.null(result)) {
        Response$new(id)
    } else {
        Response$new(id, result = result)
    }
}

#' The response to a textDocument/colorPresentation Request
#'
#' @keywords internal
color_presentation_reply <- function(id, uri, workspace, document, color) {
    if (color$alpha == 1) {
        hex_color <- grDevices::rgb(color$red, color$green, color$blue)
    } else {
        hex_color <- grDevices::rgb(color$red, color$green, color$blue, color$alpha)
    }
    result <- list(
        list(label = tolower(hex_color)),
        list(label = hex_color)
    )
    Response$new(id, result = result)
}
