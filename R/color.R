#' The response to a textDocument/documentColor Request
#'
#' @noRd
document_color_reply <- function(id, uri, workspace, document) {
    result <- NULL

    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    cached <- parse_data$colors_cache
    if (!is.null(cached) && identical(cached$content, document$content)) {
        return(Response$new(id, result = cached$result))
    }
    strings <- parse_data$range_data$strings
    xdoc <- if (is.null(strings)) parse_data_xml(parse_data) else NULL
    if (!is.null(strings) || !is.null(xdoc)) {
        # Named colors can contain spaces, e.g. "light goldenrod yellow".
        # Use the same single-line literal bounds as the shared string index.
        if (is.null(strings)) {
            str_tokens <- xml_find_all_(xdoc, "//STR_CONST[@line1=@line2 and @col2>@col1+3 and @col2<=@col1+256]")
            str_line1 <- as.integer(xml_attr(str_tokens, "line1"))
            str_col1 <- as.integer(xml_attr(str_tokens, "col1"))
            str_col2 <- as.integer(xml_attr(str_tokens, "col2"))
        } else {
            keep <- strings$col2 > strings$col1 + 3L
            str_line1 <- strings$line[keep]
            str_col1 <- strings$col1[keep]
            str_col2 <- strings$col2[keep]
        }
        str_expr <- substr(document$content[str_line1], str_col1, str_col2)
        str_texts <- substring(str_expr, 2L, nchar(str_expr) - 1L)
        escaped <- which(grepl("\\", str_texts, fixed = TRUE) & !grepl("^[rR]", str_expr))
        if (length(escaped)) {
            str_texts[escaped] <- tryCatch(as.character(str2expression(str_expr[escaped])),
                error = function(e) "")
        }

        if (length(str_texts)) {
            short_hex <- grepl("^#[0-9a-fA-F]{3,4}$", str_texts)
            if (any(short_hex) && is.null(tryCatch(grDevices::col2rgb("#abc"), error = function(e) NULL))) {
                short_hex[] <- FALSE
            }
            color_names <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz",
                gsub(" ", "", str_texts, fixed = TRUE))
            is_color <- !grepl("^[rR]", str_expr) &
                (grepl("^#[0-9a-fA-F]{6}([0-9a-fA-F]{2})?$", str_texts) |
                        short_hex | str_texts == "transparent" | color_names %in% grDevices::colors())
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
    }
    if (is.environment(parse_data)) parse_data$colors_cache <- list(content = document$content, result = result)
    Response$new(id, result = result)
}

#' The response to a textDocument/colorPresentation Request
#'
#' @noRd
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
