#' The response to a textDocument/documentLink Request
#'
#' @keywords internal
document_link_reply <- function(id, uri, workspace, document, rootPath) {
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

        paths <- fs::path_abs(str_texts, rootPath)

        is_link <- file.exists(paths) & !dir.exists(paths)
        link_paths <- path.expand(paths[is_link])
        link_expr <- str_expr[is_link]
        is_raw_string <- grepl("^[rR]", link_expr)
        link_line1 <- str_line1[is_link]
        link_col1 <- str_col1[is_link] + is_raw_string
        link_col2 <- str_col2[is_link]
        uris <- vapply(link_paths, path_to_uri, character(1))

        result <- .mapply(function(line, col1, col2, uri) {
            list(
                range = range(
                    start = document$to_lsp_position(line - 1, col1),
                    end = document$to_lsp_position(line - 1, col2 - 1)
                ),
                target = uri
            )
        }, list(link_line1, link_col1, link_col2, uris), NULL)
    }

    if (is.null(result)) {
      Response$new(id)
    } else {
      Response$new(id, result = result)
    }
}
