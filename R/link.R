#' The response to a textDocument/documentLink Request
#'
#' @keywords internal
document_link_reply <- function(id, uri, workspace, document, rootPath, rootUri) {
    result <- NULL

    xdoc <- workspace$get_xml_doc(uri)
    if (!is.null(xdoc)) {
        str_tokens <- xml_find_all(xdoc, "//STR_CONST[@line1=@line2 and @col2 > @col1 + 1]")
        str_texts <- xml_text(str_tokens)
        str_texts <- substr(str_texts, 2, nchar(str_texts) - 1)
        paths <- file.path(rootPath, str_texts)
        sel <- file.exists(paths)
        str_tokens <- str_tokens[sel]
        str_texts <- str_texts[sel]
        str_line1 <- as.integer(xml_attr(str_tokens, "line1"))
        str_col1 <- as.integer(xml_attr(str_tokens, "col1"))
        str_col2 <- as.integer(xml_attr(str_tokens, "col2"))
        uris <- file.path(rootUri, str_texts)
        result <- .mapply(function(line, col1, col2, uri) {
            list(
                range = range(
                    start = document$to_lsp_position(line - 1, col1),
                    end = document$to_lsp_position(line - 1, col2 - 1)
                ),
                target = uri
            )
        }, list(str_line1, str_col1, str_col2, uris), NULL)
    }

    if (is.null(result)) {
      Response$new(id)
    } else {
      Response$new(id, result = result)
    }
}
