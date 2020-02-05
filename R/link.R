#' The response to a textDocument/documentLink Request
#'
#' @keywords internal
document_link_reply <- function(id, uri, workspace, document, rootUri) {
    result <- NULL

    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    xdoc <- parse_data$xml_doc
    if (!is.null(xdoc)) {
        str_tokens <- xml_find_all(xdoc, "//STR_CONST[@line1=@line2 and @col2 > @col1 + 1]")
        str_texts <- xml_text(str_tokens)
        str_texts <- substr(str_texts, 2, nchar(str_texts) - 1)
        is_abs_path <- grepl("^(~|/+|[A-Za-z]:)", str_texts)

        root_path <- path_from_uri(rootUri)
        paths <- str_texts
        paths[is_abs_path] <- normalizePath(str_texts[is_abs_path], "/", mustWork = FALSE)
        paths[!is_abs_path] <- file.path(root_path, str_texts[!is_abs_path])

        sel <- file.exists(paths) & !dir.exists(paths)
        str_tokens <- str_tokens[sel]
        str_texts <- str_texts[sel]
        paths <- paths[sel]
        is_abs_path <- is_abs_path[sel]

        uris <- paths
        uris[is_abs_path] <- path_to_uri(paths[is_abs_path])
        uris[!is_abs_path] <- file.path(rootUri, str_texts[!is_abs_path])

        str_line1 <- as.integer(xml_attr(str_tokens, "line1"))
        str_col1 <- as.integer(xml_attr(str_tokens, "col1"))
        str_col2 <- as.integer(xml_attr(str_tokens, "col2"))
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
