#' The response to a textDocument/documentLink Request
#' @param rootPath Path of workspace folder
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
        # String length: `@col2-@col1-1`
        # Limit string length to 255 to avoid potential PATH_MAX error on Windows
        # On macOS and Linux, PATH_MAX is much larger but we ignore the long strings at the moment.
        str_tokens <- xml_find_all(xdoc, "//STR_CONST[@line1=@line2 and @col2>@col1+1 and @col2<=@col1+256]")
        str_line1 <- as.integer(xml_attr(str_tokens, "line1"))
        str_col1 <- as.integer(xml_attr(str_tokens, "col1"))
        str_col2 <- as.integer(xml_attr(str_tokens, "col2"))
        str_expr <- substr(document$content[str_line1], str_col1, str_col2)
        str_texts <- tryCatch(as.character(parse(text = str_expr, keep.source = FALSE)),
            error = function(e) NULL)

        if (length(str_texts)) {
            paths <- fs::path_abs(str_texts, rootPath)
            is_link <- file.exists(paths) & !dir.exists(paths)
            link_paths <- path.expand(paths[is_link])
            link_expr <- str_expr[is_link]
            is_raw_string <- grepl("^[rR]", link_expr)
            link_line1 <- str_line1[is_link]
            link_col1 <- str_col1[is_link] + is_raw_string
            link_col2 <- str_col2[is_link]

            result <- .mapply(function(line, col1, col2, path) {
                list(
                    range = range(
                        start = document$to_lsp_position(line - 1, col1),
                        end = document$to_lsp_position(line - 1, col2 - 1)
                    ),
                    tooltip = path,
                    data = list(
                        path = path
                    )
                )
            }, list(link_line1, link_col1, link_col2, link_paths), NULL)
        }
    }

    Response$new(id, result = result)
}

document_link_resolve_reply <- function(id, workspace, params) {
    path <- params$data$path
    file_size <- file.size(path)
    if (is.finite(file_size)) {
        link_file_size_limit <- getOption("languageserver.link_file_size_limit", 16 * 1024^2)
        if (file_size <= link_file_size_limit) {
            params$target <- path_to_uri(path)
            params$data <- NULL
            Response$new(
                id,
                result = params
            )
        } else {
            ResponseErrorMessage$new(
                id,
                errortype = "RequestCancelled",
                message = sprintf("File size (%s) exceeds the limit (%s).\nThe limit could be changed via \"languageserver.link_file_size_limit\" option.",
                    format_file_size(file_size),
                    format_file_size(link_file_size_limit)
                )
            )
        }
    } else {
        ResponseErrorMessage$new(
            id,
            errortype = "RequestCancelled",
            message = "File is missing."
        )
    }
}
