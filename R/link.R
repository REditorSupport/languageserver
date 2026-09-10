#' The response to a textDocument/documentLink Request
#' @param rootPath Path of workspace folder
#' @noRd
document_link_reply <- function(id, uri, workspace, document, rootPath) {
    result <- NULL

    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    xdoc <- parse_data$xml_doc
    if (!is.null(xdoc)) {
        cached <- parse_data$link_strings_cache
        if (!is.null(cached) && identical(cached$content, document$content)) {
            strings <- cached$strings
        } else {
            # String length: `@col2-@col1-1`
            # Limit string length to 255 to avoid potential PATH_MAX error on Windows
            # On macOS and Linux, PATH_MAX is much larger but we ignore the long strings at the moment.
            indexed <- parse_data$range_data$strings
            if (is.null(indexed)) {
                str_tokens <- xml_find_all(xdoc, "//STR_CONST[@line1=@line2 and @col2>@col1+1 and @col2<=@col1+256]")
                indexed <- list(
                    line = as.integer(xml_attr(str_tokens, "line1")),
                    col1 = as.integer(xml_attr(str_tokens, "col1")),
                    col2 = as.integer(xml_attr(str_tokens, "col2"))
                )
            }
            str_line1 <- indexed$line
            str_col1 <- indexed$col1
            str_col2 <- indexed$col2
            str_expr <- substr(document$content[str_line1], str_col1, str_col2)
            str_texts <- tryCatch(as.character(parse(text = str_expr, keep.source = FALSE)),
                error = function(e) NULL)
            unique_texts <- unique(str_texts)
            strings <- list(
                line = str_line1,
                col1 = str_col1 + grepl("^[rR]", str_expr),
                col2 = str_col2,
                text = unique_texts,
                match = match(str_texts, unique_texts)
            )
            if (is.environment(parse_data)) {
                parse_data$link_strings_cache <- list(content = document$content, strings = strings)
            }
        }

        if (length(strings$text)) {
            # Files can appear or disappear without a text edit. Only cache
            # literal extraction; stat each distinct candidate on every request.
            info <- with_wd(rootPath, file.info(strings$text, extra_cols = FALSE))
            exists <- !is.na(info$isdir) & !info$isdir
            is_link <- exists[strings$match]
            paths <- rep(NA_character_, length(strings$text))
            paths[exists] <- path.expand(fs::path_abs(strings$text[exists], rootPath))
            link_paths <- paths[strings$match[is_link]]
            link_line1 <- strings$line[is_link]
            link_col1 <- strings$col1[is_link]
            link_col2 <- strings$col2[is_link]

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
        link_file_size_limit <- lsp_settings$get("link_file_size_limit")
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
