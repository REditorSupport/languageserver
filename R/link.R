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

    indexed <- parse_data$range_data$strings
    xdoc <- if (is.null(indexed)) parse_data_xml(parse_data)
    if (!is.null(indexed) || !is.null(xdoc)) {
        cached <- parse_data$link_strings_cache
        if (!is.null(cached) && identical(cached$content, document$content) && identical(cached$root, rootPath)) {
            strings <- cached$strings
            targets <- cached$targets
            by_target <- cached$by_target
        } else {
            # String length: `@col2-@col1-1`
            # Limit string length to 255 to avoid potential PATH_MAX error on Windows
            # On macOS and Linux, PATH_MAX is much larger but we ignore the long strings at the moment.
            if (is.null(indexed)) {
                str_tokens <- xml_find_all_(xdoc, "//STR_CONST[@line1=@line2 and @col2>@col1+1 and @col2<=@col1+256]")
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
            str_texts <- tryCatch(as.character(str2expression(str_expr)),
                error = function(e) NULL)
            if (length(str_texts) != length(str_line1)) return(Response$new(id, result = list()))
            strings <- data.frame(line = str_line1, end_line = str_line1,
                col1 = str_col1 + grepl("^[rR]", str_expr), col2 = str_col2 - 1L,
                text = as.character(str_texts), source = rep(FALSE, length(str_texts)), project_relative = rep(FALSE, length(str_texts)))
            if (!is.null(parse_data$syntax_data)) {
                strings <- source_document_link_ranges(strings, document, parse_data$syntax_data, workspace)
            }
            keys <- if (nrow(strings)) paste(strings$source, strings$project_relative, strings$text, sep = "\r") else character()
            first <- which(!duplicated(keys))
            targets <- strings[first, , drop = FALSE]
            by_target <- match(keys, keys[first])
            from <- path_from_uri(uri)
            if (!nzchar(from)) from <- file.path(rootPath, "untitled.R")
            targets$candidates <- lapply(seq_len(nrow(targets)), function(i) {
                if (!targets$source[[i]]) return(character())
                spec <- list(path = targets$text[[i]], project_relative = targets$project_relative[[i]])
                setdiff(index_source_candidates(spec, from, rootPath), fs::path_abs(targets$text[[i]], rootPath))
            })
            if (is.environment(parse_data)) {
                parse_data$link_strings_cache <- list(content = document$content, strings = strings, root = rootPath,
                    targets = targets, by_target = by_target)
            }
        }

        if (length(strings$text)) {
            # Files can appear or disappear without a text edit. Only cache
            # literal extraction; stat each distinct candidate on every request.
            info <- with_wd(rootPath, file.info(targets$text, extra_cols = FALSE))
            exists <- !is.na(info$isdir) & !info$isdir
            paths <- rep(NA_character_, nrow(targets))
            paths[exists] <- path.expand(fs::path_abs(targets$text[exists], rootPath))
            for (i in which(!exists & lengths(targets$candidates) > 0L)) {
                candidates <- targets$candidates[[i]]
                info <- file.info(candidates, extra_cols = FALSE)
                available <- !is.na(info$isdir) & !info$isdir
                if (any(available)) paths[[i]] <- candidates[which(available)[[1L]]]
            }
            paths <- paths[by_target]
            keep <- !is.na(paths)
            link_paths <- paths[keep]
            strings <- strings[keep, , drop = FALSE]
            result <- .mapply(function(line, col1, end_line, col2, path) {
                list(
                    range = range(
                        start = document$to_lsp_position(line - 1, col1),
                        end = document$to_lsp_position(end_line - 1, col2)
                    ),
                    tooltip = path,
                    data = list(
                        path = path
                    )
                )
            }, list(strings$line, strings$col1, strings$end_line, strings$col2, link_paths), NULL)
        }
    }

    Response$new(id, result = result)
}

# A statically known source argument owns the complete path expression. Keep
# other string literals (including paths outside the workspace) independent.
source_document_link_ranges <- function(strings, document, syntax, workspace) {
    names <- which(syntax$token == "SYMBOL_FUNCTION_CALL" & syntax$text %in% c("source", "sys.source"))
    spans <- list()
    for (name in names) {
        row <- syntax$parent_row[syntax$parent_row[[name]]]
        if (!length(row) || !row) next
        call <- syntax_call_arguments(syntax, row)
        arg <- source_file_syntax(syntax, row, call)
        if (is.null(arg) || !arg) next
        if (is.null(call$package) && !source_base_binding(workspace, document, call$name,
                syntax$line1[[name]] - 1L, syntax$col1[[name]])) next
        first <- syntax$line1[[arg]]
        last <- syntax$line2[[arg]]
        start <- syntax$col1[[arg]]
        end <- syntax$col2[[arg]]
        lines <- get_range_text(document$content, first, start, last, end)
        expr <- tryCatch(str2lang(paste(lines, collapse = "\n")), error = function(e) NULL)
        if (member_head(expr, "file.path") && !source_base_binding(workspace, document, "file.path",
                first - 1L, start)) next
        spec <- index_static_path(expr)
        if (is.null(spec)) next
        literal <- length(syntax_children(syntax, arg)) == 1L &&
            syntax$token[syntax_children(syntax, arg)] == "STR_CONST"
        spans[[length(spans) + 1L]] <- data.frame(line = first, end_line = last,
            col1 = start - 1L + if (literal) 1L + grepl("^[rR]", lines[[1L]]) else 0L,
            col2 = end - as.integer(literal), text = spec$path, source = TRUE,
            project_relative = spec$project_relative)
    }
    if (!length(spans)) return(strings)
    paths <- do.call(rbind, spans)
    paths <- paths[order(paths$line, paths$col1), , drop = FALSE]
    stride <- as.double(syntax$maxcol + 1L)
    point <- strings$line * stride + strings$col1
    starts <- paths$line * stride + paths$col1
    ends <- paths$end_line * stride + paths$col2
    parent <- findInterval(point, starts)
    strings <- strings[parent == 0L | point > ends[pmax(1L, parent)], , drop = FALSE]
    combined <- rbind(strings, paths)
    combined[order(combined$line, combined$col1), , drop = FALSE]
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
