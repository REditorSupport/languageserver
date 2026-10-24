s3_documentation <- function(workspace, result) {
    method <- result$method
    doc <- NULL
    if (!is.null(method$source$comments)) doc <- convert_comment_to_documentation(method$source$comments)
    if (is.null(doc) && !is.null(method$source) && !is.null(workspace$documents)) {
        source <- workspace$documents$get(method$source$uri, NULL)
        if (!is.null(source)) {
            line <- method$source$line
            start <- detect_comments(source$content, line - 1L) + 1L
            if (start < line) doc <- convert_comment_to_documentation(source$content[seq.int(start, line - 1L)])
        }
    } else if (!is.null(method$package) && is.function(workspace$get_documentation)) {
        doc <- call_with_optional_uri(workspace$get_documentation, method$name, method$package,
            isf = TRUE, uri = result$uri
        )
    }
    if (is.null(doc) && is.function(workspace$get_documentation)) {
        doc <- call_with_optional_uri(workspace$get_documentation, result$generic, result$generic_package,
            isf = TRUE, uri = result$uri
        )
    }
    doc
}

s3_arguments <- function(result, token) {
    args <- setdiff(names(result$formals), c("...", result$supplied))
    # A bare prefix in the current argument is not a supplied positional value.
    if (nzchar(token) && length(result$actuals) && !identical(
        result$actuals[[length(result$actuals)]],
        quote(expr = )
    )) {
        last <- result$actuals[[length(result$actuals)]]
        if (is.symbol(last) && identical(member_name(last), token)) {
            args <- union(args, result$matched_parameters[[length(result$actuals)]])
        }
    }
    if (!is.null(result$argument_names)) args <- intersect(args, result$argument_names)
    args <- args[match_with(args, token)]
    lapply(seq_along(args), function(i) {
        list(
            label = args[[i]], kind = CompletionItemKind$Variable,
            detail = result$signature, sortText = sprintf("%s%03d", sort_prefixes$arg, i),
            insertText = paste0(quote_completion_name(args[[i]]), " = "),
            insertTextFormat = InsertTextFormat$PlainText,
            data = list(
                type = "s3_parameter", context_uri = result$uri, parameter = args[[i]],
                version = result$version, revision = result$revision, source_revision = result$source_revision, s3 = result[c(
                    "method", "generic", "generic_package", "uri", "description", "signature"
                )]
            )
        )
    })
}

s3_argument_values <- function(result, token, parameter = NULL) {
    items <- list()
    for (name in if (!is.null(parameter)) {
        intersect(
            parameter,
            names(result$formals)
        )
    } else {
        setdiff(names(result$formals), "...")
    }) {
        values <- extract_default_values(result$formals[[name]])
        if (!is.character(values)) next
        for (value in values[match_with(values, token)]) {
            items[[length(items) + 1L]] <- list(
                label = value, kind = CompletionItemKind$Value,
                detail = paste0("value for ", name), sortText = paste0(sort_prefixes$arg, value),
                insertText = encodeString(value, quote = "\""), insertTextFormat = InsertTextFormat$PlainText,
                data = list(
                    type = "s3_parameter", context_uri = result$uri, parameter = name,
                    version = result$version, revision = result$revision, source_revision = result$source_revision, s3 = result[c(
                        "method", "generic", "generic_package", "uri", "description", "signature"
                    )]
                )
            )
        }
    }
    items
}

s3_signature_reply <- function(id, workspace, document, point, result) {
    doc <- s3_documentation(workspace, result)
    description <- if (is.character(doc)) doc else if (is.list(doc)) doc$description
    Response$new(id, result = list(
        signatures = list(list(
            label = result$signature,
            documentation = list(kind = "markdown", value = paste(c(result$description, description),
                    collapse = "\n\n"
                )),
            parameters = parse_signature_parameters(result$signature)
        )), activeSignature = 0L,
        activeParameter = detect_active_parameter(
            document$content, result$opening$row,
            result$opening$col, point$row, point$col, result$signature
        )
    ))
}

s3_hover_reply <- function(id, workspace, document, point, result) {
    if (!check_scope(document$uri, document, point)) {
        return(NULL)
    }
    token <- document$detect_token(point)
    doc <- s3_documentation(workspace, result)
    if (identical(token$token, result$location$call$token) ||
            identical(token$full_token, result$location$call$full_token)) {
        description <- if (is.character(doc)) doc else if (is.list(doc)) doc$description
        contents <- c(sprintf("```r\n%s\n```", result$signature), result$description, description)
    } else {
        end <- token$range$end
        if (!grepl("^[ \t]*=(?!=)", substring(document$line0(end$row), end$col + 1L), perl = TRUE)) {
            return(NULL)
        }
        contents <- argument_hover_contents(doc, result$signature, token$token)
        if (is.null(contents)) contents <- sprintf("```r\n%s\n```", result$signature)
        contents <- c(contents, result$description)
    }
    Response$new(id, result = list(contents = unname(contents), range = list(
        start = document$to_lsp_position(token$range$start$row, token$range$start$col),
        end = document$to_lsp_position(token$range$end$row, token$range$end$col)
    )))
}

s3_resolve_parameter <- function(workspace, params) {
    document <- workspace$documents$get(params$data$context_uri, NULL)
    if (is.null(document) || !identical(document$version, params$data$version) ||
            !identical(workspace$s3_revision, params$data$source_revision)) {
        return(params)
    }
    if (is.function(workspace$s3_metadata$revision) &&
            !identical(workspace$s3_metadata$revision(), params$data$revision)) {
        return(params)
    }
    doc <- s3_documentation(workspace, params$data$s3)
    text <- if (is.list(doc)) doc$arguments[[params$data$parameter]]
    params$documentation <- list(
        kind = "markdown",
        value = paste(c(params$data$s3$description, text), collapse = "\n\n")
    )
    params
}

s3_quoted_values <- function(workspace, document, point) {
    result <- tryCatch(resolve_s3_call(document$uri, workspace, document, point), error = function(e) NULL)
    if (is.null(result)) {
        return(NULL)
    }
    line <- document$line0(point$row)
    chars <- strsplit(substr(line, 1L, point$col), "", fixed = TRUE)[[1L]]
    quote <- NULL
    escaped <- FALSE
    opening <- NULL
    for (i in seq_along(chars)) {
        char <- chars[[i]]
        if (escaped) {
            escaped <- FALSE
            next
        }
        if (!is.null(quote) && char == "\\") {
            escaped <- TRUE
            next
        }
        if (is.null(quote)) {
            if (char == "#") {
                return(NULL)
            }
            if (char %in% c("'", "\"")) {
                quote <- char
                opening <- i
            }
        } else if (char == quote) {
            quote <- NULL
        }
    }
    if (is.null(quote) || is.null(opening)) {
        return(NULL)
    }
    prefix <- substr(line, opening + 1L, point$col)
    token <- tryCatch(parse(text = paste0(quote, prefix, quote))[[1L]], error = function(e) prefix)
    active <- detect_active_parameter(
        document$content, result$opening$row, result$opening$col, point$row,
        point$col, result$signature
    )
    parameter <- names(result$formals)[active + 1L]
    items <- s3_argument_values(result, token, parameter)
    end <- nchar(line)
    rest <- if (point$col < end) strsplit(substring(line, point$col + 1L), "", fixed = TRUE)[[1L]] else character()
    escaped <- FALSE
    for (i in seq_along(rest)) {
        if (escaped) {
            escaped <- FALSE
            next
        }
        if (rest[[i]] == "\\") {
            escaped <- TRUE
            next
        }
        if (rest[[i]] == quote) {
            end <- point$col + i - 1L
            break
        }
    }
    for (i in seq_along(items)) {
        inserted <- encodeString(items[[i]]$label, quote = quote)
        items[[i]]$insertText <- NULL
        items[[i]]$textEdit <- list(
            newText = substr(inserted, 2L, nchar(inserted) - 1L),
            range = list(
                start = document$to_lsp_position(point$row, opening),
                end = document$to_lsp_position(point$row, end)
            )
        )
    }
    items
}
