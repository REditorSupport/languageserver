Document <- R6::R6Class(
    "Document",
    public = list(
        uri = NULL,
        version = NULL,
        nline = 0,
        content = NULL,
        parse_data = NULL,
        is_rmarkdown = NULL,
        loaded_packages = NULL,

        initialize = function(uri, version, content = "") {
            self$uri <- uri
            self$version <- version
            self$is_rmarkdown <- is_rmarkdown(self$uri)
            self$set_content(version, content)
            self$loaded_packages <- character()
        },

        set_content = function(version, content) {
            self$version <- version
            self$nline <- length(content)
            self$content <- content
        },

        update_parse_data = function(parse_data) {
            self$parse_data <- parse_data
        },

        line = function(row) {
            if (row <= self$nline) self$content[row] else ""
        },

        line0 = function(row) {
            # row is 0-indexed
            if (row < self$nline) self$content[row + 1] else ""
        },

        find_token = function(row, col, forward = TRUE) {
            # row and col are 0-indexed
            text <- self$line0(row)
            text_after <- substr(text, col + 1, nchar(text))

            # look forward
            if (forward) {
                right_token <- look_forward(text_after)$token
                end <- col + nchar(right_token)
            } else {
                right_token <- ""
                end <- col
            }

            matches <- look_backward(substr(text, 1, end))
            return(list(
                full_token = matches$full_token,
                right_token = right_token,
                package = empty_string_to_null(matches$package),
                accessor = matches$accessor,
                token = matches$token
            ))
        },

        detect_call = function(point) {
            row <- point$row
            col <- point$col

            if (col > 0) {
                fub_result <- find_unbalanced_bracket(self$content, row, col - 1)
                loc <- fub_result[[1]]
                bracket <- fub_result[[2]]
            } else {
                loc <- c(-1, -1)
                bracket <- " "
            }
            logger$info("bracket is", bracket)

            if (loc[1] < 0 || loc[2] < 0 || bracket != "(")
                return(list(token = ""))

            result <- self$find_token(loc[1], loc[2], forward = FALSE)
            logger$info("call:", result)

            list(
                full_token = result$full_token,
                package = result$package,
                accessor = result$accessor,
                token = result$token
            )
        },

        detect_token = function(point, forward = TRUE) {
            row <- point$row
            col <- point$col
            result <- self$find_token(row, col, forward = forward)

            logger$info("token:", result)

            col_end <- col + nchar(result$right_token)
            col_start <- col_end - nchar(result$full_token)

            list(
                range = list(
                    start = list(row = row, col = col_start),
                    end = list(row = row, col = col_end)
                ),
                full_token = result$full_token,
                package = result$package,
                accessor = result$accessor,
                token = result$token
            )
        },

        from_lsp_position = function(position) {
            # convert UTF-16 based position to code point based position
            text <- self$line0(position$line)
            list(
                row = position$line,
                col = code_point_from_unit(text, position$character)
            )
        },

        to_lsp_position = function(row, col) {
            # convert code point based position to UTF-16 based position
            text <- self$line0(row)
            position(
                line = row,
                character = code_point_to_unit(text, col)
            )
        }
    )
)


#' Search backwards in a document content for a specific character
#' @keywords internal
find_unbalanced_bracket <- function(content, row, column, skip_empty_line = FALSE) {
    .Call("find_unbalanced_bracket",
        PACKAGE = "languageserver",
        content, row, column, skip_empty_line
    )
}

#' check if a position is inside quotes
#' @keywords internal
enclosed_by_quotes <- function(document, point) {
    text <- document$line0(point$row)
    col <- point$col
    .Call("enclosed_by_quotes", PACKAGE = "languageserver", text, col - 1)
}

detect_comments <- function(content, row) {
    .Call("detect_comments", PACKAGE = "languageserver",
        content, row)
}

#' Expression range in UTF-16 code units
#' @keywords internal
expr_range <- function(srcref) {
    lines <- attr(srcref, "srcfile")$lines
    # R is 1-indexed, language server is 0-indexed
    first_line <- srcref[1] - 1
    first_char <- code_point_to_unit(lines[srcref[1]], srcref[5] - 1)
    last_line <- srcref[3] - 1
    last_char <- code_point_to_unit(lines[srcref[3]], srcref[6])
    return(
        range(
            start = position(first_line, first_char),
            end = position(last_line, last_char)
        )
    )
}

get_range_text <- function(content, line1, col1, line2, col2) {
    lines <- content[line1:line2]
    lines[length(lines)] <- substr(lines[length(lines)], 1L, col2)
    lines[1] <- substr(lines[1], col1, nchar(lines[1]))
    lines
}

parse_expr <- function(content, expr, env, level = 0L, srcref = attr(expr, "srcref")) {
    if (length(expr) == 0L || is.symbol(expr)) {
          return(env)
    }
    # We should handle base function specially as users may use base::fun form
    # The reason that we only take care of `base` (not `utils`) is that only `base` calls can generate symbols
    # Check if the lang is in base::fun form
    is_base_call <- function(x) {
        length(x) == 3L && as.character(x[[1L]]) %in% c("::", ":::") && as.character(x[[2L]]) == "base"
    }
    # Be able to handle `pkg::name` case (note `::` is a function)
    is_symbol <- function(x) {
        is.symbol(x) || is_base_call(x)
    }
    # Handle `base` function specically by removing the `base::` prefix
    fun_string <- function(x) {
        if (is_base_call(x)) as.character(x[[3L]]) else as.character(x)
    }
    for (i in seq_along(expr)) {
        e <- expr[[i]]
        if (missing(e) || !is.call(e) || !is_symbol(e[[1L]])) next
        f <- fun_string(e[[1L]])
        cur_srcref <- if (level == 0L) srcref[[i]] else srcref
        if (f %in% c("{", "(")) {
            Recall(content, e[-1L], env, level + 1L, cur_srcref)
        } else if (f == "if") {
            Recall(content, e[[2L]], env, level + 1L, cur_srcref)
            Recall(content, e[[3L]], env, level + 1L, cur_srcref)
            if (length(e) == 4L) {
                Recall(content, e[[4L]], env, level + 1L, cur_srcref)
            }
        } else if (f == "for") {
            if (is.symbol(e[[2L]])) {
                env$nonfuncts <- c(env$nonfuncts, as.character(e[[2L]]))
            }
            Recall(content, e[[4L]], env, level + 1L, cur_srcref)
        } else if (f == "while") {
            Recall(content, e[[2L]], env, level + 1L, cur_srcref)
            Recall(content, e[[3L]], env, level + 1L, cur_srcref)
        } else if (f == "repeat") {
            Recall(content, e[[2L]], env, level + 1L, cur_srcref)
        } else if (f %in% c("<-", "=", "delayedAssign", "makeActiveBinding", "assign")) {
            # to see the pos/env/assign.env of assigning functions is set or not
            # if unset, it means using the default value, which is top-level
            # if set, we should compare to a vector of known "top-level" candidates
            is_top_level <- function(arg_env, ...) {
                if (is.null(arg_env)) return(TRUE)
                default <- list(
                    quote(parent.frame(1)), quote(parent.frame(1L)),
                    quote(environment()),
                    quote(.GlobalEnv), quote(globalenv())
                )
                extra <- substitute(list(...))[-1L]
                top_level_envs <- c(default, as.list(extra))
                any(vapply(top_level_envs, identical, x = arg_env, FUN.VALUE = logical(1L)))
            }

            type <- NULL

            if (f %in% c("<-", "=")) {
                if (length(e) != 3L || !is.symbol(e[[2L]])) next
                symbol <- as.character(e[[2L]])
                value <- e[[3L]]
            } else if (f == "delayedAssign") {
                call <- match.call(base::delayedAssign, as.call(e))
                if (!is.character(call$x)) next
                if (!is_top_level(call$assign.env)) next
                symbol <- call$x
                value <- call$value
            } else if (f == "assign") {
                call <- match.call(base::assign, as.call(e))
                if (!is.character(call$x)) next
                if (!is_top_level(call$pos, -1L, -1)) next # -1 is the default
                if (!is_top_level(call$envir)) next
                symbol <- call$x
                value <- call$value
            } else if (f == "makeActiveBinding") {
                call <- match.call(base::makeActiveBinding, as.call(e))
                if (!is.character(call$sym)) next
                if (!is_top_level(call$env)) next
                symbol <- call$sym
                value <- call$fun
                type <- "variable"
            }

            if (is.null(type)) {
                type <- get_expr_type(value)
            }

            env$objects <- c(env$objects, symbol)

            expr_range <- expr_range(cur_srcref)
            env$definitions[[symbol]] <- list(
                name = symbol,
                type = type,
                range = expr_range
            )

            doc_line1 <- detect_comments(content, expr_range$start$line) + 1
            if (doc_line1 <= expr_range$start$line) {
                comment <- content[seq.int(doc_line1, expr_range$start$line)]
                env$documentation[[symbol]] <- convert_comment_to_documentation(comment)
            }

            if (type == "function") {
                env$functs <- c(env$functs, symbol)
                env$formals[[symbol]] <- value[[2L]]
                env$signatures[[symbol]] <- get_signature(symbol, value)
            } else {
                env$nonfuncts <- c(env$nonfuncts, symbol)
            }
        } else if (f %in% c("library", "require") && length(e) == 2L) {
            pkg <- as.character(e[[2L]])
            if (!(pkg %in% env$packages)) {
                env$packages <- c(env$packages, pkg)
            }
        }
    }
    env
}

#' Parse a document
#'
#' Build the list of called packages, functions, variables, formals and
#' signatures in the document in order to add them to the current [Workspace].
#'
#' @keywords internal
parse_document <- function(uri, content) {
    if (length(content) == 0) {
        content <- ""
    }
    if (is_rmarkdown(uri)) {
        content <- purl(content)
    }
    expr <- tryCatch(parse(text = content, keep.source = TRUE), error = function(e) NULL)
    if (!is.null(expr)) {
        parse_env <- function() {
            env <- new.env(parent = .GlobalEnv)
            env$packages <- character()
            env$nonfuncts <- character()
            env$functs <- character()
            env$formals <- list()
            env$signatures <- list()
            env$definitions <- list()
            env$documentation <- list()
            env$xml_data <- NULL
            env$xml_doc <- NULL
            env
        }
        env <- parse_env()
        parse_expr(content, expr, env)
        env$packages <- basename(find.package(env$packages, quiet = TRUE))
        env$xml_data <- xmlparsedata::xml_parse_data(expr)
        env
    }
}


parse_callback <- function(self, uri, version, parse_data) {
    if (is.null(parse_data) || !self$workspace$documents$has(uri)) return(NULL)
    logger$info("parse_callback called:", list(uri = uri, version = version))
    doc <- self$workspace$documents$get(uri)

    parse_data$version <- version
    old_parse_data <- doc$parse_data
    self$workspace$update_parse_data(uri, parse_data)

    if (!identical(old_parse_data$packages, parse_data$packages)) {
        self$resolve_task_manager$add_task(
            uri,
            resolve_task(self, uri, doc, parse_data$packages)
        )
        doc$loaded_packages <- parse_data$packages
        self$workspace$update_loaded_packages()
    }

    pending_replies <- self$pending_replies$get(uri, NULL)
    for (name in names(pending_replies)) {
        queue <- pending_replies[[name]]
        handler <- self$request_handlers[[name]]
        while (queue$size()) {
            item <- queue$peek()
            if (is.null(version) || item$version == version) {
                handler(self, item$id, item$params)
                queue$pop()
            } else if (item$version < version) {
                self$deliver(Response$new(item$id))
                queue$pop()
            } else {
                break
            }
        }
    }
}

parse_task <- function(self, uri, document, delay = 0) {
    version <- document$version
    content <- document$content
    create_task(
        target = package_call(parse_document),
        args = list(uri = uri, content = content),
        callback = function(result) parse_callback(self, uri, version, result),
        error = function(e) logger$info("parse_task:", e),
        delay = delay
    )
}

resolve_callback <- function(self, uri, version, packages) {
    if (!self$workspace$documents$has(uri)) return(NULL)
    logger$info("resolve_callback called:", list(uri = uri, version = version))
    self$workspace$load_packages(packages)
    doc <- self$workspace$documents$get(uri)
    doc$loaded_packages <- packages
    self$workspace$update_loaded_packages()
}

resolve_task <- function(self, uri, document, packages, delay = 0) {
    version <- document$version
    create_task(
        target = resolve_attached_packages,
        args = list(pkgs = packages),
        callback = function(result) resolve_callback(self, uri, version, result),
        error = function(e) logger$info("resolve_task:", e),
        delay = 0
    )
}
