Document <- R6::R6Class(
    "Document",
    public = list(
        uri = NULL,
        version = NULL,
        is_open = FALSE,
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

        did_open = function() {
            self$is_open <- TRUE
        },

        did_close = function() {
            self$is_open <- FALSE
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
#' @noRd
find_unbalanced_bracket <- function(content, row, column, skip_empty_line = FALSE) {
    .Call("find_unbalanced_bracket",
        PACKAGE = "languageserver",
        content, row, column, skip_empty_line
    )
}

#' check if a position is inside quotes
#' @noRd
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
#' @noRd
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

is_ns_call <- function(x) {
    length(x) == 3L && is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("::", ":::")
}

# Check if an expression is a simple call like `foo(bar)` or `pkg::foo(bar)`
# This rules out anonymous function call like `(function(x) x + 1)(bar)`
is_simple_call <- function(x) {
    is.call(x) && (is.symbol(x[[1L]]) || is_ns_call(x[[1]]))
}

# We should handle base function specially as users may use base::fun form
# The reason that we only take care of `base` (not `utils`) is that only `base` calls can generate symbols
# Check if the lang is in base::fun form
is_base_call <- function(x) {
    is_ns_call(x) && as.character(x[[2L]]) == "base"
}

# Handle `base` function specically by removing the `base::` prefix
fun_string <- function(x) {
    if (is_base_call(x)) as.character(x[[3L]]) else deparse(x)
}

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

parser_hooks <- list(
    "{" = function(expr, action) {
        action$parse(as.list(expr)[-1L])
    },
    "(" = function(expr, action) {
        action$parse(as.list(expr)[-1L])
    },
    "if" = function(expr, action) {
        action$parse(as.list(expr)[-1L])
    },
    "for" = function(expr, action) {
        if (is.symbol(e <- expr[[2L]])) {
            action$update(nonfuncts = as.character(e))
        }
        action$parse(expr[[4L]])
    },
    "while" = function(expr, action) {
        action$parse(as.list(expr)[-1L])
    },
    "repeat" = function(expr, action) {
        action$parse(expr[[2L]])
    },
    "<-" = function(expr, action) {
        if (length(expr) == 3L && is.symbol(expr[[2L]])) {
            action$assign(symbol = as.character(expr[[2L]]), value = expr[[3L]])
            action$parse(expr[[3L]])
        }
    },
    "=" = function(expr, action) {
        if (length(expr) == 3L && is.symbol(expr[[2L]])) {
            action$assign(symbol = as.character(expr[[2L]]), value = expr[[3L]])
            action$parse(expr[[3L]])
        }
    },
    "assign" = function(expr, action) {
        call <- match.call(base::assign, expr)
        if (is.character(call$x) && is_top_level(call$pos, -1L, -1) && is_top_level(call$envir)) {
            action$assign(symbol = call$x, value = call$value)
            action$parse(call$value)
        }
    },
    "delayedAssign" = function(expr, action) {
        call <- match.call(base::delayedAssign, expr)
        if (is.character(call$x) && is_top_level(call$assign.env)) {
            action$assign(symbol = call$x, value = call$value)
            action$parse(call$value)
        }
    },
    "makeActiveBinding" = function(expr, action) {
        call <- match.call(base::makeActiveBinding, expr)
        if (is.character(call$sym) && is_top_level(call$env)) {
            action$assign(symbol = call$sym, value = call$fun, type = "variable")
        }
    },
    "library" = function(expr, action) {
        call <- match.call(base::library, expr)
        if (!isTRUE(call$character.only)) {
            action$update(packages = as.character(call$package))
        }
    },
    "require" = function(expr, action) {
        call <- match.call(base::require, expr)
        if (!isTRUE(call$character.only)) {
            action$update(packages = as.character(call$package))
        }
    },
    "pacman::p_load" = function(expr, action) {
        fun <- if (requireNamespace("pacman")) pacman::p_load else
            function(..., char, install = TRUE,
                     update = getOption("pac_update"),
                     character.only = FALSE) NULL
        call <- match.call(fun, expr, expand.dots = FALSE)
        if (!isTRUE(call$character.only)) {
            packages <- vapply(call[["..."]], as.character, character(1L))
            action$update(packages = packages)
        }
    },
    "system.time" = function(expr, action) action$parse_args("expr"),
    "try" = function(expr, action) action$parse_args("expr"),
    "tryCatch" = function(expr, action) action$parse_args(c("expr", "finally")),
    "withCallingHandlers" = function(expr, action) action$parse_args("expr"),
    "withRestarts" = function(expr, action) action$parse_args("expr"),
    "allowInterrupts" = function(expr, action) action$parse_args("expr"),
    "suspendInterrupts" = function(expr, action) action$parse_args("expr"),
    "suppressPackageStartupMessages" = function(expr, action) action$parse_args("expr"),
    "suppressMessages" = function(expr, action) action$parse_args("expr"),
    "suppressWarnings" = function(expr, action) action$parse_args("expr")
)

parse_expr <- function(content, expr, env, srcref = attr(expr, "srcref")) {
    if (length(expr) == 0L || is.symbol(expr) || is.atomic(expr)) {
        return(env)
    }

    if (is.expression(expr)) {
        for (i in seq_along(expr)) {
            Recall(content, expr[[i]], env, srcref[[i]])
        }
    } else if (is.list(expr)) {
        for (i in seq_along(expr)) {
            e <- expr[[i]]
            if (missing(e)) next
            Recall(content, e, env, srcref)
        }
    } else if (is_simple_call(expr)) {
        f <- fun_string(expr[[1L]])
        fun <- parser_hooks[[f]]
        if (is.function(fun)) {
            action <- list(
                update = function(...) {
                    updates <- list(...)
                    for (name in names(updates)) {
                        values <- updates[[name]]
                        values <- values[nzchar(values)]
                        if (length(values)) {
                            env[[name]] <- union(env[[name]], values)
                        }
                    }
                },
                assign = function(symbol, value, type = get_expr_type(value)) {
                    if (!nzchar(symbol)) return(NULL)

                    env$objects <- c(env$objects, symbol)

                    expr_range <- expr_range(srcref)
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
                },
                parse = function(expr) {
                    parse_expr(content, expr, env, srcref)
                },
                parse_args = function(args) {
                    fn <- tryCatch(eval(expr[[1L]], globalenv()), error = function(e) NULL)
                    if (is.function(fn)) {
                        call <- match.call(fn, expr, expand.dots = FALSE)
                        for (arg in args) {
                            if (is.call(call[[arg]])) {
                                parse_expr(content, call[[arg]], env, srcref)
                            }
                        }
                    }
                }
            )
            tryCatch(fun(expr, action), error = function(e) NULL)
        }
    }
    env
}

#' Parse a document
#'
#' Build the list of called packages, functions, variables, formals and
#' signatures in the document in order to add them to the current [Workspace].
#'
#' @noRd
parse_document <- function(uri, content) {
    if (length(content) == 0) {
        content <- ""
    }
    if (is_rmarkdown(uri)) {
        content <- purl(content)
    }
    # replace tab with a space since the width of a tab is 1 in LSP but 8 in getParseData().
    content <- gsub("\t", " ", content, fixed = TRUE)
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
