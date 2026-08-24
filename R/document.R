Document <- R6::R6Class(
    "Document",
    public = list(
        uri = NULL,
        language = NULL,
        version = NULL,
        is_open = FALSE,
        nline = 0,
        content = NULL,
        parse_data = NULL,
        is_rmarkdown = NULL,
        regions = NULL,
        loaded_packages = NULL,
        requested_packages = NULL,
        pending_diagnostics = FALSE,
        diagnostics_delay = 0,

        initialize = function(uri, language = NULL, version = NULL, content = "") {
            self$uri <- uri
            self$language <- language
            self$version <- version
            self$is_rmarkdown <- is_rmarkdown(uri, language)
            self$set_content(version, content)
            self$loaded_packages <- character()
            self$requested_packages <- NULL
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
            self$regions <- if (self$is_rmarkdown) {
                parse_literate_regions(content)
            } else {
                NULL
            }
        },

        apply_content_changes = function(version, content_changes) {
            for (change in content_changes) {
                if (is.null(change$range)) {
                    self$set_content(
                        version,
                        stringi::stri_split_lines(change$text)[[1]]
                    )
                    next
                }

                start <- self$from_lsp_position(change$range$start)
                end <- self$from_lsp_position(change$range$end)
                replacement <- stringi::stri_split_lines(change$text)[[1]]

                start_line <- self$line0(start$row)
                end_line <- self$line0(end$row)
                prefix <- if (start$col > 0L) {
                    stringi::stri_sub(start_line, 1L, start$col)
                } else {
                    ""
                }
                suffix <- stringi::stri_sub(end_line, end$col + 1L)

                if (length(replacement) == 1L) {
                    changed <- paste0(prefix, replacement, suffix)
                } else {
                    middle <- if (length(replacement) > 2L) {
                        replacement[seq.int(2L, length(replacement) - 1L)]
                    } else {
                        character()
                    }
                    changed <- c(
                        paste0(prefix, replacement[[1L]]),
                        middle,
                        paste0(replacement[[length(replacement)]], suffix)
                    )
                }

                before <- if (start$row > 0L) {
                    self$content[seq_len(start$row)]
                } else {
                    character()
                }
                after <- if (end$row + 1L < length(self$content)) {
                    self$content[seq.int(end$row + 2L, length(self$content))]
                } else {
                    character()
                }
                self$set_content(version, c(before, changed, after))
            }
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
            matches <- scan_token(text, col, forward)
            return(list(
                full_token = matches$full_token,
                right_token = matches$right_token,
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

null_function <- local(function() NULL, baseenv())

parser_hooks <- list(
    "{" = function(expr, action) {
        children <- as.list(expr)[-1L]
        srcrefs <- attr(expr, "srcref")
        if (!is.null(srcrefs) && length(srcrefs) > 1) {
            # srcref[[1]] is for the opening brace, skip it
            for (i in seq_along(children)) {
                action$parse(children[[i]], srcrefs[[i + 1]])
            }
        } else {
            action$parse(children)
        }
    },
    "(" = function(expr, action) {
        action$parse(as.list(expr)[-1L])
    },
    "if" = function(expr, action) {
        children <- as.list(expr)[-1L]
        srcrefs <- attr(expr, "srcref")
        if (!is.null(srcrefs) && length(srcrefs) > 1) {
            # srcref[[1]] is for "if", skip it
            for (i in seq_along(children)) {
                action$parse(children[[i]], srcrefs[[i + 1]])
            }
        } else {
            action$parse(children)
        }
    },
    "for" = function(expr, action) {
        if (is.symbol(e <- expr[[2L]])) {
            action$update(nonfuncts = as.character(e))
        }
        action$parse(expr[[4L]])
    },
    "while" = function(expr, action) {
        children <- as.list(expr)[-1L]
        srcrefs <- attr(expr, "srcref")
        if (!is.null(srcrefs) && length(srcrefs) > 1) {
            # srcref[[1]] is for "while", skip it
            for (i in seq_along(children)) {
                action$parse(children[[i]], srcrefs[[i + 1]])
            }
        } else {
            action$parse(children)
        }
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
        fun <- if (requireNamespace("pacman", quietly = TRUE)) pacman::p_load else
            function(..., char, install = TRUE, update = getOption("pac_update"), character.only = FALSE) NULL
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
            # Use the element's own srcref if available, otherwise inherit parent's
            e_srcref <- attr(e, "srcref")
            if (is.null(e_srcref)) e_srcref <- srcref
            Recall(content, e, env, e_srcref)
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
                            if (name == "nonfuncts" &&
                                    !is.null(env$nonfuncts_n)) {
                                existing <- if (env$nonfuncts_n) {
                                    env$nonfuncts[seq_len(env$nonfuncts_n)]
                                } else {
                                    character()
                                }
                                for (value in setdiff(values, existing)) {
                                    env$nonfuncts_n <- env$nonfuncts_n + 1L
                                    env$nonfuncts[[env$nonfuncts_n]] <- value
                                }
                            } else {
                                env[[name]] <- union(env[[name]], values)
                            }
                        }
                    }
                },
                assign = function(symbol, value, type = get_expr_type(value)) {
                    if (!nzchar(symbol)) return(NULL)

                    env$objects_n <- env$objects_n + 1L
                    env$objects[[env$objects_n]] <- symbol

                    expr_range <- expr_range(srcref)
                    if (!exists(
                        symbol, envir = env$definitions_store,
                        inherits = FALSE)) {
                        env$definitions_n <- env$definitions_n + 1L
                        env$definition_names[[env$definitions_n]] <- symbol
                    }
                    assign(symbol, list(
                        name = symbol,
                        type = type,
                        range = expr_range
                    ), envir = env$definitions_store)

                    doc_line1 <- detect_comments(content, expr_range$start$line) + 1
                    if (doc_line1 <= expr_range$start$line) {
                        comment <- content[seq.int(doc_line1, expr_range$start$line)]
                        if (!exists(
                            symbol, envir = env$documentation_store,
                            inherits = FALSE)) {
                            env$documentation_n <- env$documentation_n + 1L
                            env$documentation_names[[env$documentation_n]] <- symbol
                        }
                        assign(
                            symbol,
                            convert_comment_to_documentation(comment),
                            envir = env$documentation_store
                        )
                    }

                    if (type == "function") {
                        env$functs_n <- env$functs_n + 1L
                        env$functs[[env$functs_n]] <- symbol
                        fun <- null_function
                        formals(fun) <- value[[2L]]
                        if (!exists(
                            symbol, envir = env$functions_store,
                            inherits = FALSE)) {
                            env$functions_n <- env$functions_n + 1L
                            env$function_names[[env$functions_n]] <- symbol
                        }
                        assign(symbol, fun, envir = env$functions_store)
                        assign(
                            symbol, get_signature(symbol, value),
                            envir = env$signatures_store)
                    } else {
                        env$nonfuncts_n <- env$nonfuncts_n + 1L
                        env$nonfuncts[[env$nonfuncts_n]] <- symbol
                    }
                },
                parse = function(expr, srcref_override = NULL) {
                    if (!is.null(srcref_override)) {
                        parse_expr(content, expr, env, srcref_override)
                    } else {
                        parse_expr(content, expr, env, srcref)
                    }
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
#' Parse document content
#'
#' @importFrom digest digest
#' @noRd
normalize_parse_content <- function(content, is_rmarkdown = FALSE,
    parseable_only = TRUE) {
    if (is_rmarkdown) {
        content <- purl(content, parseable_only = parseable_only)
    }
    if (length(content) == 0) {
        content <- ""
    }
    # replace tab with a space since the width of a tab is 1 in LSP but 8 in getParseData().
    gsub("\t", " ", content, fixed = TRUE)
}

get_content_hash <- function(content) {
    digest::digest(content, algo = "xxhash64")
}

parse_document <- function(uri, content, is_rmarkdown = FALSE,
    content_hash = NULL) {
    content <- normalize_parse_content(content, is_rmarkdown)
    if (is.null(content_hash)) content_hash <- get_content_hash(content)

    parse_env <- function() {
        env <- new.env(parent = .GlobalEnv)
        env$packages <- character()
        env$objects <- character()
        env$nonfuncts <- character()
        env$functs <- character()
        env$functions <- list()
        env$signatures <- list()
        env$definitions <- list()
        env$documentation <- list()
        env$xml_data <- NULL
        env$xml_doc <- NULL
        env$completion_data <- completion_parse_data(NULL)
        env$semantic_data <- empty_semantic_data()
        env$reference_index <- reference_parse_data(
            NULL, content, env$completion_data, uri, env$definitions)
        env$content_hash <- content_hash
        env$parse_error <- FALSE
        env
    }
    env <- parse_env()

    logger$info("parse_document: parsing", uri)
    expr <- tryCatch(parse(text = content, keep.source = TRUE), error = function(e) NULL)
    if (!is.null(expr)) {
        capacity <- max(length(expr), 1L)
        env$objects <- character(capacity)
        env$functs <- character(capacity)
        env$nonfuncts <- character(capacity)
        env$objects_n <- 0L
        env$functs_n <- 0L
        env$nonfuncts_n <- 0L
        env$definitions_store <- new.env(hash = TRUE, parent = emptyenv())
        env$documentation_store <- new.env(hash = TRUE, parent = emptyenv())
        env$functions_store <- new.env(hash = TRUE, parent = emptyenv())
        env$signatures_store <- new.env(hash = TRUE, parent = emptyenv())
        env$definition_names <- character(capacity)
        env$documentation_names <- character(capacity)
        env$function_names <- character(capacity)
        env$definitions_n <- 0L
        env$documentation_n <- 0L
        env$functions_n <- 0L

        parse_expr(content, expr, env)
        trim <- function(values, count) {
            if (count) values[seq_len(count)] else character()
        }
        materialize <- function(store, names, count) {
            if (!count) return(list())
            mget(names[seq_len(count)], envir = store, inherits = FALSE)
        }
        env$objects <- trim(env$objects, env$objects_n)
        env$functs <- trim(env$functs, env$functs_n)
        env$nonfuncts <- trim(env$nonfuncts, env$nonfuncts_n)
        env$definitions <- materialize(
            env$definitions_store, env$definition_names, env$definitions_n)
        env$documentation <- materialize(
            env$documentation_store,
            env$documentation_names,
            env$documentation_n
        )
        env$functions <- materialize(
            env$functions_store, env$function_names, env$functions_n)
        env$signatures <- materialize(
            env$signatures_store, env$function_names, env$functions_n)
        rm(list = c(
            "objects_n", "functs_n", "nonfuncts_n",
            "definitions_store", "documentation_store",
            "functions_store", "signatures_store",
            "definition_names", "documentation_names", "function_names",
            "definitions_n", "documentation_n", "functions_n"
        ), envir = env)

        env$packages <- basename(find.package(env$packages, quiet = TRUE))
        data <- utils::getParseData(expr)
        env$completion_data <- completion_parse_data(data)
        env$semantic_data <- semantic_parse_data(data, content)
        env$reference_index <- reference_parse_data(
            data, content, env$completion_data, uri, env$definitions)
        # Performance: XML generation is expensive, but necessary for analysis
        env$xml_data <- xmlparsedata::xml_parse_data(data)
        # IMPORTANT: Do NOT create xml_doc here - this function runs in a child process
        # and xml2 external pointers cannot be serialized across process boundaries.
        # xml_doc will be created in the main process by update_parse_data()
        
    } else {
        # Keep the parse version current even while the user is typing an
        # incomplete expression. Providers can now return an empty result
        # instead of leaving requests queued until some later valid version.
        env$parse_error <- TRUE
        env$xml_data <- paste0(
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n",
            "<exprlist>\n</exprlist>\n"
        )
    }
    env
}


normalize_package_request <- function(packages) {
    enc2utf8(unname(as.character(packages)))
}


parse_callback <- function(self, uri, version, parse_data) {
    workspace <- self$get_workspace(uri)
    if (is.null(parse_data) || !workspace$documents$has(uri)) return(NULL)
    logger$info("parse_callback called:", list(uri = uri, version = version))
    doc <- workspace$documents$get(uri)
    if (!is.null(version) && !identical(doc$version, version)) {
        logger$info("parse_callback: discarded stale result", list(
            uri = uri,
            result_version = version,
            document_version = doc$version
        ))
        return(NULL)
    }

    parse_data$version <- version
    old_parse_data <- doc$parse_data
    previous_packages <- doc$requested_packages
    if (is.null(previous_packages) && !is.null(old_parse_data) &&
            !isTRUE(old_parse_data$parse_error)) {
        previous_packages <- normalize_package_request(old_parse_data$packages)
    }
    workspace$update_parse_data(uri, parse_data)

    if (isTRUE(doc$pending_diagnostics)) {
        doc$pending_diagnostics <- FALSE
        schedule_diagnostics(
            self, uri, doc, delay = doc$diagnostics_delay)
    }

    # Cache parse results in the main process (child-process caches are not shared)
    if (!is.null(parse_data$content_hash)) {
        cache_entry <- as.list(parse_data)
        cache_entry$xml_doc <- NULL
        workspace$parse_cache$set(parse_data$content_hash, cache_entry)
    }

    if (!isTRUE(parse_data$parse_error)) {
        requested_packages <- normalize_package_request(parse_data$packages)
        if (is.null(previous_packages) ||
                !identical(previous_packages, requested_packages)) {
            doc$requested_packages <- requested_packages
            self$resolve_task_manager$add_task(
                uri,
                resolve_task(self, uri, doc, requested_packages)
            )
            doc$loaded_packages <- requested_packages
            workspace$update_loaded_packages()
        } else if (is.null(doc$requested_packages)) {
            doc$requested_packages <- requested_packages
        }
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
                self$deliver(ResponseErrorMessage$new(
                    item$id,
                    "RequestCancelled",
                    "Request superseded by a newer parse result"
                ))
                queue$pop()
            } else {
                break
            }
        }
    }
}

parse_task <- function(self, uri, document, delay = 0) {
    version <- document$version
    if (document$is_rmarkdown) {
        content <- document$content
        cache_content <- normalize_parse_content(
            content, is_rmarkdown = TRUE, parseable_only = FALSE)
    } else {
        content <- normalize_parse_content(document$content)
        cache_content <- content
    }
    content_hash <- get_content_hash(cache_content)

    # Check cache in the main process before spawning a child task
    workspace <- self$get_workspace(uri)
    if (workspace$parse_cache$has(content_hash)) {
        logger$info("parse_task: cache hit for", uri)
        cached_entry <- workspace$parse_cache$get(content_hash)
        cached_env <- list2env(cached_entry, parent = .GlobalEnv)
        parse_callback(self, uri, version, cached_env)
        return(NULL)
    }

    create_task(
        target = package_call(parse_document),
        args = list(
            uri = uri,
            content = content,
            is_rmarkdown = document$is_rmarkdown,
            content_hash = content_hash
        ),
        callback = function(result) parse_callback(self, uri, version, result),
        error = function(e) logger$info("parse_task:", e),
        delay = delay
    )
}

resolve_callback <- function(self, uri, version, packages) {
    workspace <- self$get_workspace(uri)
    if (!workspace$documents$has(uri)) return(NULL)
    logger$info("resolve_callback called:", list(uri = uri, version = version))
    workspace$load_packages(packages)
    doc <- workspace$documents$get(uri)
    doc$loaded_packages <- packages
    workspace$update_loaded_packages()
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
