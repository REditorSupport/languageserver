Document <- R6::R6Class(
    "Document",
    public = list(
        uri = NULL,
        language = NULL,
        version = NULL,
        is_open = FALSE,
        nline = 0,
        content = NULL,
        call_scan_cache = NULL,
        parse_data = NULL,
        parse_generation = 0L,
        parse_failure = NULL,
        parsed_content = NULL,
        inlay_hint_data = NULL,
        inlay_hint_formals = NULL,
        is_rmarkdown = NULL,
        regions = NULL,
        loaded_packages = NULL,
        requested_packages = NULL,
        member_refresh = FALSE,
        member_receivers = NULL,
        s3_calls = NULL,
        s3_identity = NULL,
        member_revision = NULL,
        member_ordinary_edit = FALSE,
        pending_diagnostics = FALSE,
        diagnostics_generation = NULL,
        index_content_hash = NULL,
        diagnostics_delay = 0,

        initialize = function(uri, language = NULL, version = NULL, content = "") {
            self$uri <- uri
            self$language <- language
            self$version <- version
            self$is_rmarkdown <- is_rmarkdown(uri, language)
            self$member_receivers <- ByteLruCache$new(4 * 1024^2, max_entries = 32L)
            self$s3_calls <- ByteLruCache$new(1 * 1024^2, max_entries = 32L)
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

        set_content = function(version, content, change = NULL) {
            self$member_receivers$clear()
            self$s3_calls$clear()
            current <- identical(self$version, self$parse_data$version) || isTRUE(self$member_ordinary_edit)
            # A standalone identifier changes no declaration, call, delimiter
            # or lexical scope. Full replacements and all structural edits
            # still require the accepted current-version parse.
            self$member_ordinary_edit <- FALSE
            if (current && !self$is_rmarkdown && !isTRUE(self$parse_data$parse_error) &&
                    !is.null(change$range) && length(content) == length(self$content) &&
                    change$range$start$line == length(content) - 1L &&
                    change$range$end$line == length(content) - 1L) {
                changed <- length(content)
                items <- self$parse_data$member_data$items
                last <- if (length(items)) items[[length(items)]]
                standalone <- is.null(last) || last$end[[1L]] < length(content) - 1L ||
                    (is.symbol(last$expr) && last$start[[1L]] == length(content) - 1L)
                if (standalone && !identical(content[[changed]], self$content[[changed]])) {
                    pattern <- "^[ \\t]*[[:alnum:]_.]*[ \\t]*$"
                    self$member_ordinary_edit <- grepl(pattern, self$content[[changed]]) &&
                        grepl(pattern, content[[changed]])
                }
            }
            if (!identical(self$content, content)) {
                self$inlay_hint_data <- inlay_hint_apply_change(self$inlay_hint_data, change, self$content)
            }
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
                self$set_content(version, c(before, changed, after), change)
            }
        },

        update_parse_data = function(parse_data) {
            self$member_receivers$clear()
            self$s3_calls$clear()
            self$member_ordinary_edit <- FALSE
            self$parse_data <- parse_data
            self$parsed_content <- self$content
            if (!isTRUE(parse_data$parse_error) && !isTRUE(parse_data$inlay_hint_incomplete)) {
                self$inlay_hint_data <- parse_data$range_data[c(
                    "calls", "arguments", "argument_order", "argument_lines"
                )]
                self$inlay_hint_data$last_call_end <-
                    inlay_hint_last_call_end(self$inlay_hint_data$calls)
                self$inlay_hint_formals <- new.env(parent = emptyenv())
            }
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
                if (is.null(self$call_scan_cache)) {
                    self$call_scan_cache <- .Call("new_bracket_scan_cache_c",
                        PACKAGE = "languageserver")
                }
                fub_result <- .Call("find_unbalanced_bracket_cached_c",
                    self$content, row, col - 1, FALSE, self$call_scan_cache,
                    PACKAGE = "languageserver")
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
                token = result$token,
                opening = list(row = loc[[1L]], col = loc[[2L]])
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

null_function <- utils::removeSource(local(function() NULL, baseenv()))
any_args_function <- utils::removeSource(local(function(...) NULL, baseenv()))

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
                            if (name == "nonfuncts") {
                                for (value in values) {
                                    if (!exists(value, envir = env$nonfuncts_seen,
                                            inherits = FALSE)) {
                                        env$nonfuncts$push(value)
                                        assign(value, TRUE, envir = env$nonfuncts_seen)
                                    }
                                }
                            } else {
                                env[[name]] <- union(env[[name]], values)
                            }
                        }
                    }
                },
                assign = function(symbol, value, type = get_expr_type(value)) {
                    if (!nzchar(symbol)) return(NULL)

                    env$objects$push(symbol)

                    expr_range <- expr_range(srcref)
                    if (!exists(
                        symbol, envir = env$definitions_store,
                        inherits = FALSE)) {
                        env$definition_names$push(symbol)
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
                            env$documentation_names$push(symbol)
                        }
                        assign(
                            symbol,
                            convert_comment_to_documentation(comment),
                            envir = env$documentation_store
                        )
                    }

                    if (type == "function") {
                        env$functs$push(symbol)
                        fun <- null_function
                        formals(fun) <- value[[2L]]
                        if (!exists(
                            symbol, envir = env$functions_store,
                            inherits = FALSE)) {
                            env$function_names$push(symbol)
                        }
                        assign(symbol, utils::removeSource(fun), envir = env$functions_store)
                        assign(
                            symbol, get_signature(symbol, value),
                            envir = env$signatures_store)
                    } else {
                        env$nonfuncts$push(symbol)
                        assign(symbol, TRUE, envir = env$nonfuncts_seen)
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
    content_hash = NULL, include_xml = TRUE) {
    parse_content <- normalize_parse_content(content, is_rmarkdown)
    inlay_hint_incomplete_lines <- if (is_rmarkdown) {
        which(parse_content != normalize_parse_content(
            content, is_rmarkdown, parseable_only = FALSE)) - 1L
    } else {
        integer()
    }
    content <- parse_content
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
        env$xml_deferred <- FALSE
        env$syntax_data <- syntax_parse_data(NULL)
        env$outline_data <- outline_parse_data(env$syntax_data)
        env$class_data <- NULL
        env$navigation_data <- navigation_parse_data(env$syntax_data)
        env$completion_data <- completion_parse_data(NULL)
        env$member_data <- NULL
        env$semantic_data <- empty_semantic_data()
        env$range_data <- range_provider_parse_data(NULL, content)
        env$source_specs <- list()
        env$reference_index <- reference_parse_data(
            NULL, content, env$completion_data, uri, env$definitions)
        env$content_hash <- content_hash
        env$parse_error <- FALSE
        env$inlay_hint_incomplete <- length(inlay_hint_incomplete_lines) > 0L
        env$inlay_hint_incomplete_lines <- inlay_hint_incomplete_lines
        env
    }
    env <- parse_env()

    logger$info("parse_document: parsing", uri)
    expr <- tryCatch(parse(text = content, keep.source = TRUE), error = function(e) NULL)
    if (!is.null(expr)) {
        # Native queues append in constant time. Updating vectors bound to an
        # environment copies their whole capacity on every assignment.
        env$objects <- collections::queue()
        env$functs <- collections::queue()
        env$nonfuncts <- collections::queue()
        env$nonfuncts_seen <- new.env(hash = TRUE, parent = emptyenv())
        env$definitions_store <- new.env(hash = TRUE, parent = emptyenv())
        env$documentation_store <- new.env(hash = TRUE, parent = emptyenv())
        env$functions_store <- new.env(hash = TRUE, parent = emptyenv())
        env$signatures_store <- new.env(hash = TRUE, parent = emptyenv())
        env$definition_names <- collections::queue()
        env$documentation_names <- collections::queue()
        env$function_names <- collections::queue()

        parse_expr(content, expr, env)
        names_vector <- function(queue) {
            if (queue$size()) unlist(queue$as_list(), use.names = FALSE)
            else character()
        }
        materialize <- function(store, names) {
            if (!names$size()) return(list())
            mget(names_vector(names), envir = store, inherits = FALSE)
        }
        env$objects <- names_vector(env$objects)
        env$functs <- names_vector(env$functs)
        env$nonfuncts <- names_vector(env$nonfuncts)
        env$definitions <- materialize(
            env$definitions_store, env$definition_names)
        env$documentation <- materialize(
            env$documentation_store,
            env$documentation_names
        )
        env$functions <- materialize(
            env$functions_store, env$function_names)
        env$signatures <- materialize(
            env$signatures_store, env$function_names)
        rm(list = c(
            "nonfuncts_seen",
            "definitions_store", "documentation_store",
            "functions_store", "signatures_store",
            "definition_names", "documentation_names", "function_names"
        ), envir = env)

        env$packages <- basename(find.package(env$packages, quiet = TRUE))
        data <- utils::getParseData(expr)
        env$syntax_data <- syntax_parse_data(data)
        env$outline_data <- outline_parse_data(env$syntax_data)
        env$navigation_data <- navigation_parse_data(env$syntax_data)
        env$member_data <- member_document_index(content, expr, data)
        env$completion_data <- completion_parse_data(data, env$syntax_data)
        env$semantic_data <- semantic_parse_data(data, content, env$syntax_data)
        env$range_data <- range_provider_parse_data(data, content, env$syntax_data)
        env$source_specs <- index_source_specs(expr)
        env$reference_index <- reference_parse_data(
            data, content, env$completion_data, uri, env$definitions, env$syntax_data)
        # Indexed providers need no XML. Keep it out of worker snapshots and
        # materialize it from syntax only for class extraction or a legacy provider.
        env$xml_deferred <- !include_xml
        if (include_xml) env$xml_data <- syntax_xml(env$syntax_data)
        xml <- function() if (is.null(env$xml_data)) syntax_xml(env$syntax_data) else env$xml_data
        env$class_data <- tryCatch(class_parse_data(uri, content, xml(),
                env$syntax_data, env$definitions), error = function(e) NULL)
        # These original-id lookup tables are used only while constructing
        # worker indexes. Providers use the compact physical syntax tree.
        env$syntax_data[c("id_row", "id_first", "id_next", "id_function_scope", "scope_row",
                "parent", "first_child", "next_sibling", "function_scope", "top", "order")] <- NULL
        # IMPORTANT: Do NOT create xml_doc here - this function runs in a child process
        # and xml2 external pointers cannot be serialized across process boundaries.
        # Legacy providers materialize it lazily in the main process.
    } else {
        # Keep the parse version current even while the user is typing an
        # incomplete expression. Providers can now return an empty result
        # instead of leaving requests queued until some later valid version.
        env$member_data <- member_document_index(content)
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

member_package_request <- function(parse_data) {
    list(
        packages = normalize_package_request(parse_data$packages),
        namespace_packages = normalize_package_request(unique(parse_data$member_data$packages))
    )
}


parse_callback <- function(self, uri, version, parse_data, source_content = NULL, cache = TRUE) {
    workspace <- self$get_workspace(uri)
    if (is.null(parse_data) || !workspace$documents$has(uri)) return(NULL)
    logger$info("parse_callback called:", list(uri = uri, version = version))
    doc <- workspace$documents$get(uri)
    if ((!is.null(version) && !identical(doc$version, version)) ||
            (!is.null(source_content) && !identical(doc$content, source_content))) {
        logger$info("parse_callback: discarded stale result", list(
            uri = uri,
            result_version = version,
            document_version = doc$version
        ))
        return(NULL)
    }

    parse_data$version <- version
    doc$parse_failure <- NULL
    old_parse_data <- doc$parse_data
    previous_packages <- doc$requested_packages
    if (is.null(previous_packages) && !is.null(old_parse_data) &&
            !isTRUE(old_parse_data$parse_error)) {
        previous_packages <- member_package_request(old_parse_data)
    }
    topology <- workspace$index$topology_revision
    workspace$update_parse_data(uri, parse_data)
    if (!is.null(workspace$index) && isTRUE(workspace$index$enabled) &&
            is.function(self$refresh_index_documents)) {
        self$refresh_index_documents(workspace, uri,
            refresh = isTRUE(doc$is_open) || is.null(workspace$index$package_root_for_uri(uri)) ||
                length(workspace$index$source_edges$get(index_canonical_uri(uri), character())) > 0L ||
                !identical(topology, workspace$index$topology_revision))
    }

    if (isTRUE(doc$pending_diagnostics)) {
        doc$pending_diagnostics <- FALSE
        schedule_diagnostics(
            self, uri, doc, delay = doc$diagnostics_delay)
    }

    # Cache parse results in the main process (child-process caches are not shared)
    if (cache && !is.null(parse_data$content_hash)) {
        cache_entry <- as.list(parse_data)
        cache_entry$xml_doc <- NULL
        if (!is.null(cache_entry$navigation_data)) {
            cache_entry$navigation_data$cache <- new.env(hash = TRUE, parent = emptyenv())
        }
        for (name in c("document_symbols_cache", "folding_ranges_cache", "link_strings_cache",
                "linked_editing_cache", "colors_cache")) cache_entry[[name]] <- NULL
        # Local reference identities and cached flat symbols contain the URI.
        # Identical text in another file may share semantic tokens, but must
        # not reuse those document-specific provider results.
        cache_entry$cache_uri <- uri
        source_bytes <- sum(vapply(cache_entry$member_data$items, function(item) {
            if (is.null(item$source$bytes)) 0 else item$source$bytes
        }, numeric(1L)))
        if (!is.null(cache_entry$navigation_data$bytes)) {
            source_bytes <- source_bytes + cache_entry$navigation_data$bytes
        }
        if (!is.null(cache_entry$member_data$source_bytes)) {
            source_bytes <- source_bytes + cache_entry$member_data$source_bytes
        }
        if (!is.null(cache_entry$class_data$bytes)) {
            source_bytes <- source_bytes + cache_entry$class_data$bytes
        }
        if (inherits(workspace$parse_cache, "ByteLruCache")) {
            workspace$parse_cache$set(parse_data$content_hash, cache_entry, additional_bytes = source_bytes)
        } else {
            workspace$parse_cache$set(parse_data$content_hash, cache_entry)
        }
    }

    if (!isTRUE(parse_data$parse_error)) {
        requested_packages <- member_package_request(parse_data)
        if (is.null(previous_packages) || isTRUE(doc$member_refresh) ||
                !identical(previous_packages, requested_packages)) {
            doc$member_refresh <- FALSE
            doc$requested_packages <- requested_packages
            self$resolve_task_manager$add_task(
                uri,
                resolve_task(self, uri, doc, normalize_package_request(parse_data$packages),
                    namespace_packages = parse_data$member_data$packages)
            )
            loaded_packages <- normalize_package_request(parse_data$packages)
            changed <- !setequal(doc$loaded_packages, loaded_packages)
            doc$loaded_packages <- loaded_packages
            if (changed) workspace$update_loaded_packages()
        } else if (is.null(doc$requested_packages)) {
            doc$requested_packages <- requested_packages
        }
    }

    if (is.function(self$queue_pending_replies)) {
        self$queue_pending_replies(uri)
    } else {
        drain_pending_replies(self, uri, version)
    }
    invisible(NULL)
}

#' Serve a bounded round of pending providers, rotating between methods
#' @noRd
drain_pending_replies <- function(self, uri, version, max_replies = Inf, first = 1L) {
    queues <- self$pending_replies$get(uri, NULL)
    n <- length(queues)
    processed <- 0L
    if (!n) return(list(processed = processed, ready = FALSE, first = 1L))
    eligible <- function(queue, method) {
        if (!queue$size()) return(FALSE)
        item <- queue$peek()
        if (!is.null(version) && !is.null(item$version) && item$version > version) return(FALSE)
        if (!is.null(document) && identical(item$version, document$version) &&
                method %in% c("textDocument/completion", "textDocument/hover", "textDocument/signatureHelp")) {
            point <- document$from_lsp_position(item$params$position)
            if (s3_metadata_waiting(self, uri, document, point, hover = identical(method, "textDocument/hover"))) return(FALSE)
        }
        TRUE
    }
    repeat {
        document <- if (is.function(self$get_workspace)) self$get_workspace(uri)$documents$get(uri, NULL)
        ready <- vapply(names(queues), function(method) eligible(queues[[method]], method), logical(1L))
        available <- which(ready)
        if (!length(available) || processed >= max_replies) break
        after <- available[available >= first]
        selected <- if (length(after)) after[[1L]] else available[[1L]]
        item <- queues[[selected]]$pop()
        first <- selected %% n + 1L
        if (!is.null(version) && !is.null(item$version) && item$version < version) {
            self$deliver(ResponseErrorMessage$new(item$id, "RequestCancelled",
                    "Request superseded by a newer parse result"))
        } else {
            handler <- self$request_handlers[[names(queues)[[selected]]]]
            tryCatchStack(handler(self, item$id, item$params), error = function(e) {
                self$deliver(ResponseErrorMessage$new(item$id, "InternalError", to_string(e)))
            })
        }
        processed <- processed + 1L
    }
    list(processed = processed, ready = any(ready), first = first)
}

parse_task <- function(self, uri, document, delay = 0, retry = TRUE) {
    version <- document$version
    source_content <- document$content
    document$parse_generation <- document$parse_generation + 1L
    document$parse_failure <- NULL
    generation <- document$parse_generation
    was_open <- document$is_open
    if (!is.null(document$parse_data) &&
            identical(version, document$parse_data$version) &&
            identical(source_content, document$parsed_content)) {
        # Saving unchanged content still refreshes diagnostics and package
        # metadata, but must not reparse a buffer evicted from the LRU cache.
        parse_callback(self, uri, version, document$parse_data, source_content, cache = FALSE)
        return(NULL)
    }
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
    index <- workspace$index
    index_key <- if (!is.null(index)) index$diagnostics_generation(uri)$index
    current <- function() {
        identical(self$get_workspace(uri), workspace) &&
            identical(workspace$index, index) &&
            (is.null(index) || identical(index$diagnostics_generation(uri)$index, index_key)) &&
            identical(workspace$documents$get(uri, NULL), document) &&
            identical(document$parse_generation, generation) &&
            identical(document$is_open, was_open) &&
            identical(document$version, version) && identical(document$content, source_content)
    }
    if (!is.null(index) && index$parse_failed(uri)) {
        index$await_parse(uri, cacheable = !isTRUE(document$is_open))
    }
    if (workspace$parse_cache$has(content_hash)) {
        cached_entry <- workspace$parse_cache$get(content_hash)
        if (identical(cached_entry$cache_uri, uri)) {
            logger$info("parse_task: cache hit for", uri)
            cached_env <- list2env(cached_entry, parent = .GlobalEnv)
            if (!is.null(cached_env$navigation_data)) {
                cached_env$navigation_data$cache <- new.env(hash = TRUE, parent = emptyenv())
            }
            # get() already refreshed recency. Rebuilding and sizing the same
            # snapshot would repeat worker-result work on the request loop.
            parse_callback(self, uri, version, cached_env, source_content, cache = FALSE)
            return(NULL)
        }
    }

    create_task(
        target = package_call(parse_document),
        args = list(
            uri = uri,
            content = content,
            is_rmarkdown = document$is_rmarkdown,
            content_hash = content_hash,
            include_xml = FALSE
        ),
        callback = function(result) parse_callback(self, uri, version, result, source_content),
        error = function(e) {
            logger$info("parse_task:", e)
            if (!current()) return(invisible(NULL))
            if (retry) {
                # TaskManager removes the running entry after this callback.
                # Replacing it here would cancel the task completing now.
                self$queue_parse_retry(uri, function() {
                    if (current()) self$parse_task_manager$add_task(uri,
                        parse_task(self, uri, document, retry = FALSE))
                })
            } else {
                document$parse_failure <- list(version = version,
                    message = paste("Document parsing failed:", conditionMessage(e)))
                fail_pending_replies(self, uri, version)
                if (!is.null(index)) {
                    index$fail_parse(uri)
                    self$refresh_index_documents(workspace)
                }
            }
        },
        delay = delay
    )
}

reply_parse_failure <- function(self, id, document) {
    failure <- document$parse_failure
    if (is.null(failure) || !identical(failure$version, document$version)) return(FALSE)
    self$deliver(ResponseErrorMessage$new(id, "InternalError", failure$message))
    TRUE
}

fail_pending_replies <- function(self, uri, version) {
    document <- self$get_workspace(uri)$documents$get(uri)
    for (queue in self$pending_replies$get(uri, NULL)) {
        retained <- list()
        while (queue$size()) {
            item <- queue$pop()
            if (!is.null(item$version) && !is.null(version) && item$version > version) {
                retained[[length(retained) + 1L]] <- item
            } else if (!is.null(item$version) && !is.null(version) && item$version < version) {
                self$deliver(ResponseErrorMessage$new(item$id, "RequestCancelled",
                        "Request superseded by a newer parse result"))
            } else {
                reply_parse_failure(self, item$id, document)
            }
        }
        for (item in retained) queue$push(item)
    }
    invisible(NULL)
}

resolve_callback <- function(self, uri, version, packages) {
    workspace <- self$get_workspace(uri)
    if (!workspace$documents$has(uri)) return(NULL)
    doc <- workspace$documents$get(uri)
    if (!identical(doc$version, version) &&
            !(is.list(packages) && !is.null(packages$requested) &&
                identical(doc$requested_packages, packages$requested))) return(NULL)
    if (is.list(packages) && !is.null(packages$members)) {
        # Background workspace files also prepare metadata. Prefer packages
        # requested by open documents when those results compete for space.
        open_documents <- Filter(function(document) isTRUE(document$is_open), workspace$documents$values())
        protect <- unique(unlist(lapply(open_documents, function(document) {
            c(document$requested_packages$packages, document$requested_packages$namespace_packages)
        }), use.names = FALSE))
        for (package in names(packages$members)) {
            snapshot <- packages$members[[package]]
            if (inherits(workspace$member_metadata, "MemberMetadataCache")) {
                previous <- workspace$member_metadata$catalog(package)
                if (isTRUE(nzchar(snapshot$generation)) && identical(previous$generation, snapshot$generation)) next
                workspace$member_metadata$set(package, snapshot, protect = protect)
                next
            }
            index <- member_index_thaw(snapshot)
            if (!is.null(index)) {
                if (inherits(workspace$member_metadata, c("ByteLruCache", "MemberMetadataCache"))) {
                    workspace$member_metadata$set(package, as.list(index), protect = protect)
                } else {
                    workspace$member_metadata$set(package, as.list(index))
                }
            }
        }
        if (!is.null(packages$s3) && !is.null(workspace$s3_metadata)) {
            for (package in names(packages$s3)) workspace$s3_metadata$set(package, packages$s3[[package]], protect)
        }
        packages <- packages$packages
    }
    logger$info("resolve_callback called:", list(uri = uri, version = version))
    workspace$load_packages(packages)
    doc <- workspace$documents$get(uri)
    doc$loaded_packages <- packages
    workspace$update_loaded_packages()
    s3_queue_ready(self, uri)
}

resolve_task <- function(self, uri, document, packages, delay = 0, namespace_packages = character()) {
    # Startup packages already belong to the workspace. A document with no
    # package requests needs no subprocess to rediscover that same search path.
    if (!length(packages) && !length(namespace_packages)) return(NULL)
    version <- document$version
    create_task(
        target = package_call(member_resolve_packages),
        args = list(pkgs = packages, lib_paths = .libPaths(), namespace_packages = namespace_packages,
            prepare = isTRUE(lsp_settings$get("member_completion")),
            prepare_s3 = isTRUE(lsp_settings$get("s3_dispatch"))),
        callback = function(result) resolve_callback(self, uri, version, result),
        error = function(e) {
            logger$info("resolve_task:", e)
            workspace <- self$get_workspace(uri)
            for (package in unique(c(startup_packages, packages, namespace_packages))) workspace$s3_metadata$set(package,
                list(schema = 1L, package = package, generation = "", functions = list(), objects = list(), methods = list(), dependencies = character(), error = conditionMessage(e)))
            s3_queue_ready(self, uri)
        },
        delay = delay,
        profiles = FALSE
    )
}
