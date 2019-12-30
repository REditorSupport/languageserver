Document <- R6::R6Class(
    "Document",
    public = list(
        uri = NULL,
        nline = 0,
        content = NULL,
        is_rmarkdown = NULL,

        initialize = function(uri, content = NULL) {
            self$uri <- uri
            self$is_rmarkdown <- is_rmarkdown(self$uri)
            if (!is.null(content)) {
                self$set(content)
            }
        },

        set = function(content) {
            # remove last empty line
            nline <- length(content)
            if (nline > 0L && !nzchar(content[nline])) {
                content <- content[-nline]
                nline <- nline - 1
            }
            self$nline <- nline
            self$content <- content
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
    col <- point$line
    .Call("enclosed_by_quotes", PACKAGE = "languageserver", text, col)
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


parse_expr <- function(content, expr, env, level = 0L, srcref = attr(expr, "srcref")) {
    if (length(expr) == 0L || is.symbol(expr)) {
          return(env)
    }
    for (i in seq_along(expr)) {
        e <- expr[[i]]
        if (missing(e) || !is.call(e) || !is.symbol(e[[1L]])) next
        f <- as.character(e[[1L]])
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
        } else if (f %in% c("<-", "=") && length(e) == 3L && is.symbol(e[[2L]])) {
            funct <- as.character(e[[2L]])
            env$objects <- c(env$objects, funct)
            if (is.call(e[[3L]]) && e[[3L]][[1L]] == "function") {
                # functions
                env$functs <- c(env$functs, funct)
                func <- e[[3L]]
                env$formals[[funct]] <- func[[2L]]

                signature <- func
                signature <- format(signature[1:2])
                signature <- paste0(trimws(signature, which = "left"), collapse = "")
                signature <- gsub("^function\\s*", funct, signature)
                signature <- gsub("\\s*NULL$", "", signature)
                env$signatures[[funct]] <- signature

                expr_range <- expr_range(cur_srcref)
                env$definition_ranges[[funct]] <- expr_range

                doc_line1 <- detect_comments(content, expr_range$start$line) + 1
                if (doc_line1 <= expr_range$start$line) {
                    env$documentation[[funct]] <- uncomment(
                        content[seq.int(doc_line1, expr_range$start$line)])
                }
            } else {
                # symbols
                env$nonfuncts <- c(env$nonfuncts, funct)
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
parse_document <- function(path, content = NULL, resolve = FALSE) {
    if (is.null(content)) {
        content <- readr::read_lines(path)
    }
    if (is_rmarkdown(path)) {
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
            env$definition_ranges <- list()
            env$documentation <- list()
            env$xml_data <- NULL
            env$xml_doc <- NULL
            env
        }
        env <- parse_env()
        parse_expr(content, expr, env)
        xml_data <- xmlparsedata::xml_parse_data(expr)
        env$xml_data <- xml_data
        if (resolve) {
            env$packages <- resolve_attached_packages(env$packages)
        }
        env
    }
}


parse_callback <- function(self, uri, parse_data) {
    if (is.null(parse_data)) return(NULL)
    logger$info("parse_callback called")
    self$workspace$update_parse_data(uri, parse_data)
}


parse_task <- function(self, uri, document, resolve = FALSE) {
    if (is.null(document)) {
        content <- NULL
    } else {
        content <- document$content
    }
    create_task(
        parse_document,
        list(path = path_from_uri(uri), content = content, resolve = resolve),
        callback = function(result) parse_callback(self, uri, result))
}
