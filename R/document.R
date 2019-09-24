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

        line_substr = function(row, start = 0, end = Inf) {
            # start and end are based on UTF-16
            line <- self$line0(row)
            pos <- code_point_to_unit(line, c(start, end))
            substr(line, pos[1] + 1, pos[2])
        },

        detect_token = function(position) {
            text_before <- self$line_substr(position$line, end = position$character)
            token <- stringr::str_match(text_before, "\\b(?<!\\$)(?:[^\\W]|\\.|:)+$")[1]
            logger$info("token:", token)
            if (is.na(token)) {
                ""
            } else {
                token
            }
        },

        detect_call = function(position) {
            row <- position$line
            text <- self$line0(row)
            column <- code_point_to_unit(text, position$character)

            if (position$character > 0) {
                loc <- content_backward_search(self$content, row, column - 1, "(")
            } else {
                loc <- c(-1, -1)
            }

            if (loc[1] < 0 || loc[2] < 0)
                return(list())

            trim_content <- trimws(substr(text, 1, loc[2] + 1))

            call <- stringr::str_match(
                trim_content,
                "\\b(?<!\\$)(?:([a-zA-Z][a-zA-Z0-9.]+):::?)?((?:[^\\W_]|\\.)(?:[^\\W]|\\.)*)\\($"
            )

            logger$info("call:", call)

            if (is.na(call[2])) {
                list(funct = call[3])
            } else {
                list(package = call[2], funct = call[3])
            }
        },

        detect_hover = function(position) {
            row <- position$line
            text <- self$line0(row)
            column <- code_point_to_unit(text, position$character)

            first <- stringr::str_match(
                substr(text, 1, column),
                "\\b(?:([a-zA-Z][a-zA-Z0-9.]+):::?)?((?:[^\\W_]|\\.)(?:[^\\W]|\\.)*)$"
            )[1]
            second <- stringr::str_match(
                substr(text, column + 1, nchar(text)),
                "^(?:[^\\W_]|\\.)(?:[^\\W]|\\.)*\\b"
            )[1]

            if (is.na(first)) first <- ""
            if (is.na(second)) second <- ""

            logger$info("hover:", first, second)

            # discard for example R6 methods
            pt <- column - nchar(first)
            if (substr(text, pt, pt) == "$") {
                hover_text <- ""
            } else {
                hover_text <- paste0(first, second)
            }

            list(
                range = range(
                    start = position(line = row, character = position$character - ncodeunit(first)),
                    end = position(line = row, character = position$character + ncodeunit(second))
                ),
                text = hover_text
            )
        }
    )
)


#' search backwards in a document content for a specific character
#'
#' @param content a character vector
#' @param row an 0-indexed integer
#' @param column an 0-indexed integer
#' @param char a single character
#' @param skip_empty_line a logical
#'
#' @return a tuple of positive integers, the row and column position of the
#' character if found, otherwise (-1, -1)
content_backward_search <- function(content, row, column, char, skip_empty_line = TRUE) {
    # TODO: adjust for UTF-16
    .Call("content_backward_search",
        PACKAGE = "languageserver",
        content, row, column, char, skip_empty_line
    )
}


#' parse a document
#'
#' Build the list of called packages, functions, variables, formals and
#' signatures in the document in order to add them to the current [Workspace].
#'
#' @param path a character, the path to the document
parse_document <- function(path) {
    temp_file <- NULL
    if (is_rmarkdown(path)) {
        temp_file <- tempfile(fileext = ".R")
        path <- tryCatch({
            knitr::purl(path, output = temp_file, quiet = TRUE)
        }, error = function(e) path)
    }
    expr <- tryCatch(parse(path, keep.source = TRUE), error = function(e) NULL)
    if (!is.null(temp_file) && file.exists(temp_file)) {
        file.remove(temp_file)
    }
    env <- parse_env()
    parse_expr(expr, env)
    env$packages <- resolve_package_dependencies(env$packages)
    env
}

parse_env <- function() {
    env <- new.env()
    env$packages <- character()
    env$nonfuncts <- character()
    env$functs <- character()
    env$formals <- list()
    env$signatures <- list()
    env$definition_ranges <- list()
    env
}

parse_expr <- function(expr, env, level = 0L, srcref = attr(expr, "srcref")) {
    if (length(expr) == 0L || is.symbol(expr)) {
          return(env)
      }
    for (i in seq_along(expr)) {
        e <- expr[[i]]
        if (!is.call(e) || !is.symbol(e[[1L]])) next
        f <- as.character(e[[1L]])
        cur_srcref <- if (level == 0L) srcref[[i]] else srcref
        if (f %in% c("{", "(")) {
            Recall(e[-1L], env, level + 1L, cur_srcref)
        } else if (f == "if") {
            Recall(e[[2L]], env, level + 1L, cur_srcref)
            Recall(e[[3L]], env, level + 1L, cur_srcref)
            if (length(e) == 4L) {
                Recall(e[[4L]], env, level + 1L, cur_srcref)
            }
        } else if (f == "for") {
            if (is.symbol(e[[2L]])) {
                env$nonfuncts <- c(env$nonfuncts, as.character(e[[2L]]))
            }
            Recall(e[[4L]], env, level + 1L, cur_srcref)
        } else if (f == "while") {
            Recall(e[[2L]], env, level + 1L, cur_srcref)
            Recall(e[[3L]], env, level + 1L, cur_srcref)
        } else if (f == "repeat") {
            Recall(e[[2L]], env, level + 1L, cur_srcref)
        } else if (f %in% c("<-", "=") && length(e) == 3L && is.symbol(e[[2L]])) {
            funct <- as.character(e[[2L]])
            env$objects <- c(env$objects, funct)
            if (is.call(e[[3L]]) && e[[3L]][[1L]] == "function") {
                env$functs <- c(env$functs, funct)
                func <- e[[3L]]
                env$formals[[funct]] <- func[[2L]]
                signature <- func
                signature <- utils::capture.output(print(signature[1:2]))
                signature <- paste0(trimws(signature, which = "left"), collapse = "\n")
                signature <- trimws(gsub("NULL\\s*$", "", signature))
                env$signatures[[funct]] <- signature
                # R is 1-indexed, language server is 0-indexed
                first_line <- cur_srcref[1] - 1
                first_char <- cur_srcref[5] - 1
                last_line <- cur_srcref[3] - 1
                last_char <- cur_srcref[6] - 1
                definition_range <- range(
                    position(first_line, first_char),
                    position(last_line, last_char)
                )
                env$definition_ranges[[funct]] <- definition_range
            } else {
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
