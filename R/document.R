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
            if (!nzchar(content[nline])) {
                content <- content[-nline]
                nline <- nline - 1
            }
            self$nline <- nline
            self$content <- content
        },

        line = function(lineno) {
            if (lineno <= self$nline) {
                line <- self$content[lineno]
            } else {
                line <- ""
            }
            line
        }
    )
)


#' search backwards in a document for a specific character
#'
#' @template document
#' @template position
#' @param char a single character
#' @param skip_empty_line a logical
#'
#' @return a tuple of positive integers, the line and column position of the
#' character if found, otherwise (-1, -1)
document_backward_search <- function(document, position, char, skip_empty_line = TRUE) {
    line <- position$line
    character <- position$character
    # TODO: adjust for UTF-16
    .Call("document_backward_search",
        PACKAGE = "languageserver",
        document$content, line, character - 1, char, skip_empty_line
    )
}


#' detect if the current position is inside a closure
#'
#' @template document
#' @template position
detect_closure <- function(document, position) {
    if (position$character > 0 && !is.null(document)) {
        loc <- document_backward_search(document, position, "(")
    } else {
        loc <- c(-1, -1)
    }

    if (loc[1] >= 0 && loc[2] >= 0) {
        content <- document$line(loc[1] + 1)
        trim_content <- trimws(substr(content, 1, loc[2] + 1))

        closure <- stringr::str_match(
            trim_content,
            "(?:([a-zA-Z][a-zA-Z0-9.]+):::?)?([a-zA-Z.][a-zA-Z0-9_.]*)\\($"
        )

        if (is.na(closure[2])) {
            list(funct = closure[3])
        } else {
            list(package = closure[2], funct = closure[3])
        }
    } else {
        list()
    }
}

#' detect if position contains a valid token
#'
#' @template document
#' @template position
detect_token <- function(document, position) {
    line <- position$line
    character <- position$character

    content <- document$line(line + 1)
    token <- stringr::str_match(substr(content, 1, character), "\\b[a-zA-Z0-9_.:]+$")[1]
    if (is.na(token)) {
        ""
    } else {
        token
    }
}

#' detect the token at the current position
#'
#' @template document
#' @template position
detect_hover <- function(document, position) {
    line <- position$line
    character <- position$character

    content <- document$line(line + 1)
    first <- stringr::str_match(
        substr(content, 1, character),
        "(?:([a-zA-Z][a-zA-Z0-9.]+):::?)?([a-zA-Z.][a-zA-Z0-9_.]*)$"
    )[1]
    second <- stringr::str_match(
        substr(content, character + 1, nchar(content)),
        "^[a-zA-Z0-9_.]+\\b"
    )[1]

    if (is.na(first)) first <- ""
    if (is.na(second)) second <- ""
    list(
        begin = character - nchar(first),
        end = character + nchar(second),
        text = paste0(first, second)
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

resolve_package_dependencies <- function(pkgs) {
    if (length(pkgs)) {
        deps <- tryCatch(
            callr::r(
                function(pkgs) {
                    for (pkg in pkgs) {
                        if (paste0("package:", pkg) %in% search()) {
                            next
                        }
                        tryCatch(library(pkg, character.only = TRUE),
                            error = function(e) NULL
                        )
                    }
                    search()
                },
                list(pkgs = pkgs)
            ),
            error = function(e) NULL
        )
        if (!is.null(deps)) {
            deps <- deps[startsWith(deps, "package:")]
            deps <- gsub("package:", "", deps)
            deps <- deps[!deps %in% pkgs]
            pkgs <- c(pkgs, deps)
        }
    }
    pkgs
}
