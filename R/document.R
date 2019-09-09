#' paths and uris
#'
#' @param uri a character, the path to a file in URI format
path_from_uri <- function(uri) {
    if (is.null(uri)) {
        return(NULL)
    }
    start_char <- ifelse(.Platform$OS.type == "windows", 9, 8)
    utils::URLdecode(substr(uri, start_char, nchar(uri)))
}

#' @param path a character, the path to a file
#' @rdname path_from_uri
path_to_uri <- function(path) {
    if (is.null(path)) {
        return(NULL)
    }
    prefix <- ifelse(.Platform$OS.type == "windows", "file:///", "file://")
    paste0(prefix, utils::URLencode(path))
}

#' check if a file is an RMarkdown file
#'
#' @template uri
#'
#' @return a logical
is_rmarkdown <- function(uri) {
    filename <- path_from_uri(uri)
    endsWith(tolower(filename), "rmd") || endsWith(tolower(filename), "rmarkdown")
}

#' check if a token is in a R code block in an Rmarkdown file
#'
#' In an RMarkdown document, tokens can be either inside an R code block or
#' in the text. This function will return `FALSE` if the token is in the text
#' and `TRUE` if it is in a code block. For R scripts, it always returns `TRUE`.
#'
#' @template uri
#' @template document
#' @template position
#'
#' @return a logical
check_scope <- function(uri, document, position) {
    if (is_rmarkdown(uri)) {
        line <- position$line
        !identical(sum(sapply(document[1:(line + 1)], function(x) startsWith(x, "```"))) %% 2, 0)
    } else {
        TRUE
    }
}

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
    line      <- position$line
    character <- position$character
    .Call("document_backward_search", PACKAGE = "languageserver",
          document, line, character - 1, char, skip_empty_line)
}

#' get the contents of a line
#'
#' If the line number is higher than the number of lines in the document,
#' an empty character is returned.
#'
#' @template document
#' @param lineno a numeric, the line number
#'
#' @return a character
document_line <- function(document, lineno) {
    if (lineno <= length(document)) {
        line <- document[lineno]
    } else {
        line <- ""
    }
    line
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
        content <- document_line(document, loc[1] + 1)
        trim_content <- trimws(substr(content, 1, loc[2] + 1))

        closure <- stringr::str_match(
            trim_content,
            "(?:([a-zA-Z][a-zA-Z0-9.]+):::?)?([a-zA-Z.][a-zA-Z0-9_.]*)\\($")

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
    line      <- position$line
    character <- position$character

    content <- document_line(document, line + 1)
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
    line      <- position$line
    character <- position$character

    content <- document_line(document, line + 1)
    first <- stringr::str_match(
        substr(content, 1, character),
        "(?:([a-zA-Z][a-zA-Z0-9.]+):::?)?([a-zA-Z.][a-zA-Z0-9_.]*)$")[1]
    second <- stringr::str_match(
        substr(content, character + 1, nchar(content)),
        "^[a-zA-Z0-9_.]+\\b")[1]

    if (is.na(first)) first <- ""
    if (is.na(second)) second <- ""
    list(begin = character - nchar(first),
         end = character + nchar(second),
         text = paste0(first, second))
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
    parse_expr(expr)
}


parse_expr <- function(expr) {
    packages <- character()
    nonfuncts <- character()
    functs <- character()
    formals <- list()
    signatures <- list()
    definition_ranges <- list()
    if (length(expr)) {
        for (i in seq_along(expr)) {
            e <- expr[[i]]
            if (length(e) == 3L &&
                is.symbol(e[[1L]]) &&
                (e[[1L]] == "<-" || e[[1L]] == "=") &&
                is.symbol(e[[2L]])) {
                funct <- as.character(e[[2L]])
                objects <- c(objects, funct)
                if (is.call(e[[3L]]) && e[[3L]][[1L]] == "function") {
                    functs <- c(functs, funct)
                    func <- e[[3L]]
                    formals[[funct]] <- func[[2L]]
                    signature <- func
                    signature <- utils::capture.output(print(signature[1:2]))
                    signature <- paste0(trimws(signature, which = "left"), collapse = "\n")
                    signature <- trimws(gsub("NULL\\s*$", "", signature))
                    signatures[[funct]] <- signature
                    # R is 1-indexed, language server is 0-indexed
                    first_line <- attr(expr, "srcref")[[i]][1] - 1
                    first_char <- attr(expr, "srcref")[[i]][5] - 1
                    last_line <- attr(expr, "srcref")[[i]][3] - 1
                    last_char <- attr(expr, "srcref")[[i]][6] - 1
                    definition_range <- range(position(first_line, first_char),
                                        position(last_line, last_char))
                    definition_ranges[[funct]] <- definition_range
                } else {
                    nonfuncts <- c(nonfuncts, funct)
                }
            } else if (length(e) == 2L &&
                       is.symbol(e[[1L]]) &&
                       (e[[1L]] == "library" || e[[1L]] == "require")) {
                pkg <- as.character(e[[2L]])
                packages <- c(packages, pkg)
                deps <- tryCatch(
                    callr::r(
                        function(pkg) {
                            library(pkg, character.only = TRUE); search() },
                        list(pkg = pkg)),
                    error = function(e) NULL)
                if (!is.null(deps)) {
                    deps <- deps[startsWith(deps, "package:")]
                    deps <- gsub("package:", "", deps)
                    deps <- deps[! deps %in% packages]
                    packages <- c(packages, deps)
                }
            }
        }
    }
    list(packages = packages, nonfuncts = nonfuncts, functs = functs,
         signatures = signatures, formals = formals, definition_ranges = definition_ranges)
}
