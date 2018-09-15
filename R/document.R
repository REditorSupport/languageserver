if (.Platform$OS.type == "windows") {
    path_from_uri <- function(uri) {
        if (is.null(uri)) {
            NULL
        } else {
            utils::URLdecode(substr(uri, 9, nchar(uri)))
        }
    }
    path_to_uri <- function(path) {
        if (is.null(path)) {
            NULL
        } else {
            paste0("file:///", utils::URLencode(path))
        }
    }
} else {
    path_from_uri <- function(uri) {
        if (is.null(uri)) {
            NULL
        } else {
            utils::URLdecode(substr(uri, 8, nchar(uri)))
        }
    }
    path_to_uri <- function(path) {
        if (is.null(path)) {
            NULL
        } else {
            paste0("file://", utils::URLencode(path))
        }
    }
}

is_rmarkdown <- function(path) {
    endsWith(tolower(path), "rmd") || endsWith(tolower(path), "rmarkdown")
}

check_scope <- function(uri, document, line) {
    filename <- path_from_uri(uri)
    if (is_rmarkdown(filename)) {
        !identical(sum(sapply(document[1:(line + 1)], function(x) startsWith(x, "```"))) %% 2, 0)
    } else {
        TRUE
    }
}

document_backward_search <- function(document, line, character, char, skip_empty_line = TRUE) {
    .Call("document_backward_search", PACKAGE = "languageserver",
        document, line, character - 1, char, skip_empty_line)
}

document_line <- function(document, lineno) {
    if (lineno <= length(document)) {
        line <- document[lineno]
    } else {
        line <- ""
    }
    line
}

detect_closure <- function(document, line, character) {
    if (character > 0 && !is.null(document)) {
        loc <- document_backward_search(document, line, character, "(")
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

detect_token <- function(document, line, character) {
    content <- document_line(document, line + 1)
    token <- stringr::str_match(substr(content, 1, character), "\\b[a-zA-Z0-9_.:]+$")[1]
    if (is.na(token)) {
        ""
    } else {
        token
    }
}

detect_hover <- function(document, line, character) {
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

parse_document <- function(path) {
    temp_file <- NULL
    if (is_rmarkdown(path)) {
        temp_file <- tempfile(fileext = ".R")
        path <- tryCatch({
            knitr::purl(path, output = temp_file, quiet = TRUE)
        }, error = function(e) path)
    }
    expr <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
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
    if (length(expr)) {
        for (e in expr) {
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
                    dpes <- deps[! deps %in% packages]
                    packages <- c(packages, deps)
                }
            }
        }
    }
    list(packages = packages, nonfuncts = nonfuncts, functs = functs,
         signatures = signatures, formals = formals)
}
