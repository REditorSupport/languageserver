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

is_rmarkdown <- function(uri) {
    filename <- path_from_uri(uri)
    endsWith(tolower(filename), "rmd") || endsWith(tolower(filename), "rmarkdown")
}

check_scope <- function(uri, document, line) {
    if (is_rmarkdown(uri)) {
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
    paste0(first, second)
}
