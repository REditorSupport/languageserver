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
            "(?:([a-zA-Z][a-zA-Z0-9]+):::?)?([a-zA-Z.][a-zA-Z0-9_.]*)\\($")

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
        "(?:([a-zA-Z][a-zA-Z0-9]+):::?)?([a-zA-Z.][a-zA-Z0-9_.]*)$")[1]
    second <- stringr::str_match(
        substr(content, character + 1, nchar(content)),
        "^[a-zA-Z0-9_.]+\\b")[1]

    if (is.na(first)) first <- ""
    if (is.na(second)) second <- ""
    paste0(first, second)
}
