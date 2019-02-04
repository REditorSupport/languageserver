definition_reply <- function(id, uri, workspace, document, position) {

    line <- position$line
    character <- position$character
    token <- detect_hover(document, line, character)

    logger$info("definition: ", token)

    matches <- stringr::str_match(
        token, "(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?([a-zA-Z0-9_.]*)$")

    code <- tryCatch(
        workspace$get_code(matches[4], matches[2]),
        error = function(e) list())

    if (is.null(code)) {
        Response$new(id)
    } else {
        logger$info("code: ", code)
        tmp <- file.path(tempdir(), paste0(matches[4], ".R"))
        logger$info("tmp:", tmp)
        con <- file(tmp, "w+")
        writeLines(text = code, con = con)
        close(con)
        nlines <- length(readLines(tmp)) + 1
        Response$new(
            id,
            result = list(
                uri = paste0("file://", tmp),
                range = list(
                    start = list(line = 0, character = 0),
                    end = list(line = nlines, character = 0)
                )
            )
        )
    }
}