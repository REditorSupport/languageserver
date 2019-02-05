definition_reply <- function(id, uri, workspace, document, position) {

    token <- detect_hover(document, position)

    logger$info("definition: ", token)

    matches <- stringr::str_match(
        token, "(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?([a-zA-Z0-9_.]*)$")

    # check if the function can be found in the workspace
    code <- tryCatch(
        workspace$get_code(matches[4], matches[2]),
        error = function(e) NULL
    )

    if (is.null(code)) {
        Response$new(id)
    } else {
        # if the function exists in the workspace, write the code to a file
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
                range = range(
                    start = list(line = 0, character = 0),
                    end = list(line = nlines, character = 0)
                )
            )
        )
    }
}
