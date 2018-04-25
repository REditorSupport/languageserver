hover_reply <- function(id, uri, workspace, document, position) {
    line <- position$line
    character <- position$character

    if (!check_scope(uri, document, line)) {
        Response$new(
            id,
            result = list(
                contents = NULL
            )
        )
        return(invisible(NULL))
    }

    hover <- detect_hover(document, line, character)

    logger$info("hover: ", hover)

    matches <- stringr::str_match(
        hover, "(?:([a-zA-Z][a-zA-Z0-9]+)(:::?))?([a-zA-Z0-9_.]*)$")

    contents <- tryCatch(
        workspace$get_help(matches[4], matches[2]),
        error = function(e) NULL)

    Response$new(
        id,
        result = list(
            contents = contents
        )
    )
}
