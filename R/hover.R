hover_reply <- function(id, workspace, document, position) {
    contents <- NULL
    line <- position$line
    character <- position$character

    logger$info("line: ", line)
    logger$info("character: ", character)

    if (is.null(contents)) {
        Response$new(id, result = NULL)
    } else {
        Response$new(
            id,
            result = list(
                contents = contents
            )
        )
    }
}
