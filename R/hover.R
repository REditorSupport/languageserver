#' the response to a textDocument/hover Request
#'
#' When hovering on a symbol, if it is a function, return its help text
#' if it exists in the current [Workspace].
#'
#' @template id
#' @template uri
#' @template workspace
#' @template document
#' @template position
#'
#' @return a [Response] object
hover_reply <- function(id, uri, workspace, document, position) {
    line <- position$line

    if (!check_scope(uri, document, position)) {
        return(Response$new(id))
    }

    hover_result <- detect_hover(document, position)
    hover <- hover_result$text

    logger$info("hover: ", hover)

    matches <- detect_function(hover)

    contents <- tryCatch(
        workspace$get_help(matches$funct, matches$package),
        error = function(e) list()
    )

    if (is.null(contents)) {
        Response$new(id)
    } else {
        range <- range(
            start = position(line = line, character = hover_result$begin),
            end   = position(line = line, character = hover_result$end)
        )
        Response$new(
            id,
            result = list(
                contents = contents,
                range = range
            )
        )
    }
}
