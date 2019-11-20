#' The response to a textDocument/hover Request
#'
#' When hovering on a symbol, if it is a function, return its help text
#' if it exists in the current [Workspace].
#' @keywords internal
hover_reply <- function(id, uri, workspace, document, position) {
    if (!check_scope(uri, document, position)) {
        return(Response$new(id))
    }

    token_result <- document$detect_token(position)

    contents <- workspace$get_help(token_result$token, token_result$package)

    if (is.null(contents)) {
        # try function signature
        sig <- workspace$get_signature(token_result$token, token_result$package)
        logger$info("sig: ", sig)
        if (!is.null(sig)) {
            sig <- trimws(gsub("function\\s*", token_result$token, sig))
            contents <- sprintf("```r\n%s\n```", sig)
        }
    }

    if (is.null(contents)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = list(
                contents = contents,
                range = token_result$range
            )
        )
    }
}
