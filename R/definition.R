#' Get the location of a specified function definition
#'
#' If the function is not found in a file but is found in a loaded package,
#' writes the function definition to a temporary file and returns that
#' as the location.

#' @keywords internal
definition_reply <- function(id, uri, workspace, document, position) {

    token_result <- document$detect_token(position)

    pkg <- token_result$package
    token <- token_result$token

    result <- workspace$get_definition(token, pkg)

    logger$info("definition", result)

    if (is.null(result)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
