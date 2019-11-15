#' A data structure to hold function definition locations
#'
#' The key reason for using this rather than a `list` is that this also cleans up
#' when functions are removed from files.
#' @keywords internal
DefinitionCache <- R6::R6Class("DefinitionCache",
    private = list(
        locations = list(),
        uris = list()
    ),
    public = list(
        get = function(funct) {
            private$locations[[funct]]
        },
        get_functs_for_uri = function(uri) {
            private$locations[private$uris[[uri]]]
        },
        filter = function(pattern) {
            private$locations[fuzzy_find(names(private$locations), pattern)]
        },
        update = function(uri, ranges) {
            functs <- names(ranges)
            removed_functs <- setdiff(private$uris[[uri]], functs)
            if (!is.null(removed_functs) && length(removed_functs) > 0) {
                private$locations[removed_functs] <- NULL
            }
            for (funct in functs) {
                private$locations[[funct]] <- location(
                    uri = uri,
                    range = ranges[[funct]]
                )
            }
            private$uris[[uri]] <- functs
        }
    )
)

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
