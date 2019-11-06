# as defined by the language server protocol
SymbolKind <- list(
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    Object = 19,
    Key = 20,
    Null = 21,
    EnumMember = 22,
    Struct = 23,
    Event = 24,
    Operator = 25,
    TypeParameter = 26
)



#' A data structure to hold function definition locations
#'
#' The key reason for using this rather than a `list` is that this also cleans up
#' when functions are removed from files.
#' @keywords internal
DefinitionCache <- R6::R6Class("DefinitionCache",
    public = list(
        get = function(funct) {
            private$definitions[[funct]]
        },
        get_functs_for_uri = function(uri) {
            private$definitions[private$uris[[uri]]]
        },
        filter = function(pattern) {
            private$definitions[fuzzy_find(names(private$definitions), pattern)]
        },
        update = function(uri, ranges) {
            functs <- names(ranges)
            removed_functs <- setdiff(private$uris[[uri]], functs)
            if (!is.null(removed_functs) && length(removed_functs) > 0) {
                private$definitions[removed_functs] <- NULL
            }
            for (funct in functs) {
                private$definitions[[funct]] <- location(uri, ranges[[funct]])
            }
            private$uris[[uri]] <- functs
        }
    ),
    private = list(
        definitions = list(),
        uris = list()
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
