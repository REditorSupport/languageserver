#' A data structure to hold function definition locations
#'
#' The key reason for using this rather than a `list` is that this also cleans up
#' when functions are removed from files.
#' @section Methods:
#' + `get(funct)`: get the `location` of a definition, or `NULL` if it is not found
#' + `update(uri, ranges)`: create `locations` of functions in `uri` from `ranges`, and
#' remove functions no longer in `uri`
DefinitionCache <- R6::R6Class("DefinitionCache",
    public = list(
        get = function(funct) {
            private$definitions[[funct]]
        },
        update = function(uri, ranges) {
            functs <- names(ranges)
            removed_functs <- setdiff(private$uris[[uri]], functs)
            if(!is.null(removed_functs) && length(removed_functs) > 0) {
                private$definitions[[removed_functs]] <- NULL
            }
            for(funct in functs) {
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

find_definition_in_package <- function(workspace, funct, pkg) {
    code <- workspace$get_code(funct, pkg)
    if (is.null(code)) {
        NULL
    } else {
        # if the function exists in the workspace, write the code to a file
        tmp <- file.path(tempdir(), paste0(funct, ".R"))
        logger$info("tmp: ", tmp)
        con <- file(tmp, "w+")
        writeLines(text = code, con = con)
        close(con)
        nlines <- length(readLines(tmp)) + 1
        list(
            uri = path_to_uri(tmp),
            range = range(
                start = position(line = 0, character = 0),
                end = position(line = nlines, character = 0)
            )
        )
    }
}

find_definition_in_file <- function(workspace, funct) {
    definition <- workspace$get_definition(funct)
    if(is.null(definition)) {
        NULL
    } else {
        list(
            uri = definition$uri,
            range = range(
                start = position(line = definition$range$start$line - 1,
                                 character = definition$range$start$character - 1),
                end = position(line = definition$range$end$line - 1,
                               character = definition$range$end$character - 1)))
    }
}

#' Get the location of a specified function definition
#'
#' If the function is not found in a file but is found in a loaded package,
#' writes the function definition to a temporary file and returns that
#' as the location.
#' @template id
#' @template uri
#' @template workspace
#' @template document
#' @template position
#'
#' @return A `Response` with the `uri` and [range] of the function definition,
#' or just the `id` if the function is not found.
#'
#' @keywords internal
definition_reply <- function(id, uri, workspace, document, position) {
    token <- detect_hover(document, position)

    logger$info("definition: ", token$text)

    matches <- stringr::str_match(
        token$text, "(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?([a-zA-Z0-9_.]*)$")
    pkg <- matches[2]
    funct <- matches[4]

    if(is.na(pkg)) {
        # look for in file first
        definition <- find_definition_in_file(workspace, funct)
        result <- if(is.null(definition)) {
            find_definition_in_package(workspace, funct, pkg)
        } else {
            definition
        }
    } else {
        # look for in package first
        definition <- find_definition_in_package(workspace, funct, pkg)
        result <- if(is.null(definition)) {
            find_definition_in_file(workspace, funct)
        } else {
            definition
        }
    }
    if(is.null(result)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
