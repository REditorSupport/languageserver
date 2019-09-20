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

fuzzy_find <- function(pattern, x) {
    subsequence_regex <- paste0(strsplit(pattern, "")[[1]], collapse = ".*")
    grepl(subsequence_regex, x, ignore.case = TRUE)
}

#' A data structure to hold function definition locations
#'
#' The key reason for using this rather than a `list` is that this also cleans up
#' when functions are removed from files.
#' @section Methods:
#' + `get(funct)`: get the `location` of a definition, or `NULL` if it is not found
#' + `get_functs_for_uri(uri)`: get the `location`s of all the definitions in `uri`
#' + `filter(pattern)`: get the `location`s of all the definitions matching `pattern`
#' + `update(uri, ranges)`: create `locations` of functions in `uri` from `ranges`, and
#' remove functions no longer in `uri`
DefinitionCache <- R6::R6Class("DefinitionCache",
    public = list(
        get = function(funct) {
            private$definitions[[funct]]
        },
        get_functs_for_uri = function(uri) {
            private$definitions[private$uris[[uri]]]
        },
        filter = function(pattern) {
            private$definitions[fuzzy_find(pattern, names(private$definitions))]
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

    if (is.na(pkg)) {
        # look for in file first
        definition <- workspace$get_definition(funct)
        result <- if (is.null(definition)) {
            find_definition_in_package(workspace, funct, pkg)
        } else {
            definition
        }
    } else {
        # look for in package first
        definition <- find_definition_in_package(workspace, funct, pkg)
        result <- if (is.null(definition)) {
            workspace$get_definition(funct)
        } else {
            definition
        }
    }
    if (is.null(result)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}

#' Get all the symbols in the document
#'
#' @template id
#' @template uri
#' @template workspace
#'
#' @return  A `Response` with a list of all the symbols
#' in the document, or just the id if there weren't any.
#'
#' @keywords internal
document_symbol_reply <- function(id, uri, workspace) {
    defns <- workspace$get_definitions_for_uri(uri)
    logger$info("document symbols found: ", length(defns))
    result <- lapply(names(defns),
        function(funct) {
            symbol_information(
                name = funct,
                kind = SymbolKind[["Function"]],
                location = defns[[funct]]
            )
    })
    if (is.null(result)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}

#' Get all the symbols in the workspace matching a query
#'
#' @template id
#' @template workspace
#' @param query a character
#'
#' @return  A `Response` with a list of all the symbols
#' in the workspace that match the query, or just the id
#' if there weren't any.
#'
#' @keywords internal
workspace_symbol_reply <- function(id, workspace, query) {
    defns <- workspace$get_definitions_for_query(query)
    logger$info("workspace symbols found: ", length(defns))
    result <- lapply(names(defns),
        function(funct) {
            symbol_information(
                name = funct,
                kind = SymbolKind[["Function"]],
                location = defns[[funct]]
            )
    })
    if (is.null(result)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
