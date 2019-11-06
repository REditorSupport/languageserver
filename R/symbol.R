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


#' Get all the symbols in the document
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
