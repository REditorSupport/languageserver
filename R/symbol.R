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

get_document_symbol_kind <- function(type) {
    if (is.character(type) && length(type) == 1) {
        switch(type,
            logical = SymbolKind$Boolean,
            integer = SymbolKind$Number,
            double = SymbolKind$Number,
            complex = SymbolKind$Number,
            character = SymbolKind$String,
            array = SymbolKind$Array,
            list = SymbolKind$Struct,
            `function` = SymbolKind$Function,
            `NULL` = SymbolKind$Null,
            `class` = SymbolKind$Class,
            SymbolKind$Variable
        )
    } else {
        SymbolKind$Variable
    }
}

#' Get all the symbols in the document
#' @keywords internal
document_symbol_reply <- function(id, uri, workspace, document, capabilities) {
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    defns <- workspace$get_definitions_for_uri(uri)
    logger$info("document definitions found: ", length(defns))
    definition_symbols <- lapply(names(defns), function(name) {
        def <- defns[[name]]
        symbol_information(
            name = name,
            kind = get_document_symbol_kind(def$type),
            location = location(
                uri = uri,
                range = def$range
            )
        )
    })
    result <- definition_symbols

    if (isTRUE(capabilities$hierarchicalDocumentSymbolSupport)) {
        sections <- get_document_sections(uri, document)
        section_symbols <- lapply(sections, function(section) {
            symbol_information(
                name = section$name,
                kind = switch(section$type,
                    section = SymbolKind$String,
                    subsection = SymbolKind$String,
                    chunk = SymbolKind$Key,
                    SymbolKind$String
                ),
                location = list(
                    uri = uri,
                    range = range(
                        start = document$to_lsp_position(
                            row = section$start_line - 1,
                            col = 0
                        ),
                        end = document$to_lsp_position(
                            row = section$end_line - 1,
                            col = nchar(document$line(section$end_line))
                        )
                    )
                )
            )
        })

        result <- c(result, section_symbols)
    }

    Response$new(id, result = result)
}

#' Get all the symbols in the workspace matching a query
#' @keywords internal
workspace_symbol_reply <- function(id, workspace, query) {
    defns <- workspace$get_definitions_for_query(query)
    logger$info("workspace symbols found: ", length(defns))
    result <- lapply(defns, function(def) {
        symbol_information(
            name = def$name,
            kind = get_document_symbol_kind(def$type),
            location = location(
                uri = def$uri,
                range = def$range
            )
        )
    })

    Response$new(id, result = result)
}
