#' Return parse data only when it describes the current document version
#' @noRd
current_parse_data <- function(uri, workspace, document) {
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
        (!is.null(parse_data$version) &&
            !isTRUE(parse_data$version == document$version))) {
        return(NULL)
    }
    parse_data
}

#' Find calls to a workspace function without resolving every symbol
#' @noRd
function_call_locations <- function(workspace, symbol, context_uri = NULL) {
    token_quote <- xml_single_quote(symbol)
    locations <- list()

    doc_uris <- workspace_reference_document_uris(
        workspace, context_uri, context_uri)
    for (doc_uri in doc_uris) {
        document <- workspace$documents$get(doc_uri)
        parse_data <- workspace$get_parse_data(doc_uri)
        indexed <- parse_data$reference_index
        if (!is.null(indexed)) {
            selected <- which(
                indexed$token == "SYMBOL_FUNCTION_CALL" &
                    indexed$name == symbol &
                    !indexed$qualified_call
            )
            if (length(selected)) {
                locations <- c(
                    locations,
                    indexed_reference_locations(indexed, doc_uri, selected)
                )
            }
            next
        }
        xdoc <- parse_data$xml_doc
        if (is.null(xdoc)) next

        nodes <- xml_find_all(
            xdoc,
            glue(paste0(
                "//SYMBOL_FUNCTION_CALL[text() = '{token_quote}' and ",
                "not(preceding-sibling::NS_GET or ",
                "preceding-sibling::NS_GET_INT)]"
            ),
                token_quote = token_quote)
        )
        if (!length(nodes)) next

        line1 <- as.integer(xml_attr(nodes, "line1"))
        col1 <- as.integer(xml_attr(nodes, "col1"))
        line2 <- as.integer(xml_attr(nodes, "line2"))
        col2 <- as.integer(xml_attr(nodes, "col2"))

        doc_locations <- lapply(seq_along(nodes), function(i) {
            location(
                doc_uri,
                range(
                    start = document$to_lsp_position(line1[[i]] - 1L, col1[[i]] - 1L),
                    end = document$to_lsp_position(line2[[i]] - 1L, col2[[i]])
                )
            )
        })
        locations <- c(locations, doc_locations)
    }

    locations
}

#' Resolve a function-reference code lens
#' @noRd
resolve_function_code_lens <- function(workspace, lens) {
    symbol <- lens$data$symbol
    uri <- lens$data$uri
    if (is.null(symbol) || is.null(uri)) return(lens)

    locations <- function_call_locations(workspace, symbol, context_uri = uri)
    count <- length(locations)
    title <- sprintf("%d call%s", count, if (count == 1L) "" else "s")
    lens$command <- list(
        title = title,
        tooltip = sprintf("Show the semantic call hierarchy for %s()", symbol),
        command = "editor.showCallHierarchy"
    )
    lens
}

#' The response to a textDocument/codeLens request
#' @noRd
code_lens_reply <- function(id, uri, workspace, document, client_capabilities = NULL) {
    parse_data <- current_parse_data(uri, workspace, document)
    if (is.null(parse_data)) return(NULL)

    definitions <- parse_data$definitions
    if (!length(definitions)) return(Response$new(id, result = list()))

    resolve_options <- client_capabilities$textDocument$codeLens$resolveSupport
    # Clients predating 3.18 use the server's resolveProvider flag. A 3.18
    # client can explicitly omit "command" from the properties it resolves.
    resolve_command <- is.null(resolve_options) ||
        "command" %in% resolve_options$properties

    lenses <- list()
    for (symbol in names(definitions)) {
        definition <- definitions[[symbol]]
        if (!identical(definition$type, "function")) next

        start <- definition$range$start
        lens <- list(
            range = range(
                position(start$line, start$character),
                position(start$line, start$character)
            ),
            data = list(
                uri = uri,
                symbol = symbol,
                version = document$version
            )
        )
        if (!resolve_command) {
            lens <- resolve_function_code_lens(workspace, lens)
        }
        lenses[[length(lenses) + 1L]] <- lens
    }

    Response$new(id, result = lenses)
}

#' The response to a codeLens/resolve request
#' @noRd
code_lens_resolve_reply <- function(id, workspace, lens) {
    Response$new(id, result = resolve_function_code_lens(workspace, lens))
}
