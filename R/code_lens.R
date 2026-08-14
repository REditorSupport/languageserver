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

#' Convert an LSP URI to the marshalled URI shape used by VS Code commands
#' @noRd
vscode_command_uri <- function(uri) {
    pattern <- paste0(
        "^([A-Za-z][A-Za-z0-9+.-]*):",
        "(?://([^/?#]*))?([^?#]*)(?:\\?([^#]*))?(?:#(.*))?$"
    )
    parts <- regmatches(uri, regexec(pattern, uri, perl = TRUE))[[1L]]
    if (length(parts) != 6L) return(NULL)

    decode <- function(value) {
        value <- utils::URLdecode(value)
        Encoding(value) <- "UTF-8"
        value
    }
    result <- list(`$mid` = 1L, scheme = tolower(parts[[2L]]))
    if (nzchar(parts[[3L]])) result$authority <- decode(parts[[3L]])
    if (nzchar(parts[[4L]])) result$path <- decode(parts[[4L]])
    if (nzchar(parts[[5L]])) result$query <- decode(parts[[5L]])
    if (nzchar(parts[[6L]])) result$fragment <- decode(parts[[6L]])
    result
}

#' Convert LSP locations to the internal shapes accepted by VS Code commands
#' @noRd
vscode_command_position <- function(value) {
    list(
        lineNumber = value$line + 1L,
        column = value$character + 1L
    )
}

#' @noRd
vscode_command_range <- function(value) {
    list(
        startLineNumber = value$start$line + 1L,
        startColumn = value$start$character + 1L,
        endLineNumber = value$end$line + 1L,
        endColumn = value$end$character + 1L
    )
}

#' @noRd
vscode_command_location <- function(value) {
    list(
        uri = vscode_command_uri(value$uri),
        range = vscode_command_range(value$range)
    )
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
    command <- list(
        title = title,
        tooltip = sprintf("Show calls to %s()", symbol),
        command = ""
    )
    anchor <- vscode_command_uri(uri)
    if (count && !is.null(anchor)) {
        command$command <- "editor.action.peekLocations"
        command$arguments <- list(
            anchor,
            vscode_command_position(lens$range$start),
            lapply(locations, vscode_command_location)
        )
    }
    lens$command <- command
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
