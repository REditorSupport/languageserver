#' `textDocument/completion` request handler
#'
#' Handler to the `textDocument/completion` [Request].
#' @noRd
text_document_completion  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    self$deliver(completion_reply(id, uri, self$workspace, document, point,
        self$ClientCapabilities$textDocument$completion))
}

#' `completionItem/resolve` request handler
#'
#' Handler to the `completionItem/resolve` [Request].
#' @noRd
completion_item_resolve  <- function(self, id, params) {
    self$deliver(completion_item_resolve_reply(
        id, self$workspace, params))
}

#' `textDocument/hover` request handler
#'
#' Handler to the `textDocument/hover` [Request].
#'
#' @noRd
text_document_hover  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    self$deliver(hover_reply(id, uri, self$workspace, document, point))
}

#' `textDocument/signatureHelp` request handler
#'
#' Handler to the `textDocument/signatureHelp` [Request].
#'
#' @noRd
text_document_signature_help  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    self$deliver(signature_reply(id, uri, self$workspace, document, point))
}

#' `textDocument/definition` request handler
#'
#' Handler to the `textDocument/definition` [Request].
#' @noRd
text_document_definition  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    rootPath <- get_root_path_for_uri(uri, self$rootPath)
    self$deliver(definition_reply(id, uri, self$workspace, document, point, rootPath))
}

#' `textDocument/typeDefinition` request handler
#'
#' Handler to the `textDocument/typeDefinition` [Request].
#'
#' @noRd
text_document_type_definition  <- function(self, id, params) {

}

#' `textDocument/implementation` request handler
#'
#' Handler to the `textDocument/implementation` [Request].
#' @noRd
text_document_implementation  <- function(self, id, params) {

}

#' `textDocument/references` request handler
#'
#' Handler to the `textDocument/references` [Request].
#' @noRd
text_document_references  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    self$deliver(references_reply(id, uri, self$workspace, document, point))
}

#' `textDocument/documentHighlight` request handler
#'
#' Handler to the `textDocument/documentHighlight` [Request].
#' @noRd
text_document_document_highlight  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    self$deliver(document_highlight_reply(id, uri, self$workspace, document, point))
}

#' `textDocument/documentSymbol` request handler
#'
#' Handler to the `textDocument/documentSymbol` [Request].
#' @noRd
text_document_document_symbol  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    reply <- document_symbol_reply(id, uri, self$workspace, document,
        self$ClientCapabilities$textDocument$documentSymbol)
    if (is.null(reply)) {
        queue <- self$pending_replies$get(uri)[["textDocument/documentSymbol"]]
        queue$push(list(
            id = id,
            version = document$version,
            params = params
        ))
    } else {
        self$deliver(reply)
    }
}

#' `textDocument/codeAction` request handler
#'
#' Handler to the `textDocument/codeAction` [Request].
#' @noRd
text_document_code_action  <- function(self, id, params) {
    logger$info("text_document_code_action: ", params)
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    range <- list(
        start = document$from_lsp_position(params$range$start),
        end = document$from_lsp_position(params$range$end)
    )
    context <- params$context
    self$deliver(document_code_action_reply(id, uri, self$workspace, document, range, context))
}

#' `textDocument/codeLens` request handler
#'
#' Handler to the `textDocument/codeLens` [Request].
#' @noRd
text_document_code_lens  <- function(self, id, params) {

}

#' `codeLens/resolve` request handler
#'
#' Handler to the `codeLens/resolve` [Request].
#' @noRd
code_lens_resolve  <- function(self, id, params) {

}


#' `textDocument/documentLink` request handler
#'
#' Handler to the `textDocument/documentLink` [Request].
#' @noRd
text_document_document_link  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    rootPath <- get_root_path_for_uri(uri, self$rootPath)
    reply <- document_link_reply(id, uri, self$workspace, document, rootPath)
    if (is.null(reply)) {
        queue <- self$pending_replies$get(uri)[["textDocument/documentLink"]]
        queue$push(list(
            id = id,
            version = document$version,
            params = params
        ))
    } else {
        self$deliver(reply)
    }
}

#' `documentLink/resolve` request handler
#'
#' Handler to the `documentLink/resolve` [Request].
#' @noRd
document_link_resolve  <- function(self, id, params) {
    reply <- document_link_resolve_reply(id, self$workspace, params)
    self$deliver(reply)

    if (!is.null(reply$error)) {
        self$deliver(
            Notification$new(
                method = "window/showMessage",
                params = list(
                    type = MessageType$Error,
                    message = reply$error$message
                )
            )
        )
    }
}

#' `textDocument/documentColor` request handler
#'
#' Handler to the `textDocument/documentColor` [Request].
#' @noRd
text_document_document_color  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    reply <- document_color_reply(id, uri, self$workspace, document)
    if (is.null(reply)) {
        queue <- self$pending_replies$get(uri)[["textDocument/documentColor"]]
        queue$push(list(
            id = id,
            version = document$version,
            params = params
        ))
    } else {
        self$deliver(reply)
    }
}

#' `textDocument/colorPresentation` request handler
#'
#' Handler to the `textDocument/colorPresentation` [Request].
#' @noRd
text_document_color_presentation  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    color <- params$color
    self$deliver(color_presentation_reply(id, uri, self$workspace, document, color))
}

#' `textDocument/formatting` request handler
#'
#' Handler to the `textDocument/formatting` [Request].
#' @noRd
text_document_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    options <- params$options
    self$deliver(formatting_reply(id, uri, self$workspace$documents$get(uri), options))
}

#' `textDocument/rangeFormatting` request handler
#'
#' Handler to the `textDocument/rangeFormatting` [Request].
#' @noRd
text_document_range_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    range <- list(
        start = document$from_lsp_position(params$range$start),
        end = document$from_lsp_position(params$range$end)
    )
    options <- params$options
    self$deliver(range_formatting_reply(id, uri, document, range, options))
}


#' `textDocument/onTypeFormatting` request handler
#'
#' Handler to the `textDocument/onTypeFormatting` [Request].
#' @noRd
text_document_on_type_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    ch <- params$ch
    options <- params$options
    self$deliver(on_type_formatting_reply(id, uri, document, point, ch, options))
}


#' `textDocument/rename` request handler
#'
#' Handler to the `textDocument/rename` [Request].
#' @noRd
text_document_rename <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    newName <- params$newName
    self$deliver(rename_reply(id, uri, self$workspace, document, point, newName))
}

#' `textDocument/prepareRename` request handler
#'
#' Handler to the `textDocument/prepareRename` [Request].
#' @noRd
text_document_prepare_rename  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    self$deliver(prepare_rename_reply(id, uri, self$workspace, document, point))
}

#' `textDocument/foldingRange` request handler
#'
#' Handler to the `textDocument/foldingRange` [Request].
#' @noRd
text_document_folding_range  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    reply <- document_folding_range_reply(id, uri, self$workspace, document)
    if (is.null(reply)) {
        queue <- self$pending_replies$get(uri)[["textDocument/foldingRange"]]
        queue$push(list(
            id = id,
            version = document$version,
            params = params
        ))
    } else {
        self$deliver(reply)
    }
}

#' `textDocument/selectionRange` request handler
#'
#' Handler to the `textDocument/selectionRange` [Request].
#' @noRd
text_document_selection_range <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    points <- lapply(params$positions, document$from_lsp_position)
    self$deliver(selection_range_reply(id, uri, self$workspace, document, points))
}

#' `textDocument/prepareCallHierarchy` request handler
#'
#' Handler to the `textDocument/prepareCallHierarchy` [Request].
#' @noRd
text_document_prepare_call_hierarchy <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    self$deliver(prepare_call_hierarchy_reply(id, uri, self$workspace, document, point))
}

#' `callHierarchy/incomingCalls` request handler
#'
#' Handler to the `callHierarchy/incomingCalls` [Request].
#' @noRd
call_hierarchy_incoming_calls <- function(self, id, params) {
    self$deliver(
        call_hierarchy_incoming_calls_reply(id, self$workspace, params$item)
    )
}

#' `callHierarchy/outgoingCalls` request handler
#'
#' Handler to the `callHierarchy/outgoingCalls` [Request].
#' @noRd
call_hierarchy_outgoing_calls <- function(self, id, params) {
    self$deliver(
        call_hierarchy_outgoing_calls_reply(id, self$workspace, params$item)
    )
}

#' `textDocument/linkedEditingRange` request handler
#'
#' Handler to the `textDocument/linkedEditingRange` [Request].
#' @noRd
text_document_linked_editing_range <- function(self, id, params) {

}
