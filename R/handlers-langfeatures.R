#' `textDocument/completion` request handler
#'
#' Handler to the `textDocument/completion` [Request].
#' @keywords internal
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
#' @keywords internal
completion_item_resolve  <- function(self, id, params) {
    self$deliver(completion_item_resolve_reply(
        id, self$workspace, params))
}

#' `textDocument/hover` request handler
#'
#' Handler to the `textDocument/hover` [Request].
#'
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
text_document_definition  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    point <- document$from_lsp_position(params$position)
    self$deliver(definition_reply(id, uri, self$workspace, document, point))
}

#' `textDocument/typeDefinition` request handler
#'
#' Handler to the `textDocument/typeDefinition` [Request].
#'
#' @keywords internal
text_document_type_definition  <- function(self, id, params) {

}

#' `textDocument/implementation` request handler
#'
#' Handler to the `textDocument/implementation` [Request].
#' @keywords internal
text_document_implementation  <- function(self, id, params) {

}

#' `textDocument/references` request handler
#'
#' Handler to the `textDocument/references` [Request].
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
text_document_code_action  <- function(self, id, params) {

}

#' `textDocument/codeLens` request handler
#'
#' Handler to the `textDocument/codeLens` [Request].
#' @keywords internal
text_document_code_lens  <- function(self, id, params) {

}

#' `codeLens/resolve` request handler
#'
#' Handler to the `codeLens/resolve` [Request].
#' @keywords internal
code_lens_resolve  <- function(self, id, params) {

}


#' `textDocument/documentLink` request handler
#'
#' Handler to the `textDocument/documentLink` [Request].
#' @keywords internal
text_document_document_link  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    document <- self$workspace$documents$get(uri)
    rootPath <- if (length(self$rootPath)) self$rootPath else dirname(path_from_uri(uri))
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
#' @keywords internal
document_link_resolve  <- function(self, id, params) {

}

#' `textDocument/documentColor` request handler
#'
#' Handler to the `textDocument/documentColor` [Request].
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
text_document_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    options <- params$options
    self$deliver(formatting_reply(id, uri, self$workspace$documents$get(uri), options))
}

#' `textDocument/rangeFormatting` request handler
#'
#' Handler to the `textDocument/rangeFormatting` [Request].
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
text_document_rename  <- function(self, id, params) {
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
#' @keywords internal
text_document_prepare_rename  <- function(self, id, params) {

}

#' `textDocument/foldingRange` request handler
#'
#' Handler to the `textDocument/foldingRange` [Request].
#' @keywords internal
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
#' @keywords internal
text_document_selection_range <- function(self, id, params) {

}
