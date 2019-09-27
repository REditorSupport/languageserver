#' textDocument/completion request handler
#'
#' Handler to the [textDocument/completion](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @param params a [completion_params] object
#'
#' @keywords internal
text_document_completion  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(completion_reply(
        id, uri, self$workspace, self$documents[[uri]], params$position))
}

#' completionItem/resolve request handler
#'
#' Handler to the [completionItem/resolve](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @param params a [completion_params] object
#'
#' @keywords internal
completion_item_resolve  <- function(self, id, params) {

}

#' textDocument/hover request handler
#'
#' Handler to the [textDocument/hover](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @template tdpp
#'
#' @keywords internal
text_document_hover  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(hover_reply(
        id, uri, self$workspace, self$documents[[uri]], params$position))
}

#' textDocument/signatureHelp request handler
#'
#' Handler to the [textDocument/signatureHelp](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @template tdpp
#'
#' @keywords internal
text_document_signature_help  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(signature_reply(
        id, uri, self$workspace, self$documents[[uri]], params$position))
}

#' textDocument/definition request handler
#'
#' Handler to the [textDocument/definition](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @template tdpp
#'
#' @keywords internal
text_document_definition  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(definition_reply(
            id, uri, self$workspace, self$documents[[uri]], params$position))
}

#' textDocument/typeDefinition request handler
#'
#' Handler to the [textDocument/typeDefinition](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @template tdpp
#'
#' @keywords internal
text_document_type_definition  <- function(self, id, params) {

}

#' textDocument/implementation request handler
#'
#' Handler to the [textDocument/implementation](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @template tdpp
#'
#' @keywords internal
text_document_implementation  <- function(self, id, params) {

}

#' textDocument/references request handler
#'
#' Handler to the [textDocument/references](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a [reference_params] object
#'
#' @keywords internal
text_document_references  <- function(self, id, params) {

}

#' textDocument/documentHighlight request handler
#'
#' Handler to the [textDocument/documentHighlight](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @template tdpp
#'
#' @keywords internal
text_document_document_highlight  <- function(self, id, params) {

}

#' textDocument/documentSymbol request handler
#'
#' Handler to the [textDocument/documentSymbol](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @param params a [document_symbol_params] object
#'
#' @keywords internal
text_document_document_symbol  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(document_symbol_reply(
            id, uri, self$workspace))
}

#' textDocument/codeAction request handler
#'
#' Handler to the [textDocument/codeAction](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a [code_action_params] object
#'
#' @keywords internal
text_document_code_action  <- function(self, id, params) {

}

#' textDocument/codeLens request handler
#'
#' Handler to the [textDocument/codeLens](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a [code_lens_params] object
#'
#' @return a list of CodeLens objects
#'
#' @keywords internal
text_document_code_lens  <- function(self, id, params) {

}

#' codeLens/resolve request handler
#'
#' Handler to the [codeLens/resolve](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a CodeLens object
#'
#' @keywords internal
code_lens_resolve  <- function(self, id, params) {

}


#' textDocument/documentLink request handler
#'
#' Handler to the [textDocument/documentLink](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a [document_link_params] object
#'
#' @return a list of DocumentLink objects
#'
#' @keywords internal
text_document_document_link  <- function(self, id, params) {

}

#' documentLink/resolve request handler
#'
#' Handler to the [documentLink/resolve](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a DocumentLink object
#'
#' @keywords internal
document_link_resolve  <- function(self, id, params) {

}

#' textDocument/documentColor request handler
#'
#' Handler to the [textDocument/documentColor](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a document_color_params object
#'
#' @keywords internal
text_document_document_color  <- function(self, id, params) {

}

#' textDocument/colorPresentation request handler
#'
#' Handler to the [textDocument/colorPresentation](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a color_presentation_params object
#'
#' @keywords internal
text_document_color_presentation  <- function(self, id, params) {

}

#' textDocument/formatting request handler
#'
#' Handler to the [textDocument/formatting](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @param params a [document_formatting_params] object
#'
#' @keywords internal
text_document_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    options <- params$options
    self$deliver(formatting_reply(id, uri, self$documents[[uri]], options))
}

#' textDocument/rangeFormatting request handler
#'
#' Handler to the [textDocument/rangeFormatting](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @param params a [document_range_formatting_params] object
#'
#' @keywords internal
text_document_range_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    range <- params$range
    options <- params$options
    self$deliver(range_formatting_reply(id, uri, self$documents[[uri]], range, options))
}


#' textDocument/onTypeFormatting request handler
#'
#' Handler to the [textDocument/onTypeFormatting](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a [document_on_type_formatting_params] object
#'
#' @keywords internal
text_document_on_type_formatting  <- function(self, id, params) {

}


#' textDocument/rename request handler
#'
#' Handler to the [textDocument/rename](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a [rename_params] object
#'
#' @keywords internal
text_document_rename  <- function(self, id, params) {

}

#' textDocument/prepareRename request handler
#'
#' Handler to the [textDocument/prepareRename](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @template tdpp
#'
#' @keywords internal
text_document_prepare_rename  <- function(self, id, params) {

}

#' textDocument/foldingRange request handler
#'
#' Handler to the [textDocument/foldingRange](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' Not implemented yet.
#'
#' @template self
#' @template id
#' @param params a folding_range_params object
#'
#' @keywords internal
text_document_folding_range  <- function(self, id, params) {

}
