#' `textDocument/completion` request handler
#'
#' Handler to the `textDocument/completion` [Request].
#' @keywords internal
text_document_completion  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(completion_reply(
        id, uri, self$workspace, self$documents[[uri]], params$position))
}

#' `completionItem/resolve` request handler
#'
#' Handler to the `completionItem/resolve` [Request].
#' @keywords internal
completion_item_resolve  <- function(self, id, params) {
    logger$info("completion_item_resolve:", params)
    respond <- FALSE
    if (is.null(params$data) || is.null(params$data$package) || params$data$package == WORKSPACE) {
        
    } else {
        if (params$data$type == "parameter") {
            doc <- self$workspace$get_documentation(params$data$funct, params$data$package)
            doc_string <- doc$arguments[[params$label]]
            if (!is.null(doc_string)) {
                params$documentation <- list(kind = "markdown", value = doc_string)
                respond <- TRUE
            }
        } else if (params$data$type %in% c("function", "nonfunction", "lazydata")) {
            doc <- self$workspace$get_documentation(params$label, params$data$package)
            doc_string <- doc$description
            if (!is.null(doc_string)) {
                params$documentation <- list(kind = "markdown", value = doc_string)
                respond <- TRUE
            }
        }
    }
    if (respond) {
        params$data <- NULL
        logger$info(params)
        self$deliver(Response$new(
            id,
            result = params
        ))
    } else {
        self$deliver(Response$new(id))
    }
}

#' `textDocument/hover` request handler
#'
#' Handler to the `textDocument/hover` [Request].
#'
#' @keywords internal
text_document_hover  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(hover_reply(
        id, uri, self$workspace, self$documents[[uri]], params$position))
}

#' `textDocument/signatureHelp` request handler
#'
#' Handler to the `textDocument/signatureHelp` [Request].
#'
#' @keywords internal
text_document_signature_help  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(signature_reply(
        id, uri, self$workspace, self$documents[[uri]], params$position))
}

#' `textDocument/definition` request handler
#'
#' Handler to the `textDocument/definition` [Request].
#' @keywords internal
text_document_definition  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(definition_reply(
            id, uri, self$workspace, self$documents[[uri]], params$position))
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

}

#' `textDocument/documentHighlight` request handler
#'
#' Handler to the `textDocument/documentHighlight` [Request].
#' @keywords internal
text_document_document_highlight  <- function(self, id, params) {

}

#' `textDocument/documentSymbol` request handler
#'
#' Handler to the `textDocument/documentSymbol` [Request].
#' @keywords internal
text_document_document_symbol  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$deliver(document_symbol_reply(
            id, uri, self$workspace))
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

}

#' `textDocument/colorPresentation` request handler
#'
#' Handler to the `textDocument/colorPresentation` [Request].
#' @keywords internal
text_document_color_presentation  <- function(self, id, params) {

}

#' `textDocument/formatting` request handler
#'
#' Handler to the `textDocument/formatting` [Request].
#' @keywords internal
text_document_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    options <- params$options
    self$deliver(formatting_reply(id, uri, self$documents[[uri]], options))
}

#' `textDocument/rangeFormatting` request handler
#'
#' Handler to the `textDocument/rangeFormatting` [Request].
#' @keywords internal
text_document_range_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    range <- params$range
    options <- params$options
    self$deliver(range_formatting_reply(id, uri, self$documents[[uri]], range, options))
}


#' `textDocument/onTypeFormatting` request handler
#'
#' Handler to the `textDocument/onTypeFormatting` [Request].
#' @keywords internal
text_document_on_type_formatting  <- function(self, id, params) {

}


#' `textDocument/rename` request handler
#'
#' Handler to the `textDocument/rename` [Request].
#' @keywords internal
text_document_rename  <- function(self, id, params) {

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

}
