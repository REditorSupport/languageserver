#' `textDocument/didOpen` notification handler
#'
#' Handler to the `textDocument/didOpen` [Notification].
#' @keywords internal
text_document_did_open <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    content <- readr::read_lines(path_from_uri(uri))
    if (is.null(self$documents[[uri]])) {
        self$documents[[uri]] <- Document$new(uri, content)
    } else {
        self$documents[[uri]]$set(content)
    }
    self$text_sync(uri, document = NULL, run_lintr = TRUE, parse = TRUE)
}

#' `textDocument/didChange` notification handler
#'
#' Handler to the `textDocument/didChange` [Notification].
#' @keywords internal
text_document_did_change <- function(self, params) {
    textDocument <- params$textDocument
    contentChanges <- params$contentChanges
    text <- contentChanges[[1]]$text
    uri <- textDocument$uri
    logger$info("did change: ", uri)
    content <- stringr::str_split(text, "\r\n|\n")[[1]]
    if (is.null(self$documents[[uri]])) {
        doc <- Document$new(uri, content)
        self$documents[[uri]] <- doc
    } else {
        self$documents[[uri]]$set(content)
        doc <- self$documents[[uri]]
    }
    self$text_sync(uri, document = doc, run_lintr = TRUE, parse = FALSE)
}

#' `textDocument/willSave` notification handler
#'
#' Handler to the `textDocument/willSave` [Notification].
#' @keywords internal
text_document_will_save <- function(self, params) {

}

#' `textDocument/didSave` notification handler
#'
#' Handler to the `textDocument/didSave` [Notification].
#' @keywords internal
text_document_did_save <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    logger$info("did save:", uri)
    content <- readr::read_lines(path_from_uri(uri))
    self$documents[[uri]] <- Document$new(uri, content)
    self$text_sync(uri, document = NULL, run_lintr = TRUE, parse = TRUE)
}

#' `textDocument/didClose` notification handler
#'
#' Handler to the `textDocument/didClose` [Notification].
#' @keywords internal
text_document_did_close <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    rm(list = uri, envir = self$documents)
}

#' `textDocument/willSaveWaitUntil` notification handler
#'
#' Handler to the `textDocument/willSaveWaitUntil` [Request].
#' @keywords internal
text_document_will_save_wait_until <- function(self, id, params) {

}
