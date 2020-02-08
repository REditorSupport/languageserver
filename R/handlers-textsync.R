#' `textDocument/didOpen` notification handler
#'
#' Handler to the `textDocument/didOpen` [Notification].
#' @keywords internal
text_document_did_open <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    version <- textDocument$version
    logger$info("did open:", list(uri = uri, version = version))
    content <- readr::read_lines(path_from_uri(uri))
    if (self$workspace$documents$has(uri)) {
        self$workspace$documents$get(uri)$set(version, content)
    } else {
        self$workspace$documents$set(uri, Document$new(uri, version, content))
    }
    self$text_sync(uri, version = version, document = NULL, run_lintr = TRUE, parse = TRUE, resolve = TRUE)
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
    version <- textDocument$version
    logger$info("did change:", list(uri = uri, version = version))
    content <- stringr::str_split(text, "\r\n|\n")[[1]]
    if (self$workspace$documents$has(uri)) {
        doc <- self$workspace$documents$get(uri)
        doc$set(version, content)
    } else {
        doc <- Document$new(uri, version, content)
        self$workspace$documents$set(uri, doc)
    }
    self$text_sync(uri, version = version, document = doc, run_lintr = TRUE, parse = TRUE, resolve = FALSE)
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
    version <- textDocument$version
    logger$info("did save:", list(uri = uri, version = version))
    content <- readr::read_lines(path_from_uri(uri))
    self$workspace$documents$set(uri, Document$new(uri, version, content))
    self$text_sync(uri, version = version, document = NULL, run_lintr = TRUE, parse = TRUE, resolve = TRUE)
}

#' `textDocument/didClose` notification handler
#'
#' Handler to the `textDocument/didClose` [Notification].
#' @keywords internal
text_document_did_close <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$workspace$documents$remove(uri)
    self$pending_replies$remove(uri)
}

#' `textDocument/willSaveWaitUntil` notification handler
#'
#' Handler to the `textDocument/willSaveWaitUntil` [Request].
#' @keywords internal
text_document_will_save_wait_until <- function(self, id, params) {

}
