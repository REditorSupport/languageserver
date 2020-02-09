#' `textDocument/didOpen` notification handler
#'
#' Handler to the `textDocument/didOpen` [Notification].
#' @keywords internal
text_document_did_open <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    version <- textDocument$version
    text <- textDocument$text
    logger$info("did open:", list(uri = uri, version = version))
    path <- path_from_uri(uri)
    if (!is.null(text)) {
        content <- stringr::str_split(text, "\r\n|\n")[[1]]
    } else if (file.exists(path)) {
        content <- readr::read_lines(path)
    } else {
        content <- NULL
    }
    if (self$workspace$documents$has(uri)) {
        doc <- self$workspace$documents$get(uri)
        doc$set_content(version, content)
    } else {
        doc <- Document$new(uri, version, content)
        self$workspace$documents$set(uri, doc)
    }
    self$text_sync(uri, document = doc, run_lintr = TRUE, parse = TRUE, resolve = TRUE)
}

#' `textDocument/didChange` notification handler
#'
#' Handler to the `textDocument/didChange` [Notification].
#' @keywords internal
text_document_did_change <- function(self, params) {
    textDocument <- params$textDocument
    contentChanges <- params$contentChanges
    uri <- textDocument$uri
    version <- textDocument$version
    text <- contentChanges[[1]]$text
    logger$info("did change:", list(uri = uri, version = version))
    content <- stringr::str_split(text, "\r\n|\n")[[1]]
    if (self$workspace$documents$has(uri)) {
        doc <- self$workspace$documents$get(uri)
        doc$set_content(version, content)
    } else {
        doc <- Document$new(uri, version, content)
        self$workspace$documents$set(uri, doc)
    }
    self$text_sync(uri, document = doc, run_lintr = TRUE, parse = TRUE)
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
    text <- params$text
    logger$info("did save:", list(uri = uri))
    path <- path_from_uri(uri)
    if (!is.null(text)) {
        content <- stringr::str_split(text, "\r\n|\n")[[1]]
    } else if (file.exists(path)) {
        content <- readr::read_lines(path)
    } else {
        content <- NULL
    }
    if (self$workspace$documents$has(uri)) {
        doc <- self$workspace$documents$get(uri)
        doc$set_content(NULL, content)
    } else {
        doc <- Document$new(uri, NULL, content)
        self$workspace$documents$set(uri, doc)
    }
    self$text_sync(uri, document = doc, run_lintr = TRUE, parse = TRUE, resolve = TRUE)
}

#' `textDocument/didClose` notification handler
#'
#' Handler to the `textDocument/didClose` [Notification].
#' @keywords internal
text_document_did_close <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    # do not remove document in package
    if (!(is_package(self$rootUri) && startsWith(uri, self$rootUri))) {
        self$workspace$documents$remove(uri)
    }
    self$pending_replies$remove(uri)
}

#' `textDocument/willSaveWaitUntil` notification handler
#'
#' Handler to the `textDocument/willSaveWaitUntil` [Request].
#' @keywords internal
text_document_will_save_wait_until <- function(self, id, params) {

}
