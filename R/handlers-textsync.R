# Notification
text_document_did_open <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$documents[[uri]] <- readLines(path_from_uri(uri))
    diagnostic_queue$put(uri, NULL)
}

# Notification
text_document_did_change <- function(self, params) {
    textDocument <- params$textDocument
    contentChanges <- params$contentChanges
    text <- contentChanges$text
    uri <- textDocument$uri
    self$documents[[uri]] <- stringr::str_split(text, "\n")[[1]]
    diagnostic_queue$put(uri, text)
}

# Notification
text_document_will_save <- function(self, params) {

}

# Notification
text_document_did_save <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    self$documents[[uri]] <- readLines(path_from_uri(uri))
    diagnostic_queue$put(uri, NULL)
}

# Notification
text_document_did_close <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    rm(list = uri, envir = self$documents)
}

# Request
text_document_will_save_wait_until <- function(self, id, params) {

}
