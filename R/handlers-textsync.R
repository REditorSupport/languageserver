# Notification
text_document_did_open <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    doc <- readLines(path_from_uri(uri), warn = FALSE)
    self$documents[[uri]] <- doc
    self$sync_input_dict$set(uri, list(document = NULL, parse = TRUE))
}

# Notification
text_document_did_change <- function(self, params) {
    textDocument <- params$textDocument
    contentChanges <- params$contentChanges
    text <- contentChanges[[1]]$text
    uri <- textDocument$uri
    logger$info("did change: ", uri)
    doc <- stringr::str_split(text, "\r\n|\n")[[1]]
    # remove last empty line
    if (nchar(doc[[length(doc)]]) == 0) doc <- doc[-length(doc)]
    self$documents[[uri]] <- doc
    if (self$sync_input_dict$has(uri)) {
        # make sure we do not accidentially override list call with `parse = FALSE`
        item <- self$sync_input_dict$pop(uri)
        self$sync_input_dict$set(uri, list(document = doc, parse = item$parse))
    } else {
        self$sync_input_dict$set(uri, list(document = doc, parse = FALSE))
    }
}

# Notification
text_document_will_save <- function(self, params) {

}

# Notification
text_document_did_save <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    logger$info("did save:", uri)
    self$documents[[uri]] <- readLines(path_from_uri(uri), warn = FALSE)
    self$sync_input_dict$set(uri, list(document = NULL, parse = TRUE))
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
