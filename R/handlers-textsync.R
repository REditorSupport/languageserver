parse_document <- function(path) {
    expr <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
    if (length(expr)) {
        variables <- character()
        closures <- character()
        for (e in expr) {
            if (length(e) == 3L &&
                    is.symbol(e[[1L]]) &&
                    (e[[1L]] == "<-" || e[[1L]] == "=") &&
                    is.symbol(e[[2L]])) {
                symbol <- as.character(e[[2L]])
                if (is.call(e[[3L]]) && e[[3L]][[1L]] == "function") {
                    closures <- union(closures, symbol)
                } else {
                    variables <- union(variables, symbol)
                }
            }
        }
        list(expr = expr, variables = variables, closures = closures)
    }
}

# Notification
text_document_did_open <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    path <- path_from_uri(uri)
    doc <- readLines(path, warn = FALSE)
    attr(doc, "expr") <- parse_document(path)
    self$documents[[uri]] <- doc
    self$sync_input_dict$set(uri, TRUE)
}

# Notification
text_document_did_change <- function(self, params) {
    textDocument <- params$textDocument
    contentChanges <- params$contentChanges
    text <- contentChanges[[1L]]$text
    uri <- textDocument$uri
    doc <- stringr::str_split(text, "\r\n|\n")[[1]]
    # remove last empty line
    if (!nzchar(doc[[length(doc)]])) {
        doc <- doc[-length(doc)]
    }
    attr(doc, "expr") <- attr(self$documents[[uri]], "expr")
    self$documents[[uri]] <- doc
    self$sync_input_dict$set(uri, doc)
}

# Notification
text_document_will_save <- function(self, params) {

}

# Notification
text_document_did_save <- function(self, params) {
    textDocument <- params$textDocument
    uri <- textDocument$uri
    logger$info("did save:", uri)
    path <- path_from_uri(uri)
    doc <- readLines(path, warn = FALSE)
    attr(doc, "expr") <- parse_document(path)
    self$documents[[uri]] <- doc
    self$sync_input_dict$set(uri, TRUE)
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
