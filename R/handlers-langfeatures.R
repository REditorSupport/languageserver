
# Request
text_document_completion  <- function(self, id, params) {
    textDocument <- params$textDocument
    self$deliver(completion_reply(
        id, self$workspace, self$documents[[textDocument$uri]], params$position))
}

# Request
completio_item_resolve  <- function(self, id, params) {

}

# Request
text_document_hover  <- function(self, id, params) {
    textDocument <- params$textDocument
    self$deliver(hover_reply(
        id, self$workspace, self$documents[[textDocument$uri]], params$position))
}

# Request
text_document_signature_help  <- function(self, id, params) {
    textDocument <- params$textDocument
    self$deliver(signature_reply(
        id, self$workspace, self$documents[[textDocument$uri]], params$position))
}

# Request
text_document_definition  <- function(self, id, params) {

}

# Request
text_document_references  <- function(self, id, params) {

}

# Request
text_document_document_highlight  <- function(self, id, params) {

}

# Request
text_document_document_symbol  <- function(self, id, params) {

}

# Request
text_document_code_action  <- function(self, id, params) {

}

# Request
text_document_code_lens  <- function(self, id, params) {

}

# Request
code_lens_resolve  <- function(self, id, params) {

}

# Request
text_document_document_link  <- function(self, id, params) {

}

# Request
document_link_resolve  <- function(self, id, params) {

}

# Request
text_document_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    options <- params$options
    self$deliver(formatting_reply(id, self$documents[[textDocument$uri]], options))
}

# Request
text_document_range_formatting  <- function(self, id, params) {
    textDocument <- params$textDocument
    range <- params$range
    options <- params$options
    self$deliver(range_formatting_reply(id, self$documents[[textDocument$uri]], range, options))
}

# Request
text_document_on_type_formatting  <- function(self, id, params) {

}

# Request
text_document_rename  <- function(self, id, params) {

}
