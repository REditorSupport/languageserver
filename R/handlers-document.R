# Notification
text_document_did_open <- function(self, params) {
    textDocument <- params$textDocument
    publish_diagnostics(self, textDocument$uri)
}

# Notification
text_document_did_change <- function(self, params) {

}

# Notification
text_document_will_save <- function(self, params) {

}

# Notification
text_document_did_save <- function(self, params) {
    textDocument <- params$textDocument
    publish_diagnostics(self, textDocument$uri)
}

# Notification
text_document_did_close <- function(self, params) {

}

# Request
text_document_will_save_wait_until <- function(self, id, params) {

}

# Request
text_document_completion  <- function(self, id, params) {

}

# Request
completio_item_resolve  <- function(self, id, params) {

}

# Request
text_document_hover  <- function(self, id, params) {

}

# Request
text_document_signature_help  <- function(self, id, params) {

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
text_document_formatting  <- function(self, id, params) {

}

# Request
text_document_range_formatting  <- function(self, id, params) {

}

# Request
text_document_on_type_formatting  <- function(self, id, params) {

}

# Request
text_document_definition  <- function(self, id, params) {

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
text_document_rename  <- function(self, id, params) {

}
