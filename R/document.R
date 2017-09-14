# Notification
did_open <- function(self, params) {
    textDocument <- params$textDocument
    publish_diagnostics(self, textDocument$uri)
}

# Notification
did_change <- function(self, params) {

}

# Notification
will_save <- function(self, params) {

}

# Notification
did_save <- function(self, params) {
    textDocument <- params$textDocument
    publish_diagnostics(self, textDocument$uri)
}

# Notification
did_close <- function(self, params) {

}

# Request
will_save_wait_until <- function(self, id, params) {

}

# Request
completion  <- function(self, id, params) {

}

# Request
completio_item_resolve  <- function(self, id, params) {

}

# Request
hover  <- function(self, id, params) {

}

# Request
signature_help  <- function(self, id, params) {

}

# Request
references  <- function(self, id, params) {

}

# Request
document_highlight  <- function(self, id, params) {

}

# Request
document_symbol  <- function(self, id, params) {

}

# Request
formatting  <- function(self, id, params) {

}

# Request
range_formatting  <- function(self, id, params) {

}

# Request
on_type_formatting  <- function(self, id, params) {

}

# Request
definition  <- function(self, id, params) {

}

# Request
code_action  <- function(self, id, params) {

}

# Request
code_lens  <- function(self, id, params) {

}

# Request
code_lens_resolve  <- function(self, id, params) {

}

# Request
document_link  <- function(self, id, params) {

}

# Request
document_link_resolve  <- function(self, id, params) {

}

# Request
rename  <- function(self, id, params) {

}
