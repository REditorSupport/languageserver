workspace <- R6::R6Class("textDocument",
    public = list(
        did_open <- function(self, params) {
            textDocument <- params$textDocument
            publish_diagnostics(self, textDocument$uri)
        },

        did_change <- function(self, params) {

        },

        will_save <- function(self, params) {

        },

        did_save <- function(self, params) {
            textDocument <- params$textDocument
            publish_diagnostics(self, textDocument$uri)
        },

        did_close <- function(self, params) {

        },

        will_save_wait_until <- function(self, id, params) {

        },

        completion  <- function(self, id, params) {

        },

        hover  <- function(self, id, params) {

        },

        signature_help  <- function(self, id, params) {

        },

        references  <- function(self, id, params) {

        },

        document_highlight  <- function(self, id, params) {

        },

        document_symbol  <- function(self, id, params) {

        },

        formatting  <- function(self, id, params) {

        },

        range_formatting  <- function(self, id, params) {

        },

        on_type_formatting  <- function(self, id, params) {

        },

        definition  <- function(self, id, params) {

        },

        code_action  <- function(self, id, params) {

        },

        code_lens  <- function(self, id, params) {

        },

        document_link  <- function(self, id, params) {

        },

        rename  <- function(self, id, params) {

        },
    )
)

workspace <- R6::R6Class("completionItem",
    public = list(
        resolve  <- function(self, id, params) {

        }
    )
)
workspace <- R6::R6Class("codeLens",
    public = list(
        resolve  <- function(self, id, params) {

        }
    )
)

workspace <- R6::R6Class("documentLink",
    public = list(
        resolve  <- function(self, id, params) {
        }
    )
)
