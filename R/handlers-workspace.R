# Notification
workspace_did_change_configuration <- function(self, params) {
    settings <- params$settings
    if (!is.null(settings$debug) && isTRUE(settings$debug)) {
        logger$debug_mode()
    }
    if (!is.null(settings$diagnostics) && !isTRUE(settings$diagnostics)) {
        self$run_lintr <- FALSE
    }
}

workspace_did_change_watched_files <- function(self, params){

}

# Request
workspace_symbol <- function(self, id, params) {

}

workspace_execute_command <- function(self, id, params) {

}
