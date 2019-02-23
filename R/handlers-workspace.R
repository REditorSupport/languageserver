#' workspace/didChangeWorkspaceFolders notification handler
#'
#' Handler to the [workspace/didChangeWorkspaceFolders](https://microsoft.github.io/language-server-protocol/specification#workspace_didChangeWorkspaceFolders) [Notification].
#'
#' Not implemented yet.
#'
#' @template self
#' @param params a did_change_workspace_folder_params object
#'
#' @keywords internal
workspace_did_change_workspace_folder_params <- function(self, params) {

}

#' workspace/didChangeConfiguration notification handler
#'
#' Handler to the [workspace/didChangeConfiguration](https://microsoft.github.io/language-server-protocol/specification#workspace_didChangeConfiguration) [Notification]
#'
#' @template self
#' @param params a [did_change_configuration_params] object
#'
#' @keywords internal
workspace_did_change_configuration <- function(self, params) {
    settings <- params$settings

    # flatten vscode r-lsp settings
    settings <- tryCatch(settings$r$lsp, error = function(e) settings)

    logger$debug_mode(settings$debug)
    logger$info(settings)

    if (!is.null(settings$diagnostics) && !isTRUE(settings$diagnostics)) {
        logger$info("disable diagnostics")
        self$run_lintr <- FALSE
    }
}

#' workspace/didChangeWatchedFiles notification handler
#'
#' Handler to the [workspace/didChangeWatchedFiles](https://microsoft.github.io/language-server-protocol/specification#workspace_didChangeWatchedFiles) [Notification].
#'
#' Not implemented yet.
#'
#' @template self
#' @param params a did_change_watched_files_params object
#'
#' @keywords internal
workspace_did_change_watched_files <- function(self, params){

}

#' workspace/symbol request handler
#'
#' Handler to the [workspace/symbol](https://microsoft.github.io/language-server-protocol/specification#workspace_symbol) [Request].
#'
#' Not implemented yet.
#'
#' @template self
#' @param params a workspace_symbol_params object
#'
#' @keywords internal
workspace_symbol <- function(self, id, params) {

}

#' workspace/executeCommand request handler
#'
#' Handler to the [workspace/executeCommand](https://microsoft.github.io/language-server-protocol/specification#workspace_executeCommand) [Request].
#'
#' Not implemented yet.
#'
#' @template self
#' @param params an execute_command_params object
#'
#' @keywords internal
workspace_execute_command <- function(self, id, params) {

}
