#' `workspace/didChangeWorkspaceFolders` notification handler
#'
#' Handler to the `workspace/didChangeWorkspaceFolders` [Notification].
#' @noRd
workspace_did_change_workspace_folder_params <- function(self, params) {

}

#' `workspace/didChangeConfiguration` notification handler
#'
#' Handler to the `workspace/didChangeConfiguration` [Notification]
#' @noRd
workspace_did_change_configuration <- function(self, params) {
    settings <- params$settings

    # flatten vscode r-lsp settings
    vscode_setting <- tryCatch(settings$r$lsp, error = function(e) NULL)
    settings <- if (is.null(vscode_setting)) settings else vscode_setting

    logger$info("settings ", settings)

    lsp_settings$update_from_workspace(settings)
}

#' `workspace/didChangeWatchedFiles` notification handler
#'
#' Handler to the `workspace/didChangeWatchedFiles` [Notification].
#' @noRd
workspace_did_change_watched_files <- function(self, params) {
    logger$info("workspace_did_change_watched_files:", params)
}

#' `workspace/symbol` request handler
#'
#' Handler to the `workspace/symbol` [Request].
#' @noRd
workspace_symbol <- function(self, id, params) {
    self$deliver(workspace_symbol_reply(
            id, self$workspace, params$query))
}

#' `workspace/executeCommand` request handler
#'
#' Handler to the `workspace/executeCommand` [Request].
#' @noRd
workspace_execute_command <- function(self, id, params) {

}
