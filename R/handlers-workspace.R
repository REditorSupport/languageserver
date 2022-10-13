FileChangeType <- list(
    Created = 1,
    Changed = 2,
    Deleted = 3
)

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
    vscode_settings <- settings$r$lsp
    settings <- if (is.null(vscode_settings)) settings else vscode_settings

    logger$info("settings ", settings)

    lsp_settings$update_from_workspace(settings)

    if (!lsp_settings$get("diagnostics")) {
        for (uri in self$workspace$documents$keys()) {
            diagnostics_callback(self, uri, NULL, list())
        }
    }
}

#' `workspace/didChangeWatchedFiles` notification handler
#'
#' Handler to the `workspace/didChangeWatchedFiles` [Notification].
#' @noRd
workspace_did_change_watched_files <- function(self, params) {
    # All open documents will be automatically handled by lsp requests.
    # Only non-open documents in a package should be handled here.

    project_root <- self$rootPath
    if (is_package(project_root)) {
        source_dir <- file.path(project_root, "R")
        for (file_event in params$changes) {
            uri <- file_event$uri
            path <- path_from_uri(uri)

            if (dirname(path) != source_dir) {
                next
            }

            if (self$workspace$documents$has(uri)) {
                doc <- self$workspace$documents$get(uri)
                if (doc$is_open) {
                    # skip open documents
                    next
                }
            }

            type <- file_event$type

            if (type == FileChangeType$Created || type == FileChangeType$Changed) {
                logger$info("load", path)
                doc <- Document$new(uri, language = "r", version = NULL, content = stringi::stri_read_lines(path))
                self$workspace$documents$set(uri, doc)
                self$text_sync(uri, document = doc, parse = TRUE)
            } else if (type == FileChangeType$Deleted) {
                logger$info("remove", path)
                self$workspace$documents$remove(uri)
            }
        }
        self$workspace$update_loaded_packages()
    }
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
