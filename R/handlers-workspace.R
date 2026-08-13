FileChangeType <- list(
    Created = 1,
    Changed = 2,
    Deleted = 3
)

#' Refresh a shallow summary without overwriting an open editor buffer
#' @noRd
refresh_index_summary <- function(workspace, index, uri) {
    document_uri <- Filter(function(candidate) {
        identical(index_canonical_uri(candidate), index_canonical_uri(uri))
    }, workspace$documents$keys())
    if (length(document_uri)) {
        doc <- workspace$documents$get(document_uri[[1L]])
        if (isTRUE(doc$is_open)) {
            return(index$update_content(document_uri[[1L]], doc$content))
        }
    }
    index$update_path(path_from_uri(uri))
}

#' `workspace/didChangeWorkspaceFolders` notification handler
#'
#' Handler to the `workspace/didChangeWorkspaceFolders` [Notification]
#' @noRd
workspace_did_change_workspace_folders <- function(self, params) {
    event <- params$event
    for (folder in event$added) {
        uri <- uri_escape_unicode(folder$uri)
        self$add_workspace(uri)
        workspace <- self$get_workspace(uri)
        self$load_workspace(workspace)
    }
    for (folder in event$removed) {
        self$remove_workspace(uri_escape_unicode(folder$uri))
    }
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

    index_settings <- intersect(names(settings), c(
        "index_mode", "index_include", "index_exclude",
        "index_max_files", "index_max_file_size_mb", "index_batch_size",
        "index_time_budget_ms", "index_persistent_cache"
    ))
    lsp_settings$update_from_workspace(settings)

    if (length(index_settings)) {
        for (workspace in self$workspaces$values()) {
            open_documents <- Filter(function(doc) isTRUE(doc$is_open),
                workspace$documents$values())
            workspace$index <- WorkspaceIndex$new(workspace$root)
            self$load_workspace(workspace)
            if (isTRUE(workspace$index$enabled)) {
                for (doc in open_documents) {
                    workspace$index$update_content(
                        doc$uri, doc$content, cacheable = FALSE)
                }
                self$refresh_index_documents(workspace)
            } else {
                self$prune_legacy_documents(workspace)
            }
        }
    }

    if (!lsp_settings$get("diagnostics")) {
        for (workspace in self$workspaces$values()) {
            for (uri in workspace$documents$keys()) {
                diagnostics_callback(self, uri, NULL, list())
            }
        }
    }
}

#' `workspace/didChangeWatchedFiles` notification handler
#'
#' Handler to the `workspace/didChangeWatchedFiles` [Notification].
#' @noRd
workspace_did_change_watched_files <- function(self, params) {
    for (file_event in params$changes) {
        uri <- uri_escape_unicode(file_event$uri)
        path <- path_from_uri(uri)
        workspace <- self$get_workspace(uri)

        if (workspace$documents$has(uri)) {
            doc <- workspace$documents$get(uri)
            if (doc$is_open) {
                # skip open documents
                next
            }
        }

        type <- file_event$type
        index <- workspace$index
        if (!is.null(index) && isTRUE(index$enabled)) {
            if (!index$should_index(path) && type != FileChangeType$Deleted) next

            dependents <- index$dependents(
                uri, include_candidates = type == FileChangeType$Created)
            if (type == FileChangeType$Created ||
                    type == FileChangeType$Changed) {
                logger$info("index", path)
                index$update_path(path)
                # A newly created file can make a previously unresolved
                # static source call resolvable.
                for (dependent in dependents) {
                    refresh_index_summary(workspace, index, dependent)
                }
                package_root <- index$package_root_for_uri(uri)
                if (!is.null(package_root) || workspace$documents$has(uri)) {
                    if (workspace$documents$has(uri)) {
                        workspace$documents$remove(uri)
                    }
                    if (is.function(self$load_index_document)) {
                        self$load_index_document(workspace, uri)
                    }
                }
            } else if (type == FileChangeType$Deleted) {
                logger$info("remove", path)
                index$remove(uri)
                if (workspace$documents$has(uri)) {
                    workspace$documents$remove(uri)
                    workspace$diagnostics_globals_cache <- NULL
                    workspace$type_hierarchy_cache$clear()
                }
                for (dependent in dependents) {
                    refresh_index_summary(workspace, index, dependent)
                }
            }
            if (is.function(self$refresh_index_documents)) {
                self$refresh_index_documents(workspace)
            }
            workspace$update_loaded_packages()
            next
        }

        # Compatibility path when project indexing is disabled.
        if (!is_package(workspace$root)) next
        source_dir <- file.path(workspace$root, "R")
        if (dirname(path) != source_dir) next

        if (type == FileChangeType$Created || type == FileChangeType$Changed) {
            logger$info("load", path)
            doc <- Document$new(uri, language = "r", version = NULL, content = stringi::stri_read_lines(path))
            workspace$documents$set(uri, doc)
            self$text_sync(uri, document = doc, parse = TRUE)
        } else if (type == FileChangeType$Deleted) {
            logger$info("remove", path)
            workspace$documents$remove(uri)
            workspace$diagnostics_globals_cache <- NULL
            workspace$type_hierarchy_cache$clear()
        }
        workspace$update_loaded_packages()
    }
}

#' `workspace/symbol` request handler
#'
#' Handler to the `workspace/symbol` [Request].
#' @noRd
workspace_symbol <- function(self, id, params) {
    self$deliver(workspace_symbol_reply(
            id, self$workspaces$values(), params$query))
}

#' `workspace/executeCommand` request handler
#'
#' Handler to the `workspace/executeCommand` [Request].
#' @noRd
workspace_execute_command <- function(self, id, params) {

}
