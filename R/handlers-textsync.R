#' `textDocument/didOpen` notification handler
#'
#' Handler to the `textDocument/didOpen` [Notification].
#' @noRd
update_document_index <- function(self, workspace, uri, content,
    cacheable = FALSE) {
    if (is.null(workspace$index) || !isTRUE(workspace$index$enabled)) {
        return(invisible(NULL))
    }
    doc <- workspace$documents$get(uri, NULL)
    parsed <- if (!is.null(doc) &&
            identical(doc$version, doc$parse_data$version) &&
            identical(content, doc$parsed_content) &&
            !isTRUE(doc$parse_data$parse_error) &&
            !is.null(doc$parse_data$source_specs)) doc$parse_data else NULL
    if (is.null(parsed) && !is.null(workspace$index$task_manager)) {
        workspace$index$await_parse(uri, cacheable = cacheable)
    } else {
        workspace$index$update_content(uri, content, cacheable = cacheable,
            parse_data = parsed)
    }
    self$refresh_index_documents(workspace, uri)
    invisible(NULL)
}

text_document_did_open <- function(self, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    cancel_formatting_requests(self, uri = uri,
        message = "Request superseded by a reopened document")
    language <- textDocument$languageId
    version <- textDocument$version
    text <- textDocument$text
    logger$info("did open:", list(uri = uri, version = version))
    path <- path_from_uri(uri)
    if (!is.null(text)) {
        content <- split_document_lines(text)
    } else if (file.exists(path)) {
        content <- read_text_lines(path)
    } else {
        content <- NULL
    }
    
    workspace <- self$get_workspace(uri)

    if (workspace$documents$has(uri)) {
        doc <- workspace_remove_document(self, workspace, uri)
    }
    doc <- Document$new(uri, language = language, version = version, content = content)
    workspace$documents$set(uri, doc)
    doc$did_open()
    update_document_index(self, workspace, uri, doc$content)
    # Performance: Parse immediately on open (no delay) to have data ready for initial requests
    self$text_sync(uri, document = doc, run_diagnostics = TRUE, parse = TRUE, delay = 0)
}

#' `textDocument/didChange` notification handler
#'
#' Handler to the `textDocument/didChange` [Notification].
#' @noRd
text_document_did_change <- function(self, params) {
    textDocument <- params$textDocument
    contentChanges <- params$contentChanges
    uri <- uri_escape_unicode(textDocument$uri)
    version <- textDocument$version
    logger$info("did change:", list(uri = uri, version = version))
    cancel_formatting_requests(self, uri = uri,
        message = "Request superseded by a newer document version")

    workspace <- self$get_workspace(uri)
    pending <- self$pending_replies$get(uri, NULL)
    for (queue in pending) {
        retained <- list()
        while (queue$size()) {
            item <- queue$pop()
            if (!is.null(item$version) && item$version < version) {
                s3_forget_hint_waiters(workspace, uri, item$id)
                self$deliver(ResponseErrorMessage$new(
                    item$id,
                    "RequestCancelled",
                    "Request superseded by a newer document version"
                ))
            } else {
                retained[[length(retained) + 1L]] <- item
            }
        }
        for (item in retained) queue$push(item)
    }
    
    if (workspace$documents$has(uri)) {
        doc <- workspace$documents$get(uri)
        doc$apply_content_changes(version, contentChanges)
    } else {
        # Incremental changes are only valid for an open document. Be
        # tolerant of a client that sends a full replacement before didOpen.
        full_change <- Filter(function(change) is.null(change$range), contentChanges)
        content <- if (length(full_change)) {
            split_document_lines(full_change[[length(full_change)]]$text)
        } else {
            ""
        }
        doc <- Document$new(uri, language = NULL, version = version, content = content)
        workspace$documents$set(uri, doc)
    }
    doc$did_open()
    # The accepted background parse updates definitions and source edges.
    # Parsing a shallow index here would block every keystroke a second time.
    # text_sync's await_parse also invalidates any pending disk snapshot.
    self$text_sync(
        uri,
        document = doc,
        run_diagnostics = TRUE,
        parse = TRUE,
        parse_delay = lsp_settings$get("parse_delay"),
        diagnostics_delay = lsp_settings$get("diagnostics_delay")
    )
}

#' `textDocument/willSave` notification handler
#'
#' Handler to the `textDocument/willSave` [Notification].
#' @noRd
text_document_will_save <- function(self, params) {

}

#' `textDocument/didSave` notification handler
#'
#' Handler to the `textDocument/didSave` [Notification].
#' @noRd
text_document_did_save <- function(self, params) {
    textDocument <- params[["textDocument"]]
    text <- params[["text"]]
    uri <- uri_escape_unicode(textDocument[["uri"]])
    logger$info("did save:", list(uri = uri))

    workspace <- self$get_workspace(uri)

    # https://github.com/microsoft/language-server-protocol/issues/2110
    # follow dbaeumer's take on not handling files that weren't claimed before didSave
    if (!workspace$documents$has(uri)) {
        return(NULL)
    }

    path <- path_from_uri(uri)
    if (!is.null(text)) {
        content <- split_document_lines(text)
    } else if (file.exists(path)) {
        content <- read_text_lines(path)
    } else {
        content <- NULL
    }
    doc <- workspace$documents$get(uri)
    if (!identical(doc$content, content)) {
        cancel_formatting_requests(self, uri = uri,
            message = "Request superseded by updated document content")
    }
    doc$set_content(doc$version, content)
    doc$member_refresh <- isTRUE(lsp_settings$get("member_completion")) || isTRUE(lsp_settings$get("s3_dispatch"))
    doc$did_open()
    update_document_index(
        self, workspace, uri, doc$content, cacheable = TRUE)
    self$text_sync(uri, document = doc, run_diagnostics = TRUE, parse = TRUE)
}

#' `textDocument/didClose` notification handler
#'
#' Handler to the `textDocument/didClose` [Notification].
#' @noRd
text_document_did_close <- function(self, params) {
    textDocument <- params$textDocument
    uri <- uri_escape_unicode(textDocument$uri)
    cancel_formatting_requests(self, uri = uri,
        message = "Request cancelled because the document was closed")
    # Retained package documents keep their version and content after close.
    # Discard buffer parses before queuing a refresh from disk.
    if (!is.null(self$parse_task_manager)) self$parse_task_manager$cancel(uri)
    path <- path_from_uri(uri)
    workspace <- self$get_workspace(uri)
    index <- workspace$index
    s3_forget_hint_waiters(workspace, uri)
    is_from_workspace <- nzchar(path) && path_has_parent(path, workspace$root)

    if (!is_from_workspace && is.function(index$remove)) {
        index$remove(uri)
    } else if (is.function(index$finish_document)) {
        index$finish_document(uri)
    }

    doc <- workspace$documents$get(uri, NULL)
    if (!is.null(doc)) doc$did_close()
    if (is_from_workspace && isTRUE(index$enabled)) {
        if (file.exists(path)) {
            if (is.function(self$load_index_document) && is.function(index$package_has_hint_inheritance) &&
                    index$package_has_hint_inheritance(uri) &&
                    length(attr(doc$parse_data$member_data$type_hints, "topics", exact = TRUE))) {
                self$load_index_document(workspace, uri, reload = TRUE)
            } else if (is.null(index$task_manager)) {
                index$update_path(path)
            } else {
                index$queue_path(path, force = TRUE)
            }
        } else {
            index$remove(uri)
        }
        self$prune_index_documents(workspace)
    } else if (is_from_workspace && is_package(workspace$root)) {
        # Package providers still retain full documents with indexing off.
        # Closing the editor discards unsaved content in this path too.
        if (is.function(self$load_index_document)) {
            self$load_index_document(workspace, uri, reload = TRUE)
        }
    } else {
        if (!is.null(self$diagnostics_task_manager)) self$diagnostics_task_manager$cancel(uri)
        if (!is.null(self$resolve_task_manager)) self$resolve_task_manager$cancel(uri)
        if (!is.null(self$member_task_manager)) self$member_task_manager$cancel(uri)
        if (!is.null(doc)) {
            diagnostics_callback(self, uri, NULL, list())
            workspace_remove_document(self, workspace, uri)
            workspace$diagnostics_globals_cache <- NULL
            workspace$type_hierarchy_cache$clear()
            workspace$update_loaded_packages()
        }
    }

    pending <- self$pending_replies$pop(uri, NULL)
    for (queue in pending) {
        while (queue$size()) {
            item <- queue$pop()
            self$deliver(ResponseErrorMessage$new(
                item$id, "RequestCancelled", "Request cancelled because the document was closed"
            ))
        }
    }
}

#' `textDocument/willSaveWaitUntil` notification handler
#'
#' Handler to the `textDocument/willSaveWaitUntil` [Request].
#' @noRd
text_document_will_save_wait_until <- function(self, id, params) {

}
