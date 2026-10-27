request_snapshot_settings <- function() {
    values <- list(index_mode = "auto", index_persistent_cache = FALSE, diagnostics = FALSE)
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer(for (name in names(old)) lsp_settings$set(name, old[[name]]), envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

request_snapshot_manager <- function() {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::ordered_dict()
    manager$add_task <- function(id, task) {
        if (is.null(task)) manager$tasks$pop(id, NULL) else manager$tasks$set(id, task)
    }
    manager$cancel <- function(id) manager$tasks$pop(id, NULL)
    manager$promote <- function(...) NULL
    manager$has <- function(id) manager$tasks$has(id)
    manager$finish <- function() {
        if (!manager$tasks$size()) return(invisible(NULL))
        task <- manager$tasks$pop(manager$tasks$keys()[[1L]])$.__enclos_env__$private
        result <- do.call(task$target, task$args)
        if (is.function(task$callback)) task$callback(result)
        invisible(NULL)
    }
    manager
}

RequestSnapshotServer <- R6::R6Class("RequestSnapshotServer", inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(
        replies = NULL,
        initialize = function(root) {
            workspace <- testthat::with_mocked_bindings(Workspace$new(root),
                workspace_startup_packages = function() startup_packages,
                workspace_startup_s3 = function() list(), .package = "languageserver")
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, workspace)
            self$workspace_cache <- collections::dict()
            self$index_task_manager <- request_snapshot_manager()
            self$parse_task_manager <- request_snapshot_manager()
            self$resolve_task_manager <- request_snapshot_manager()
            self$pending_replies <- collections::dict()
            self$replies <- list()
            self$load_workspace(workspace)
        },
        deliver = function(response, ...) {
            if (!is.null(response$id)) self$replies[[length(self$replies) + 1L]] <- response
        }
    ))

request_snapshot_open <- function(server, path) {
    workspace <- server$get_workspace(path_to_uri(path))
    document <- Document$new(path_to_uri(path), "r", 1L, stringi::stri_read_lines(path))
    document$did_open()
    workspace$documents$set(document$uri, document)
    server$text_sync(document$uri, document, parse = TRUE)
    document
}

request_snapshot_step <- function(server, parse = TRUE) {
    for (workspace in server$workspaces$values()) workspace$index$process_batch()
    server$process_workspace_documents(time_budget_ms = Inf)
    server$index_task_manager$finish()
    if (parse) server$parse_task_manager$finish()
    server$process_index_requests()
}

request_snapshot_drain <- function(server) {
    for (i in seq_len(500L)) {
        request_snapshot_step(server)
        if (server$index_task_manager$tasks$size() == 0L && server$parse_task_manager$tasks$size() == 0L &&
                (is.null(server$index_requests) || server$index_requests$size() == 0L) &&
                all(vapply(server$workspaces$values(), function(workspace) workspace$index$complete(), logical(1L)))) {
            for (workspace in server$workspaces$values()) server$prune_index_documents(workspace)
            return(invisible(NULL))
        }
    }
    stop("index requests did not settle")
}

request_snapshot_prepare <- function(server, id) {
    key <- paste0("number:", id)
    for (i in seq_len(100L)) {
        request_snapshot_step(server, parse = FALSE)
        request <- server$index_requests$get(key, NULL)
        if (!is.null(request) && request$prepared) return(request)
    }
    stop("index request was not prepared")
}

test_that("queued references and rename cancel changed-content saves at the same version", {
    request_snapshot_settings()
    for (handler in list(text_document_references, text_document_rename)) {
        for (prepared in c(FALSE, TRUE)) {
            root <- withr::local_tempdir()
            helper <- file.path(root, "helper.R")
            writeLines("original <- function(x) x", helper)
            writeLines(c("source('helper.R')", "original(1)"), file.path(root, "caller.R"))
            server <- RequestSnapshotServer$new(root)
            document <- request_snapshot_open(server, helper)
            request_snapshot_drain(server)
            params <- list(textDocument = list(uri = document$uri), position = position(0L, 3L), newName = "renamed")
            handler(server, 1L, params)
            expect_identical(server$index_requests$size(), 1L)
            if (prepared) request_snapshot_prepare(server, 1L)

            text_document_did_save(server, list(textDocument = list(uri = document$uri),
                    text = "replacement <- function(x) x"))
            expect_identical(document$version, 1L)
            server$process_index_requests()
            expect_identical(server$index_requests$size(), 0L)
            expect_length(server$replies, 1L)
            expect_identical(server$replies[[1L]]$error$code, -32800)
            request_snapshot_drain(server)
            expect_length(server$replies, 1L)
            expect_identical(document$content, "replacement <- function(x) x")
        }
    }
})

test_that("prepared references and rename include new and changed closed source consumers", {
    request_snapshot_settings()
    for (handler in list(text_document_references, text_document_rename)) {
        for (created in c(TRUE, FALSE)) {
            root <- withr::local_tempdir()
            helper <- file.path(root, "helper.R")
            caller <- file.path(root, "caller.R")
            newcomer <- file.path(root, "newcomer.R")
            writeLines("shared <- function(x) x", helper)
            writeLines(c("source('helper.R')", "shared(1)"), caller)
            if (!created) writeLines("unrelated <- 1", newcomer)
            server <- RequestSnapshotServer$new(root)
            document <- request_snapshot_open(server, helper)
            request_snapshot_drain(server)
            params <- list(textDocument = list(uri = document$uri), position = position(0L, 3L), newName = "renamed")
            handler(server, 1L, params)
            request <- request_snapshot_prepare(server, 1L)
            expect_setequal(request$held_documents, c(document$uri, path_to_uri(caller)))
            expect_true(server$get_workspace(document$uri)$index$document_pending(path_to_uri(caller)))

            writeLines(c("source('helper.R')", "shared(2)"), newcomer)
            change <- list(uri = path_to_uri(newcomer),
                type = if (created) FileChangeType$Created else FileChangeType$Changed)
            workspace_did_change_watched_files(server, list(changes = list(change)))
            request_snapshot_drain(server)
            expect_length(server$replies, 1L)
            reply <- server$replies[[1L]]
            expect_null(reply$error)
            uris <- if (identical(handler, text_document_rename)) {
                names(reply$result$changes)
            } else {
                vapply(reply$result, `[[`, "", "uri")
            }
            expect_setequal(uris, c(document$uri, path_to_uri(caller), path_to_uri(newcomer)))
        }
    }
})
