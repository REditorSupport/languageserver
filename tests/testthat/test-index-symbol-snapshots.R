symbol_snapshot_settings <- function() {
    values <- list(index_mode = "auto", index_persistent_cache = FALSE, diagnostics = FALSE)
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer(for (name in names(old)) lsp_settings$set(name, old[[name]]), envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

symbol_snapshot_manager <- function() {
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

SymbolSnapshotServer <- R6::R6Class("SymbolSnapshotServer", inherit = LanguageServer,
    private = list(finalize = function() NULL), public = list(
        replies = NULL,
        initialize = function(roots) {
            self$workspaces <- collections::dict()
            self$workspace_cache <- collections::dict()
            self$index_task_manager <- symbol_snapshot_manager()
            self$parse_task_manager <- symbol_snapshot_manager()
            self$resolve_task_manager <- symbol_snapshot_manager()
            self$pending_replies <- collections::dict()
            self$replies <- list()
            testthat::with_mocked_bindings({
                self$add_workspace(NULL)
                for (root in roots) self$add_workspace(path_to_uri(root))
            }, workspace_startup_packages = function() startup_packages,
            workspace_startup_s3 = function() list(), .package = "languageserver")
            self$load_workspaces()
        },
        deliver = function(response, ...) {
            if (!is.null(response$id)) self$replies[[length(self$replies) + 1L]] <- response
        }
    ))

symbol_snapshot_step <- function(server) {
    for (workspace in server$workspaces$values()) {
        if (isTRUE(workspace$index$enabled)) workspace$index$process_batch()
    }
    server$process_workspace_documents(time_budget_ms = Inf)
    server$index_task_manager$finish()
    server$parse_task_manager$finish()
    server$process_index_requests()
}

symbol_snapshot_drain <- function(server) {
    for (i in seq_len(500L)) {
        symbol_snapshot_step(server)
        if (!server$index_task_manager$tasks$size() && !server$parse_task_manager$tasks$size() &&
                (is.null(server$index_requests) || !server$index_requests$size()) &&
                all(vapply(server$workspaces$values(), function(workspace) workspace$index$complete(), logical(1L)))) {
            return(invisible(NULL))
        }
    }
    stop("symbol requests did not settle")
}

symbol_snapshot_open <- function(server, path) {
    uri <- path_to_uri(path)
    document <- Document$new(uri, "r", 1L, stringi::stri_read_lines(path))
    document$did_open()
    server$get_workspace(uri)$documents$set(uri, document)
    server$text_sync(uri, document, parse = TRUE)
    document
}

test_that("workspace symbols return available results despite unrelated failed files", {
    symbol_snapshot_settings()
    parent <- index_normalize_path(withr::local_tempdir())
    roots <- file.path(parent, paste0("workspace", seq_len(2L)))
    for (root in roots) dir.create(root)
    paths <- file.path(roots, c("one.R", "two.R"))
    writeLines("available_one <- function(value) value", paths[[1L]])
    writeLines("available_two <- function(value) value", paths[[2L]])
    bad <- file.path(roots[[1L]], "bad.R")
    writeLines("failed <- function(value) value", bad)
    server <- SymbolSnapshotServer$new(roots)
    workspace <- server$get_workspace(path_to_uri(paths[[1L]]))
    workspace$index$fail_parse(path_to_uri(bad))
    workspace_symbol(server, 1L, list(query = "available_"))
    expect_length(server$replies, 0L)
    symbol_snapshot_drain(server)
    expect_length(server$replies, 1L)
    expect_null(server$replies[[1L]]$error)
    expect_setequal(vapply(server$replies[[1L]]$result, `[[`, "", "name"), c("available_one", "available_two"))
    expect_true(workspace$index$parse_failed(path_to_uri(bad)))

    document <- symbol_snapshot_open(server, paths[[1L]])
    symbol_snapshot_drain(server)
    for (handler in list(text_document_references, text_document_rename)) {
        server$replies <- list()
        handler(server, 2L, list(textDocument = list(uri = document$uri),
                position = position(0L, 3L), newName = "renamed"))
        symbol_snapshot_drain(server)
        expect_length(server$replies, 1L)
        expect_identical(server$replies[[1L]]$error$code, -32603)
    }
})

test_that("queued workspace symbols cancel added, removed, and replaced workspace sets", {
    symbol_snapshot_settings()
    for (mutation in c("add", "remove", "replace")) {
        parent <- index_normalize_path(withr::local_tempdir())
        roots <- file.path(parent, paste0("workspace", seq_len(3L)))
        for (root in roots) dir.create(root)
        for (i in seq_along(roots)) writeLines(paste0("workspace_symbol_", i, " <- 1"), file.path(roots[[i]], "main.R"))
        server <- SymbolSnapshotServer$new(roots[1:2])
        workspace_symbol(server, 10L, list(query = "workspace_symbol_"))
        expect_length(server$replies, 0L)
        expect_identical(server$index_requests$size(), 1L)
        if (mutation == "add") {
            workspace_did_change_workspace_folders(server, list(event = list(
                added = list(list(uri = path_to_uri(roots[[3L]]))), removed = list())))
            added <- server$get_workspace(path_to_uri(roots[[3L]]))
            expect_false(added$index$full_started)
        } else if (mutation == "remove") {
            workspace_did_change_workspace_folders(server, list(event = list(
                added = list(), removed = list(list(uri = path_to_uri(roots[[2L]]))))))
        } else {
            server$remove_workspace(path_to_uri(roots[[2L]]))
            testthat::with_mocked_bindings(server$add_workspace(path_to_uri(roots[[2L]])),
                workspace_startup_packages = function() startup_packages,
                workspace_startup_s3 = function() list(), .package = "languageserver")
            server$load_workspace(server$get_workspace(path_to_uri(roots[[2L]])))
        }
        server$process_index_requests()
        expect_length(server$replies, 1L)
        reply <- if (length(server$replies)) server$replies[[1L]] else NULL
        expect_identical(reply$error$code, -32800)
        expect_identical(server$index_requests$size(), 0L)
        symbol_snapshot_drain(server)
        expect_length(server$replies, 1L)
    }
})

test_that("queued workspace symbols use a complete unchanged multi-root snapshot", {
    symbol_snapshot_settings()
    parent <- index_normalize_path(withr::local_tempdir())
    roots <- file.path(parent, paste0("workspace", seq_len(2L)))
    for (root in roots) dir.create(root)
    for (i in seq_along(roots)) writeLines(paste0("snapshot_symbol_", i, " <- 1"), file.path(roots[[i]], "main.R"))
    server <- SymbolSnapshotServer$new(roots)
    workspace_symbol(server, 1L, list(query = "snapshot_symbol_"))
    expect_length(server$replies, 0L)
    symbol_snapshot_drain(server)
    expect_length(server$replies, 1L)
    expect_null(server$replies[[1L]]$error)
    expect_setequal(vapply(server$replies[[1L]]$result, `[[`, "", "name"), c("snapshot_symbol_1", "snapshot_symbol_2"))
})
