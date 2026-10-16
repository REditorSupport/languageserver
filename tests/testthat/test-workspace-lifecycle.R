lifecycle_settings <- function(...) {
    values <- list(...)
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer({
        for (name in names(old)) lsp_settings$set(name, old[[name]])
    }, envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

lifecycle_manager <- function(run = FALSE) {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::ordered_dict()
    manager$cancelled <- character()
    manager$discoveries <- 0L
    manager$add_task <- function(id, task) {
        if (!is.null(task)) manager$tasks$set(id, task)
    }
    manager$cancel <- function(id) {
        manager$cancelled <- c(manager$cancelled, id)
        manager$tasks$pop(id, NULL)
        invisible(NULL)
    }
    manager$finish_one <- function() {
        if (!manager$tasks$size()) return(invisible(NULL))
        task <- manager$tasks$pop(manager$tasks$keys()[[1L]])
        private <- task$.__enclos_env__$private
        if (!is.null(private$args$settings)) manager$discoveries <- manager$discoveries + 1L
        result <- do.call(private$target, private$args)
        if (is.function(private$callback)) private$callback(result)
        invisible(NULL)
    }
    manager$check_tasks <- function() NULL
    manager$run_tasks <- function() if (run) manager$finish_one() else NULL
    manager$has_work <- function() manager$tasks$size() > 0L
    manager
}

LifecycleServer <- R6::R6Class("LifecycleServer",
    inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(
        messages = NULL,
        initialize = function(root = NULL) {
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, Workspace$new(NULL))
            self$workspace_cache <- collections::dict()
            self$pending_replies <- collections::dict()
            self$request_callbacks <- collections::dict()
            self$index_task_manager <- lifecycle_manager(run = TRUE)
            self$parse_task_manager <- lifecycle_manager()
            self$diagnostics_task_manager <- lifecycle_manager()
            self$resolve_task_manager <- lifecycle_manager()
            self$formatting_task_manager <- lifecycle_manager()
            self$messages <- list()
            self$register_handlers()
            if (!is.null(root)) {
                self$rootUri <- path_to_uri(root)
                self$rootPath <- root
                self$add_workspace(self$rootUri)
            }
        },
        deliver = function(message, ...) self$messages[[length(self$messages) + 1L]] <- message
    )
)

lifecycle_parse <- function(workspace, uri, content, version = 1L, open = TRUE) {
    document <- workspace$documents$get(uri, NULL)
    if (is.null(document)) {
        document <- Document$new(uri, version = version, content = content)
        workspace$documents$set(uri, document)
    } else {
        document$set_content(version, content)
    }
    if (open) document$did_open()
    parsed <- parse_document(uri, content)
    parsed$version <- version
    workspace$update_parse_data(uri, parsed)
    document
}

lifecycle_drain_index <- function(server, workspace) {
    for (i in seq_len(100L)) {
        index <- workspace$index
        index$process_batch()
        server$index_task_manager$finish_one()
        if (!index$discovering && !index$worker_running && !length(index$pending)) return(invisible(NULL))
    }
    stop("index did not drain")
}

test_that("pruning an unneeded source releases its pending full parse", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = FALSE)
    root <- withr::local_tempdir()
    main <- path_to_uri(file.path(root, "main.R"))
    helper <- path_to_uri(file.path(root, "helper.R"))
    writeLines("source('helper.R')", path_from_uri(main))
    writeLines("helper <- function() 1", path_from_uri(helper))
    server <- LifecycleServer$new(root)
    workspace <- server$get_workspace(main)
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)
    lifecycle_parse(workspace, main, "source('helper.R')")
    server$refresh_index_documents(workspace, main)
    server$process_workspace_documents()
    expect_true(server$parse_task_manager$tasks$has(helper))
    pending <- server$parse_task_manager$tasks$get(helper)

    lifecycle_parse(workspace, main, "main <- 1", version = 2L)
    server$refresh_index_documents(workspace, main)
    expect_false(workspace$documents$has(helper))
    private <- pending$.__enclos_env__$private
    private$callback(do.call(private$target, private$args))
    lifecycle_drain_index(server, workspace)
    expect_false(workspace$documents$has(helper))
    expect_true(workspace$index$indexing_status()$done)
    expect_true(refactor_index_ready(workspace))
    expect_true(helper %in% server$parse_task_manager$cancelled)
})

test_that("closing an edited package buffer restores its full disk parse", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: lifecyclefixture", file.path(root, "DESCRIPTION"))
    uri <- path_to_uri(file.path(root, "R", "helper.R"))
    saved <- "saved <- function(x) x"
    writeLines(saved, path_from_uri(uri))
    server <- LifecycleServer$new(root)
    workspace <- server$get_workspace(uri)
    document <- lifecycle_parse(workspace, uri, saved)
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)
    document$set_content(2L, "unsaved <- function(y) y")
    workspace$index$await_parse(uri, cacheable = FALSE)
    server$text_sync(uri, document, parse = TRUE)
    text_document_did_close(server, list(textDocument = list(uri = uri)))
    lifecycle_drain_index(server, workspace)
    server$process_workspace_documents()
    expect_true(server$parse_task_manager$tasks$has(uri))
    server$parse_task_manager$finish_one()
    lifecycle_drain_index(server, workspace)
    restored <- workspace$documents$get(uri)
    expect_identical(restored$content, saved)
    expect_false(restored$is_open)
    expect_identical(names(restored$parse_data$definitions), "saved")
    expect_true(refactor_index_ready(workspace))
})

test_that("queued disk reloads preserve a buffer reopened before the read", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: reopenedfixture", file.path(root, "DESCRIPTION"))
    uri <- path_to_uri(file.path(root, "R", "helper.R"))
    writeLines("saved <- 1", path_from_uri(uri))
    server <- LifecycleServer$new(root)
    workspace <- server$get_workspace(uri)
    document <- lifecycle_parse(workspace, uri, "old <- 2", open = FALSE)
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)
    expect_false(workspace$index$indexing_status()$done)
    reopened <- lifecycle_parse(workspace, uri, "unsaved <- 3", version = 2L)
    server$process_workspace_documents()
    expect_identical(workspace$documents$get(uri), reopened)
    expect_identical(reopened$content, "unsaved <- 3")
    expect_false(server$parse_task_manager$has_work())
})

test_that("discovery completes across more roots than the worker snapshot cache", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = FALSE, index_batch_size = 1L)
    root <- withr::local_tempdir()
    server <- LifecycleServer$new()
    workspaces <- lapply(seq_len(17L), function(i) {
        directory <- file.path(root, paste0("workspace", i))
        dir.create(directory)
        for (j in seq_len(20L)) writeLines("value <- 1", file.path(directory, paste0(j, ".R")))
        uri <- path_to_uri(directory)
        server$add_workspace(uri)
        workspace <- server$get_workspace(uri)
        server$load_workspace(workspace)
        workspace
    })
    for (i in seq_len(1600L)) {
        server$process_events()
        discovering <- vapply(workspaces, function(workspace) workspace$index$discovering, logical(1L))
        # This fixture measures discovery; ordinary disk indexing is covered
        # separately and need not compete for its deterministic worker slot.
        for (workspace in workspaces[!discovering]) workspace$index$pending <- character()
        if (!any(discovering)) break
    }
    expect_false(any(vapply(workspaces, function(workspace) workspace$index$discovering, logical(1L))))
    expect_equal(vapply(workspaces, function(workspace) workspace$index$files$size(), integer(1L)), rep(20L, 17L))
    expect_equal(server$index_task_manager$discoveries, 17L)
})

test_that("a source load completed after its edge was removed does not cancel its own callback", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = FALSE)
    root <- withr::local_tempdir()
    main <- path_to_uri(file.path(root, "main.R"))
    helper <- path_to_uri(file.path(root, "helper.R"))
    writeLines("source('helper.R')", path_from_uri(main))
    writeLines("helper <- 1", path_from_uri(helper))
    server <- LifecycleServer$new(root)
    workspace <- server$get_workspace(main)
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)
    lifecycle_parse(workspace, main, "source('helper.R')")
    server$refresh_index_documents(workspace, main)
    lifecycle_parse(workspace, main, "main <- 1", version = 2L)
    server$refresh_index_documents(workspace, main)
    server$process_workspace_documents()
    server$parse_task_manager$finish_one()
    expect_false(workspace$documents$has(helper))
    expect_false(helper %in% server$parse_task_manager$cancelled)
    expect_true(workspace$index$indexing_status()$done)
})

test_that("detaching releases worker cursors and catalogs without affecting reattached work", {
    lifecycle_settings(index_persistent_cache = FALSE, index_batch_size = 1L)
    root <- withr::local_tempdir()
    for (i in seq_len(2L)) writeLines("value <- 1", file.path(root, paste0(i, ".R")))
    manager <- lifecycle_manager()
    index <- WorkspaceIndex$new(root)
    index$attach_worker(manager)
    index$discover()
    index$process_batch()
    old_key <- manager$tasks$values()[[1L]]$.__enclos_env__$private$args$key
    manager$finish_one()
    index_worker_batch(old_key, root, list(), list(), bootstrap = TRUE)
    discoveries <- get(".languageserver_workspace_discoveries", globalenv())
    catalogs <- get(".languageserver_workspace_indexes", globalenv())
    expect_true(discoveries$has(old_key))
    expect_true(catalogs$has(old_key))

    index$detach_worker()
    index$attach_worker(manager)
    index$discover()
    index$process_batch()
    new_key <- tail(manager$tasks$values(), 1L)[[1L]]$.__enclos_env__$private$args$key
    expect_false(identical(new_key, old_key))
    manager$finish_one()
    expect_false(discoveries$has(old_key))
    expect_false(catalogs$has(old_key))
    manager$finish_one()
    expect_true(discoveries$has(new_key))
    index_worker_release(old_key)
    expect_true(discoveries$has(new_key))
    index_worker_release(new_key)
})

test_that("closing unsaved package content restores client requests for its disk version", {
    skip_on_cran()
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: closeclientfixture", file.path(root, "DESCRIPTION"))
    path <- file.path(root, "R", "helper.R")
    saved <- c("saved <- function(x) x", "saved(1)", "")
    writeLines(saved, path)
    uri <- path_to_uri(path)
    client <- language_client(root)
    client %>% did_open(path)
    client %>% respond_document_symbol(path)
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(text = "unsaved <- function(y) y\nunsaved(1)\n"))))
    notify(client, "textDocument/didClose", list(textDocument = list(uri = uri)))
    result <- respond_signature(client, path, c(1L, 6L),
        timeout = if (identical(Sys.getenv("R_COVR"), "true")) 60 else 15,
        retry_when = function(result) !length(result$signatures))
    expect_length(result$signatures, 1L)
    expect_match(result$signatures[[1L]]$label, "saved(x)", fixed = TRUE)
})
