workspace_loading_settings <- function(...) {
    values <- list(...)
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer({
        for (name in names(old)) lsp_settings$set(name, old[[name]])
    }, envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

workspace_loading_manager <- function() {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::dict()
    manager$cancelled <- character()
    manager$add_task <- function(id, task) manager$tasks$set(id, task)
    manager$cancel <- function(id) manager$cancelled <- c(manager$cancelled, id)
    manager
}

workspace_loading_result <- function(manager) {
    private <- tail(manager$tasks$values(), 1L)[[1L]]$.__enclos_env__$private
    list(private = private, result = do.call(private$target, private$args))
}

workspace_loading_drain <- function(index, manager) {
    for (i in seq_len(100L)) {
        if (!index$discovering) return(invisible(NULL))
        index$process_batch()
        if (index$worker_running) {
            completed <- workspace_loading_result(manager)
            completed$private$callback(completed$result)
        }
    }
    stop("discovery did not finish")
}

WorkspaceLoadingServer <- R6::R6Class("WorkspaceLoadingServer",
    inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(
        replies = NULL,
        initialize = function(workspace) {
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, workspace)
            self$workspace_cache <- collections::dict()
            self$index_task_manager <- workspace_loading_manager()
            self$parse_task_manager <- workspace_loading_manager()
            self$pending_replies <- collections::dict()
            self$replies <- list()
        },
        deliver = function(response, ...) {
            self$replies[[length(self$replies) + 1L]] <- response
        }
    )
)

test_that("initialized queues discovery and handles requests before scanning files", {
    workspace_loading_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    writeLines("value <- 1", file.path(root, "main.R"))
    workspace <- Workspace$new(root)
    server <- WorkspaceLoadingServer$new(workspace)
    testthat::local_mocked_bindings(
        index_worker_discover = function(...) stop("must run in the worker"),
        .package = "languageserver"
    )
    unlockBinding("load_cache", workspace$index)
    workspace$index$load_cache <- function(...) stop("must not read caches on the event loop")

    expect_no_error(on_initialized(server, list()))
    expect_true(workspace$index$discovering)
    expect_equal(workspace$index$files$size(), 0L)
    expect_equal(server$index_task_manager$tasks$size(), 0L)
    workspace_symbol(server, 1L, list(query = "value"))
    expect_length(server$replies, 1L)
    workspace$index$process_batch()
    expect_equal(server$index_task_manager$tasks$size(), 1L)
    expect_true(workspace$index$worker_running)
})

test_that("background discovery streams caches in bounded batches and honors settings", {
    workspace_loading_settings(index_persistent_cache = TRUE, index_batch_size = 2L,
        index_include = "**/*.R", index_max_files = 10000L)
    root <- withr::local_tempdir()
    paths <- file.path(root, paste0(seq_len(5L), ".R"))
    for (path in paths) writeLines("cached <- function(x) x", path)
    dir.create(file.path(root, "renv"))
    writeLines("ignored <- 1", file.path(root, "renv", "ignored.R"))
    cached <- lapply(paths, function(path) {
        index_shallow_summary(path, stringi::stri_read_lines(path), root)
    })
    cache <- file.path(root, "cache.rds")
    saveRDS(list(version = index_cache_version, root = index_normalize_path(root), summaries = cached), cache)
    index <- WorkspaceIndex$new(root)
    unlockBinding("cache_file", index)
    index$cache_file <- function() cache
    manager <- workspace_loading_manager()
    index$attach_worker(manager)
    index$discover()
    index$process_batch()
    completed <- workspace_loading_result(manager)
    expect_length(completed$result$files, 2L)
    expect_false(completed$result$done)
    completed$private$callback(completed$result)
    expect_equal(index$files$size(), 0L)
    index$process_batch()
    expect_equal(index$files$size(), 2L)
    workspace_loading_drain(index, manager)
    expect_equal(index$files$size(), 5L)
    expect_equal(index$summaries$size(), 5L)
    expect_length(index$pending, 0L)
    expect_false(index$truncated)
    expect_true(all(vapply(index$summaries$values(), function(summary) {
        identical(names(summary$definitions), "cached")
    }, logical(1L))))

    index$.__enclos_env__$private$dirty_since <- -Inf
    repeat {
        index$process_batch()
        if (!index$worker_running) break
        completed <- workspace_loading_result(manager)
        expect_lte(length(completed$private$args$updates), 2L)
        completed$private$callback(completed$result)
        if (!index$cache_dirty) break
    }
    expect_equal(length(readRDS(cache)$summaries), 5L)
    reloaded <- WorkspaceIndex$new(root, cache_path = cache)
    reloaded$discover()
    expect_equal(reloaded$summaries$size(), 5L)
    expect_length(reloaded$pending, 0L)

    lsp_settings$set("index_max_files", 2L)
    index$discover()
    workspace_loading_drain(index, manager)
    expect_equal(index$files$size(), 2L)
    expect_equal(index$summaries$size(), 2L)
    expect_true(index$truncated)
})

test_that("discovery preserves newer buffers, file events and worker lifecycles", {
    workspace_loading_settings(index_persistent_cache = FALSE, index_batch_size = 20L)
    root <- withr::local_tempdir()
    paths <- file.path(root, c("buffer.R", "deleted.R", "changed.R"))
    for (path in paths) writeLines("disk <- 1", path)
    uris <- vapply(paths, path_to_uri, character(1L))
    index <- WorkspaceIndex$new(root)
    manager <- workspace_loading_manager()
    index$attach_worker(manager)
    index$discover()
    index$process_batch()
    completed <- workspace_loading_result(manager)
    index$update_content(uris[[1L]], "unsaved <- 2", cacheable = FALSE)
    unlink(paths[[2L]])
    index$remove(uris[[2L]])
    writeLines("changed <- 3", paths[[3L]])
    index$queue_path(paths[[3L]])
    completed$private$callback(completed$result)
    workspace_loading_drain(index, manager)
    expect_identical(names(index$summaries$get(uris[[1L]])$definitions), "unsaved")
    expect_false(index$files$has(uris[[2L]]))
    expect_false(uris[[1L]] %in% index$pending)
    expect_true(uris[[3L]] %in% index$pending)
    index$process_batch()
    completed <- workspace_loading_result(manager)
    completed$private$callback(completed$result)
    expect_identical(names(index$summaries$get(uris[[3L]])$definitions), "changed")

    index$discover()
    index$process_batch()
    completed <- workspace_loading_result(manager)
    index$detach_worker()
    completed$private$callback(completed$result)
    expect_false(index$discovering)
    expect_false(index$worker_running)
    expect_null(index$.__enclos_env__$private$discovery_result)
    expect_length(manager$cancelled, 1L)
})

test_that("discovery results yield to the event loop between cached files", {
    workspace_loading_settings(index_persistent_cache = FALSE, index_batch_size = 1L)
    root <- withr::local_tempdir()
    paths <- file.path(root, c("first.R", "second.R"))
    for (path in paths) writeLines("disk <- 1", path)
    index <- WorkspaceIndex$new(root)
    manager <- workspace_loading_manager()
    index$attach_worker(manager)
    index$discover()
    index$process_batch()
    private <- tail(manager$tasks$values(), 1L)[[1L]]$.__enclos_env__$private
    private$callback(list(files = lapply(paths, function(path) {
        summary <- index_shallow_summary(path, "disk <- 1", root)
        summary[c("uri", "path", "size", "mtime", "package_root")]
    }), summaries = list(NULL, NULL), done = TRUE, truncated = FALSE))
    index$process_batch()
    expect_equal(index$files$size(), 1L)
    expect_true(index$discovering)
    index$process_batch()
    expect_equal(index$files$size(), 2L)
    expect_true(index$discovering)
    index$process_batch()
    expect_false(index$discovering)
})

test_that("package files are read incrementally and removed workspaces discard queued reads", {
    workspace_loading_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines(c("Package: loadingfixture", "Version: 0.0.1"), file.path(root, "DESCRIPTION"))
    paths <- file.path(root, "R", c("one.R", "two.R", "three.R"))
    for (path in paths) writeLines("value <- 1", path)
    workspace <- Workspace$new(root)
    server <- WorkspaceLoadingServer$new(workspace)
    server$load_workspace(workspace)
    workspace_loading_drain(workspace$index, server$index_task_manager)
    expect_equal(workspace$documents$size(), 0L)
    server$process_workspace_documents(max_documents = 1L)
    expect_equal(workspace$documents$size(), 1L)
    expect_equal(server$parse_task_manager$tasks$size(), 1L)
    server$workspaces$clear()
    expect_no_error(server$process_workspace_documents())
    expect_equal(workspace$documents$size(), 1L)
})

test_that("diagnostics and refactors respect pending discovery", {
    workspace_loading_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "main.R")
    writeLines("value <- 1", path)
    workspace <- Workspace$new(root)
    index <- workspace$index
    index$attach_worker(workspace_loading_manager())
    index$discover()
    unlockBinding("update_path", index)
    index$update_path <- function(...) stop("must not parse files on the event loop")
    expect_false(refactor_index_ready(workspace))
    expect_no_error(workspace$get_diagnostics_globals(path_to_uri(path)))
    expect_true(index$discovering)
})

test_that("cached source edges are revalidated after the discovery worker returns", {
    workspace_loading_settings(index_persistent_cache = TRUE)
    root <- withr::local_tempdir()
    main <- file.path(root, "main.R")
    helper <- file.path(root, "helper.R")
    writeLines("source('helper.R')", main)
    summary <- index_shallow_summary(main, "source('helper.R')", root)
    cache <- file.path(root, "cache.rds")
    saveRDS(list(version = index_cache_version, root = root, summaries = list(summary)), cache)
    index <- WorkspaceIndex$new(root)
    unlockBinding("cache_file", index)
    index$cache_file <- function() cache
    manager <- workspace_loading_manager()
    index$attach_worker(manager)
    index$discover()
    index$process_batch()
    completed <- workspace_loading_result(manager)
    writeLines("helper <- 1", helper)
    completed$private$callback(completed$result)
    workspace_loading_drain(index, manager)
    expect_false(index$summaries$has(summary$uri))
    expect_true(summary$uri %in% index$pending)
})

test_that("lost discovery worker state restarts and superseded scans cannot install results", {
    workspace_loading_settings(index_persistent_cache = FALSE, index_batch_size = 1L)
    root <- withr::local_tempdir()
    for (file in c("one.R", "two.R")) writeLines("value <- 1", file.path(root, file))
    index <- WorkspaceIndex$new(root)
    manager <- workspace_loading_manager()
    index$attach_worker(manager)
    index$discover()
    index$process_batch()
    completed <- workspace_loading_result(manager)
    completed$private$callback(completed$result)
    index$process_batch()
    index$process_batch()
    private <- tail(manager$tasks$values(), 1L)[[1L]]$.__enclos_env__$private
    states <- get(".languageserver_workspace_discoveries", globalenv())
    states$remove(private$args$key)
    completed <- workspace_loading_result(manager)
    expect_true(completed$result$restart)
    completed$private$callback(completed$result)
    workspace_loading_drain(index, manager)
    expect_equal(index$files$size(), 2L)

    index$discover()
    index$process_batch()
    completed <- workspace_loading_result(manager)
    index$discover()
    completed$private$callback(completed$result)
    expect_null(index$.__enclos_env__$private$discovery_result)
    expect_false(index$worker_running)
    workspace_loading_drain(index, manager)
    expect_equal(index$files$size(), 2L)
})

test_that("legacy package loading yields and preserves editor buffers", {
    workspace_loading_settings(index_mode = "off", index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines(c("Package: legacyfixture", "Version: 0.0.1"), file.path(root, "DESCRIPTION"))
    paths <- file.path(root, "R", c("one.R", "two.R"))
    for (path in paths) writeLines("disk <- 1", path)
    workspace <- Workspace$new(root)
    server <- WorkspaceLoadingServer$new(workspace)
    server$load_workspace(workspace)
    expect_equal(workspace$documents$size(), 0L)
    uri <- path_to_uri(paths[[1L]])
    doc <- Document$new(uri, version = 1L, content = "unsaved <- 2")
    doc$did_open()
    workspace$documents$set(uri, doc)
    server$process_workspace_documents(max_documents = 1L)
    expect_identical(workspace$documents$get(uri), doc)
    server$process_workspace_documents()
    expect_equal(workspace$documents$size(), 2L)
    expect_equal(server$parse_task_manager$tasks$size(), 1L)
})
