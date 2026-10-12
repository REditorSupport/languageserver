test_that("source topology invalidates only affected workspace scopes", {
    root <- withr::local_tempdir()
    workspace <- Workspace$new(root)
    paths <- file.path(root, c("main.R", "first.R", "second.R", "other.R"))
    uris <- vapply(paths, path_to_uri, character(1L))
    for (i in seq_along(paths)) {
        writeLines("value <- 1", paths[[i]])
        workspace$documents$set(uris[[i]], Document$new(uris[[i]]))
    }
    index <- workspace$index
    index$update_content(uris[[1L]], "source('first.R')")
    namespace <- workspace$get_namespace(WORKSPACE, uri = uris[[1L]])
    generation <- index$scope_generation(uris[[1L]])
    references <- index$scope_generation(uris[[2L]], references = TRUE)
    index$update_content(uris[[4L]], "unrelated <- 2")
    expect_identical(index$scope_generation(uris[[1L]]), generation)
    expect_identical(index$scope_generation(uris[[2L]], references = TRUE), references)
    expect_identical(workspace$get_namespace(WORKSPACE, uri = uris[[1L]]), namespace)

    index$update_content(uris[[2L]], "source('second.R')")
    expect_setequal(index$source_closure(uris[[1L]]), uris[1:3])
    expect_setequal(index$dependent_closure(uris[[3L]]), uris[1:3])
    index$remove(uris[[2L]])
    expect_setequal(index$source_closure(uris[[1L]]), uris[1:2])
    expect_equal(index$dependent_closure(uris[[3L]]), uris[[3L]])
})

test_that("workspace query catalog preserves order and per-document fuzzy behavior", {
    root <- withr::local_tempdir()
    index <- WorkspaceIndex$new(root)
    summaries <- lapply(seq_len(2L), function(i) {
        symbols <- if (i == 1L) c("alpha", "a.b", "abc") else
            c("alpha", "a.b", "abc", "caf\u00e9")
        list(uri = path_to_uri(file.path(root, paste0(i, ".R"))),
            sources = character(), definitions = stats::setNames(
                lapply(symbols, function(name) list(name = name, type = "double", range = list())), symbols))
    })
    for (summary in summaries) index$set_summary(summary)
    oracle <- function(pattern) {
        unlist(lapply(summaries, function(summary) {
            matches <- names(summary$definitions)[fuzzy_find(names(summary$definitions), pattern)]
            lapply(unname(summary$definitions[matches]), function(definition) c(uri = summary$uri, definition))
        }), recursive = FALSE)
    }
    for (pattern in c("", "alpha", "a.b", "a[bc]", "caf\u00e9", "absent")) {
        expect_identical(index$definitions_for_query(pattern), oracle(pattern))
    }
    replacement <- summaries[[1L]]
    replacement$definitions <- list(replaced = list(name = "replaced", type = "double", range = list()))
    index$set_summary(replacement)
    expect_equal(index$definitions_for_query("replaced")[[1L]]$name, "replaced")
    index$remove(replacement$uri)
    expect_length(index$definitions_for_query("replaced"), 0L)
})

index_test_manager <- function() {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::dict()
    manager$cancelled <- character()
    manager$add_task <- function(id, task) manager$tasks$set(id, task)
    manager$cancel <- function(id) manager$cancelled <- c(manager$cancelled, id)
    manager
}

index_test_result <- function(manager) {
    task <- tail(manager$tasks$values(), 1L)[[1L]]
    private <- task$.__enclos_env__$private
    list(private = private, result = do.call(index_worker_batch, private$args))
}

test_that("background disk summaries reject stale buffers, files and lifecycles", {
    old_cache <- lsp_settings$get("index_persistent_cache")
    withr::defer(lsp_settings$set("index_persistent_cache", old_cache))
    lsp_settings$set("index_persistent_cache", FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "main.R")
    writeLines("value <- 1", path)
    uri <- path_to_uri(path)
    manager <- index_test_manager()
    index <- WorkspaceIndex$new(root)
    index$attach_worker(manager)
    index$queue_path(path)
    expect_length(index$process_batch(), 0L)
    expect_true(index$worker_running)
    completed <- index_test_result(manager)
    index$await_parse(uri, cacheable = FALSE)
    completed$private$callback(completed$result)
    expect_false(index$summaries$has(uri))
    expect_false(index$worker_running)

    parsed <- parse_document(uri, "value <- 7")
    index$update_content(uri, "value <- 7", parse_data = parsed)
    expect_false(index$summaries$get(uri)$cacheable)
    index$queue_path(path, force = TRUE)
    index$process_batch()
    completed <- index_test_result(manager)
    writeLines("value <- 200", path)
    completed$private$callback(completed$result)
    expect_true(uri %in% index$pending)
    expect_equal(index$summaries$get(uri)$content_hash, get_content_hash("value <- 7"))

    index$process_batch()
    completed <- index_test_result(manager)
    index$detach_worker()
    completed$private$callback(completed$result)
    expect_null(index$on_update)
    expect_length(manager$cancelled, 1L)
    expect_equal(index$summaries$get(uri)$content_hash, get_content_hash("value <- 7"))
})

test_that("worker persistence applies deltas and refuses missing or stale state", {
    root <- withr::local_tempdir()
    path <- file.path(root, "main.R")
    writeLines("value <- 1", path)
    summary <- index_shallow_summary(path, "value <- 1", root)
    summary$cacheable <- TRUE
    key <- tempfile("index-test-")
    withr::defer({
        states <- get0(".languageserver_workspace_indexes", globalenv(), inherits = FALSE)
        if (is.environment(states) && is.function(states$has) && states$has(key)) states$remove(key)
    })
    cache_file <- file.path(root, "index.rds")
    result <- index_worker_batch(key, root, stats::setNames(list(summary), summary$uri),
        list(), bootstrap = TRUE, cache_file = cache_file, revision = 1L)
    withr::defer(unlink(result$temp_file))
    expect_false(result$sync_needed)
    expect_identical(readRDS(result$temp_file)$summaries[[1L]]$definitions, summary$definitions)
    unlink(result$temp_file)
    result <- index_worker_batch(key, root, stats::setNames(list(NULL), summary$uri),
        list(), cache_file = cache_file, revision = 2L)
    expect_length(readRDS(result$temp_file)$summaries, 0L)
    unlink(result$temp_file)
    missing <- tempfile("index-test-missing-")
    result <- index_worker_batch(missing, root, list(), list(), cache_file = cache_file)
    expect_true(result$sync_needed)
    expect_null(result$temp_file)
    states <- get(".languageserver_workspace_indexes", globalenv())
    states$remove(missing)

    old_cache <- lsp_settings$get("index_persistent_cache")
    withr::defer(lsp_settings$set("index_persistent_cache", old_cache))
    lsp_settings$set("index_persistent_cache", TRUE)
    index <- WorkspaceIndex$new(root)
    unlockBinding("cache_file", index)
    index$cache_file <- function() cache_file
    manager <- index_test_manager()
    index$attach_worker(manager)
    saveRDS(list(previous = TRUE), cache_file)
    index$update_content(summary$uri, "value <- 3")
    index$.__enclos_env__$private$dirty_since <- -Inf
    index$process_batch()
    completed <- index_test_result(manager)
    index$update_content(summary$uri, "value <- 4")
    completed$private$callback(completed$result)
    expect_identical(readRDS(cache_file), list(previous = TRUE))
    expect_false(file.exists(completed$result$temp_file))
    index$process_batch()
    completed <- index_test_result(manager)
    completed$private$callback(completed$result)
    expect_false(index$cache_dirty)
    expect_equal(readRDS(cache_file)$summaries[[1L]]$content_hash, get_content_hash("value <- 4"))
})

test_that("opening and saving buffers await the full parse without shallow duplication", {
    root <- withr::local_tempdir()
    path <- file.path(root, "main.R")
    writeLines("value <- 1", path)
    uri <- path_to_uri(path)
    workspace <- Workspace$new(root)
    workspace$index$attach_worker(index_test_manager())
    doc <- Document$new(uri, version = 1L, content = "value <- 1")
    doc$did_open()
    workspace$documents$set(uri, doc)
    server <- list(refresh_index_documents = function(...) NULL)
    testthat::local_mocked_bindings(
        index_shallow_summary = function(...) stop("must use the pending full parse"),
        .package = "languageserver"
    )
    expect_no_error(update_document_index(server, workspace, uri, doc$content))
    expect_false(workspace$index$summaries$has(uri))
    expect_false(uri %in% workspace$index$pending)
})

test_that("awaited buffers retain their gate when persistent cacheability changes", {
    root <- withr::local_tempdir()
    path <- file.path(root, "main.R")
    writeLines("value <- 1", path)
    uri <- path_to_uri(path)
    index <- WorkspaceIndex$new(root)
    index$update_content(uri, "value <- 1", cacheable = TRUE)
    index$await_parse(uri, cacheable = FALSE)
    index$queue_path(path)
    expect_false(uri %in% index$pending)
    expect_false(index$summaries$get(uri)$cacheable)
    index$queue_path(path, force = TRUE)
    expect_true(uri %in% index$pending)
})

test_that("closing a package buffer discards its parse without cancelling the disk refresh", {
    withr::local_envvar(R_COVR = "false")
    old_cache <- lsp_settings$get("index_persistent_cache")
    withr::defer(lsp_settings$set("index_persistent_cache", old_cache))
    lsp_settings$set("index_persistent_cache", FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines(c("Package: closefixture", "Version: 0.0.1"), file.path(root, "DESCRIPTION"))
    path <- file.path(root, "R", "main.R")
    saved <- "saved <- function(x) x"
    writeLines(saved, path)
    uri <- path_to_uri(path)
    workspace <- Workspace$new(root)
    document <- Document$new(uri, version = 1L, content = saved)
    document$did_open()
    workspace$documents$set(uri, document)
    accepted <- parse_document(uri, saved)
    accepted$version <- 1L
    workspace$update_parse_data(uri, accepted)
    index_manager <- index_test_manager()
    workspace$index$attach_worker(index_manager)
    document$set_content(2L, "unsaved <- function(y) y")
    workspace$index$await_parse(uri, cacheable = FALSE)

    parse_manager <- TaskManager$new("close fixture", use_session = TRUE,
        min_idle_sessions = 0L, max_running_tasks = 1L, cancellation_grace = 10)
    withr::defer(parse_manager$stop())
    server <- list(get_workspace = function(...) workspace,
        refresh_index_documents = function(...) NULL,
        prune_index_documents = function(...) NULL,
        parse_task_manager = parse_manager,
        resolve_task_manager = list(add_task = function(...) NULL),
        pending_replies = collections::dict(), request_handlers = list(),
        deliver = function(...) NULL)
    server$pending_replies$set(uri, list())
    state <- new.env(parent = emptyenv())
    state$status <- "idle"
    state$ready <- FALSE
    pending <- parse_document(uri, document$content)
    session <- list(call = function(...) state$status <- "busy",
        get_state = function() state$status,
        read = function() {
            if (!state$ready || state$status != "busy") return(NULL)
            state$status <- "idle"
            list(code = 200L, error = NULL, result = pending)
        },
        close = function(...) NULL, kill = function(...) state$status <- "finished")
    parse_manager$.__enclos_env__$private$sessions <- list(session)
    parse_manager$add_task(uri, parse_task(server, uri, document))
    parse_manager$run_tasks()
    expect_true(parse_manager$has_work())

    text_document_did_close(server, list(textDocument = list(uri = uri)))
    expect_false(document$is_open)
    expect_identical(workspace$documents$get(uri), document)
    expect_true(uri %in% workspace$index$pending)
    state$ready <- TRUE
    parse_manager$check_tasks()
    expect_false(parse_manager$has_work())
    expect_identical(document$parse_data, accepted)
    expect_true(uri %in% workspace$index$pending)

    workspace$index$process_batch()
    completed <- index_test_result(index_manager)
    completed$private$callback(completed$result)
    summary <- workspace$index$summaries$get(uri)
    expect_identical(names(summary$definitions), "saved")
    expect_identical(summary$content_hash, get_content_hash(saved))
    expect_true(summary$cacheable)
    expect_false(uri %in% workspace$index$pending)
})

test_that("unstable file reads are retried without retrying excluded or missing files", {
    root <- withr::local_tempdir()
    path <- file.path(root, "main.R")
    writeLines("value <- 1", path)
    index <- WorkspaceIndex$new(root)
    manager <- index_test_manager()
    index$attach_worker(manager)
    index$queue_path(path)
    index$process_batch()
    task <- tail(manager$tasks$values(), 1L)[[1L]]
    private <- task$.__enclos_env__$private
    private$callback(list(summaries = list(NULL), retry = TRUE, processed = 1L,
            temp_file = NULL, sync_needed = FALSE))
    expect_true(path_to_uri(path) %in% index$pending)
    index$process_batch()
    task <- tail(manager$tasks$values(), 1L)[[1L]]
    private <- task$.__enclos_env__$private
    unlink(path)
    private$callback(list(summaries = list(NULL), retry = TRUE, processed = 1L,
            temp_file = NULL, sync_needed = FALSE))
    expect_length(index$pending, 0L)
})

test_that("refactors wait for outstanding background index results", {
    workspace <- Workspace$new(withr::local_tempdir())
    workspace$index$worker_running <- TRUE
    expect_false(refactor_index_ready(workspace))
    workspace$index$worker_running <- FALSE
    expect_true(refactor_index_ready(workspace))
})


test_that("persistent worker catalogs evict old workspace states within a bound", {
    root <- withr::local_tempdir()
    keys <- paste0(tempfile("bounded-index-test-"), seq_len(17L))
    for (key in keys) index_worker_batch(key, root, list(), list(), bootstrap = TRUE)
    states <- get(".languageserver_workspace_indexes", globalenv())
    expect_lte(states$size(), 16L)
    expect_false(states$has(keys[[1L]]))
    result <- index_worker_batch(keys[[1L]], root, list(), list(),
        cache_file = file.path(root, "cache.rds"))
    expect_true(result$sync_needed)
    expect_null(result$temp_file)
    for (key in keys) if (states$has(key)) states$remove(key)
})
