lifecycle_settings <- function(...) {
    values <- list(...)
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer({
        for (name in names(old)) lsp_settings$set(name, old[[name]])
    }, envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

lifecycle_manager <- function(run = FALSE, discovery_batch_size = NULL) {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::ordered_dict()
    manager$cancelled <- character()
    manager$discoveries <- 0L
    manager$add_task <- function(id, task) {
        if (!is.null(task)) {
            private <- task$.__enclos_env__$private
            if (!is.null(discovery_batch_size) && !is.null(private$args$batch_size)) {
                private$args$batch_size <- discovery_batch_size
            }
            manager$tasks$set(id, task)
        }
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

test_that("folderless parsing publishes diagnostics and drains pending replies", {
    lifecycle_settings(diagnostics = TRUE, diagnostics_cache_ttl = 0)
    root <- withr::local_tempdir()
    withr::local_dir(root)
    writeLines("linters: list(assignment_linter())", file.path(root, ".lintr"))
    path <- file.path(root, "standalone.R")
    content <- c('foo = function() "red"', "foo()", "")
    writeLines(content, path)

    for (uri in c(path_to_uri(path), "untitled:Untitled-794")) {
        server <- LifecycleServer$new()
        on_initialize(server, 1L, list(
            rootUri = NULL, rootPath = NULL, workspaceFolders = NULL,
            capabilities = list()
        ))
        expect_length(server$rootPath, 0L)
        server$messages <- list()
        text_document_did_open(server, list(textDocument = list(
            uri = uri, languageId = "r", version = 1L,
            text = paste(content, collapse = "\n")
        )))
        params <- list(textDocument = list(uri = uri))
        text_document_document_color(server, 2L, params)
        text_document_code_lens(server, 3L, params)
        queues <- server$pending_replies$get(uri)
        expect_equal(queues[["textDocument/documentColor"]]$size(), 1L)
        expect_equal(queues[["textDocument/codeLens"]]$size(), 1L)
        expect_length(server$messages, 0L)

        server$parse_task_manager$finish_one()
        expect_true(server$diagnostics_task_manager$tasks$has(uri))
        expect_identical(server$process_pending_replies(time_budget_ms = Inf), 2L)
        expect_length(server$messages, 2L)
        expect_equal(server$messages[[1L]]$id, 2L)
        expect_equal(server$messages[[1L]]$result[[1L]]$color,
            list(red = 1, green = 0, blue = 0, alpha = 1))
        expect_equal(server$messages[[2L]]$id, 3L)
        expect_length(server$messages[[2L]]$result, 1L)
        expect_equal(queues[["textDocument/documentColor"]]$size(), 0L)
        expect_equal(queues[["textDocument/codeLens"]]$size(), 0L)

        server$diagnostics_task_manager$finish_one()
        notification <- server$messages[[3L]]
        expect_equal(notification$method, "textDocument/publishDiagnostics")
        expect_equal(notification$params$uri, uri)
        expect_equal(notification$params$version, 1L)
        expect_length(notification$params$diagnostics, 1L)
        expect_equal(notification$params$diagnostics[[1L]]$code, "assignment_linter")
    }
})

test_that("diagnostics only exclude temporary files in non-temporary workspaces", {
    lifecycle_settings(diagnostics = TRUE, diagnostics_cache_ttl = 0)
    outside <- file.path(dirname(dirname(tempdir())), "languageserver-nontemp")
    temp_uri <- path_to_uri(file.path(tempdir(), "standalone.R"))
    outside_uri <- path_to_uri(file.path(outside, "standalone.R"))
    for (root in list(NULL, character(), tempdir(), outside)) {
        for (uri in c(temp_uri, outside_uri)) {
            server <- LifecycleServer$new()
            server$rootPath <- root
            document <- Document$new(uri, version = 1L, content = "x = 1")
            schedule_diagnostics(server, uri, document)
            expected <- !(identical(root, outside) && identical(uri, temp_uri))
            expect_identical(server$diagnostics_task_manager$tasks$has(uri), expected)
        }
    }
})

test_that("a failed package parse retries after task cleanup and restores readiness", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = TRUE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: retryfixture", file.path(root, "DESCRIPTION"))
    uri <- path_to_uri(file.path(root, "R", "helper.R"))
    main <- path_to_uri(file.path(root, "R", "main.R"))
    writeLines("helper <- function() 1", path_from_uri(uri))
    writeLines("main <- function() helper()", path_from_uri(main))
    server <- LifecycleServer$new(root)
    workspace <- server$get_workspace(uri)
    lifecycle_parse(workspace, main, "main <- function() helper()")
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)

    manager <- TaskManager$new("parse retry", use_session = TRUE,
        min_idle_sessions = 0, max_running_tasks = 1, cpu_load = 1)
    withr::defer(manager$stop())
    server$parse_task_manager <- manager
    state <- new.env(parent = emptyenv())
    state$status <- "idle"
    state$attempts <- 0L
    state$killed <- FALSE
    session <- list(
        call = function(target, args) {
            state$status <- "busy"
            state$attempts <- state$attempts + 1L
            state$result <- function() do.call(target, args)
        },
        get_state = function() state$status,
        read = function() {
            if (state$status != "busy") return(NULL)
            state$status <- "idle"
            if (state$attempts == 1L) list(code = 500L, error = simpleError("parse worker failed"))
            else list(code = 200L, result = state$result())
        },
        kill = function(...) state$killed <- TRUE,
        close = function(...) NULL
    )
    manager$.__enclos_env__$private$sessions <- list(session)
    server$process_workspace_documents()
    expect_true(workspace$index$document_pending(uri))
    expect_false(refactor_index_ready(workspace))
    manager$run_tasks()
    manager$check_tasks()
    expect_false(manager$has_work())
    expect_false(state$killed)
    expect_false(workspace$index$indexing_status()$done)
    server$refresh_workspace_diagnostics(now = Inf)
    expect_false(server$diagnostics_task_manager$has_work())

    # The normal event loop dispatches the retry only after check_tasks removed
    # the failed entry. This also exercises real TaskManager callback cleanup.
    server$process_events()
    manager$check_tasks()
    lifecycle_drain_index(server, workspace)
    expect_identical(state$attempts, 2L)
    expect_false(state$killed)
    expect_false(manager$has_work())
    expect_true(workspace$index$indexing_status()$done)
    expect_true(refactor_index_ready(workspace))
    expect_identical(names(workspace$documents$get(uri)$parse_data$definitions), "helper")
    server$refresh_workspace_diagnostics(now = Inf)
    expect_true(server$diagnostics_task_manager$tasks$has(main))
})

test_that("repeated parse failures settle without replacing an unsaved buffer", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = FALSE)
    root <- withr::local_tempdir()
    uri <- path_to_uri(file.path(root, "main.R"))
    writeLines("saved <- 1", path_from_uri(uri))
    server <- LifecycleServer$new(root)
    workspace <- server$get_workspace(uri)
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)
    text_document_did_open(server, list(textDocument = list(
        uri = uri, languageId = "r", version = 1L, text = "unsaved <- 2")))
    document <- workspace$documents$get(uri)
    manager <- server$parse_task_manager
    manager$tasks$pop(uri)$fail(simpleError("worker failed"))
    server$process_parse_retries()
    expect_identical(manager$tasks$get(uri)$.__enclos_env__$private$args$content, "unsaved <- 2")
    manager$tasks$pop(uri)$fail(simpleError("retry failed"))
    server$process_parse_retries()
    expect_false(manager$has_work())
    expect_true(workspace$index$indexing_status()$done)
    expect_false(workspace$index$document_pending(uri))
    expect_false(workspace$index$.__enclos_env__$private$awaiting_parses$has(uri))
    expect_true(workspace$index$parse_failed(uri))
    expect_false(refactor_index_ready(workspace))
    expect_identical(document$content, "unsaved <- 2")
    expect_null(document$parse_data)

    # Lookups and a new discovery pass must not quietly index disk content.
    workspace$index$queue_path(path_from_uri(uri), invalidate = FALSE)
    previous <- workspace$index$summaries$get(uri, NULL)
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)
    server$process_workspace_documents()
    expect_true(workspace$index$indexing_status()$done)
    expect_true(workspace$index$parse_failed(uri))
    expect_identical(workspace$index$summaries$get(uri, NULL), previous)
    expect_false(workspace$index$has_definition("saved"))
    expect_identical(workspace$documents$get(uri), document)
    expect_false(manager$has_work())

    text_document_did_change(server, list(textDocument = list(uri = uri, version = 2L),
            contentChanges = list(list(text = "new_unsaved <- 3"))))
    expect_false(workspace$index$indexing_status()$done)
    manager$finish_one()
    lifecycle_drain_index(server, workspace)
    expect_true(workspace$index$indexing_status()$done)
    expect_false(workspace$index$parse_failed(uri))
    expect_true(refactor_index_ready(workspace))
    expect_identical(document$content, "new_unsaved <- 3")
    expect_identical(names(workspace$index$summaries$get(uri)$definitions), "new_unsaved")
    expect_false(workspace$index$summaries$get(uri)$cacheable)
})

test_that("failed closed package parses recover after a file change", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = TRUE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: failedfixture", file.path(root, "DESCRIPTION"))
    uri <- path_to_uri(file.path(root, "R", "helper.R"))
    main <- path_to_uri(file.path(root, "R", "main.R"))
    writeLines("helper <- 1", path_from_uri(uri))
    server <- LifecycleServer$new(root)
    workspace <- server$get_workspace(uri)
    lifecycle_parse(workspace, main, "main <- function() helper")
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)
    server$process_workspace_documents()
    manager <- server$parse_task_manager
    manager$tasks$pop(uri)$fail(simpleError("worker failed"))
    server$process_parse_retries()
    manager$tasks$pop(uri)$fail(simpleError("retry failed"))
    expect_true(workspace$index$indexing_status()$done)
    expect_false(refactor_index_ready(workspace))
    server$refresh_workspace_diagnostics(now = Inf)
    expect_true(server$diagnostics_task_manager$tasks$has(main))
    server$refresh_index_documents(workspace)
    workspace$index$on_update(uri)
    server$process_workspace_documents()
    server$process_parse_retries()
    expect_false(manager$has_work())

    writeLines("helper_changed <- 2", path_from_uri(uri))
    workspace_did_change_watched_files(server, list(changes = list(list(uri = uri, type = 2L))))
    lifecycle_drain_index(server, workspace)
    server$process_workspace_documents()
    manager$finish_one()
    lifecycle_drain_index(server, workspace)
    expect_true(workspace$index$indexing_status()$done)
    expect_false(workspace$index$parse_failed(uri))
    expect_true(refactor_index_ready(workspace))
    expect_identical(workspace$documents$get(uri)$content, "helper_changed <- 2")
})

test_that("stale parse failures and deferred retries leave newer work intact", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = FALSE)
    changes <- c("version", "content", "document", "task", "index", "workspace", "close", "detach")
    for (retry in c(TRUE, FALSE)) for (change in changes) {
        root <- withr::local_tempdir()
        uri <- path_to_uri(file.path(root, "main.R"))
        server <- LifecycleServer$new(root)
        workspace <- server$get_workspace(uri)
        document <- Document$new(uri, version = 1L, content = "old <- 1")
        document$did_open()
        workspace$documents$set(uri, document)
        workspace$index$await_parse(uri)
        task <- parse_task(server, uri, document, retry = retry)
        if (retry) task$fail(simpleError("first failure"))
        if (change == "version") document$set_content(2L, "old <- 1")
        if (change == "content") document$set_content(1L, "new <- 2")
        if (change == "document") workspace$documents$set(uri, Document$new(uri, version = 1L, content = "old <- 1"))
        if (change == "task") server$text_sync(uri, document, parse = TRUE)
        if (change == "index") workspace$index <- WorkspaceIndex$new(root)
        if (change == "workspace") server$remove_workspace(path_to_uri(root))
        if (change == "close") document$did_close()
        if (change == "detach") {
            workspace$index$attach_worker(server$index_task_manager)
            workspace$index$detach_worker()
        }
        before <- server$parse_task_manager$tasks$values()
        status <- workspace$index$indexing_status()
        task$fail(simpleError("late failure"))
        server$process_parse_retries()
        expect_identical(server$parse_task_manager$tasks$values(), before, info = change)
        expect_identical(workspace$index$indexing_status(), status, info = change)
        expect_false(workspace$index$parse_failed(uri), info = change)
    }
})

test_that("closed package files load preindexed source dependencies outside R", {
    lifecycle_settings(index_persistent_cache = FALSE, diagnostics = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    dir.create(file.path(root, "scripts"))
    writeLines("Package: sourcedfixture", file.path(root, "DESCRIPTION"))
    main <- path_to_uri(file.path(root, "R", "main.R"))
    helper <- path_to_uri(file.path(root, "scripts", "helper.R"))
    nested <- path_to_uri(file.path(root, "scripts", "nested.R"))
    writeLines(c("source('scripts/helper.R')", "main <- function() helper()"), path_from_uri(main))
    writeLines(c("source('nested.R')", "helper <- function(value = 2) value", "helper(1)"), path_from_uri(helper))
    writeLines("nested <- function(flag = TRUE) flag", path_from_uri(nested))
    server <- LifecycleServer$new(root)
    workspace <- server$get_workspace(main)
    # Finish shallow indexing before loading the package document, without
    # an on_update callback that could queue its dependencies for us.
    workspace$index$attach_worker(server$index_task_manager)
    workspace$index$discover()
    lifecycle_drain_index(server, workspace)
    expect_identical(workspace$index$source_edges$get(main), helper)
    expect_identical(workspace$index$source_edges$get(helper), nested)
    topology <- workspace$index$topology_revision

    server$load_index_document(workspace, main)
    server$parse_task_manager$finish_one()
    expect_identical(workspace$index$topology_revision, topology)
    expect_true(workspace$index$document_pending(helper))
    for (i in seq_len(3L)) {
        server$process_workspace_documents()
        while (server$parse_task_manager$has_work()) server$parse_task_manager$finish_one()
    }
    expect_true(workspace$documents$has(helper))
    expect_true(workspace$documents$has(nested))
    helper_document <- workspace$documents$get(helper, NULL)
    if (!is.null(helper_document)) {
        result <- signature_reply(1L, helper, workspace, helper_document, list(row = 2L, col = 7L))$result
        expect_match(result$signatures[[1L]]$label, "helper(value = 2)", fixed = TRUE)
    }
    expect_true(workspace$index$indexing_status()$done)

    # A cached full package parse must also restore missing source documents
    # when discovery finishes without changing any source edges.
    generation <- workspace$documents$get(main)$parse_generation
    workspace$documents$remove(helper)
    workspace$documents$remove(nested)
    server$load_workspace(workspace)
    lifecycle_drain_index(server, workspace)
    for (i in seq_len(3L)) {
        server$process_workspace_documents()
        while (server$parse_task_manager$has_work()) server$parse_task_manager$finish_one()
    }
    expect_identical(workspace$documents$get(main)$parse_generation, generation)
    expect_true(workspace$documents$has(helper))
    expect_true(workspace$documents$has(nested))

    writeLines("main <- function() 1", path_from_uri(main))
    document <- workspace$documents$get(main)
    document$set_content(NULL, "main <- function() 1")
    parse_callback(server, main, NULL, parse_document(main, document$content))
    expect_false(workspace$documents$has(helper))
    expect_false(workspace$documents$has(nested))
})

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
    lifecycle_settings(index_mode = "full", index_persistent_cache = FALSE, diagnostics = FALSE, index_batch_size = 1L)
    root <- withr::local_tempdir()
    server <- LifecycleServer$new()
    server$index_task_manager <- lifecycle_manager(run = TRUE, discovery_batch_size = 1L)
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
    keys <- vapply(workspaces, function(workspace) workspace$index$.__enclos_env__$private$worker_key, "")
    withr::defer(for (key in keys) index_worker_release(key))
    for (i in seq_along(workspaces)) server$process_events()
    discoveries <- get(".languageserver_workspace_discoveries", globalenv())
    expect_true(all(vapply(keys, discoveries$has, logical(1L))))
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
    # Retain unfinished cursors independently of the server acceptance budget.
    manager <- lifecycle_manager(discovery_batch_size = 1L)
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
