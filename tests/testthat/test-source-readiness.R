source_read_settings <- function(...) {
    values <- utils::modifyList(list(index_mode = "auto", index_persistent_cache = FALSE, diagnostics = FALSE), list(...))
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer(for (name in names(old)) lsp_settings$set(name, old[[name]]), envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

source_read_manager <- function() {
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

SourceReadServer <- R6::R6Class("SourceReadServer", inherit = LanguageServer,
    private = list(finalize = function() NULL), public = list(
        replies = NULL,
        initialize = function(root) {
            workspace <- testthat::with_mocked_bindings(Workspace$new(root),
                workspace_startup_packages = function() startup_packages,
                workspace_startup_s3 = provider_startup_s3, .package = "languageserver")
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, workspace)
            self$workspace_cache <- collections::dict()
            self$index_task_manager <- source_read_manager()
            self$parse_task_manager <- source_read_manager()
            self$resolve_task_manager <- source_read_manager()
            self$pending_replies <- collections::dict()
            self$request_handlers <- list(`textDocument/completion` = text_document_completion,
                `textDocument/hover` = text_document_hover, `textDocument/signatureHelp` = text_document_signature_help)
            self$replies <- list()
            self$load_workspace(workspace)
        },
        deliver = function(response, ...) {
            if (!is.null(response$id)) self$replies[[length(self$replies) + 1L]] <- response
        }
    ))

source_read_open <- function(server, path, content = stringi::stri_read_lines(path)) {
    uri <- path_to_uri(path)
    document <- Document$new(uri, "r", 1L, content)
    document$did_open()
    server$get_workspace(uri)$documents$set(uri, document)
    server$text_sync(uri, document, parse = TRUE)
    document
}

source_read_step <- function(server) {
    for (workspace in server$workspaces$values()) workspace$index$process_batch()
    server$process_workspace_documents(time_budget_ms = Inf)
    server$index_task_manager$finish()
    server$parse_task_manager$finish()
    server$process_context_replies()
    for (workspace in server$workspaces$values()) {
        if (isTRUE(workspace$index$prune_pending) && workspace$index$complete()) server$prune_index_documents(workspace)
    }
    server$process_pending_replies(time_budget_ms = Inf)
}

source_read_drain <- function(server) {
    for (i in seq_len(100L)) {
        source_read_step(server)
        if (!server$index_task_manager$tasks$size() && !server$parse_task_manager$tasks$size() &&
                all(vapply(server$workspaces$values(), function(workspace) workspace$index$complete(), logical(1L)))) {
            return(invisible(NULL))
        }
    }
    stop("source reads did not settle")
}

source_read_requests <- function(server, document) {
    params <- function(row, col) list(textDocument = list(uri = document$uri), position = position(row, col))
    text_document_completion(server, 1L, params(2L, 3L))
    text_document_hover(server, 2L, params(1L, 2L))
    text_document_signature_help(server, 3L, params(1L, 6L))
}

test_that("failed source reads release all providers and retry after file events", {
    source_read_settings()
    root <- index_normalize_path(withr::local_tempdir())
    helper <- file.path(root, "helper.R")
    main <- file.path(root, "main.R")
    writeLines("helper_fn <- function(value) value", helper)
    writeLines(c("source('helper.R')", "print()", "pri"), main)
    original <- stringi::stri_read_lines
    failing <- TRUE
    reads <- 0L
    testthat::local_mocked_bindings(stri_read_lines = function(path, ...) {
        if (identical(index_normalize_path(path), helper)) {
            reads <<- reads + 1L
            if (failing) stop("source is unreadable")
        }
        original(path, ...)
    }, .package = "stringi")
    server <- SourceReadServer$new(root)
    document <- source_read_open(server, main)
    server$parse_task_manager$finish()
    source_read_requests(server, document)
    expect_length(server$replies, 0L)
    source_read_drain(server)
    workspace <- server$get_workspace(document$uri)
    helper_uri <- path_to_uri(helper)
    expect_true(workspace$index$parse_failed(helper_uri))
    expect_false(source_context_waiting(workspace, document))
    expect_length(server$replies, 3L)
    expect_true(workspace$index$complete())
    expect_false(workspace$documents$has(helper_uri))
    count <- reads
    for (i in seq_len(5L)) {
        server$refresh_index_documents(workspace)
        source_read_step(server)
    }
    expect_identical(reads, count)
    failing <- FALSE
    workspace_did_change_watched_files(server, list(changes = list(list(uri = helper_uri, type = 2L))))
    source_read_drain(server)
    expect_false(workspace$index$parse_failed(helper_uri))
    expect_true(workspace$documents$has(helper_uri))
    expect_true(workspace$index$has_definition("helper_fn", uri = document$uri))
})

test_that("disappearing source targets settle and recover when recreated", {
    source_read_settings()
    root <- index_normalize_path(withr::local_tempdir())
    helper <- file.path(root, "helper.R")
    main <- file.path(root, "main.R")
    writeLines("helper_fn <- function(value) value", helper)
    writeLines(c("source('helper.R')", "print()", "pri"), main)
    server <- SourceReadServer$new(root)
    document <- source_read_open(server, main)
    server$parse_task_manager$finish()
    unlink(helper)
    source_read_requests(server, document)
    source_read_drain(server)
    workspace <- server$get_workspace(document$uri)
    expect_length(server$replies, 3L)
    expect_false(source_context_waiting(workspace, document))
    expect_identical(workspace$index$indexing_status()$failed, 0L)
    writeLines("helper_fn <- function(value) value", helper)
    workspace_did_change_watched_files(server, list(changes = list(list(uri = path_to_uri(helper), type = 1L))))
    source_read_drain(server)
    expect_true(workspace$documents$has(path_to_uri(helper)))
})

test_that("pending open source buffers still wait when their disk files disappear", {
    source_read_settings()
    root <- index_normalize_path(withr::local_tempdir())
    helper <- file.path(root, "helper.R")
    main <- file.path(root, "main.R")
    writeLines("helper_fn <- function(value) value", helper)
    writeLines(c("source('helper.R')", "helper_fn()"), main)
    server <- SourceReadServer$new(root)
    document <- source_read_open(server, main)
    source_read_drain(server)
    workspace <- server$get_workspace(document$uri)
    peer <- workspace$documents$get(path_to_uri(helper))
    peer$did_open()
    peer$set_content(2L, "helper_fn <- function(changed) changed")
    server$text_sync(peer$uri, peer, parse = TRUE)
    unlink(helper)
    expect_true(source_context_waiting(workspace, document))
    text_document_signature_help(server, 1L, list(textDocument = list(uri = document$uri), position = position(1L, 10L)))
    expect_length(server$replies, 0L)
    source_read_drain(server)
    expect_match(server$replies[[1L]]$result$signatures[[1L]]$label, "helper_fn(changed", fixed = TRUE)
    expect_false(workspace$index$parse_failed(peer$uri))
})

test_that("quoted namespace text does not bypass signature source readiness", {
    source_read_settings()
    root <- index_normalize_path(withr::local_tempdir())
    helper <- file.path(root, "helper.R")
    main <- file.path(root, "main.R")
    writeLines("helper_fn <- function(old) old", helper)
    call <- 'helper_fn("base::fun")'
    writeLines(c("source('helper.R')", call), main)
    server <- SourceReadServer$new(root)
    document <- source_read_open(server, main)
    source_read_drain(server)
    workspace <- server$get_workspace(document$uri)
    peer <- workspace$documents$get(path_to_uri(helper))
    peer$did_open()
    peer$set_content(2L, "helper_fn <- function(changed) changed")
    server$text_sync(peer$uri, peer, parse = TRUE)

    text_document_signature_help(server, 1L, list(textDocument = list(uri = document$uri),
        position = position(1L, nchar(call) - 2L)))
    expect_length(server$replies, 0L)
    queue <- server$pending_replies$get(document$uri)[["textDocument/signatureHelp"]]
    expect_identical(queue$size(), 1L)
    for (i in seq_len(3L)) {
        server$queue_pending_replies(document$uri)
        expect_identical(server$process_pending_replies(max_replies = 3L, time_budget_ms = Inf), 0L)
        expect_identical(queue$size(), 1L)
        expect_identical(queue$peek()$id, 1L)
        expect_length(server$replies, 0L)
    }
    source_read_drain(server)
    expect_identical(queue$size(), 0L)
    expect_length(server$replies, 1L)
    expect_match(server$replies[[1L]]$result$signatures[[1L]]$label, "helper_fn(changed", fixed = TRUE)
})

test_that("closing a source graph prunes terminal and orphan failures", {
    source_read_settings()
    root <- index_normalize_path(withr::local_tempdir())
    main <- file.path(root, "main.R")
    writeLines("source('helper.R')", main)
    writeLines("helper <- 1", file.path(root, "helper.R"))
    server <- SourceReadServer$new(root)
    document <- source_read_open(server, main)
    server$parse_task_manager$finish()
    workspace <- server$get_workspace(document$uri)
    workspace$index$fail_parse(path_to_uri(file.path(root, "helper.R")))
    workspace$index$fail_parse(path_to_uri(file.path(root, "orphan.R")))
    document$did_close()
    server$prune_index_documents(workspace)
    expect_identical(workspace$index$indexing_status()$failed, 0L)
    expect_true(workspace$index$complete())
    expect_identical(workspace$index$files$size(), 0L)
})

test_that("failed closed reloads discard stale parses and open buffers recover", {
    source_read_settings()
    root <- index_normalize_path(withr::local_tempdir())
    helper <- file.path(root, "helper.R")
    main <- file.path(root, "main.R")
    writeLines("helper_fn <- function(value) value", helper)
    writeLines(c("source('helper.R')", "helper_fn()"), main)
    server <- SourceReadServer$new(root)
    document <- source_read_open(server, main)
    source_read_drain(server)
    workspace <- server$get_workspace(document$uri)
    uri <- path_to_uri(helper)
    expect_true(workspace$documents$has(uri))
    original <- stringi::stri_read_lines
    testthat::local_mocked_bindings(stri_read_lines = function(path, ...) {
        if (identical(index_normalize_path(path), helper)) stop("source is unreadable")
        original(path, ...)
    }, .package = "stringi")
    expect_null(server$load_index_document(workspace, uri, reload = TRUE))
    expect_false(workspace$documents$has(uri))
    expect_false(workspace$index$summaries$has(uri))
    expect_true(workspace$index$parse_failed(uri))
    source_read_open(server, helper, "helper_fn <- function(buffer) buffer")
    source_read_drain(server)
    expect_false(workspace$index$parse_failed(uri))
    text_document_signature_help(server, 1L,
        list(textDocument = list(uri = document$uri), position = position(1L, 10L)))
    expect_match(server$replies[[1L]]$result$signatures[[1L]]$label, "helper_fn(buffer", fixed = TRUE)
})

test_that("empty source files are parsed rather than marked as failed", {
    source_read_settings()
    root <- index_normalize_path(withr::local_tempdir())
    helper <- file.path(root, "helper.R")
    main <- file.path(root, "main.R")
    writeLines(character(), helper)
    writeLines(c("source('helper.R')", "print()", "pri"), main)
    server <- SourceReadServer$new(root)
    document <- source_read_open(server, main)
    source_read_requests(server, document)
    source_read_drain(server)
    workspace <- server$get_workspace(document$uri)
    expect_length(server$replies, 3L)
    expect_true(workspace$documents$has(path_to_uri(helper)))
    expect_false(workspace$index$parse_failed(path_to_uri(helper)))
})

test_that("discarded read-failure summaries are removed from the worker cache", {
    source_read_settings(index_mode = "full", index_persistent_cache = TRUE)
    root <- index_normalize_path(withr::local_tempdir())
    path <- file.path(root, "helper.R")
    content <- "helper_fn <- function(value) value"
    writeLines(content, path)
    cache <- file.path(root, "cache.rds")
    index <- WorkspaceIndex$new(root, cache_path = cache)
    manager <- source_read_manager()
    index$attach_worker(manager)
    uri <- path_to_uri(path)
    index$update_content(uri, content)
    index$fail_parse(uri, discard_summary = TRUE)
    expect_false(index$summaries$has(uri))
    index$.__enclos_env__$private$dirty_since <- -Inf
    withr::defer(index_worker_release(index$diagnostics_generation(uri)$index))
    for (i in seq_len(10L)) {
        index$process_batch()
        manager$finish()
        if (file.exists(cache)) break
    }
    expect_true(file.exists(cache))
    expect_length(readRDS(cache)$summaries, 0L)
    expect_true(index$parse_failed(uri))
})
