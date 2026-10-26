policy_settings <- function(...) {
    values <- utils::modifyList(list(index_mode = "auto", index_persistent_cache = FALSE), list(...))
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer(for (name in names(old)) lsp_settings$set(name, old[[name]]), envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

policy_manager <- function() {
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

PolicyServer <- R6::R6Class("PolicyServer", inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(
        replies = NULL,
        initialize = function(workspace) {
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, workspace)
            self$workspace_cache <- collections::dict()
            self$index_task_manager <- policy_manager()
            self$parse_task_manager <- policy_manager()
            self$resolve_task_manager <- policy_manager()
            self$pending_replies <- collections::dict()
            self$replies <- list()
            self$request_handlers <- list(`textDocument/completion` = text_document_completion,
                `textDocument/hover` = text_document_hover, `textDocument/signatureHelp` = text_document_signature_help)
        },
        deliver = function(response, ...) {
            if (!is.null(response$id)) self$replies[[length(self$replies) + 1L]] <- response
        }
    ))

policy_workspace <- function(root) {
    testthat::with_mocked_bindings(Workspace$new(root),
        workspace_startup_packages = function() startup_packages,
        workspace_startup_s3 = function() list(), .package = "languageserver")
}

policy_open <- function(server, path, content = stringi::stri_read_lines(path)) {
    workspace <- server$get_workspace(path_to_uri(path))
    doc <- Document$new(path_to_uri(path), "r", 1L, content)
    doc$did_open()
    workspace$documents$set(doc$uri, doc)
    server$text_sync(doc$uri, doc, parse = TRUE)
    doc
}

policy_drain <- function(server, limit = 500L) {
    for (i in seq_len(limit)) {
        for (workspace in server$workspaces$values()) workspace$index$process_batch()
        server$process_workspace_documents(time_budget_ms = Inf)
        server$index_task_manager$finish()
        server$parse_task_manager$finish()
        server$process_context_replies()
        server$process_pending_replies(time_budget_ms = Inf)
        server$process_index_requests()
        if (server$index_task_manager$tasks$size() == 0L && server$parse_task_manager$tasks$size() == 0L &&
                all(vapply(server$workspaces$values(), function(ws) ws$index$complete(), logical(1L)))) {
            for (workspace in server$workspaces$values()) server$prune_index_documents(workspace)
            return(invisible(NULL))
        }
    }
    stop("indexing did not settle")
}

test_that("auto chooses package indexing and lazy standalone indexing", {
    policy_settings()
    root <- withr::local_tempdir()
    expect_identical(WorkspaceIndex$new(root)$mode, "lazy")
    writeLines("Imports: stats", file.path(root, "DESCRIPTION"))
    dir.create(file.path(root, "R"))
    expect_identical(WorkspaceIndex$new(root)$mode, "lazy")
    writeLines("Package: policyfixture", file.path(root, "DESCRIPTION"))
    expect_identical(WorkspaceIndex$new(root)$mode, "full")
    lsp_settings$set("index_mode", "lazy")
    expect_identical(WorkspaceIndex$new(root)$mode, "lazy")
    lsp_settings$set("index_mode", "full")
    expect_identical(WorkspaceIndex$new(withr::local_tempdir())$mode, "full")
    lsp_settings$set("index_mode", "off")
    expect_false(WorkspaceIndex$new(root)$enabled)
})

test_that("lazy startup avoids discovery and cached unrelated summaries", {
    policy_settings(index_persistent_cache = TRUE)
    root <- withr::local_tempdir()
    for (i in seq_len(1000L)) writeLines("unrelated <- function(x) x", file.path(root, paste0(i, ".R")))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    testthat::local_mocked_bindings(index_worker_discover = function(...) stop("startup crawled workspace"),
        .package = "languageserver")
    unlockBinding("load_cache", workspace$index)
    workspace$index$load_cache <- function(...) stop("startup loaded a whole cache")
    server$load_workspace(workspace)
    expect_false(workspace$index$discovering)
    expect_null(workspace$index$cache_file())
    workspace$index$process_batch()
    expect_identical(server$index_task_manager$tasks$size(), 0L)
    doc <- policy_open(server, file.path(root, "main.R"), c("hello <- function(value) value", "print()", "hel"))
    text_document_completion(server, 1L, list(textDocument = list(uri = doc$uri), position = position(2L, 3L)))
    expect_true("hello" %in% vapply(server$replies[[1L]]$result$items, `[[`, "", "label"))
    text_document_signature_help(server, 2L, list(textDocument = list(uri = doc$uri), position = position(1L, 6L)))
    text_document_hover(server, 3L, list(textDocument = list(uri = doc$uri), position = position(1L, 2L)))
    server$parse_task_manager$finish()
    server$process_pending_replies(time_budget_ms = Inf)
    expect_length(server$replies, 3L)
    expect_identical(workspace$index$files$size(), 1L)
    expect_false(workspace$index$has_definition("unrelated"))
})

test_that("lazy source graphs produce useful first provider replies and prune on close", {
    policy_settings()
    root <- withr::local_tempdir()
    writeLines(c("source('helper.R')", "helper_fun()", "helper_f"), file.path(root, "main.R"))
    writeLines(c("source('leaf.R')", "helper_fun <- function(value = 1) value"), file.path(root, "helper.R"))
    writeLines(c("source('main.R')", "leaf_fun <- function(x) x"), file.path(root, "leaf.R"))
    writeLines("unrelated <- function(x) x", file.path(root, "unrelated.R"))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    doc <- policy_open(server, file.path(root, "main.R"))
    text_document_completion(server, 1L, list(textDocument = list(uri = doc$uri), position = position(2L, 8L)))
    text_document_signature_help(server, 2L, list(textDocument = list(uri = doc$uri), position = position(1L, 11L)))
    text_document_hover(server, 3L, list(textDocument = list(uri = doc$uri), position = position(1L, 3L)))
    policy_drain(server)
    expect_identical(workspace$index$files$size(), 3L)
    expect_length(server$replies, 3L)
    replies <- stats::setNames(server$replies, vapply(server$replies, function(reply) as.character(reply$id), ""))
    expect_true("helper_fun" %in% vapply(replies$`1`$result$items, `[[`, "", "label"))
    expect_match(replies$`2`$result$signatures[[1L]]$label, "helper_fun(value", fixed = TRUE)
    expect_match(paste(replies$`3`$result$contents, collapse = " "), "helper_fun", fixed = TRUE)
    expect_false(workspace$index$full_started)
    doc$did_close()
    server$prune_index_documents(workspace)
    expect_identical(workspace$documents$size(), 0L)
    expect_identical(workspace$index$files$size(), 0L)
})

test_that("opening nested packages discovers their namespace without scanning neighbors", {
    policy_settings()
    root <- withr::local_tempdir()
    for (package in c("one", "two")) {
        dir.create(file.path(root, package, "R"), recursive = TRUE)
        writeLines(paste0("Package: ", package), file.path(root, package, "DESCRIPTION"))
        for (file in c("first", "second")) writeLines(paste0(file, " <- function(x) x"),
            file.path(root, package, "R", paste0(file, ".R")))
    }
    writeLines("standalone <- 1", file.path(root, "main.R"))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    policy_open(server, file.path(root, "main.R"))
    policy_drain(server)
    policy_open(server, file.path(root, "one", "R", "first.R"))
    policy_drain(server)
    expect_identical(workspace$index$files$size(), 3L)
    expect_true(workspace$index$has_definition("standalone"))
    expect_false(workspace$index$files$has(path_to_uri(file.path(root, "two", "R", "first.R"))))
    policy_open(server, file.path(root, "two", "R", "first.R"))
    policy_drain(server)
    expect_identical(workspace$index$files$size(), 5L)
    expect_false(workspace$index$full_started)
})

test_that("lazy watchers ignore unrelated files but resolve missing source targets", {
    policy_settings()
    root <- withr::local_tempdir()
    writeLines("source('future.R')", file.path(root, "main.R"))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    policy_open(server, file.path(root, "main.R"))
    policy_drain(server)
    unrelated <- file.path(root, "other.R")
    writeLines("other <- 1", unrelated)
    workspace_did_change_watched_files(server, list(changes = list(list(uri = path_to_uri(unrelated), type = 1L))))
    expect_identical(server$index_task_manager$tasks$size(), 0L)
    expect_length(workspace$index$pending, 0L)
    future <- file.path(root, "future.R")
    writeLines("future <- function(x) x", future)
    workspace_did_change_watched_files(server, list(changes = list(list(uri = path_to_uri(future), type = 1L))))
    policy_drain(server)
    expect_true(workspace$index$has_definition("future"))
    expect_false(workspace$index$has_definition("other"))
    unlink(future)
    workspace_did_change_watched_files(server, list(changes = list(list(uri = path_to_uri(future), type = 3L))))
    policy_drain(server)
    expect_identical(workspace$index$files$size(), 1L)
    expect_false(workspace$index$has_definition("future"))
})

test_that("package descriptor changes reselect auto policy without losing open buffers", {
    policy_settings()
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("helper <- function(x) x", file.path(root, "R", "helper.R"))
    writeLines("saved <- 1", file.path(root, "main.R"))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    doc <- policy_open(server, file.path(root, "main.R"), "unsaved <- 2")
    policy_drain(server)
    descriptor <- file.path(root, "DESCRIPTION")
    writeLines("Package: converted", descriptor)
    workspace_did_change_watched_files(server, list(changes = list(list(uri = path_to_uri(descriptor), type = 1L))))
    policy_drain(server)
    expect_identical(workspace$index$mode, "full")
    expect_true(workspace$index$has_definition("helper"))
    expect_identical(workspace$documents$get(doc$uri), doc)
    expect_identical(doc$content, "unsaved <- 2")
    unlink(descriptor)
    workspace_did_change_watched_files(server, list(changes = list(list(uri = path_to_uri(descriptor), type = 3L))))
    policy_drain(server)
    expect_identical(workspace$index$mode, "lazy")
    expect_identical(workspace$index$files$size(), 1L)
    expect_false(workspace$index$has_definition("helper"))
    expect_true(workspace$index$has_definition("unsaved"))
})

test_that("root descriptor events compare normalized index paths", {
    policy_settings()
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("helper <- function(x) x", file.path(root, "R", "helper.R"))
    writeLines("saved <- 1", file.path(root, "main.R"))
    normalize <- index_normalize_path
    testthat::local_mocked_bindings(index_normalize_path = function(path) {
        normalize(chartr("\\", "/", path))
    }, .package = "languageserver")
    # Workspace retains the caller's path spelling; its index canonicalizes
    # separators, as it does for a Windows root received through file URIs.
    workspace <- policy_workspace(chartr("/", "\\", root))
    expect_false(identical(workspace$root, workspace$index$root))
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    doc <- policy_open(server, file.path(root, "main.R"), "unsaved <- 2")
    policy_drain(server)
    descriptor <- file.path(root, "DESCRIPTION")
    writeLines("Package: converted", descriptor)
    workspace_did_change_watched_files(server, list(changes = list(list(uri = path_to_uri(descriptor), type = 1L))))
    policy_drain(server)
    expect_identical(workspace$index$mode, "full")
    expect_true(workspace$index$has_definition("helper"))
    expect_identical(workspace$documents$get(doc$uri), doc)
    expect_identical(doc$content, "unsaved <- 2")
})

test_that("local references stay lazy and stale global refactoring requests are cancelled", {
    policy_settings()
    root <- withr::local_tempdir()
    writeLines("shared <- function(value) value", file.path(root, "main.R"))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    doc <- policy_open(server, file.path(root, "main.R"))
    policy_drain(server)
    params <- list(textDocument = list(uri = doc$uri), position = position(0L, 21L))
    text_document_references(server, 1L, params)
    expect_false(workspace$index$full_started)
    expect_length(server$replies[[1L]]$result, 2L)
    params$position <- position(0L, 3L)
    text_document_references(server, 2L, params)
    expect_identical(server$index_requests$size(), 1L)
    doc$set_content(2L, "replacement <- 1")
    server$process_index_requests()
    expect_identical(server$index_requests$size(), 0L)
    expect_identical(tail(server$replies, 1L)[[1L]]$error$code, -32800)
})

test_that("global symbol queries explicitly complete a full scan and remain cancellable", {
    policy_settings()
    root <- withr::local_tempdir()
    writeLines("closed_fun <- function(x) x", file.path(root, "closed.R"))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    workspace_symbol(server, 1L, list(query = "closed_fun"))
    expect_true(workspace$index$full_started)
    expect_length(server$replies, 0L)
    policy_drain(server)
    expect_length(server$replies[[1L]]$result, 1L)
    workspace$index$discover()
    workspace_symbol(server, 2L, list(query = "closed_fun"))
    cancel_request(server, list(id = 2L))
    expect_identical(tail(server$replies, 1L)[[1L]]$error$code, -32800)
    expect_identical(server$index_requests$size(), 0L)
})

test_that("global references and renames include unopened source consumers", {
    policy_settings()
    root <- withr::local_tempdir()
    helper <- file.path(root, "helper.R")
    caller <- file.path(root, "caller.R")
    writeLines("shared <- function(x) x", helper)
    writeLines(c("source('helper.R')", "shared(1)"), caller)
    writeLines("shared(1)", file.path(root, "unrelated.R"))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    doc <- policy_open(server, helper)
    policy_drain(server)
    params <- list(textDocument = list(uri = doc$uri), position = position(0L, 3L), newName = "renamed")
    text_document_references(server, 1L, params)
    policy_drain(server)
    refs <- server$replies[[1L]]$result
    expect_setequal(vapply(refs, `[[`, "", "uri"), c(doc$uri, path_to_uri(caller)))
    text_document_rename(server, 2L, params)
    policy_drain(server)
    expect_setequal(names(server$replies[[2L]]$result$changes), c(doc$uri, path_to_uri(caller)))
    expect_false(workspace$index$has_definition("shared", uri = path_to_uri(file.path(root, "unrelated.R"))))
})

test_that("unrelated definitions cannot block cached help after a full scan", {
    policy_settings(index_mode = "full")
    root <- withr::local_tempdir()
    writeLines("print()", file.path(root, "main.R"))
    writeLines("print <- function(unrelated) unrelated", file.path(root, "other.R"))
    workspace <- policy_workspace(root)
    server <- PolicyServer$new(workspace)
    server$load_workspace(workspace)
    doc <- policy_open(server, file.path(root, "main.R"))
    policy_drain(server)
    expect_true(workspace$index$has_definition("print"))
    expect_false(workspace$index$has_definition("print", uri = doc$uri))
    server$resolve_task_manager$tasks$set(doc$uri, list())
    text_document_signature_help(server, 1L, list(textDocument = list(uri = doc$uri), position = position(0L, 6L)))
    text_document_hover(server, 2L, list(textDocument = list(uri = doc$uri), position = position(0L, 2L)))
    expect_length(server$replies, 2L)
    expect_match(server$replies[[1L]]$result$signatures[[1L]]$label, "print(x", fixed = TRUE)
})
