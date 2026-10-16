IndexReviewServer <- R6::R6Class("IndexReviewServer",
    inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(
        messages = NULL,
        diagnostic_tasks = NULL,
        parse_tasks = NULL,
        initialize = function(root) {
            workspace <- Workspace$new(root)
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, workspace)
            self$rootPath <- root
            self$workspace_cache <- collections::dict()
            self$pending_replies <- collections::dict()
            self$messages <- list()
            self$diagnostic_tasks <- list()
            self$parse_tasks <- collections::dict()
            self$index_task_manager <- list(add_task = function(...) NULL, cancel = function(...) NULL)
            self$parse_task_manager <- list(add_task = function(id, task) self$parse_tasks$set(id, task))
            self$diagnostics_task_manager <- list(add_task = function(id, task) {
                self$diagnostic_tasks[[length(self$diagnostic_tasks) + 1L]] <- task
            })
        },
        deliver = function(message, ...) {
            self$messages[[length(self$messages) + 1L]] <- message
        },
        get_workspace = function(uri) self$workspaces$values()[[1L]]
    )
)

index_review_settings <- function(...) {
    values <- list(...)
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer({
        for (name in names(old)) lsp_settings$set(name, old[[name]])
    }, envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

index_review_run_diagnostics <- function(task) {
    private <- task$.__enclos_env__$private
    result <- do.call(private$target, private$args)
    private$callback(result)
    result
}

test_that("diagnostics refresh after discovery and bypass cached incomplete globals", {
    index_review_settings(diagnostics = TRUE, index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: diagnosticfixture", file.path(root, "DESCRIPTION"))
    writeLines("linters: list(object_usage_linter())", file.path(root, ".lintr"))
    main <- file.path(root, "R", "main.R")
    helper <- file.path(root, "R", "helper.R")
    lines <- c("main <- function() helper()", "")
    writeLines(lines, main)
    writeLines("helper <- function(x = 1) x", helper)
    server <- IndexReviewServer$new(root)
    workspace <- server$get_workspace()
    index <- workspace$index
    server$load_workspace(workspace)
    uri <- path_to_uri(main)
    document <- Document$new(uri, version = 1L, content = lines)
    document$did_open()
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, lines)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    first <- diagnostics_task(server, uri, document)
    initial <- index_review_run_diagnostics(first)
    expect_true(any(vapply(initial, function(diagnostic) grepl("helper", diagnostic$message), logical(1L))))
    expect_null(diagnostics_task(server, uri, document))
    before <- length(server$messages)

    index$files$set(path_to_uri(helper), list(uri = path_to_uri(helper), path = helper, package_root = root))
    index$update_path(helper)
    index$on_update(path_to_uri(helper))
    server$refresh_workspace_diagnostics(now = Inf)
    expect_length(server$diagnostic_tasks, 0L)
    index$discovering <- FALSE
    index$on_update(character())
    server$refresh_workspace_diagnostics(now = Inf)
    expect_length(server$diagnostic_tasks, 0L)
    server$process_workspace_documents()
    helper_uri <- path_to_uri(helper)
    workspace$update_parse_data(helper_uri, parse_document(helper_uri, "helper <- function(x = 1) x"))
    server$refresh_index_documents(workspace)
    server$refresh_workspace_diagnostics(now = Inf)
    expect_length(server$diagnostic_tasks, 1L)
    corrected <- index_review_run_diagnostics(server$diagnostic_tasks[[1L]])
    expect_length(corrected, 0L)
    expect_length(server$messages, before + 1L)
    expect_length(tail(server$messages, 1L)[[1L]]$params$diagnostics, 0L)
    server$refresh_workspace_diagnostics(now = Inf)
    expect_length(server$diagnostic_tasks, 1L)

    index$on_update(helper_uri)
    index$update_content(path_to_uri(file.path(root, "unrelated.R")), "unrelated <- 1")
    index$on_update(path_to_uri(file.path(root, "unrelated.R")))
    server$refresh_workspace_diagnostics(now = Inf)
    expect_length(server$diagnostic_tasks, 1L)

    stale <- diagnostics_task(server, uri, document)
    expect_null(stale)
    index$remove(path_to_uri(helper))
    workspace$documents$remove(helper_uri)
    stale <- diagnostics_task(server, uri, document)
    index$update_path(helper)
    before <- length(server$messages)
    index_review_run_diagnostics(stale)
    expect_length(server$messages, before)
    current <- diagnostics_task(server, uri, document)
    expect_length(index_review_run_diagnostics(current), 0L)
})

test_that("diagnostic generations change only for relevant symbol changes", {
    index_review_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    paths <- file.path(root, c("main.R", "helper.R", "other.R"))
    writeLines("source('helper.R')", paths[[1L]])
    writeLines("helper <- 1", paths[[2L]])
    writeLines("other <- 1", paths[[3L]])
    index <- WorkspaceIndex$new(root)
    for (path in paths) index$update_path(path)
    uri <- path_to_uri(paths[[1L]])
    initial <- index$diagnostics_generation(uri)
    index$update_content(path_to_uri(paths[[3L]]), "unrelated <- 2")
    expect_identical(index$diagnostics_generation(uri), initial)
    index$update_content(path_to_uri(paths[[2L]]), "helper <- 200")
    expect_identical(index$diagnostics_generation(uri), initial)
    index$remove(path_to_uri(paths[[2L]]))
    expect_false(identical(index$diagnostics_generation(uri), initial))
    replacement <- WorkspaceIndex$new(root)
    expect_false(identical(replacement$diagnostics_generation(uri)$index, initial$index))
})

test_that("repeated missing-source lookups preserve in-flight index generations", {
    index_review_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    main <- file.path(root, "main.R")
    helper <- file.path(root, "helper.R")
    writeLines("source('helper.R')", main)
    writeLines("helper <- function() 1", helper)
    workspace <- Workspace$new(root)
    index <- workspace$index
    index$update_path(main)
    tasks <- collections::dict()
    index$attach_worker(list(add_task = function(id, task) tasks$set(id, task)))
    uri <- path_to_uri(main)
    helper_uri <- path_to_uri(helper)
    for (i in seq_len(10L)) workspace$get_diagnostics_globals(uri)
    expect_identical(index$pending, helper_uri)
    index$process_batch()
    private <- tasks$values()[[1L]]$.__enclos_env__$private
    generation <- private$args$requests[[1L]]$generation
    result <- do.call(index_worker_batch, private$args)
    for (i in seq_len(10L)) {
        index$source_closure(uri, update = TRUE)
        workspace$get_diagnostics_globals(uri)
    }
    expect_length(index$pending, 0L)
    expect_identical(index$.__enclos_env__$private$generations$get(helper_uri, 0L), generation)
    private$callback(result)
    expect_true(index$summaries$has(helper_uri))

    index$queue_path(helper)
    index$process_batch()
    private <- tasks$values()[[1L]]$.__enclos_env__$private
    result <- do.call(index_worker_batch, private$args)
    writeLines("changed <- function() 2", helper)
    index$queue_path(helper)
    private$callback(result)
    expect_identical(names(index$summaries$get(helper_uri)$definitions), "helper")
    expect_true(helper_uri %in% index$pending)
})

test_that("top-level refactors wait for queued package documents and their full parses", {
    index_review_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: refactorfixture", file.path(root, "DESCRIPTION"))
    main <- file.path(root, "R", "main.R")
    other <- file.path(root, "R", "other.R")
    lines <- c("result <- sqrt(1 + 2)", "")
    writeLines(lines, main)
    writeLines("extracted_value <- 3", other)
    server <- IndexReviewServer$new(root)
    workspace <- server$get_workspace()
    index <- workspace$index
    server$load_workspace(workspace)
    uri <- path_to_uri(main)
    document <- Document$new(uri, version = 1L, content = lines)
    document$did_open()
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, lines)
    parsed$version <- document$version
    workspace$update_parse_data(uri, parsed)
    index$update_path(other)
    index$on_update(path_to_uri(other))
    index$discovering <- FALSE
    index$pending <- character()
    index$worker_running <- FALSE
    selected <- list(start = list(row = 0L, col = 15L), end = list(row = 0L, col = 20L))
    actions <- function() refactor_code_actions(uri, workspace, document, selected, list("refactor.extract"))
    expect_false(refactor_index_ready(workspace))
    expect_length(actions(), 0L)
    server$process_workspace_documents()
    expect_false(refactor_index_ready(workspace))
    expect_length(actions(), 0L)
    other_uri <- path_to_uri(other)
    workspace$update_parse_data(other_uri, parse_document(other_uri, "extracted_value <- 3"))
    expect_true(refactor_index_ready(workspace))
    expect_identical(refactor_unique_name("extracted_value", workspace, uri), "extracted_value_2")
    expect_true(length(actions()) > 0L)
    expect_match(actions()[[1L]]$edit$changes[[uri]][[1L]]$newText, "extracted_value_2", fixed = TRUE)
})

test_that("legacy loading and index replacement preserve document readiness gates", {
    index_review_settings(index_mode = "off", index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: readinessfixture", file.path(root, "DESCRIPTION"))
    path <- file.path(root, "R", "code.R")
    writeLines("value <- 1", path)
    uri <- path_to_uri(path)
    server <- IndexReviewServer$new(root)
    workspace <- server$get_workspace()
    server$load_workspace(workspace)
    expect_false(refactor_index_ready(workspace))
    old <- workspace$index
    workspace$index <- WorkspaceIndex$new(root)
    server$load_workspace(workspace)
    server$process_workspace_documents(max_documents = 1L)
    expect_false(refactor_index_ready(workspace))
    expect_true(old$indexing_status()$done)
    server$process_workspace_documents()
    expect_false(refactor_index_ready(workspace))
    workspace$update_parse_data(uri, parse_document(uri, "value <- 1"))
    expect_true(refactor_index_ready(workspace))

    workspace$documents$remove(uri)
    server$load_workspace(workspace)
    unlink(path)
    server$process_workspace_documents()
    expect_true(refactor_index_ready(workspace))
})

test_that("clients receive corrected diagnostics when a new package file is indexed", {
    skip_on_cran()
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: clientdiagnosticfixture", file.path(root, "DESCRIPTION"))
    writeLines("linters: list(object_usage_linter())", file.path(root, ".lintr"))
    main <- file.path(root, "R", "main.R")
    helper <- file.path(root, "R", "helper.R")
    writeLines(c("main <- function() review_helper()", ""), main)
    uri <- path_to_uri(main)
    client <- language_client(root, diagnostics = TRUE)
    notify(client, "workspace/didChangeConfiguration", list(settings = list(diagnostics_cache_ttl = 60)))
    client %>% did_open(main)
    wait_diagnostics <- function(predicate) {
        timeout <- if (identical(Sys.getenv("R_COVR"), "true")) 60 else 15
        deadline <- Sys.time() + timeout
        while (Sys.time() < deadline) {
            if (client$diagnostics$has(uri) && predicate(client$diagnostics$get(uri))) return(TRUE)
            data <- client$fetch(blocking = TRUE, timeout = 0.1)
            if (!is.null(data)) client$handle_raw(data)
        }
        FALSE
    }
    missing_helper <- function(diagnostics) {
        any(vapply(diagnostics, function(diagnostic) {
            grepl("review_helper", diagnostic$message, fixed = TRUE)
        }, logical(1L)))
    }
    expect_true(wait_diagnostics(missing_helper))
    writeLines("review_helper <- function() 1", helper)
    notify(client, "workspace/didChangeWatchedFiles", list(changes = list(
        list(uri = path_to_uri(helper), type = FileChangeType$Created))))
    expect_true(wait_diagnostics(function(diagnostics) !length(diagnostics)))
    unlink(helper)
    notify(client, "workspace/didChangeWatchedFiles", list(changes = list(
        list(uri = path_to_uri(helper), type = FileChangeType$Deleted))))
    expect_true(wait_diagnostics(missing_helper))
})

test_that("a newer disk summary takes precedence over a retained stale document parse", {
    index_review_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: snapshotfixture", file.path(root, "DESCRIPTION"))
    path <- file.path(root, "R", "helper.R")
    uri <- path_to_uri(path)
    original <- "old_helper <- function(x) {\tx }"
    updated <- "new_helper <- function(y) {\ty }"
    writeLines(original, path)
    workspace <- Workspace$new(root)
    document <- Document$new(uri, content = original)
    workspace$documents$set(uri, document)
    workspace$update_parse_data(uri, parse_document(uri, original))
    expect_true(refactor_index_ready(workspace))
    before <- workspace$index$diagnostics_generation(uri)
    writeLines(updated, path)
    workspace$index$update_path(path)
    globals <- workspace$get_diagnostics_globals(uri)
    expect_true(exists("new_helper", globals, inherits = FALSE))
    expect_false(exists("old_helper", globals, inherits = FALSE))
    expect_false(refactor_index_ready(workspace))
    expect_false(identical(workspace$index$diagnostics_generation(uri), before))
    document$set_content(NULL, updated)
    workspace$update_parse_data(uri, parse_document(uri, updated))
    expect_true(refactor_index_ready(workspace))
})
