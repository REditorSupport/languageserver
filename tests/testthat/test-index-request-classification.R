classification_settings <- function(mode = "auto") {
    values <- list(index_mode = mode, index_persistent_cache = FALSE, diagnostics = FALSE)
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer(for (name in names(old)) lsp_settings$set(name, old[[name]]), envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

classification_manager <- function() {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::ordered_dict()
    manager$promoted <- character()
    manager$add_task <- function(id, task) {
        if (is.null(task)) manager$tasks$pop(id, NULL) else manager$tasks$set(id, task)
    }
    manager$cancel <- function(id) manager$tasks$pop(id, NULL)
    manager$promote <- function(id) manager$promoted <- c(manager$promoted, id)
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

ClassificationServer <- R6::R6Class("ClassificationServer", inherit = LanguageServer,
    private = list(finalize = function() NULL), public = list(
        replies = NULL,
        initialize = function(root = NULL) {
            workspace <- testthat::with_mocked_bindings(Workspace$new(root),
                workspace_startup_packages = function() startup_packages,
                workspace_startup_s3 = function() list(), .package = "languageserver")
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, workspace)
            self$workspace_cache <- collections::dict()
            self$index_task_manager <- classification_manager()
            self$parse_task_manager <- classification_manager()
            self$resolve_task_manager <- classification_manager()
            self$pending_replies <- collections::dict()
            self$replies <- list()
            self$load_workspace(workspace)
        },
        deliver = function(response, ...) {
            if (!is.null(response$id)) self$replies[[length(self$replies) + 1L]] <- response
        }
    ))

classification_open <- function(server, path) {
    uri <- path_to_uri(path)
    document <- Document$new(uri, "r", 1L, stringi::stri_read_lines(path))
    document$did_open()
    server$get_workspace(uri)$documents$set(uri, document)
    server$text_sync(uri, document, parse = TRUE)
    document
}

classification_params <- function(document, col = 22L) {
    list(textDocument = list(uri = document$uri), position = position(0L, col),
        newName = "renamed", context = list(includeDeclaration = FALSE))
}

classification_drain <- function(server) {
    for (i in seq_len(200L)) {
        for (workspace in server$workspaces$values()) workspace$index$process_batch()
        server$process_workspace_documents(time_budget_ms = Inf)
        server$index_task_manager$finish()
        server$parse_task_manager$finish()
        server$process_index_requests()
        if (!server$index_task_manager$tasks$size() && !server$parse_task_manager$tasks$size() &&
                (is.null(server$index_requests) || !server$index_requests$size()) &&
                all(vapply(server$workspaces$values(), function(workspace) workspace$index$complete(), logical(1L)))) return(invisible(NULL))
    }
    stop("classification requests did not settle")
}

test_that("pending local references and rename wait for parsing without discovery", {
    classification_settings()
    for (handler in list(text_document_references, text_document_rename)) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        writeLines("local_fn <- function(value) value", path)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        workspace <- server$get_workspace(document$uri)
        handler(server, 1L, classification_params(document))
        expect_length(server$replies, 0L)
        expect_identical(server$index_requests$size(), 1L)
        expect_false(workspace$index$full_started)
        expect_identical(server$index_task_manager$tasks$size(), 0L)
        expect_true(document$uri %in% server$parse_task_manager$promoted)
        server$process_index_requests()
        expect_false(workspace$index$full_started)
        server$parse_task_manager$finish()
        server$process_index_requests()
        expect_false(workspace$index$full_started)
        expect_identical(server$index_requests$size(), 0L)
        expect_length(server$replies, 1L)
        reply <- server$replies[[1L]]
        expect_null(reply$error)
        if (identical(handler, text_document_rename)) {
            expect_length(reply$result$changes[[document$uri]], 2L)
            expect_true(all(vapply(reply$result$changes[[document$uri]], function(edit) edit$newText == "renamed", logical(1L))))
        } else {
            expect_length(reply$result, 1L)
            expect_identical(reply$result[[1L]]$range$start$character, 28L)
        }
    }
})

test_that("same-version stale parses do not classify current local requests", {
    classification_settings()
    for (handler in list(text_document_references, text_document_rename)) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        writeLines(paste0(strrep(" ", 22L), "global_name <- 1"), path)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        server$parse_task_manager$finish()
        text_document_did_save(server, list(textDocument = list(uri = document$uri),
                text = "local_fn <- function(value) value"))
        expect_identical(document$version, document$parse_data$version)
        expect_false(identical(document$content, document$parsed_content))
        handler(server, 1L, classification_params(document))
        server$process_index_requests()
        expect_false(server$get_workspace(document$uri)$index$full_started)
        expect_length(server$replies, 0L)
        server$parse_task_manager$finish()
        server$process_index_requests()
        expect_false(server$get_workspace(document$uri)$index$full_started)
        expect_length(server$replies, 1L)
        expect_null(server$replies[[1L]]$error)
    }
})

test_that("pending global refactoring starts discovery after classification", {
    classification_settings()
    for (handler in list(text_document_references, text_document_rename)) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        caller <- file.path(root, "caller.R")
        writeLines("global_fn <- function(value) value", path)
        writeLines(c("source('main.R')", "global_fn(1)"), caller)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        params <- classification_params(document, 3L)
        params$context$includeDeclaration <- TRUE
        handler(server, 1L, params)
        expect_false(server$get_workspace(document$uri)$index$full_started)
        server$parse_task_manager$finish()
        server$process_index_requests()
        expect_true(server$get_workspace(document$uri)$index$full_started)
        classification_drain(server)
        expect_length(server$replies, 1L)
        expect_null(server$replies[[1L]]$error)
        uris <- if (identical(handler, text_document_rename)) {
            names(server$replies[[1L]]$result$changes)
        } else {
            vapply(server$replies[[1L]]$result, `[[`, "", "uri")
        }
        expect_setequal(uris, c(document$uri, path_to_uri(caller)))
    }
})

test_that("pending classification remains usable without an enabled workspace index", {
    for (folderless in c(FALSE, TRUE)) {
        classification_settings("off")
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        writeLines("local_fn <- function(value) value", path)
        server <- ClassificationServer$new(if (folderless) NULL else root)
        document <- classification_open(server, path)
        text_document_references(server, 1L, classification_params(document))
        expect_identical(server$index_requests$size(), 1L)
        expect_true(document$uri %in% server$parse_task_manager$promoted)
        server$parse_task_manager$finish()
        server$process_index_requests()
        expect_length(server$replies, 1L)
        expect_null(server$replies[[1L]]$error)
        expect_length(server$replies[[1L]]$result, 1L)
    }
})

test_that("pending classification cancels changed snapshots and client cancellations", {
    classification_settings()
    for (change in c("content", "index", "workspace", "cancel")) {
        root <- index_normalize_path(withr::local_tempdir())
        path <- file.path(root, "main.R")
        writeLines("local_fn <- function(value) value", path)
        server <- ClassificationServer$new(root)
        document <- classification_open(server, path)
        workspace <- server$get_workspace(document$uri)
        text_document_references(server, "local-request", classification_params(document))
        if (change == "content") document$set_content(1L, "changed <- 1")
        if (change == "index") workspace$index <- WorkspaceIndex$new(root)
        if (change == "workspace") server$workspaces$clear()
        if (change == "cancel") cancel_request(server, list(id = "local-request"))
        server$process_index_requests()
        expect_identical(server$index_requests$size(), 0L)
        expect_length(server$replies, 1L)
        expect_identical(server$replies[[1L]]$error$code, -32800)
        expect_identical(server$replies[[1L]]$id, "local-request")
        expect_false(workspace$index$full_started)
    }
})

test_that("failed current parses settle pending refactoring without discovery", {
    classification_settings()
    root <- index_normalize_path(withr::local_tempdir())
    path <- file.path(root, "main.R")
    writeLines("local_fn <- function(value) value", path)
    server <- ClassificationServer$new(root)
    document <- classification_open(server, path)
    text_document_rename(server, 1L, classification_params(document))
    document$parse_failure <- list(version = document$version, message = "current parse failed")
    server$get_workspace(document$uri)$index$fail_parse(document$uri)
    server$parse_task_manager$cancel(document$uri)
    server$process_index_requests()
    expect_identical(server$index_requests$size(), 0L)
    expect_length(server$replies, 1L)
    expect_identical(server$replies[[1L]]$error$code, -32603)
    expect_false(server$get_workspace(document$uri)$index$full_started)
})

test_that("ready local requests are not starved behind pending workspace queries", {
    classification_settings()
    root <- index_normalize_path(withr::local_tempdir())
    path <- file.path(root, "main.R")
    writeLines("local_fn <- function(value) value", path)
    server <- ClassificationServer$new(root)
    document <- classification_open(server, path)
    for (id in seq_len(21L)) workspace_symbol(server, id, list(query = "local_fn"))
    text_document_references(server, 22L, classification_params(document))
    server$parse_task_manager$finish()
    server$process_index_requests(max_requests = 20L)
    server$process_index_requests(max_requests = 20L)
    expect_length(server$replies, 1L)
    expect_identical(server$replies[[1L]]$id, 22L)
    expect_null(server$replies[[1L]]$error)
    expect_length(server$replies[[1L]]$result, 1L)
    expect_identical(server$index_requests$size(), 21L)
    expect_true(server$get_workspace(document$uri)$index$discovering)
})
