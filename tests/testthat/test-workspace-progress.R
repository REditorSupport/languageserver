WorkspaceProgressServer <- R6::R6Class("WorkspaceProgressServer",
    inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(
        messages = NULL,
        initialize = function(root, supported = TRUE) {
            self$workspaces <- collections::dict()
            self$workspaces$set(path_to_uri(root), Workspace$new(root))
            self$workspace_cache <- collections::dict()
            self$request_callbacks <- collections::dict()
            self$pending_replies <- collections::dict()
            self$ClientCapabilities <- list(window = list(workDoneProgress = supported))
            tasks <- collections::dict()
            manager <- list(add_task = function(id, task) tasks$set(id, task), cancel = function(...) NULL)
            self$index_task_manager <- manager
            self$parse_task_manager <- manager
            self$messages <- list()
        },
        deliver = function(message, callback = NULL) {
            self$messages[[length(self$messages) + 1L]] <- message
            if (!is.null(callback)) self$request_callbacks$set(as.character(message$id), callback)
        },
        get_workspace = function(uri) {
            self$workspaces$values()[[1L]]
        }
    )
)

progress_fixture <- function(supported = TRUE, mode = "full") {
    old_mode <- lsp_settings$get("index_mode")
    withr::defer(lsp_settings$set("index_mode", old_mode), envir = parent.frame())
    lsp_settings$set("index_mode", mode)
    root <- withr::local_tempdir(.local_envir = parent.frame())
    server <- WorkspaceProgressServer$new(root, supported)
    workspace <- server$workspaces$values()[[1L]]
    server$load_workspace(workspace)
    list(root = root, server = server, workspace = workspace, index = workspace$index)
}

progress_values <- function(server) {
    messages <- Filter(function(message) identical(message$method, "$/progress"), server$messages)
    lapply(messages, function(message) message$params$value)
}

progress_accept <- function(server) {
    request <- tail(Filter(function(message) inherits(message, "Request"), server$messages), 1L)[[1L]]
    server$handle_response(list(id = request$id, result = NULL))
    invisible(request)
}

test_that("workspace progress is capability gated and starts after client acknowledgment", {
    unsupported <- progress_fixture(FALSE)
    unsupported$server$report_workspace_progress()
    expect_length(unsupported$server$messages, 0L)
    expect_true(unsupported$index$discovering)

    fixture <- progress_fixture()
    server <- fixture$server
    expect_length(server$messages, 1L)
    request <- server$messages[[1L]]
    expect_identical(request$method, "window/workDoneProgress/create")
    expect_true(is.character(request$params$token))
    server$report_workspace_progress(now = 0)
    expect_length(progress_values(server), 0L)
    progress_accept(server)
    begin <- progress_values(server)[[1L]]
    expect_identical(begin$kind, "begin")
    expect_match(begin$title, "Indexing R workspace", fixed = TRUE)
    expect_identical(begin$message, "Discovering R files")
    expect_false(begin$cancellable)
    expect_null(begin$percentage)
    payload <- jsonlite::fromJSON(tail(server$messages, 1L)[[1L]]$to_json())
    expect_identical(payload$params$token, request$params$token)
    expect_identical(payload$params$value$kind, "begin")
})

test_that("automatic standalone workspaces do not start discovery or progress", {
    fixture <- progress_fixture(mode = "auto")
    expect_true(fixture$index$is_lazy())
    expect_false(fixture$index$discovering)
    expect_length(fixture$server$messages, 0L)
    fixture$server$report_workspace_progress()
    expect_length(fixture$server$messages, 0L)
})

test_that("progress counts queued and running files and waits for full package parses", {
    fixture <- progress_fixture()
    index <- fixture$index
    server <- fixture$server
    progress_accept(server)
    server$report_workspace_progress(now = 0)
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message, "Discovering R files")
    paths <- file.path(fixture$root, c("first.R", "second.R", "third.R"))
    for (path in paths) writeLines("value <- 1", path)
    uris <- vapply(paths, path_to_uri, character(1L))
    for (i in seq_along(paths)) {
        index$files$set(uris[[i]], list(uri = uris[[i]], path = paths[[i]]))
    }
    index$discovering <- FALSE
    index$pending <- uris[1:2]
    index$await_parse(uris[[3L]], cacheable = TRUE)
    server$report_workspace_progress(now = 1)
    value <- tail(progress_values(server), 1L)[[1L]]
    expect_identical(value$message, "Indexed 0 of 3 R files")
    expect_equal(value$percentage, 0L)
    index$process_batch()
    expect_length(index$pending, 0L)
    expect_false(index$indexing_status()$done)
    server$report_workspace_progress(now = 2)
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message, "Indexed 0 of 3 R files")

    for (i in 1:2) index$update_content(uris[[i]], "value <- 1")
    server$report_workspace_progress(now = 3)
    value <- tail(progress_values(server), 1L)[[1L]]
    expect_identical(value$message, "Indexed 2 of 3 R files")
    expect_equal(value$percentage, 66L)
    index$update_content(uris[[3L]], "value <- 1", parse_data = parse_document(uris[[3L]], "value <- 1"))
    server$report_workspace_progress(now = 4)
    value <- tail(progress_values(server), 1L)[[1L]]
    expect_identical(value$kind, "end")
    expect_identical(value$message, "Indexed 3 R files")
    previous <- length(server$messages)
    server$report_workspace_progress(now = 5)
    expect_length(server$messages, previous)
})

test_that("progress reports are throttled, deduplicated and monotonic", {
    fixture <- progress_fixture()
    server <- fixture$server
    index <- fixture$index
    progress_accept(server)
    server$report_workspace_progress(now = 0)
    before <- length(server$messages)
    for (i in seq_len(3L)) index$files$set(paste0(i), list(uri = paste0(i)))
    server$report_workspace_progress(now = 0.1)
    expect_length(server$messages, before)
    server$report_workspace_progress(now = 0.3)
    expect_length(server$messages, before + 1L)
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message, "Discovered 3 R files")
    server$report_workspace_progress(now = 1)
    expect_length(server$messages, before + 1L)
    index$discovering <- FALSE
    index$pending <- "1"
    server$report_workspace_progress(now = 2)
    first <- tail(progress_values(server), 1L)[[1L]]$percentage
    index$pending <- c("1", "2")
    server$report_workspace_progress(now = 3)
    expect_gte(tail(progress_values(server), 1L)[[1L]]$percentage, first)
})

test_that("completion, removal, restart and rejected creation close progress correctly", {
    fixture <- progress_fixture()
    server <- fixture$server
    progress_accept(server)
    fixture$index$discovering <- FALSE
    fixture$index$truncated <- TRUE
    server$report_workspace_progress(now = 1)
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message, "Indexed 0 R files (file limit reached)")

    fixture <- progress_fixture()
    server <- fixture$server
    request <- server$messages[[1L]]
    server$remove_workspace(path_to_uri(fixture$root))
    expect_length(progress_values(server), 0L)
    server$handle_response(list(id = request$id, result = NULL))
    values <- progress_values(server)
    expect_identical(vapply(values, `[[`, character(1L), "kind"), c("begin", "end"))
    expect_identical(values[[2L]]$message, "Workspace removed")

    fixture <- progress_fixture()
    server <- fixture$server
    progress_accept(server)
    first_token <- server$messages[[1L]]$params$token
    server$load_workspace(fixture$workspace)
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message, "Indexing restarted")
    expect_false(identical(tail(server$messages, 1L)[[1L]]$params$token, first_token))
    request <- tail(server$messages, 1L)[[1L]]
    server$handle_response(list(id = request$id, error = list(code = -32601L, message = "unsupported")))
    expect_equal(server$.__enclos_env__$private$workspace_progress$size(), 0L)
    before <- length(server$messages)
    server$report_workspace_progress(now = 1)
    expect_length(server$messages, before)
})

test_that("queued package files keep progress open before their documents are read", {
    fixture <- progress_fixture()
    server <- fixture$server
    index <- fixture$index
    progress_accept(server)
    path <- file.path(fixture$root, "main.R")
    writeLines("value <- 1", path)
    uri <- path_to_uri(path)
    index$files$set(uri, list(uri = uri, path = path))
    index$discovering <- FALSE
    server$.__enclos_env__$private$queue_workspace_document(fixture$workspace, uri)
    server$report_workspace_progress(now = 1)
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message, "Indexed 0 of 1 R files")
    server$process_workspace_documents()
    expect_false(index$indexing_status()$done)
    server$report_workspace_progress(now = 2)
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message, "Indexed 0 of 1 R files")
})

test_that("work-done progress reaches the client while requests stay responsive", {
    skip_on_cran()
    root <- withr::local_tempdir()
    writeLines("progress_helper <- function(x) x", file.path(root, "helper.R"))
    client <- language_client(root, capabilities = list(window = list(workDoneProgress = TRUE)))
    tokens <- character()
    values <- list()
    client$request_handlers[["window/workDoneProgress/create"]] <- function(self, id, params) {
        tokens <<- c(tokens, params$token)
        self$deliver(Response$new(id = id, result = NULL))
    }
    client$notification_handlers[["$/progress"]] <- function(self, params) {
        expect_true(params$token %in% tokens)
        values[[length(values) + 1L]] <<- params$value
    }
    result <- respond(client, "completionItem/resolve", list(label = "progress probe"), retry = FALSE)
    expect_identical(result$label, "progress probe")
    client %>% respond_workspace_symbol("progress_helper", retry_when = function(result) !length(result))
    deadline <- Sys.time() + if (identical(Sys.getenv("R_COVR"), "true")) 60 else 10
    while (!any(vapply(values, function(value) identical(value$kind, "end"), logical(1L))) &&
            Sys.time() < deadline) {
        respond(client, "completionItem/resolve", list(label = "progress probe"), retry = FALSE)
        Sys.sleep(0.05)
    }
    expect_length(tokens, 1L)
    expect_identical(values[[1L]]$kind, "begin")
    expect_identical(tail(values, 1L)[[1L]]$kind, "end")
    expect_identical(tail(values, 1L)[[1L]]$message, "Indexed 1 R files")
})

test_that("shutdown ends progress and parse retries keep it active until settled", {
    fixture <- progress_fixture()
    server <- fixture$server
    progress_accept(server)
    on_shutdown(server, 10L, list())
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message, "Server stopped")
    expect_true(server$exit_flag)

    fixture <- progress_fixture()
    server <- fixture$server
    progress_accept(server)
    uri <- path_to_uri(file.path(fixture$root, "failed.R"))
    document <- Document$new(uri, content = "value <- 1")
    document$did_open()
    fixture$workspace$documents$set(uri, document)
    fixture$index$queue_document(uri)
    fixture$index$await_parse(uri)
    task <- parse_task(server, uri, document)
    task$fail(simpleError("worker exited"))
    server$report_workspace_progress(now = 0)
    expect_false(any(vapply(progress_values(server), function(value) value$kind == "end", logical(1L))))
    expect_false(fixture$index$indexing_status()$done)
    server$process_parse_retries()
    tasks <- environment(server$parse_task_manager$add_task)$tasks
    tasks$get(uri)$fail(simpleError("worker exited again"))
    expect_false(fixture$index$document_pending(uri))
    expect_true(fixture$index$parse_failed(uri))
    fixture$index$discovering <- FALSE
    server$report_workspace_progress(now = 1)
    expect_true(fixture$index$indexing_status()$done)
    expect_identical(tail(progress_values(server), 1L)[[1L]]$message,
        "Indexing finished (1 R files failed)")
})
