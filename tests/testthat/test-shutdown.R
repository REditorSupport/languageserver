ShutdownLanguageServer <- R6::R6Class(
    "ShutdownLanguageServer",
    inherit = LanguageServer,
    private = list(
        finalize = function() self$request_callbacks$clear()
    ),
    public = list(
        deliveries = NULL,
        initialize = function() {
            self$exit_flag <- FALSE
            self$deliveries <- list()
            self$workspaces <- collections::dict()
            self$pending_replies <- collections::dict()
            self$request_callbacks <- collections::dict()
            self$register_handlers()
        },
        deliver = function(message, callback = NULL) {
            self$deliveries[[length(self$deliveries) + 1L]] <- message
        }
    )
)

test_that("shutdown stops every worker before replying while leaving exit pending", {
    server <- ShutdownLanguageServer$new()
    stopped <- character()
    managers <- c("parse", "diagnostics", "resolve", "member", "formatting", "index")
    for (name in managers) {
        manager <- new.env(parent = emptyenv())
        manager$stop <- local({
            task_name <- name
            function() {
                expect_length(server$deliveries, 0L)
                stopped <<- c(stopped, task_name)
            }
        })
        server[[paste0(name, "_task_manager")]] <- manager
    }
    server$handle_request(list(id = "stop", method = "shutdown", params = NULL))
    expect_identical(stopped, managers)
    expect_true(server$shutdown_requested)
    expect_false(server$exit_flag)
    expect_length(server$deliveries, 1L)
    expect_identical(server$deliveries[[1L]]$id, "stop")
    expect_null(server$deliveries[[1L]]$result)

    server$handle_notification(list(method = "exit"))
    expect_true(server$exit_flag)
})

test_that("a shut down server rejects requests and suppresses other incoming work", {
    server <- ShutdownLanguageServer$new()
    calls <- character()
    server$request_handlers$probe <- function(...) calls <<- c(calls, "request")
    server$notification_handlers$probe <- function(...) calls <<- c(calls, "notification")
    server$request_callbacks$set(request_id_key(10L), function(...) calls <<- c(calls, "response"))

    server$handle_request(list(id = 1L, method = "shutdown"))
    for (method in c("probe", "unknown", "shutdown")) {
        server$handle_request(list(id = method, method = method))
        response <- tail(server$deliveries, 1L)[[1L]]
        expect_identical(response$id, method)
        expect_identical(response$error$code, ErrorCodes$InvalidRequest)
    }
    server$handle_notification(list(method = "probe"))
    server$handle_notification(list(method = "initialized"))
    server$handle_response(list(id = 10L, result = NULL))
    expect_length(calls, 0L)
    expect_false(server$exit_flag)
    server$handle_notification(list(method = "exit"))
    expect_true(server$exit_flag)
})

test_that("the event loop waits for a later exit without processing queued background work", {
    events <- character()
    messages <- list(Request$new(1L, "shutdown", NULL)$to_json(), NULL, NULL,
        Notification$new("exit", NULL)$to_json())
    message_number <- 0L
    server_class <- R6::R6Class(
        inherit = ShutdownLanguageServer,
        public = list(
            fetch = function(...) {
                message_number <<- message_number + 1L
                if (message_number > length(messages)) stop("exit was not handled")
                messages[[message_number]]
            },
            process_events = function() events <<- c(events, "background"),
            wait_input = function(timeout) {
                events <<- c(events, paste0("wait:", timeout))
                TRUE
            }
        )
    )
    server <- server_class$new()
    server$queue_pending_replies("file:///queued.R")
    manager <- new.env(parent = emptyenv())
    manager$wait_timeout <- function(...) stop("stopped workers must not determine the wait")
    server$parse_task_manager <- manager

    server$run()
    expect_true(server$shutdown_requested)
    expect_true(server$exit_flag)
    expect_identical(message_number, 4L)
    expect_identical(events, "wait:0.01")
    expect_identical(server$input_wait_timeout(0.05), 0.05)
})

shutdown_client <- function(.local_envir = parent.frame()) {
    script <- "options(languageserver.formatting_style = NULL); languageserver::run()"
    source_path <- getNamespaceInfo(asNamespace("languageserver"), "path")
    if (file.exists(file.path(source_path, "R", "languageserver.R"))) {
        # Development tests must exercise this checkout rather than a stale
        # installed copy. Package checks use their installed test library.
        script <- paste0("pkgload::load_all(", encodeString(source_path, quote = '"'),
            ", quiet = TRUE); ", script)
    }
    client <- LanguageClient$new(file.path(R.home("bin"), "R"), c("--no-echo", "-e", script))
    withr::defer({
        if (client$process$is_alive()) client$process$kill_tree()
    }, envir = .local_envir)
    client$start(working_dir = NULL)
    data <- client$fetch(blocking = TRUE, timeout = worker_timeout(15, covr = 60))
    expect_false(is.null(data))
    client$handle_raw(data)
    client
}

shutdown_response <- function(client) {
    request <- client$request("shutdown", NULL)
    client$deliver(request)
    data <- client$fetch(blocking = TRUE, timeout = worker_timeout(15, covr = 60))
    expect_false(is.null(data))
    response <- jsonlite::fromJSON(data, simplifyVector = FALSE)
    expect_equal(response$id, request$id)
    expect_null(response$error)
    expect_true("result" %in% names(response))
    expect_null(response$result)
    response
}

test_that("stdio remains connected after the shutdown response until exit arrives", {
    skip_on_cran()
    client <- shutdown_client()
    shutdown_response(client)
    client$process$wait(100)
    expect_true(client$process$is_alive())

    request <- client$request("completionItem/resolve", list(label = "late request"))
    client$deliver(request)
    data <- client$fetch(blocking = TRUE, timeout = worker_timeout(5, covr = 30))
    expect_false(is.null(data))
    response <- jsonlite::fromJSON(data, simplifyVector = FALSE)
    expect_equal(response$id, request$id)
    expect_equal(response$error$code, ErrorCodes$InvalidRequest)
    expect_silent(notify(client, "exit"))
    client$process$wait(worker_timeout(5, covr = 60) * 1000)
    expect_false(client$process$is_alive())
    expect_identical(client$process$get_exit_status(), 0L)
})

test_that("closing stdio after shutdown still terminates the server", {
    skip_on_cran()
    client <- shutdown_client()
    shutdown_response(client)
    expect_true(client$process$is_alive())
    processx::processx_conn_close(client$process$get_input_connection())
    client$process$wait(worker_timeout(5, covr = 60) * 1000)
    expect_false(client$process$is_alive())
    expect_identical(client$process$get_exit_status(), 0L)
})
