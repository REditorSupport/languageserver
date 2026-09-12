BareLanguageServer <- R6::R6Class(
    "BareLanguageServer",
    inherit = LanguageServer,
    private = list(
        close_connection = function(connection) {
            tryCatch({
                if (isOpen(connection)) {
                    close(connection)
                }
            }, error = function(e) NULL)
        },
        finalize = function() {
            input <- self$inputcon
            output <- self$outputcon
            # Explicit cleanup is followed by R6 finalization during GC. R
            # reuses connection numbers, so release the old handles before
            # another cleanup can mistake a new connection for an owned one.
            self$inputcon <- NULL
            self$outputcon <- NULL
            if (!is.null(input)) private$close_connection(input)
            if (!is.null(output) && !identical(output, input)) {
                private$close_connection(output)
            }
            self$request_callbacks$clear()
        }
    ),
    public = list(
        initialize = function() {
            self$inputcon <- rawConnection(raw(), open = "r+")
            self$outputcon <- textConnection(NULL, open = "w")
            self$exit_flag <- FALSE
            self$pending_replies <- collections::dict()
            self$workspaces <- collections::dict()
            self$workspace_cache <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, Workspace$new(NULL))
            self$rootUri <- character()
            self$request_callbacks <- collections::dict()
            self$register_handlers()
        },
        close_connections = function() {
            private$finalize()
        }
    )
)

ErrorLanguageServer <- R6::R6Class(
    "ErrorLanguageServer",
    inherit = BareLanguageServer,
    public = list(
        stops = NULL,
        initialize = function() {
            super$initialize()
            self$stops <- new.env(parent = emptyenv())
            self$stops$count <- 0L
            manager <- new.env(parent = baseenv())
            manager$stop <- function() {
                self$stops$count <- self$stops$count + 1L
            }
            self$parse_task_manager <- manager
            self$diagnostics_task_manager <- manager
            self$resolve_task_manager <- manager
        },
        fetch = function(...) NULL,
        process_events = function() stop("event loop failed")
    )
)

test_that("Server fixture cleanup leaves subsequently opened connections alive", {
    previous <- BareLanguageServer$new()
    previous$close_connections()

    current <- BareLanguageServer$new()
    withr::defer(current$close_connections())
    # Both explicit cleanup and the later GC finalizer must be harmless even
    # after the old connection numbers have been reused by another fixture.
    previous$close_connections()
    expect_true(isOpen(current$inputcon))
    expect_true(isOpen(current$outputcon))
    rm(previous)
    invisible(gc())
    expect_true(isOpen(current$inputcon))
    expect_true(isOpen(current$outputcon))
})

test_that("LanguageServer removes workspaces and preserves open documents", {
    old_diagnostics <- lsp_settings$get("diagnostics")
    withr::defer(lsp_settings$set("diagnostics", old_diagnostics))
    lsp_settings$set("diagnostics", FALSE)
    server <- BareLanguageServer$new()
    withr::defer(server$close_connections())
    root <- withr::local_tempdir()
    uri <- path_to_uri(root)
    workspace <- Workspace$new(root)
    open_uri <- path_to_uri(file.path(root, "open.R"))
    closed_uri <- path_to_uri(file.path(root, "closed.R"))
    open_document <- Document$new(open_uri, content = "open <- TRUE")
    open_document$did_open()
    workspace$documents$set(open_uri, open_document)
    workspace$documents$set(
        closed_uri,
        Document$new(closed_uri, content = "closed <- TRUE")
    )
    server$workspaces$set(uri, workspace)
    server$workspace_cache$set(open_uri, workspace)

    expect_null(server$remove_workspace(character()))
    server$remove_workspace(uri)

    expect_false(server$workspaces$has(uri))
    expect_true(
        server$workspaces$get(DEFAULT_WORKSPACE)$documents$has(open_uri)
    )
    expect_false(
        server$workspaces$get(DEFAULT_WORKSPACE)$documents$has(closed_uri)
    )
    expect_equal(server$workspace_cache$size(), 0L)
})

test_that("LanguageServer detects closed input and reads UTF-8 TCP bytes", {
    server <- BareLanguageServer$new()
    withr::defer(server$close_connections())
    close(server$inputcon)
    server$inputcon <- file(tempfile())
    server$check_connection()
    expect_true(server$exit_flag)

    closed_input <- server$inputcon
    utf8_input <- rawConnection(charToRaw("\u00e9"), open = "rb")
    server$inputcon <- utf8_input
    server$tcp <- TRUE
    expect_equal(server$read_char(2L), "\u00e9")

    server$inputcon <- rawConnection(raw(), open = "r+")
    close(utf8_input)
    close(closed_input)
})

test_that("LanguageServer stops managers after an event loop error", {
    old_log_file <- lsp_settings$get("log_file")
    withr::defer(lsp_settings$set("log_file", old_log_file))
    lsp_settings$set("log_file", withr::local_tempfile())

    server <- ErrorLanguageServer$new()
    withr::defer(server$close_connections())
    expect_null(server$run())
    expect_equal(server$stops$count, 3L)
})

test_that("LanguageServer drains queued input before background callbacks", {
    events <- character()
    messages <- collections::queue()
    for (message in c("edit", "completion", "shutdown")) messages$push(message)
    server_class <- R6::R6Class(
        inherit = BareLanguageServer,
        public = list(
            fetch = function(...) {
                if (messages$size()) messages$pop() else NULL
            },
            handle_raw = function(data) {
                events <<- c(events, data)
                if (data == "shutdown") self$exit_flag <- TRUE
            },
            process_events = function() events <<- c(events, "background")
        )
    )
    server <- server_class$new()
    withr::defer(server$close_connections())
    server$run()
    expect_identical(events, c("edit", "completion", "shutdown"))

    # A busy client cannot starve workers indefinitely.
    server$exit_flag <- FALSE
    events <- character()
    for (i in seq_len(25L)) messages$push(as.character(i))
    expect_identical(server$process_input(), 20L)
    expect_identical(messages$size(), 5L)
    server$process_events()
    expect_identical(server$process_input(), 5L)
    expect_identical(events, c(as.character(1:20), "background", as.character(21:25)))
    expect_identical(server$process_input(), 0L)
})

test_that("run configures boolean and file debug modes", {
    old_debug <- lsp_settings$get("debug")
    old_log_file <- lsp_settings$get("log_file")
    withr::defer({
        lsp_settings$set("debug", old_debug)
        lsp_settings$set("log_file", old_log_file)
    })
    fake_server <- new.env(parent = emptyenv())
    fake_server$runs <- 0L
    fake_server$run <- function() {
        fake_server$runs <- fake_server$runs + 1L
    }
    runner <- run
    stub(runner, "LanguageServer$new", function(...) fake_server)

    runner(debug = TRUE)
    expect_true(lsp_settings$get("debug"))
    expect_null(lsp_settings$get("log_file"))

    log_file <- withr::local_tempfile()
    runner(debug = log_file)
    expect_equal(lsp_settings$get("log_file"), log_file)
    expect_equal(fake_server$runs, 2L)
})
