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

test_that("adding a workspace preserves matching fallback editor buffers", {
    server <- BareLanguageServer$new()
    withr::defer(server$close_connections())
    root <- withr::local_tempdir()
    uri <- path_to_uri(file.path(root, "unsaved.R"))
    fallback <- server$workspaces$get(DEFAULT_WORKSPACE)
    document <- Document$new(uri, version = 3L, content = "unsaved <- 1")
    document$loaded_packages <- "utils"
    document$did_open()
    fallback$documents$set(uri, document)
    expect_identical(server$get_workspace(uri), fallback)

    server$add_workspace(path_to_uri(root))
    workspace <- server$get_workspace(uri)
    expect_false(fallback$documents$has(uri))
    expect_identical(workspace$documents$get(uri, NULL), document)
    expect_equal(document$content, "unsaved <- 1")
    expect_equal(document$version, 3L)
    expect_true(document$is_open)
    expect_true("utils" %in% workspace$loaded_packages)
})

test_that("adding a nested workspace transfers buffers from their current workspace", {
    server <- BareLanguageServer$new()
    withr::defer(server$close_connections())
    root <- withr::local_tempdir()
    nested <- file.path(root, "nested")
    dir.create(nested)
    root_uri <- path_to_uri(root)
    server$add_workspace(root_uri)
    server$rootUri <- root_uri
    parent <- server$get_workspace(root_uri)
    uri <- path_to_uri(file.path(nested, "unsaved.R"))
    writeLines("saved <- 1", path_from_uri(uri))
    document <- Document$new(uri, version = 3L, content = "unsaved <- 1")
    document$did_open()
    parsed <- parse_document(uri, document$content)
    parsed$version <- document$version
    parent$documents$set(uri, document)
    parent$update_parse_data(uri, parsed)
    expect_identical(server$get_workspace(uri), parent)

    server$add_workspace(path_to_uri(nested))
    workspace <- server$get_workspace(uri)
    expect_identical(workspace$documents$get(uri, NULL), document)
    expect_false(parent$documents$has(uri))
    expect_false(parent$index$summaries$has(index_canonical_uri(uri)))
    summary <- workspace$index$summaries$get(index_canonical_uri(uri))
    expect_true("unsaved" %in% names(summary$definitions))
    expect_false("saved" %in% names(summary$definitions))
})

test_that("workspace folder changes retain new unsaved buffer summaries after discovery", {
    old_cache <- lsp_settings$get("index_persistent_cache")
    withr::defer(lsp_settings$set("index_persistent_cache", old_cache))
    lsp_settings$set("index_persistent_cache", FALSE)

    for (background in c(FALSE, TRUE)) {
        server <- BareLanguageServer$new()
        withr::defer(server$close_connections())
        if (background) server$index_task_manager <- new.env(parent = emptyenv())
        root <- withr::local_tempdir()
        root_uri <- path_to_uri(root)
        helper_uri <- path_to_uri(file.path(root, "helper.R"))
        writeLines("helper <- 1", path_from_uri(helper_uri))
        uri <- path_to_uri(file.path(root, "unsaved.R"))
        content <- c("unsaved <- 1", "source('helper.R')")
        document <- Document$new(uri, version = 3L, content = content)
        document$did_open()
        fallback <- server$get_workspace(uri)
        fallback$documents$set(uri, document)
        parsed <- parse_document(uri, content)
        parsed$version <- document$version
        fallback$update_parse_data(uri, parsed)

        workspace_did_change_workspace_folders(server, list(event = list(
            added = list(list(uri = root_uri, name = basename(root))),
            removed = list()
        )))

        workspace <- server$get_workspace(uri)
        expect_identical(workspace$documents$get(uri, NULL), document)
        expect_false(fallback$documents$has(uri))
        expect_false(file.exists(path_from_uri(uri)))
        expect_equal(document$content, content)
        expect_identical(document$version, 3L)
        expect_true(document$is_open)
        summary <- workspace$index$summaries$get(index_canonical_uri(uri), NULL)
        expect_false(is.null(summary))
        expect_true("unsaved" %in% names(summary$definitions))
        expect_false(summary$cacheable)
        expect_equal(summary$content_hash, get_content_hash(content))
        expect_equal(workspace$index$source_edges$get(index_canonical_uri(uri)), helper_uri)
        expect_false(index_canonical_uri(uri) %in% workspace$index$pending)
        server$close_connections()
    }
})

test_that("workspace folder changes restore edited buffers over persistent disk caches", {
    old_cache <- lsp_settings$get("index_persistent_cache")
    withr::defer(lsp_settings$set("index_persistent_cache", old_cache))
    lsp_settings$set("index_persistent_cache", TRUE)
    withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())

    for (background in c(FALSE, TRUE)) {
        server <- BareLanguageServer$new()
        withr::defer(server$close_connections())
        if (background) server$index_task_manager <- new.env(parent = emptyenv())
        root <- withr::local_tempdir()
        root_uri <- path_to_uri(root)
        server$add_workspace(root_uri)
        server$rootUri <- root_uri
        parent <- server$get_workspace(root_uri)
        nested <- file.path(root, "nested")
        dir.create(nested)
        nested_uri <- path_to_uri(nested)
        uri <- path_to_uri(file.path(nested, "edited.R"))
        helper_uri <- path_to_uri(file.path(nested, "helper.R"))
        writeLines("helper <- 1", path_from_uri(helper_uri))
        writeLines("saved <- 1", path_from_uri(uri))
        disk_index <- WorkspaceIndex$new(nested)
        disk_index$discover()
        disk_index$update_path(path_from_uri(uri))
        disk_index$save_cache()
        expect_true(file.exists(disk_index$cache_file()))
        content <- c("unsaved <- 2", "source('helper.R')")
        document <- Document$new(uri, version = 4L, content = content)
        document$did_open()
        parent$documents$set(uri, document)
        parsed <- parse_document(uri, content)
        parsed$version <- document$version
        parent$update_parse_data(uri, parsed)

        workspace_did_change_workspace_folders(server, list(event = list(
            added = list(list(uri = nested_uri, name = basename(nested))),
            removed = list()
        )))

        workspace <- server$get_workspace(uri)
        expect_identical(workspace$documents$get(uri, NULL), document)
        expect_false(parent$documents$has(uri))
        expect_equal(document$content, content)
        expect_identical(document$version, 4L)
        expect_true(document$is_open)
        summary <- workspace$index$summaries$get(index_canonical_uri(uri))
        expect_true("unsaved" %in% names(summary$definitions))
        expect_false("saved" %in% names(summary$definitions))
        expect_false(summary$cacheable)
        expect_equal(summary$content_hash, get_content_hash(content))
        expect_equal(workspace$index$source_edges$get(index_canonical_uri(uri)), helper_uri)
        expect_false(index_canonical_uri(uri) %in% workspace$index$pending)
        workspace$index$save_cache()
        cached <- readRDS(workspace$index$cache_file())
        expect_false(any(vapply(cached$summaries, function(item) {
            identical(item$uri, index_canonical_uri(uri))
        }, logical(1L))))
        server$close_connections()
    }
})

test_that("removing a nested workspace transfers open buffers to their parent", {
    old_diagnostics <- lsp_settings$get("diagnostics")
    withr::defer(lsp_settings$set("diagnostics", old_diagnostics))
    lsp_settings$set("diagnostics", FALSE)
    server <- BareLanguageServer$new()
    withr::defer(server$close_connections())
    root <- format(fs::path(withr::local_tempdir()))
    nested <- file.path(root, "nested")
    dir.create(nested)
    root_uri <- path_to_uri(root)
    nested_uri <- path_to_uri(nested)
    server$add_workspace(root_uri)
    server$rootUri <- root_uri
    server$add_workspace(nested_uri)
    workspace <- server$get_workspace(nested_uri)
    uri <- path_to_uri(file.path(nested, "unsaved.R"))
    document <- Document$new(uri, version = 3L, content = "unsaved <- 1")
    document$loaded_packages <- "tools"
    document$did_open()
    workspace$documents$set(uri, document)
    expect_identical(server$get_workspace(uri), workspace)

    server$remove_workspace(nested_uri)
    parent <- server$get_workspace(uri)
    expect_identical(parent$root, root)
    expect_identical(parent$documents$get(uri, NULL), document)
    expect_true("tools" %in% parent$loaded_packages)
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

test_that("Pending providers rotate between documents and methods within a dispatch budget", {
    server <- BareLanguageServer$new()
    withr::defer(server$close_connections())
    seen <- integer()
    server$request_handlers <- list(first = function(self, id, params) seen <<- c(seen, id),
        second = function(self, id, params) seen <<- c(seen, id))
    workspace <- server$workspaces$get(DEFAULT_WORKSPACE)
    uris <- c("file:///pending-first.R", "file:///pending-second.R")
    for (uri in uris) {
        document <- Document$new(uri, version = 1L, content = "value <- 1")
        parsed <- parse_document(uri, document$content)
        parsed$version <- 1L
        document$update_parse_data(parsed)
        workspace$documents$set(uri, document)
        queues <- list(first = collections::queue(), second = collections::queue())
        offset <- if (uri == uris[[1L]]) 0L else 10L
        for (id in 1:3) queues$first$push(list(id = offset + id, version = 1L, params = NULL))
        queues$second$push(list(id = offset + 4L, version = 1L, params = NULL))
        server$pending_replies$set(uri, queues)
        server$queue_pending_replies(uri)
        server$queue_pending_replies(uri)
    }
    expect_identical(server$process_pending_replies(4L, Inf), 4L)
    expect_identical(seen, c(1L, 11L, 4L, 14L))
    expect_equal(server$input_wait_timeout(), 0)
    expect_identical(server$process_pending_replies(20L, Inf), 4L)
    expect_identical(seen, c(1L, 11L, 4L, 14L, 2L, 12L, 3L, 13L))
    expect_equal(server$input_wait_timeout(), 0.01)

    server$pending_replies$get(uris[[1L]])$first$push(list(id = 99L, version = 1L, params = NULL))
    server$queue_pending_replies(uris[[1L]])
    workspace$documents$get(uris[[1L]])$set_content(2L, "value <- 2")
    expect_identical(server$process_pending_replies(), 0L)
    expect_false(99L %in% seen)
    expect_equal(server$pending_replies$get(uris[[1L]])$first$size(), 1L)
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

test_that("process_events schedules diagnostics for open documents when NAMESPACE changes", {
    old_diagnostics <- lsp_settings$get("diagnostics")
    withr::defer(lsp_settings$set("diagnostics", old_diagnostics))

    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"), recursive = TRUE)
    writeLines(c("Package: nspollpkg", "Version: 0.1.0"), file.path(root, "DESCRIPTION"))
    namespace_file <- file.path(root, "NAMESPACE")
    writeLines("importFrom(utils, head)", namespace_file)

    server <- BareLanguageServer$new()
    withr::defer(server$close_connections())
    server$rootPath <- root

    noop_manager <- list(
        check_tasks = function() NULL,
        run_tasks = function() NULL,
        has_work = function() FALSE
    )
    added_tasks <- collections::dict()
    diag_manager <- list(
        check_tasks = function() NULL,
        run_tasks = function() NULL,
        has_work = function() FALSE,
        add_task = function(id, task) added_tasks$set(id, task)
    )
    server$parse_task_manager <- noop_manager
    server$diagnostics_task_manager <- diag_manager
    server$resolve_task_manager <- noop_manager
    server$formatting_task_manager <- noop_manager

    workspace <- Workspace$new(root)
    workspace$import_from_namespace_file()
    doc_path <- file.path(root, "R", "main.R")
    writeLines("f <- function(x) rnorm(x)", doc_path)
    doc_uri <- path_to_uri(doc_path)
    doc <- Document$new(doc_uri, content = "f <- function(x) rnorm(x)")
    doc$did_open()
    workspace$documents$set(doc_uri, doc)
    server$workspaces$set(path_to_uri(root), workspace)

    writeLines(c("importFrom(utils, head)", "importFrom(stats, rnorm)"), namespace_file)
    Sys.setFileTime(namespace_file, Sys.time() - 2)
    workspace$namespace_file_mt <- Sys.time() - 10

    lsp_settings$set("diagnostics", TRUE)
    expect_no_error(server$process_events())
    expect_equal(workspace$imported_objects$get("rnorm"), "stats")
    expect_true(added_tasks$has(doc_uri))
    expect_s3_class(added_tasks$get(doc_uri), "Task")

    added_tasks$clear()
    writeLines("importFrom(stats, sd)", namespace_file)
    Sys.setFileTime(namespace_file, Sys.time() - 2)
    workspace$namespace_file_mt <- Sys.time() - 10

    lsp_settings$set("diagnostics", FALSE)
    expect_no_error(server$process_events())
    expect_equal(workspace$imported_objects$get("sd"), "stats")
    expect_false(added_tasks$has(doc_uri))
})

test_that("busy parse workers do not starve diagnostics or background indexes", {
    server <- BareLanguageServer$new()
    withr::defer(server$close_connections())
    counts <- new.env(parent = emptyenv())
    counts$diagnostics <- counts$index <- 0L
    noop <- list(check_tasks = function() NULL, run_tasks = function() NULL,
        has_work = function() TRUE)
    server$parse_task_manager <- noop
    server$resolve_task_manager <- noop
    server$formatting_task_manager <- noop
    server$diagnostics_task_manager <- list(check_tasks = function() NULL,
        run_tasks = function() counts$diagnostics <- counts$diagnostics + 1L)
    server$index_task_manager <- list(check_tasks = function() NULL,
        run_tasks = function() counts$index <- counts$index + 1L)
    server$process_events()
    expect_equal(counts$diagnostics, 1L)
    expect_equal(counts$index, 1L)
})
