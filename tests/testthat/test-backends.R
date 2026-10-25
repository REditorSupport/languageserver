local_backend_settings <- function(..., .env = parent.frame()) {
    values <- list(...)
    previous <- lapply(names(values), lsp_settings$get)
    names(previous) <- names(values)
    withr::defer(for (key in names(previous)) lsp_settings$set(key, previous[[key]]), envir = .env)
    for (key in names(values)) lsp_settings$set(key, values[[key]])
}

fake_backend <- function(id = "fixture", run = function(request) list(),
    modes = c("document", "range", "ranges", "on_type")) {
    list(id = id, run = run, document_types = c("r", "literate"), modes = modes)
}

backend_fixture <- function(content = "x=1", uri = "untitled:backends") {
    self <- new.env(parent = baseenv())
    workspace <- new.env(parent = baseenv())
    workspace$root <- withr::local_tempdir(.local_envir = parent.frame())
    workspace$documents <- collections::dict()
    workspace$diagnostics_cache <- ByteLruCache$new(1024^2)
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    document$did_open()
    document$parsed_content <- document$content
    document$parse_data <- list(version = document$version)
    workspace$documents$set(uri, document)
    self$get_workspace <- function(...) workspace
    self$workspaces <- collections::dict()
    self$workspaces$set("root", workspace)
    self$deliveries <- list()
    self$deliver <- function(message) self$deliveries[[length(self$deliveries) + 1L]] <- message
    self$diagnostics_task_manager <- TaskManager$new("backend-diagnostics")
    self$formatting_task_manager <- TaskManager$new("backend-formatting")
    self$formatting_requests <- collections::dict()
    self$backend_poll_time <- -Inf
    withr::defer({
        self$diagnostics_task_manager$stop()
        self$formatting_task_manager$stop()
    }, envir = parent.frame())
    list(self = self, workspace = workspace, document = document, uri = uri)
}

test_that("adapter descriptors and execution options are validated", {
    expect_error(backend_descriptor("diagnostics", "unknown"), "Unknown diagnostics")
    expect_error(backend_descriptor("formatting", fake_backend(modes = "whole")), "Invalid")
    expect_error(backend_descriptor("diagnostics", list(id = "x", run = identity)), "Invalid")
    invalid <- fake_backend()
    invalid$package <- "lintr"
    invalid$version <- "invalid"
    expect_error(backend_descriptor("diagnostics", invalid), "Invalid")
    invalid$package <- NULL
    invalid$executable <- c("first", "second")
    expect_error(backend_descriptor("diagnostics", invalid), "Invalid")
    fixture <- backend_fixture()
    local_backend_settings(diagnostics_backend = fake_backend(), backend_options = list(fixture = list(timeout = -1)))
    expect_error(backend_request("diagnostics", fixture$uri, fixture$document), "timeout")
    lsp_settings$set("backend_options", list(fixture = list("unnamed")))
    expect_error(backend_request("diagnostics", fixture$uri, fixture$document), "named list")
    lsp_settings$set("backend_options", list(fixture = list(env = list(KEY = c("a", "b")))))
    expect_error(backend_request("diagnostics", fixture$uri, fixture$document), "named strings")
})

test_that("invalid global execution options cancel obsolete work and report an error", {
    fixture <- backend_fixture()
    local_backend_settings(diagnostics_backend = fake_backend(), formatting_backend = fake_backend(),
        backend_options = list(), diagnostics = TRUE)
    withr::local_options(languageserver.backend_options = NULL)
    task <- diagnostics_task(fixture$self, fixture$uri, fixture$document)
    fixture$self$diagnostics_task_manager$add_task(fixture$uri, task)
    expect_no_error(workspace_did_change_configuration(fixture$self,
            list(settings = list(backend_options = TRUE))))
    expect_false(fixture$self$diagnostics_task_manager$has_work())
    expect_equal(tail(fixture$self$deliveries, 1)[[1L]]$method, "window/showMessage")
})

test_that("disabled diagnostics and unused formatting do not load R backends during polling", {
    fixture <- backend_fixture()
    local_backend_settings(diagnostics_backend = "lintr", formatting_backend = "styler",
        backend_options = list(), diagnostics = FALSE)
    state <- new.env(parent = emptyenv())
    state$loads <- character()
    testthat::local_mocked_bindings(backend_r_options = function(descriptor, load = TRUE) {
        if (load) state$loads <- c(state$loads, descriptor$id)
        list()
    }, .package = "languageserver")
    backend_settings_identity()
    poll_backend_configuration(fixture$self, force = TRUE)
    expect_length(state$loads, 0L)
})

test_that("lintr options are snapshotted without loading the backend in the server", {
    snapshot <- backend_r_options
    stub(snapshot, "requireNamespace", function(...) stop("Unexpected namespace loading"))
    options <- snapshot(backend_descriptor("diagnostics", "lintr"))
    expect_true(backend_string(options[["lintr.linter_file"]]))
})

test_that("diagnostics re-enabled with an index rebuild use the replacement index", {
    fixture <- backend_fixture()
    adapter <- fake_backend()
    adapter$needs_workspace_globals <- TRUE
    local_backend_settings(diagnostics_backend = adapter, formatting_backend = fake_backend(),
        backend_options = list(), diagnostics = FALSE, index_mode = "auto")
    withr::local_options(languageserver.diagnostics = NULL, languageserver.index_mode = NULL)
    fixture$workspace$index <- list(enabled = TRUE, detach_worker = function() NULL,
        diagnostics_generation = function(uri) list(index = "retired"))
    fixture$workspace$get_diagnostics_globals <- function(...) stop("Diagnostics used the retired index")
    fixture$self$load_workspace <- function(workspace) NULL
    fixture$self$prune_legacy_documents <- function(workspace) NULL
    expect_no_error(workspace_did_change_configuration(fixture$self,
            list(settings = list(diagnostics = TRUE, index_mode = "off"))))
    expect_false(fixture$workspace$index$enabled)
    task <- fixture$self$diagnostics_task_manager$.__enclos_env__$private$pending_tasks$get(fixture$uri)
    expect_s3_class(task, "Task")
    expect_null(task$.__enclos_env__$private$args$request$globals)
})

test_that("polling invalid option changes clears diagnostics and cancels formatting", {
    fixture <- backend_fixture()
    local_backend_settings(diagnostics_backend = fake_backend(), formatting_backend = fake_backend(),
        backend_options = list(), diagnostics = TRUE)
    task <- diagnostics_task(fixture$self, fixture$uri, fixture$document)
    fixture$self$diagnostics_task_manager$add_task(fixture$uri, task)
    enqueue_formatting(fixture$self, 1L, fixture$uri, fixture$document, list(tabSize = 2L))
    lsp_settings$set("backend_options", TRUE)
    poll_backend_configuration(fixture$self, force = TRUE)
    expect_false(fixture$self$diagnostics_task_manager$has_work())
    expect_false(fixture$self$formatting_task_manager$has_work())
    expect_equal(fixture$self$formatting_requests$size(), 0L)
    lsp_settings$set("backend_options", list())
    poll_backend_configuration(fixture$self, force = TRUE)
    expect_true(fixture$self$diagnostics_task_manager$has_work())
})

test_that("custom adapters receive immutable contents, context and explicit sources", {
    fixture <- backend_fixture(c("old", ""))
    adapter <- fake_backend(run = function(request) {
        list(list(range = range(position(0L, 0L), position(0L, 1L)),
                message = request$content[[1L]]))
    })
    local_backend_settings(diagnostics_backend = adapter, backend_options = list())
    request <- backend_request("diagnostics", fixture$uri, fixture$document, fixture$workspace)
    fixture$document$set_content(2L, "new")
    expect_equal(request$content, c("old", ""))
    expect_equal(request$workspace_root, fixture$workspace$root)
    expect_false(file.exists(request$path))
    result <- run_backend(request)
    expect_equal(result[[1L]]$message, "old")
    expect_equal(result[[1L]]$source, "fixture")
    expect_false(inherits(result, "Response"))
})

test_that("stale document, backend and configuration results are not cached or published", {
    for (change in c("content", "reopened", "backend", "config", "workspace")) {
        fixture <- backend_fixture()
        path <- file.path(fixture$workspace$root, "buffer.R")
        fixture$uri <- path_to_uri(path)
        fixture$document$uri <- fixture$uri
        fixture$workspace$documents$clear()
        fixture$workspace$documents$set(fixture$uri, fixture$document)
        adapter <- fake_backend()
        adapter$config_files <- "fixture.toml"
        local_backend_settings(diagnostics_backend = adapter, backend_options = list(), diagnostics_cache_ttl = 60)
        task <- diagnostics_task(fixture$self, fixture$uri, fixture$document)
        switch(change,
            content = fixture$document$set_content(1L, "new"),
            reopened = fixture$workspace$documents$set(fixture$uri,
                Document$new(fixture$uri, language = "r", version = 1L, content = "x=1")),
            backend = lsp_settings$set("diagnostics_backend", fake_backend("replacement")),
            config = writeLines("changed", file.path(fixture$workspace$root, "fixture.toml")),
            workspace = fixture$self$get_workspace <- function(...) list(documents = collections::dict()))
        task$.__enclos_env__$private$callback(list())
        expect_length(fixture$self$deliveries, 0L)
        expect_equal(fixture$workspace$diagnostics_cache$size(), 0L, info = change)
    }
})

test_that("cache identity ignores instrumentation counters but tracks adapter replacement", {
    state <- new.env(parent = baseenv())
    state$counter <- 0L
    adapter <- function(request) list()
    environment(adapter) <- state
    signature <- list(adapter = list(id = "fixture", run = adapter))
    first <- diagnostics_cache_key("untitled:cache", "x", signature)
    state$counter <- 1L
    expect_identical(diagnostics_cache_key("untitled:cache", "x", signature), first)
    replacement <- function(request) list()
    environment(replacement) <- new.env(parent = baseenv())
    signature$adapter$run <- replacement
    expect_false(identical(diagnostics_cache_key("untitled:cache", "x", signature), first))
})

test_that("backend switching, re-enable and file changes refresh open diagnostics", {
    fixture <- backend_fixture()
    withr::local_dir(fixture$workspace$root)
    local_backend_settings(diagnostics_backend = fake_backend(), formatting_backend = fake_backend(),
        backend_options = list(), diagnostics = TRUE)
    diagnostics_task(fixture$self, fixture$uri, fixture$document)
    workspace_did_change_configuration(fixture$self, list(settings = list(diagnostics = FALSE)))
    expect_false(fixture$self$diagnostics_task_manager$has_work())
    expect_equal(tail(fixture$self$deliveries, 1)[[1L]]$params$diagnostics, list())
    workspace_did_change_configuration(fixture$self, list(settings = list(diagnostics = TRUE)))
    expect_true(fixture$self$diagnostics_task_manager$has_work())
    expect_equal(backend_generation(fixture$self, "diagnostics"), 2L)

    adapter <- fake_backend()
    adapter$config_files <- "fixture.toml"
    lsp_settings$set("diagnostics_backend", adapter)
    diagnostics_task(fixture$self, fixture$uri, fixture$document)
    # The synthetic pathless policy uses cwd; no original file is created.
    request <- backend_request("diagnostics", fixture$uri, fixture$document, fixture$workspace)
    config <- file.path(request$working_directory, "fixture.toml")
    withr::defer(unlink(config))
    writeLines("changed", config)
    poll_backend_configuration(fixture$self, force = TRUE)
    expect_true(fixture$self$diagnostics_task_manager$has_work())
})

test_that("effective option precedence does not cancel masked backend settings", {
    fixture <- backend_fixture()
    local_backend_settings(diagnostics_backend = fake_backend(), formatting_backend = fake_backend(),
        backend_options = list(), diagnostics = TRUE)
    withr::local_options(languageserver.diagnostics_backend = fake_backend())
    previous <- backend_generation(fixture$self, "diagnostics")
    workspace_did_change_configuration(fixture$self, list(settings = list(diagnostics_backend = "jarl")))
    expect_equal(backend_generation(fixture$self, "diagnostics"), previous)
    expect_length(fixture$self$deliveries, 0L)
})

test_that("formatting cancels obsolete configuration once and respects modes", {
    fixture <- backend_fixture()
    local_backend_settings(formatting_backend = fake_backend(), backend_options = list())
    enqueue_formatting(fixture$self, 1L, fixture$uri, fixture$document, list(tabSize = 2L))
    old_task <- fixture$self$formatting_task_manager$.__enclos_env__$private$pending_tasks$get("1")
    workspace_did_change_configuration(fixture$self, list(settings = list(formatting_backend = "air")))
    expect_equal(fixture$self$deliveries[[1L]]$error$code, -32800L)
    old_task$.__enclos_env__$private$callback(list())
    expect_length(fixture$self$deliveries, 1L)
    expect_false(fixture$self$formatting_task_manager$has_work())
    enqueue_formatting(fixture$self, 2L, fixture$uri, fixture$document, list(tabSize = 2L),
        range = list(start = list(row = 0L, col = 0L), end = list(row = 0L, col = 3L)))
    expect_equal(fixture$self$deliveries[[2L]]$error$code, -32803L)
    expect_match(fixture$self$deliveries[[2L]]$error$message, "does not support range formatting")
    expect_false(fixture$self$formatting_task_manager$has_work())
    document <- Document$new(fixture$uri, language = "r", content = c("f(", ""))
    reply <- on_type_formatting_reply(3L, fixture$uri, document, list(row = 1L, col = 0L),
        "\n", list(tabSize = 2L, insertSpaces = TRUE))
    expect_equal(reply$result[[1L]]$newText, "  ")
})

test_that("formatting capabilities match the selected backend modes", {
    local_backend_settings(formatting_backend = "air")
    capabilities <- update_server_capabilities(ServerCapabilities, list())
    expect_true(capabilities$documentFormattingProvider)
    expect_false(capabilities$documentRangeFormattingProvider)
    expect_equal(capabilities$documentOnTypeFormattingProvider, list(firstTriggerCharacter = "\n"))
    lsp_settings$set("formatting_backend", "styler")
    capabilities <- update_server_capabilities(ServerCapabilities, list())
    expect_true(capabilities$documentRangeFormattingProvider$rangesSupport)
    lsp_settings$set("formatting_backend", fake_backend(modes = "range"))
    capabilities <- update_server_capabilities(ServerCapabilities, list())
    expect_false(capabilities$documentFormattingProvider)
    expect_false(capabilities$documentRangeFormattingProvider$rangesSupport)
    expect_null(capabilities$documentOnTypeFormattingProvider)
})

test_that("unsupported formatting requests fail explicitly after a backend change", {
    fixture <- backend_fixture()
    local_backend_settings(formatting_backend = "air", backend_options = list())
    request <- backend_request("formatting", fixture$uri, fixture$document,
        mode = "ranges", ranges = list())
    error <- tryCatch(run_backend(request), error = identity)
    expect_s3_class(error, "backend_unsupported")
    reply <- on_type_formatting_reply(1L, fixture$uri, fixture$document,
        list(row = 0L, col = 0L), ")", list(tabSize = 2L))
    expect_equal(reply$error$code, -32803L)
})

test_that("lintr R configs and GitHub locations invalidate idle diagnostics with native precedence", {
    skip_if_not_installed("lintr", "3.3.0.1")
    fixture <- backend_fixture(c("value = 1", ""))
    withr::local_dir(fixture$workspace$root)
    withr::local_options(lintr.linter_file = ".lintr")
    local_backend_settings(diagnostics_backend = "lintr", formatting_backend = "styler", backend_options = list(),
        diagnostics = TRUE, diagnostics_cache_ttl = 0)
    nested <- file.path(fixture$workspace$root, "nested")
    dir.create(nested)
    fixture$uri <- path_to_uri(file.path(nested, "buffer.R"))
    fixture$document$uri <- fixture$uri
    fixture$workspace$documents$clear()
    fixture$workspace$documents$set(fixture$uri, fixture$document)
    config <- file.path(fixture$workspace$root, ".lintr")
    r_config <- paste0(config, ".R")
    writeLines("linters: list()", config)
    diagnostics_task(fixture$self, fixture$uri, fixture$document)
    writeLines("linters <- list(assignment_linter())", r_config)
    poll_backend_configuration(fixture$self, force = TRUE)
    task <- fixture$self$diagnostics_task_manager$.__enclos_env__$private$pending_tasks$get(fixture$uri)
    expect_equal(run_backend(task$.__enclos_env__$private$args$request)[[1L]]$code, "assignment_linter")
    expect_true(normalizePath(r_config, winslash = "/") %in%
            task$.__enclos_env__$private$args$request$signature$configuration$paths)
    expect_equal(asNamespace("lintr")$find_config(path_from_uri(fixture$uri)), normalizePath(r_config, winslash = "/"))

    unlink(c(config, r_config))
    github <- file.path(nested, ".github", "linters")
    dir.create(github, recursive = TRUE)
    github_config <- file.path(github, ".lintr")
    writeLines("linters: list()", github_config)
    poll_backend_configuration(fixture$self, force = TRUE)
    task <- fixture$self$diagnostics_task_manager$.__enclos_env__$private$pending_tasks$get(fixture$uri)
    expect_length(run_backend(task$.__enclos_env__$private$args$request), 0L)
    writeLines("linters <- list(assignment_linter())", paste0(github_config, ".R"))
    backend_watched_configuration(fixture$self, paste0(github_config, ".R"))
    task <- fixture$self$diagnostics_task_manager$.__enclos_env__$private$pending_tasks$get(fixture$uri)
    expect_equal(run_backend(task$.__enclos_env__$private$args$request)[[1L]]$code, "assignment_linter")
    expect_equal(asNamespace("lintr")$find_config(path_from_uri(fixture$uri)),
        normalizePath(paste0(github_config, ".R"), winslash = "/"))
})

test_that("missing backends report availability once without disabling language features", {
    fixture <- backend_fixture()
    local_backend_settings(diagnostics_backend = "jarl", backend_options = list(
        jarl = list(executable = file.path(fixture$workspace$root, "missing"))))
    for (i in 1:2) {
        # Different directories must not repeatedly announce the same missing CLI.
        uri <- path_to_uri(file.path(fixture$workspace$root, paste0("nested", i), "buffer.R"))
        fixture$document$uri <- uri
        fixture$workspace$documents$clear()
        fixture$workspace$documents$set(uri, fixture$document)
        task <- diagnostics_task(fixture$self, uri, fixture$document)
        invocation <- task$.__enclos_env__$private$args$request
        error <- tryCatch(run_backend(invocation), error = identity)
        expect_s3_class(error, "backend_unavailable")
        task$fail(error)
    }
    notifications <- Filter(function(item) identical(item$method, "window/showMessage"), fixture$self$deliveries)
    expect_length(notifications, 1L)
    expect_match(notifications[[1L]]$params$message, "executable was not found")
    expect_equal(fixture$document$content, "x=1")
    error <- tryCatch(backend_error("missing", unavailable = TRUE), error = identity)
    wrapped <- structure(list(message = "worker wrapper", call = NULL, parent = error),
        class = c("error", "condition"))
    expect_true(backend_unavailable(wrapped))
    expect_equal(backend_error_message(wrapped), "missing")
})

test_that("idle polling and watched config events refresh before the open-file filter", {
    fixture <- backend_fixture()
    path <- file.path(fixture$workspace$root, "buffer.R")
    fixture$uri <- path_to_uri(path)
    fixture$document$uri <- fixture$uri
    fixture$workspace$documents$clear()
    fixture$workspace$documents$set(fixture$uri, fixture$document)
    adapter <- fake_backend()
    adapter$config_files <- "fixture.toml"
    local_backend_settings(diagnostics_backend = adapter, formatting_backend = fake_backend(),
        backend_options = list(), diagnostics = TRUE)
    diagnostics_task(fixture$self, fixture$uri, fixture$document)
    poll_backend_configuration(fixture$self, now = 100)
    expect_false(fixture$self$diagnostics_task_manager$has_work())
    config <- file.path(fixture$workspace$root, "fixture.toml")
    writeLines("first", config)
    poll_backend_configuration(fixture$self, now = 101)
    expect_true(fixture$self$diagnostics_task_manager$has_work())
    fixture$self$diagnostics_task_manager$cancel(fixture$uri)
    config_document <- Document$new(path_to_uri(config), language = "r", content = "first")
    config_document$did_open()
    fixture$workspace$documents$set(config_document$uri, config_document)
    writeLines("second", config)
    workspace_did_change_watched_files(fixture$self, list(changes = list(list(
        uri = config_document$uri, type = FileChangeType$Changed))))
    expect_true(fixture$self$diagnostics_task_manager$has_work())
    expect_equal(tail(fixture$self$deliveries, 1)[[1L]]$params$diagnostics, list())
})

test_that("configuration watches are registered only for capable clients", {
    self <- new.env(parent = baseenv())
    self$deliveries <- list()
    self$request <- function(method, params) list(method = method, params = params)
    self$deliver <- function(message, callback) self$deliveries[[length(self$deliveries) + 1L]] <- message
    register_backend_watches(self)
    expect_length(self$deliveries, 0L)
    self$ClientCapabilities <- list(workspace = list(didChangeWatchedFiles = list(dynamicRegistration = TRUE)))
    register_backend_watches(self)
    expect_equal(self$deliveries[[1L]]$method, "client/registerCapability")
    watcher <- self$deliveries[[1L]]$params$registrations[[1L]]$registerOptions$watchers[[1L]]
    expect_match(watcher$globPattern, "jarl.toml", fixed = TRUE)
    expect_equal(watcher$kind, 7L)
})

test_that("CLI byte ranges use exact UTF-8 input and UTF-16 positions", {
    content <- c('"\U0001f600\u00e9"', "value")
    expect_equal(backend_byte_range(content, c(1, 7)), range(position(0L, 1L), position(0L, 4L)))
    expect_equal(backend_byte_range(content, c(9, 14)), range(position(1L, 0L), position(1L, 5L)))
    expect_error(backend_byte_range(content, c(2, 5)), "splits a UTF-8")
    expect_error(backend_byte_range(content, c(0, 99)), "invalid byte range")
})

test_that("Jarl parses findings, syntax errors and excluded buffers distinctly", {
    fixture <- backend_fixture('"\U0001f600"')
    local_backend_settings(diagnostics_backend = "jarl", backend_options = list())
    request <- backend_request("diagnostics", fixture$uri, fixture$document, fixture$workspace)
    result <- list(status = 1L, stderr = "", stdout = '{"diagnostics":[{"range":[1,5],"message":{"name":"rule","body":"finding"}}],"errors":[]}')
    testthat::local_mocked_bindings(backend_cli_run = function(...) result, .package = "languageserver")
    diagnostics <- jarl_diagnostics(request)
    expect_equal(diagnostics[[1L]]$range, range(position(0L, 1L), position(0L, 3L)))
    expect_equal(diagnostics[[1L]]$code, "rule")
    result <- list(status = 255L, stderr = "", stdout = '{"diagnostics":[],"errors":[{"syntax_errors":[{"range":[0,1],"message":"syntax"}]}]}')
    expect_equal(jarl_diagnostics(request)[[1L]]$severity, DiagnosticSeverity$Error)
    result <- list(status = 0L, stderr = "", stdout = "")
    expect_identical(jarl_diagnostics(request), list())
    result$stdout <- "bad JSON"
    expect_error(jarl_diagnostics(request), "malformed JSON")
    result <- list(status = 255L, stderr = "IO failure", stdout = '{"diagnostics":[],"errors":[{"error":"IO failure","syntax_errors":[]}]}')
    expect_error(jarl_diagnostics(request), "IO failure")
    result$stderr <- ""
    expect_error(jarl_diagnostics(request), "IO failure")
})

test_that("CLI formatting preserves input boundaries and returns no unchanged edits", {
    fixture <- backend_fixture(c('x <- "\U0001f600"', ""))
    local_backend_settings(formatting_backend = "air", backend_options = list())
    request <- backend_request("formatting", fixture$uri, fixture$document, fixture$workspace)
    expect_identical(backend_text_edits(request, paste(request$content, collapse = "\r\n")), list())
    edits <- backend_text_edits(request, 'x <- "changed"\n')
    expect_length(edits, 1L)
    expect_equal(edits[[1L]]$range$end, position(1L, 0L))
    result <- list(status = 0L, stderr = "", stdout = "")
    testthat::local_mocked_bindings(backend_cli_run = function(...) result, .package = "languageserver")
    expect_error(air_formatting(request), "empty buffer")
    result$status <- 2L
    result$stderr <- "parse error"
    expect_error(air_formatting(request), "parse error")
})

test_that("lintr worker options are explicit, resettable and restored", {
    skip_if_not_installed("lintr", "3.3.0.1")
    fixture <- backend_fixture("x = 1")
    local_backend_settings(diagnostics_backend = "lintr", backend_options = list())
    withr::local_options(lintr.linters = list())
    request <- backend_request("diagnostics", fixture$uri, fixture$document, fixture$workspace)
    options(lintr.linters = list(assignment_linter = lintr::assignment_linter()))
    expect_length(run_backend(request), 0L)
    expect_length(getOption("lintr.linters"), 1L)
    request$r_options["lintr.linters"] <- list(list(assignment_linter = lintr::assignment_linter()))
    expect_equal(run_backend(request)[[1L]]$code, "assignment_linter")
})
