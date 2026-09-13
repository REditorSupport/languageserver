formatting_fixture <- function(content = "x=1", uri = "file:///formatting.R") {
    self <- new.env(parent = baseenv())
    document <- Document$new(uri, version = 1L, content = content)
    document$did_open()
    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- new.env(parent = baseenv())
    workspace$documents <- documents
    workspace$root <- tempdir()
    workspace$type_hierarchy_cache <- collections::dict()
    workspace$update_loaded_packages <- function() NULL
    self$get_workspace <- function(...) workspace
    self$text_sync <- function(...) NULL
    self$deliveries <- list()
    self$deliver <- function(reply) {
        self$deliveries[[length(self$deliveries) + 1L]] <- reply
    }
    self$pending_replies <- collections::dict()
    self$pending_replies$set(uri, list())
    self$formatting_requests <- collections::dict()
    self$formatting_task_manager <- TaskManager$new("formatting-test",
        use_session = TRUE, min_idle_sessions = 0, max_running_tasks = 1)
    withr::defer_parent(self$formatting_task_manager$stop())
    list(self = self, document = document, uri = uri,
        manager = self$formatting_task_manager,
        params = list(textDocument = list(uri = uri),
            options = list(tabSize = 2L, insertSpaces = TRUE)))
}

formatting_test_task <- function(fixture, id) {
    fixture$manager$.__enclos_env__$private$pending_tasks$get(as.character(id))
}

test_that("explicit formatting handlers enqueue immutable snapshots", {
    fixture <- formatting_fixture(c("x=1", "y=2"))
    fixture$params$range <- list(start = position(0L, 0L), end = position(0L, 3L))
    fixture$params$ranges <- list(fixture$params$range)
    handlers <- list(text_document_formatting, text_document_range_formatting,
        text_document_ranges_formatting)
    for (id in seq_along(handlers)) {
        handlers[[id]](fixture$self, id, fixture$params)
    }
    expect_length(fixture$self$deliveries, 0L)
    expect_equal(fixture$self$formatting_requests$size(), 3L)
    fixture$document$set_content(2L, "new=3")
    for (id in seq_along(handlers)) {
        args <- formatting_test_task(fixture, id)$.__enclos_env__$private$args
        expect_equal(args$snapshot$content, c("x=1", "y=2"))
        expect_equal(args$snapshot$version, 1L)
        expect_equal(is.null(args$range), id != 2L)
        expect_equal(is.null(args$ranges), id != 3L)
    }
})

test_that("formatting rejects changed, closed, removed and reopened documents", {
    for (change in c("version", "content", "closed", "removed", "reopened")) {
        fixture <- formatting_fixture()
        text_document_formatting(fixture$self, 1L, fixture$params)
        task <- formatting_test_task(fixture, 1L)
        documents <- fixture$self$get_workspace()$documents
        switch(change,
            version = fixture$document$version <- 2L,
            content = fixture$document$set_content(1L, "different=2"),
            closed = fixture$document$did_close(),
            removed = documents$remove(fixture$uri),
            reopened = documents$set(fixture$uri,
                Document$new(fixture$uri, version = 1L, content = "x=1"))
        )
        task$.__enclos_env__$private$callback(list(list(newText = "x <- 1")))
        expect_equal(fixture$self$deliveries[[1L]]$error$code, -32800L)
        expect_equal(fixture$self$formatting_requests$size(), 0L)
    }
})

test_that("formatting cancellation answers once and leaves other requests queued", {
    fixture <- formatting_fixture()
    text_document_formatting(fixture$self, 1L, fixture$params)
    text_document_formatting(fixture$self, 2L, fixture$params)
    task <- formatting_test_task(fixture, 1L)
    cancel_request(fixture$self, list(id = "1"))
    expect_length(fixture$self$deliveries, 1L)
    expect_equal(fixture$self$deliveries[[1L]]$id, 1L)
    expect_equal(fixture$self$deliveries[[1L]]$error$code, -32800L)
    expect_equal(fixture$self$formatting_requests$size(), 1L)
    task$.__enclos_env__$private$callback(list(list(newText = "stale")))
    expect_length(fixture$self$deliveries, 1L)
    cancel_formatting_requests(fixture$self, uri = "file:///another.R")
    expect_length(fixture$self$deliveries, 1L)
    cancel_formatting_requests(fixture$self, uri = fixture$uri)
    expect_length(fixture$self$deliveries, 2L)
    expect_false(fixture$manager$has_work())
})

test_that("formatting worker errors complete and remove their requests", {
    fixture <- formatting_fixture()
    text_document_formatting(fixture$self, 1L, fixture$params)
    formatting_test_task(fixture, 1L)$fail(simpleError("formatter failed"))
    expect_equal(fixture$self$deliveries[[1L]]$error$code, -32603L)
    expect_equal(fixture$self$formatting_requests$size(), 0L)
})

test_that("document lifecycle notifications cancel obsolete formatting work", {
    for (event in c("change", "reopen", "close")) {
        fixture <- formatting_fixture()
        text_document_formatting(fixture$self, 1L, fixture$params)
        params <- list(textDocument = list(uri = fixture$uri,
                version = 2L, languageId = "r", text = "x=2"),
            contentChanges = list(list(text = "x=2")))
        switch(event,
            change = text_document_did_change(fixture$self, params),
            reopen = text_document_did_open(fixture$self, params),
            close = text_document_did_close(fixture$self, params)
        )
        replies <- Filter(function(reply) !is.null(reply$id), fixture$self$deliveries)
        expect_length(replies, 1L)
        expect_equal(replies[[1L]]$error$code, -32800L)
        expect_false(fixture$manager$has_work())
    }
})

test_that("saving identical content preserves formatting but disk changes cancel it", {
    path <- withr::local_tempfile(fileext = ".R")
    writeLines("x=1", path)
    fixture <- formatting_fixture(uri = path_to_uri(path))
    text_document_formatting(fixture$self, 1L, fixture$params)
    text_document_did_save(fixture$self, fixture$params)
    expect_length(fixture$self$deliveries, 0L)
    expect_true(fixture$manager$has_work())
    writeLines("x=2", path)
    text_document_did_save(fixture$self, fixture$params)
    expect_length(fixture$self$deliveries, 1L)
    expect_equal(fixture$self$deliveries[[1L]]$error$code, -32800L)
    expect_false(fixture$manager$has_work())
})

test_that("background formatting preserves custom styles and literate boundaries", {
    skip_on_cran()
    withr::local_options(languageserver.formatting_style = function(options) {
        style <- styler::tidyverse_style(indent_by = options$tabSize)
        style$token$force_assignment_op <- NULL
        style
    })
    fixture <- formatting_fixture(
        c("Prose", "```{r}", "x=1", "```", "```{python}", "x=1", "```"),
        "file:///formatting.qmd")
    text_document_formatting(fixture$self, 1L, fixture$params)
    deadline <- Sys.time() + 15
    while (fixture$manager$has_work() && Sys.time() < deadline) {
        fixture$manager$run_tasks()
        fixture$manager$check_tasks()
        Sys.sleep(0.01)
    }
    expect_length(fixture$self$deliveries, 1L)
    reply <- fixture$self$deliveries[[1L]]
    expect_null(reply$error)
    expect_length(reply$result, 1L)
    expect_equal(reply$result[[1L]]$newText, "x = 1")
    expect_equal(reply$result[[1L]]$range$start$line, 2L)
    expect_equal(reply$result[[1L]]$range$end$line, 2L)

    # Reusing the same session must pick up a changed server-side option.
    options(languageserver.formatting_style = NULL)
    text_document_formatting(fixture$self, 2L, fixture$params)
    deadline <- Sys.time() + 15
    while (fixture$manager$has_work() && Sys.time() < deadline) {
        fixture$manager$run_tasks()
        fixture$manager$check_tasks()
        Sys.sleep(0.01)
    }
    expect_length(fixture$self$deliveries, 2L)
    expect_equal(fixture$self$deliveries[[2L]]$result[[1L]]$newText, "x <- 1")

    fixture$params$range <- list(start = position(2L, 0L), end = position(2L, 3L))
    fixture$params$ranges <- list(fixture$params$range, fixture$params$range)
    text_document_range_formatting(fixture$self, 3L, fixture$params)
    text_document_ranges_formatting(fixture$self, 4L, fixture$params)
    deadline <- Sys.time() + 15
    while (fixture$manager$has_work() && Sys.time() < deadline) {
        fixture$manager$run_tasks()
        fixture$manager$check_tasks()
        Sys.sleep(0.01)
    }
    expect_length(fixture$self$deliveries, 4L)
    for (id in 3:4) {
        reply <- fixture$self$deliveries[[id]]
        expect_null(reply$error)
        expect_length(reply$result, 1L)
        expect_equal(reply$result[[1L]]$newText, "x <- 1")
        expect_equal(reply$result[[1L]]$range$start$line, 2L)
        expect_equal(reply$result[[1L]]$range$end$line, 2L)
    }
})

test_that("a running formatter leaves input responsive and can be cancelled", {
    skip_on_cran()
    # Coverage collection deliberately waits for workers instead of killing
    # them; this test specifically exercises prompt process cancellation.
    skip_if(identical(Sys.getenv("R_COVR"), "true"))
    marker <- withr::local_tempfile()
    script <- sprintf(paste0(
        "options(languageserver.diagnostics = FALSE, ",
        "languageserver.formatting_style = function(options) { ",
        "if (isTRUE(options$testDelay)) { writeLines('started', %s); Sys.sleep(30) }; ",
        "styler::tidyverse_style(indent_by = options$tabSize) }); ",
        "languageserver::run()"), encodeString(marker, quote = '"'))
    client <- LanguageClient$new(file.path(R.home("bin"), "R"),
        c("--no-echo", "-e", script))
    withr::defer({
        if (client$process$is_alive()) {
            client$deliver(client$request("shutdown", NULL))
            client$process$wait(1000)
            if (client$process$is_alive()) client$process$kill()
        }
    })
    client$catch_callback_error <- FALSE
    client$start(working_dir = NULL)
    client$handle_raw(client$fetch(blocking = TRUE, timeout = 10))
    notify(client, "initialized")
    uri <- path_to_uri(withr::local_tempfile(fileext = ".R"))
    did_open(client, uri = uri, text = "f(x+1)")
    params <- list(textDocument = list(uri = uri),
        options = list(tabSize = 2L, insertSpaces = TRUE, testDelay = TRUE))
    replies <- list()
    request <- client$request("textDocument/formatting", params)
    client$deliver(request, callback = function(self, result, error = NULL) {
        replies[[length(replies) + 1L]] <<- list(result = result, error = error)
    })
    deadline <- Sys.time() + 10
    while (!file.exists(marker) && Sys.time() < deadline) Sys.sleep(0.01)
    expect_true(file.exists(marker))

    # On-type formatting uses its own synchronous path and must not wait for
    # the explicit-formatting worker to finish its 30-second operation.
    result <- respond_on_type_formatting(client, uri = uri,
        pos = c(0L, 6L), ch = ")", timeout = 5, retry = FALSE)
    expect_length(result, 1L)
    expect_equal(result[[1L]]$newText, "f(x + 1)")
    expect_length(replies, 0L)
    notify(client, "$/cancelRequest", list(id = request$id))
    deadline <- Sys.time() + 5
    while (!length(replies) && Sys.time() < deadline) {
        data <- client$fetch(blocking = TRUE, timeout = 1)
        if (!is.null(data)) client$handle_raw(data)
    }
    expect_length(replies, 1L)
    expect_equal(replies[[1L]]$error$code, -32800L)

    # A fresh task must still work after the cancelled session was retired.
    result <- respond_formatting(client, uri = uri, timeout = 10, retry = FALSE)
    expect_equal(result[[1L]]$newText, "f(x + 1)\n")
    expect_length(replies, 1L)
})
