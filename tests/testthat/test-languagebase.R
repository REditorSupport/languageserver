TestLanguageBase <- R6::R6Class(
    "TestLanguageBase",
    inherit = LanguageBase,
    public = list(
        lines = NULL,
        chars = NULL,
        writes = NULL,
        notifications = NULL,
        initialize = function(lines = list(), chars = list()) {
            self$lines <- lines
            self$chars <- chars
            self$writes <- character()
            self$notifications <- list()
            super$initialize()
        },
        register_handlers = function() {
            self$request_handlers <- list(
                echo = function(self, id, params) {
                    self$deliver(Response$new(id, result = params))
                },
                fail = function(...) stop("request failed")
            )
            self$notification_handlers <- list(
                record = function(self, params) {
                    self$notifications[[length(self$notifications) + 1L]] <- params
                },
                fail = function(...) stop("notification failed")
            )
        },
        read_line = function() {
            if (!length(self$lines)) return(character())
            value <- self$lines[[1L]]
            self$lines <- self$lines[-1L]
            value
        },
        read_char = function(...) {
            if (!length(self$chars)) return(character())
            value <- self$chars[[1L]]
            self$chars <- self$chars[-1L]
            value
        },
        write_text = function(text) {
            self$writes <- c(self$writes, text)
        },
        check_connection = function() invisible(NULL)
    )
)

test_that("LanguageBase reads fragmented headers and content", {
    base <- TestLanguageBase$new(
        lines = list("Content-Length: 2", character(), ""),
        chars = list(character(), "a", "b")
    )
    expect_equal(base$read_header(), 2L)
    expect_equal(base$read_content(2L), "ab")

    malformed <- TestLanguageBase$new(lines = list("Wrong: header", ""))
    expect_error(malformed$read_header(), "Unexpected non-empty line")
})

BufferedLanguageBase <- R6::R6Class("BufferedLanguageBase",
    inherit = TestLanguageBase,
    public = list(
        chunks = NULL,
        reads = 0L,
        eof = FALSE,
        initialize = function(chunks = list()) {
            self$chunks <- chunks
            super$initialize()
        },
        read_raw = function(n) {
            self$reads <- self$reads + 1L
            if (!length(self$chunks)) return(raw())
            value <- self$chunks[[1L]]
            self$chunks <- self$chunks[-1L]
            value
        },
        wait_input = function(timeout) invisible(NULL),
        on_input_eof = function() self$eof <- TRUE
    )
)

test_that("Buffered fetch accepts difftime timeout budgets", {
    base <- BufferedLanguageBase$new()
    expect_null(base$fetch(blocking = TRUE, timeout = as.difftime(0, units = "secs")))
    expect_null(base$fetch(blocking = TRUE, timeout = as.difftime(0, units = "mins")))
})

message_buffer_frame <- function(body, headers = "") {
    charToRaw(paste0("Content-Length: ", nchar(body, "bytes"), "\r\n",
        headers, "\r\n", body))
}

test_that("buffered fetch returns between fragmented UTF-8 frame reads", {
    body <- enc2utf8('{"text":"你好\U0001f600"}')
    bytes <- message_buffer_frame(body)
    # Split within both the header and a multibyte body character.
    base <- BufferedLanguageBase$new(list(
        bytes[seq_len(7L)], bytes[seq.int(8L, length(bytes) - 2L)],
        utils::tail(bytes, 2L)
    ))
    expect_null(base$fetch())
    expect_equal(base$reads, 1L)
    expect_true(base$input_read_pending())
    expect_null(base$fetch())
    expect_equal(base$reads, 2L)
    expect_identical(base$fetch(), body)
    expect_equal(base$reads, 3L)
    expect_null(base$fetch())
    expect_false(base$input_read_pending())
})

test_that("clients without raw-read support retain legacy fetch", {
    LegacyClient <- R6::R6Class("LegacyClient", inherit = TestLanguageBase,
        public = list(raw_input_available = FALSE,
            read_raw = function(...) stop("must use the legacy transport")))
    client <- LegacyClient$new(lines = list("Content-Length: 2", ""), chars = list("{}"))
    client$raw_input_available <- FALSE
    expect_identical(client$fetch(), "{}")
    expect_false(client$input_read_pending())
})

test_that("buffered fetch drains complete frames before observing EOF", {
    base <- BufferedLanguageBase$new(list(
        c(message_buffer_frame("first"), message_buffer_frame("second")),
        structure(raw(), eof = TRUE)
    ))
    expect_identical(base$fetch(), "first")
    expect_identical(base$fetch(), "second")
    expect_equal(base$reads, 1L)
    expect_false(base$eof)
    expect_null(base$fetch())
    expect_true(base$eof)

    truncated <- BufferedLanguageBase$new(list(
        utils::head(message_buffer_frame("body"), -1L), structure(raw(), eof = TRUE)
    ))
    expect_null(truncated$fetch())
    expect_error(truncated$fetch(), "connection closed during a message")
})

test_that("clients drain output before reporting EOF without polling a closed stream", {
    client <- LanguageClient$new()
    skip_if_not(client$raw_input_available)
    bodies <- c(enc2utf8('{"text":"你好\U0001f600"}'), "{}")
    # Use the same kind of pollable stream as subprocess stdout. Regular
    # processx file connections do not signal read completion on Windows.
    pipes <- processx::conn_create_pipepair(nonblocking = c(TRUE, FALSE))
    connection <- pipes[[1L]]
    withr::defer(processx::processx_conn_close(connection))
    processx::conn_write(pipes[[2L]], do.call(c, lapply(bodies, message_buffer_frame)))
    processx::processx_conn_close(pipes[[2L]])
    alive <- TRUE
    process <- new.env(parent = emptyenv())
    process$get_output_connection <- function() connection
    process$is_alive <- function() alive
    polls <- 0L
    process$poll_io <- function(timeout) {
        # Pipe reads are asynchronous on Windows. Wait for unfinished reads,
        # but never poll once processx has observed the end of the stream.
        expect_true(processx::conn_is_incomplete(connection))
        polls <<- polls + 1L
        c(output = processx::poll(list(connection), timeout)[[1L]])
    }
    client$process <- process
    # This fixture owns no subprocess, so detach it before the client finalizer.
    withr::defer(client$process <- NULL)

    expect_identical(client$fetch(blocking = TRUE, timeout = 5), bodies[[1L]])
    expect_identical(client$fetch(blocking = TRUE, timeout = 5), bodies[[2L]])
    expect_error(client$fetch(blocking = TRUE, timeout = 5), "Server output connection closed.", fixed = TRUE)
    expect_false(processx::conn_is_incomplete(connection))
    completed_polls <- polls
    expect_true(client$wait_input(0))
    expect_identical(polls, completed_polls)
    alive <- FALSE
    expect_error(client$fetch(blocking = TRUE), "Server is dead.", fixed = TRUE)
})

test_that("clients retain subprocess startup diagnostics after failure", {
    client <- LanguageClient$new(file.path(R.home("bin"), "R"),
        c("--vanilla", "--slave", "-e", 'stop("client startup marker")'))
    withr::defer(if (client$process$is_alive()) client$process$kill())
    client$process$wait(10000)
    expect_false(client$process$is_alive())
    expect_error(client$fetch(blocking = TRUE), "Server is dead.", fixed = TRUE)
    expect_match(paste(client$process$read_error_lines(), collapse = "\n"), "client startup marker", fixed = TRUE)
})

test_that("buffered blocking fetch applies its timeout to a partial body", {
    bytes <- message_buffer_frame("body")
    base <- BufferedLanguageBase$new(list(utils::head(bytes, -1L)))
    # Drive elapsed time deterministically so the test never needs to sleep.
    elapsed <- c(0, 0.5, 1.1)
    mockery::stub(base$fetch, "proc.time", function() {
        value <- elapsed[[1L]]
        elapsed <<- utils::tail(elapsed, -1L)
        c(elapsed = value)
    })
    expect_null(base$fetch(blocking = TRUE, timeout = 1))
    expect_equal(base$reads, 2L)
    base$chunks <- list(utils::tail(bytes, 1L))
    elapsed <- 1.2
    expect_identical(base$fetch(), "body")
})

test_that("native message buffers grow, compact, and scan headers incrementally", {
    create <- function() .Call("new_message_buffer_c", PACKAGE = "languageserver")
    append <- function(buffer, chunk) invisible(.Call("message_buffer_append_c",
        buffer, chunk, PACKAGE = "languageserver"))
    fetch <- function(buffer) .Call("message_buffer_fetch_c", buffer,
        PACKAGE = "languageserver")
    buffer <- create()
    body <- enc2utf8('{"text":"你好\U0001f600"}')
    bytes <- message_buffer_frame(body, paste0("Content-Type: ", strrep("x", 5000L), "\r\n"))
    for (i in seq_len(length(bytes) - 1L)) {
        append(buffer, bytes[[i]])
        if (!is.null(fetch(buffer))) stop("returned an incomplete frame")
    }
    append(buffer, utils::tail(bytes, 1L))
    expect_identical(fetch(buffer), body)
    expect_false(.Call("message_buffer_pending_c", buffer, PACKAGE = "languageserver"))

    # Consume a prefix, then grow while the next message is only half received.
    second <- message_buffer_frame(strrep("y", 12000L))
    append(buffer, c(message_buffer_frame(strrep("x", 3000L)), utils::head(second, 4000L)))
    expect_identical(fetch(buffer), strrep("x", 3000L))
    expect_null(fetch(buffer))
    append(buffer, utils::tail(second, -4000L))
    expect_identical(fetch(buffer), strrep("y", 12000L))

    append(buffer, rep(message_buffer_frame("small"), 300L))
    for (i in seq_len(250L)) fetch(buffer)
    append(buffer, message_buffer_frame(body))
    for (i in seq_len(50L)) fetch(buffer)
    expect_identical(fetch(buffer), body)
    expect_null(fetch(buffer))
})

test_that("native message buffers reject invalid framing and unusable pointers", {
    create <- function() .Call("new_message_buffer_c", PACKAGE = "languageserver")
    fetch <- function(buffer) .Call("message_buffer_fetch_c", buffer,
        PACKAGE = "languageserver")
    for (header in c("Wrong: header", "Content-Length: nope", "Content-Length: -1",
            "Content-Length: 2147483648", "Content-Type: json",
            "Content-Length: 1\r\nContent-Length: 2")) {
        buffer <- create()
        .Call("message_buffer_append_c", buffer,
            charToRaw(paste0(header, "\r\n\r\n{}")), PACKAGE = "languageserver")
        expect_error(fetch(buffer))
    }
    expect_error(.Call("message_buffer_append_c", create(), "text",
        PACKAGE = "languageserver"), "raw bytes")
    restored <- unserialize(serialize(create(), NULL))
    expect_error(fetch(restored), "invalid message buffer")
    expect_error(fetch(NULL), "invalid message buffer")

    buffer <- create()
    .Call("message_buffer_append_c", buffer, message_buffer_frame(""),
        PACKAGE = "languageserver")
    expect_identical(fetch(buffer), "")
    expect_error(.Call("stdin_read_raw", 0L, PACKAGE = "languageserver"), "read size")
    expect_error(.Call("stdin_wait", NA_real_, PACKAGE = "languageserver"), "timeout")
})

test_that("LanguageBase delivery stores callbacks and ignores NULL", {
    base <- TestLanguageBase$new()
    expect_null(base$deliver(NULL))
    request <- base$request("echo", list(value = 1L))
    callback <- function(...) NULL
    base$deliver(request, callback)

    expect_length(base$writes, 1L)
    expect_true(base$request_callbacks$has(as.character(request$id)))
})

test_that("LanguageBase handles malformed and unknown payloads", {
    old_log_file <- lsp_settings$get("log_file")
    withr::defer(lsp_settings$set("log_file", old_log_file))
    lsp_settings$set("log_file", withr::local_tempfile())

    base <- TestLanguageBase$new()
    expect_null(base$handle_raw("{"))
    expect_null(base$handle_raw("{}"))

    base$handle_request(list(id = 1L, method = "fail", params = list()))
    base$handle_request(list(id = 2L, method = "unknown", params = list()))
    expect_length(base$writes, 2L)
    expect_match(base$writes[[1L]], '"code":-32603', fixed = TRUE)
    expect_match(base$writes[[2L]], '"code":-32601', fixed = TRUE)

    expect_null(base$handle_notification(list(
        method = "fail", params = list()
    )))
    expect_null(base$handle_notification(list(
        method = "unknown", params = list()
    )))
})

test_that("LanguageBase isolates callback failures for results and errors", {
    base <- TestLanguageBase$new()
    result_request <- base$request("result", list())
    error_request <- base$request("error", list())
    base$request_callbacks$set(
        as.character(result_request$id),
        function(self, result) stop("result callback failed")
    )
    base$request_callbacks$set(
        as.character(error_request$id),
        function(self, result, error) stop("error callback failed")
    )

    expect_null(base$handle_response(list(
        id = result_request$id, result = "ok", error = NULL
    )))
    expect_null(base$handle_response(list(
        id = error_request$id,
        result = NULL,
        error = list(message = "broken")
    )))
})
