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
