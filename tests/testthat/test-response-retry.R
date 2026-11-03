response_retry_fixture <- function(ready_after = 1L, response_delay = 0) {
    state <- new.env(parent = emptyenv())
    state$now <- as.POSIXct("2026-01-01", tz = "UTC")
    state$started <- state$now
    state$attempts <- 0L
    state$timeouts <- numeric()
    state$failures <- character()
    state$error <- NULL
    state$silent <- FALSE
    state$fetch_error <- NULL
    scope <- new.env(parent = environment(respond))
    scope$Sys.time <- function() state$now
    scope$Sys.sleep <- function(seconds) state$now <- state$now + seconds
    scope$fail <- function(message) state$failures <- c(state$failures, message)
    helper <- respond
    environment(helper) <- scope

    client <- new.env(parent = emptyenv())
    client$request_callbacks <- collections::dict()
    unrelated <- function(...) NULL
    client$request_callbacks$set(request_id_key("1"), unrelated)
    client$request <- function(method, params) {
        state$attempts <- state$attempts + 1L
        list(id = state$attempts, method = method, params = params)
    }
    client$deliver <- function(request, callback) {
        state$request <- request
        client$request_callbacks$set(request_id_key(request$id), callback)
    }
    client$fetch <- function(blocking, timeout) {
        state$timeouts <- c(state$timeouts, timeout)
        if (!is.null(state$fetch_error)) stop(state$fetch_error)
        if (state$silent) {
            state$now <- state$now + timeout + 0.01
            return(NULL)
        }
        state$now <- state$now + response_delay
        list(id = state$request$id, error = state$error,
            result = list(label = if (state$attempts >= ready_after) "ready" else "fallback"))
    }
    client$handle_raw <- function(response) {
        callback <- client$request_callbacks$get(request_id_key(response$id))
        callback(client, response$result, response$error)
    }
    list(state = state, client = client, respond = helper, unrelated = unrelated)
}

test_that("Response retries keep a bounded stack across many fallback replies", {
    fixture <- response_retry_fixture(ready_after = 2100L)
    result <- fixture$respond(fixture$client, "signatureHelp", params = NULL, timeout = 1000,
        retry_when = function(result) result$label != "ready")
    expect_identical(result$label, "ready")
    expect_identical(fixture$state$attempts, 2100L)
    expect_length(fixture$state$failures, 0L)
    expect_identical(unlist(fixture$client$request_callbacks$keys(), use.names = FALSE), request_id_key("1"))
    expect_identical(fixture$client$request_callbacks$get(request_id_key("1")), fixture$unrelated)
})

test_that("Retry backoff consumes the original response deadline", {
    fixture <- response_retry_fixture(ready_after = Inf)
    result <- fixture$respond(fixture$client, "signatureHelp", params = NULL, timeout = 0.65,
        retry_when = function(result) result$label != "ready")
    expect_null(result)
    expect_identical(fixture$state$attempts, 4L)
    expect_equal(as.numeric(difftime(fixture$state$now, fixture$state$started, units = "secs")), 0.65,
        tolerance = 1e-6)
    expect_identical(fixture$state$failures, "timeout when obtaining desired response")
    expect_equal(fixture$state$timeouts, c(0.65, 0.45, 0.25, 0.05), tolerance = 1e-6)
    expect_identical(unlist(fixture$client$request_callbacks$keys(), use.names = FALSE), request_id_key("1"))
})

test_that("Polling and retry pauses share one timeout budget", {
    fixture <- response_retry_fixture(ready_after = Inf, response_delay = 0.3)
    expect_null(fixture$respond(fixture$client, "signatureHelp", params = NULL, timeout = 1,
            retry_when = function(result) result$label != "ready"))
    expect_identical(fixture$state$attempts, 2L)
    expect_equal(fixture$state$timeouts, c(1, 0.5), tolerance = 1e-6)
    expect_identical(fixture$state$failures, "timeout when obtaining desired response")
})

test_that("Async errors propagate or return according to allow_error", {
    fixture <- response_retry_fixture()
    error <- list(code = -32603L, message = "fixture backend failure")
    fixture$state$error <- error
    expect_null(fixture$respond(fixture$client, "signatureHelp", params = NULL, timeout = 1))
    expect_identical(fixture$state$failures, "LSP request signatureHelp failed: fixture backend failure")
    expect_identical(fixture$state$attempts, 1L)
    expect_identical(unlist(fixture$client$request_callbacks$keys(), use.names = FALSE), request_id_key("1"))

    allowed <- response_retry_fixture()
    allowed$state$error <- error
    expect_identical(allowed$respond(allowed$client, "signatureHelp", params = NULL, timeout = 1,
            allow_error = TRUE), error)
    expect_length(allowed$state$failures, 0L)
    expect_identical(unlist(allowed$client$request_callbacks$keys(), use.names = FALSE), request_id_key("1"))
})

test_that("Timeouts and polling errors remove only the active request callback", {
    fixture <- response_retry_fixture()
    fixture$state$silent <- TRUE
    expect_null(fixture$respond(fixture$client, "signatureHelp", params = NULL, timeout = 1))
    expect_identical(fixture$state$failures, "timeout when obtaining response")
    expect_identical(unlist(fixture$client$request_callbacks$keys(), use.names = FALSE), request_id_key("1"))

    failed <- response_retry_fixture()
    failed$state$fetch_error <- simpleError("fixture connection closed")
    expect_error(failed$respond(failed$client, "signatureHelp", params = NULL, timeout = 1), "fixture connection closed")
    expect_identical(unlist(failed$client$request_callbacks$keys(), use.names = FALSE), request_id_key("1"))
})

test_that("Non-retrying requests and shutdown retain their original behavior", {
    fixture <- response_retry_fixture(ready_after = Inf)
    expect_identical(fixture$respond(fixture$client, "signatureHelp", params = NULL, timeout = 1,
            retry = FALSE), list(label = "fallback"))
    expect_identical(fixture$state$attempts, 1L)
    expect_length(fixture$state$failures, 0L)
    shutdown <- response_retry_fixture()
    expect_null(shutdown$respond(shutdown$client, "shutdown", params = NULL, timeout = 1))
    expect_length(shutdown$state$timeouts, 0L)
    expect_identical(unlist(shutdown$client$request_callbacks$keys(), use.names = FALSE), request_id_key("1"))
})
