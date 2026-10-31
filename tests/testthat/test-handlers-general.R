test_that("session headers redact credential variables while retaining useful settings", {
    environment <- c(GITHUB_PAT = "sample-pat", VSCODE_CLI_REQUIRE_TOKEN = "sample-token",
        API_KEY = "sample-key", AWS_SECRET_ACCESS_KEY = "sample-secret", database_password = "sample-password",
        PATH = "/example/bin", R_LIBS_USER = "/example/library", TOKENIZER_MODE = "ordinary")
    header <- session_environment(environment)
    expect_identical(unlist(header[seq_len(5L)], use.names = FALSE), rep("<redacted>", 5L))
    expect_identical(unlist(header[-seq_len(5L)]), environment[-seq_len(5L)])
    expect_identical(environment[["GITHUB_PAT"]], "sample-pat")
})

test_that("shutdown responds with a null result", {
    self <- new.env(parent = baseenv())
    self$exit_flag <- FALSE
    self$deliveries <- list()
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }

    on_shutdown(self, id = 1L, params = NULL)

    expect_true(self$exit_flag)
    expect_length(self$deliveries, 1L)
    response <- self$deliveries[[1L]]
    expect_null(response$result)
    expect_match(response$to_json(), '"result":null', fixed = TRUE)
})

test_that("request dictionary keys preserve ID type and numeric identity", {
    expect_identical(request_id_key(1L), request_id_key(1))
    expect_identical(request_id_key(1000000L), request_id_key(1e6))
    expect_identical(request_id_key(0), request_id_key(-0))
    expect_false(identical(request_id_key(1), request_id_key("1")))
    expect_false(identical(request_id_key(1e15 + 1), request_id_key(1e15 + 2)))
})

test_that("cancelling pending providers preserves distinct string and number IDs", {
    for (request_id in list(1, "1", 1e6)) {
        self <- new.env(parent = emptyenv())
        self$pending_replies <- collections::dict()
        self$deliveries <- list()
        self$deliver <- function(message) {
            self$deliveries[[length(self$deliveries) + 1L]] <- message
        }
        ids <- list(1L, "1", 1000000L, "1000000")
        queue <- collections::queue()
        for (id in ids) queue$push(list(id = id))
        self$pending_replies$set("file:///pending-ids.R", list(completion = queue))
        cancel_request(self, list(id = request_id))
        expected <- if (is.character(request_id)) 2L else if (request_id == 1) 1L else 3L
        expect_length(self$deliveries, 1L)
        expect_identical(self$deliveries[[1L]]$id, ids[[expected]])
        expect_identical(self$deliveries[[1L]]$error$code, ErrorCodes$RequestCancelled)
        remaining <- lapply(seq_len(queue$size()), function(...) queue$pop()$id)
        expect_identical(remaining, ids[-expected])
    }
})

test_that("index cancellation uses the same typed ID lookup as pending providers", {
    self <- new.env(parent = emptyenv())
    self$pending_replies <- collections::dict()
    self$index_requests <- collections::dict()
    self$deliveries <- list()
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }
    for (id in list(1000000L, "1000000")) {
        self$index_requests$set(request_id_key(id), list(id = id))
    }
    cancel_request(self, list(id = 1e6))
    expect_length(self$deliveries, 1L)
    expect_identical(self$deliveries[[1L]]$id, 1000000L)
    expect_false(self$index_requests$has(request_id_key(1000000L)))
    expect_true(self$index_requests$has(request_id_key("1000000")))
})
