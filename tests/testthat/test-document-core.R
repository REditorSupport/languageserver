test_that("Document changes handle whole files and multiline replacements", {
    uri <- "file:///document-core.R"
    document <- Document$new(uri, version = 1L, content = c("one", "two", "three"))

    document$apply_content_changes(2L, list(list(text = "replacement")))
    expect_identical(document$content, "replacement")
    expect_equal(document$version, 2L)

    document$set_content(2L, c("one", "two", "three"))
    document$apply_content_changes(3L, list(list(
        range = range(position(1L, 1L), position(1L, 2L)),
        text = "A\nB\nC"
    )))
    expect_identical(document$content, c("one", "tA", "B", "Co", "three"))
    expect_equal(document$line(99L), "")
    expect_equal(
        document$detect_call(list(row = 0L, col = 0L)),
        list(token = "")
    )

    expect_null(null_function())
    expect_identical(normalize_parse_content(character()), "")
})

test_that("Parse callbacks discard stale work and superseded replies", {
    uri <- "file:///parse-callback.R"
    document <- Document$new(uri, version = 2L, content = "value <- 1")
    previous <- parse_document(uri, document$content)
    previous$version <- document$version
    document$update_parse_data(previous)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)

    self <- new.env(parent = baseenv())
    self$get_workspace <- function(...) workspace
    self$pending_replies <- collections::dict()
    self$request_handlers <- list(test = function(...) stop("not expected"))
    self$deliveries <- list()
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }

    expect_null(parse_callback(self, uri, 2L, NULL))
    expect_null(parse_callback(self, uri, 1L, parse_document(uri, "value <- 0")))

    queue <- collections::queue()
    queue$push(list(id = 1L, version = 1L, params = list()))
    queue$push(list(id = 2L, version = 3L, params = list()))
    self$pending_replies$set(uri, list(test = queue))
    current <- parse_document(uri, document$content)
    parse_callback(self, uri, 2L, current)

    expect_length(self$deliveries, 1L)
    expect_equal(self$deliveries[[1L]]$id, 1L)
    expect_equal(self$deliveries[[1L]]$error$code, ErrorCodes$RequestCancelled)
    expect_equal(queue$size(), 1L)
    expect_equal(queue$peek()$id, 2L)

    missing_workspace <- Workspace$new(NULL)
    self$get_workspace <- function(...) missing_workspace
    expect_null(resolve_callback(self, uri, 2L, character()))
})

test_that("Parse callbacks only resolve when the package request changes", {
    uri <- "file:///resolve-request.R"
    workspace <- Workspace$new(NULL)

    resolve_count <- 0L
    self <- new.env(parent = baseenv())
    self$get_workspace <- function(...) workspace
    self$resolve_task_manager <- new.env(parent = baseenv())
    self$resolve_task_manager$add_task <- function(id, task) {
        resolve_count <<- resolve_count + 1L
    }
    self$pending_replies <- collections::dict()
    self$request_handlers <- list()

    parse_data <- function(packages, parse_error = FALSE) {
        data <- parse_document(uri, "")
        data$packages <- packages
        data$parse_error <- parse_error
        data
    }

    document <- Document$new(uri, version = 1L, content = "")
    workspace$documents$set(uri, document)

    named_packages <- c(first = "stats", second = "utils")
    parse_callback(self, uri, 1L, parse_data(named_packages))
    expect_equal(resolve_count, 1L)
    expect_identical(document$requested_packages, c("stats", "utils"))

    # Representation-only differences must not schedule another subprocess.
    parse_callback(self, uri, 1L, parse_data(c("stats", "utils")))
    expect_equal(resolve_count, 1L)

    # Failed parses must not discard the last successful package request.
    parse_callback(self, uri, 1L, parse_data(character(), parse_error = TRUE))
    parse_callback(self, uri, 1L, parse_data(c("stats", "utils")))
    expect_equal(resolve_count, 1L)

    # Package order controls masking, so reordering must trigger resolution.
    parse_callback(self, uri, 1L, parse_data(c("utils", "stats")))
    expect_equal(resolve_count, 2L)
    expect_identical(document$requested_packages, c("utils", "stats"))

    # Adding or removing a package must also trigger resolution.
    parse_callback(self, uri, 1L, parse_data(c("utils", "stats", "methods")))
    expect_equal(resolve_count, 3L)
})
