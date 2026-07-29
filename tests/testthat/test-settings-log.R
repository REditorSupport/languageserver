test_that("Settings combine defaults, options, and workspace values", {
    settings <- Settings$new()
    expect_false(settings$get("debug"))
    expect_equal(settings$get("max_completions"), 200)
    expect_null(settings$get("unknown"))
    expect_identical(settings$set("debug", TRUE), settings)
    expect_true(settings$get("debug"))

    withr::local_options(list(
        languageserver.debug = FALSE,
        languageserver.max_completions = 25L
    ))
    settings$update_from_options()
    expect_false(settings$get("debug"))
    expect_equal(settings$get("max_completions"), 25L)

    settings$update_from_workspace(list(
        debug = TRUE,
        max_completions = 50L,
        parse_delay = 0.01
    ))
    expect_false(settings$get("debug"))
    expect_equal(settings$get("max_completions"), 25L)
    expect_equal(settings$get("parse_delay"), 0.01)
})

test_that("Log serialization handles scalars, collections, and conditions", {
    expect_equal(to_string(), "\n")
    expect_equal(to_string("message", 2L), "message 2\n")
    expect_equal(to_string(character()), "\n")
    expect_match(to_string(c("a", "b")), '"a"')
    expect_match(to_string(list(value = 1L)), '"value"')

    condition <- simpleError("broken")
    expect_match(to_string(condition), "broken")

    environment_value <- new.env(parent = emptyenv())
    environment_value$value <- 1L
    expect_match(to_string(environment_value), "environment")
})

test_that("Logger writes at the configured severity thresholds", {
    path <- withr::local_tempfile()
    old <- list(
        debug = lsp_settings$get("debug"),
        trace = lsp_settings$get("trace"),
        log_file = lsp_settings$get("log_file")
    )
    withr::defer({
        lsp_settings$set("debug", old$debug)
        lsp_settings$set("trace", old$trace)
        lsp_settings$set("log_file", old$log_file)
    })
    lsp_settings$set("log_file", path)
    lsp_settings$set("debug", FALSE)
    lsp_settings$set("trace", FALSE)

    logger$info("hidden info")
    logger$trace("hidden trace")
    logger$error("visible error")
    expect_match(readLines(path), "visible error")

    lsp_settings$set("debug", TRUE)
    logger$info("visible info")
    logger$trace("still hidden")
    lsp_settings$set("trace", TRUE)
    logger$trace("visible trace")

    output <- readLines(path)
    expect_true(any(grepl("visible info", output, fixed = TRUE)))
    expect_true(any(grepl("visible trace", output, fixed = TRUE)))
    expect_false(any(grepl("hidden", output, fixed = TRUE)))
})

test_that("log_write accepts both file paths and connections", {
    path <- withr::local_tempfile()
    log_write("first", log_file = path)

    connection <- file(path, open = "at")
    withr::defer(close(connection))
    log_write("second", log_file = connection)

    output <- readLines(path)
    expect_true(any(grepl("first", output, fixed = TRUE)))
    expect_true(any(grepl("second", output, fixed = TRUE)))
})

test_that("new loggers exercise default and enabled output routes", {
    old <- list(
        debug = lsp_settings$get("debug"),
        trace = lsp_settings$get("trace"),
        log_file = lsp_settings$get("log_file")
    )
    withr::defer({
        lsp_settings$set("debug", old$debug)
        lsp_settings$set("trace", old$trace)
        lsp_settings$set("log_file", old$log_file)
    })

    fallback <- capture.output(log_write("fallback"), type = "message")
    expect_match(paste(fallback, collapse = "\n"), "fallback")

    path <- withr::local_tempfile()
    lsp_settings$set("log_file", path)
    test_logger <- Logger$new()
    test_logger$error("error route")
    lsp_settings$set("debug", TRUE)
    test_logger$info("info route")
    lsp_settings$set("trace", TRUE)
    test_logger$trace("trace route")

    output <- paste(readLines(path), collapse = "\n")
    expect_match(output, "error route")
    expect_match(output, "info route")
    expect_match(output, "trace route")
})
