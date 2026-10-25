test_that("Completion of function arguments works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "str(obj",
            "utils::str(obj",
            "str(stats::o",
            "seq.int(fr"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 6))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(1, 14))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(2, 12))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 0)

    result <- client %>% respond_completion(temp_file, c(3, 10))
    arg_items <- result$items %>% keep(~ .$label == "from")
    expect_length(arg_items, 1)
})

test_that("Completion of function arguments is case insensitive", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "str(OBJ",
            "utils::str(OBJ",
            "str(stats::O"
        ),
        temp_file)

    client %>% did_open(temp_file)

    # The first request waits for parsing and namespace startup, which can
    # exceed 10 seconds in parallel Windows checks or under coverage.
    timeout <- if (.Platform$OS.type == "windows" ||
            identical(Sys.getenv("R_COVR"), "true")) 30 else 10
    result <- client %>% respond_completion(temp_file, c(0, 6), timeout = timeout)
    arg_items <- result$items %>% keep(~ .$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(1, 14))
    arg_items <- result$items %>% keep(~ .$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(2, 12))
    arg_items <- result$items %>% keep(~ .$label == "object")
    expect_length(arg_items, 0)
})

test_that("Completion of options works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "options(sci",
            "options(scipen = 999, useFancy"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 11))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, "scipen")

    result <- client %>% respond_completion(temp_file, c(1, 30))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, "useFancyQuotes")
})

test_that("Completion of function arguments preserves the order of arguments", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "eval(",
            "formatC(",
            "print.default(",
            "seq.int("
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 5))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, names(formals(eval)))

    result <- client %>% respond_completion(temp_file, c(1, 8))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, names(formals(formatC)))

    result <- client %>% respond_completion(temp_file, c(2, 14))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, names(formals(print.default)))

    result <- client %>% respond_completion(temp_file, c(3, 8))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, names(formals(args(seq.int))))
})

test_that("Completion of local function arguments works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "local({",
            "  test <- function(vararg1, vararg2=1) {",
            "    vararg1 + vararg2",
            "  }",
            "  test(vararg",
            "  )",
            "})"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(4, 13),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    arg_items <- result$items %>% keep(~ .$label == "vararg1")
    expect_length(arg_items, 1)

    arg_items <- result$items %>% keep(~ .$label == "vararg2")
    expect_length(arg_items, 1)
})

test_that("Completion of local function arguments works in untitled documents", {
    skip_on_cran()
    client <- language_client()

    uri <- "untitled:Untitled-1"

    client %>% did_open(uri = uri, text = c(
        "local({",
        "  test <- function(vararg1, vararg2=1) {",
        "    vararg1 + vararg2",
        "  }",
        "  test(vararg",
        "  )",
        "})"
    ))

    result <- client %>% respond_completion(
        NULL, c(4, 13), uri = uri,
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    arg_items <- result$items %>% keep(~ .$label == "vararg1")
    expect_length(arg_items, 1)

    arg_items <- result$items %>% keep(~ .$label == "vararg2")
    expect_length(arg_items, 1)
})

test_that("Completion of user function arguments preserves the order of arguments", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test <- function(var3, var2, var1) {",
            "  var1 + var2 + var3",
            "}",
            "test(",
            ")"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(3, 5),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, c("var3", "var2", "var1"))
})


test_that("Completion of argument values from defaults works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Test function with default values",
            "my_func <- function(method = c('auto', 'manual', 'custom')) {",
            "  method <- match.arg(method)",
            "  method",
            "}",
            "",
            "# Test completion with named argument",
            "my_func(method = a)",
            "",
            "# Test completion with positional argument (first position)",
            "my_func(m)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    # Test named argument completion
    result <- client %>% respond_completion(
        temp_file, c(7, 17),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # With simplified implementation, substring 'a' matches 'auto' and 'manual'
    expect_true("auto" %in% labels)
    expect_true("manual" %in% labels)

    # Check that insertText is properly quoted
    insert_texts <- value_items %>% map_chr(~ .$insertText)
    expect_true('"auto"' %in% insert_texts)

    # Test positional argument completion
    result <- client %>% respond_completion(
        temp_file, c(10, 8),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # Substring 'm' matches 'manual' and 'custom'
    expect_true("manual" %in% labels)
    expect_true("custom" %in% labels)
})

test_that("Completion of argument values with partial match works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(type = c('normal', 'special', 'advanced')) {",
            "  type <- match.arg(type)",
            "  type",
            "}",
            "",
            "my_func(type = a )"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 15),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # Substring 'a' matches all values: 'normal', 'special', 'advanced'
    expect_true("advanced" %in% labels)
    expect_true("normal" %in% labels)
    expect_true("special" %in% labels)
})

test_that("Completion of argument values works with base R functions", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Test with memCompress() which has type parameter with character vector defaults",
            "memCompress(raw(10), type = gz)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(1, 30),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # memCompress has type parameter with values "gzip", "bzip2", "xz", "zstd", "none"
    expect_true("gzip" %in% labels)
    expect_true(length(labels) > 0)
})

test_that("Completion of argument values for multiple parameter function", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_func <- function(x, mode = c('read', 'write', 'append'), ",
            "                      style = c('plain', 'fancy')) {",
            "  mode <- match.arg(mode)",
            "  style <- match.arg(style)",
            "  list(x, mode, style)",
            "}",
            "",
            "# Test second argument",
            "test_func(1, rea)",
            "",
            "# Test third argument with named param",
            "test_func(mode = wri, style = pla)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    # Test second argument (mode) - positional
    result <- client %>% respond_completion(
        temp_file, c(8, 15),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # Substring 'rea' matches 'read' only
    expect_true("read" %in% labels)
    expect_false("write" %in% labels)
    expect_false("append" %in% labels)
    expect_false("plain" %in% labels)
    expect_false("fancy" %in% labels)

    # Test third argument (style) - using named parameter
    result <- client %>% respond_completion(
        temp_file, c(11, 32),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # Substring 'pla' matches 'plain' only
    expect_true("plain" %in% labels)
    expect_false("append" %in% labels)
    expect_false("fancy" %in% labels)
})

test_that("Completion of argument values works with named arguments out of order", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_func <- function(a = 1, b = c('x', 'y', 'z'), c = 3) {",
            "  b <- match.arg(b)",
            "  b",
            "}",
            "",
            "# Named argument out of order",
            "test_func(c = 5, b = \"\")"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(6, 21),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # With empty string token, all values from all parameters are shown
    expect_true("x" %in% labels)
    expect_true("y" %in% labels)
    expect_true("z" %in% labels)
})

test_that("Completion of argument values is case insensitive", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(method = c('Auto', 'Manual', 'Custom')) {",
            "  method <- match.arg(method)",
            "  method",
            "}",
            "",
            "my_func(method = M)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 17),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # Substring 'M' (case insensitive) matches 'Manual' and 'Custom'
    expect_true("Manual" %in% labels)
    expect_true("Custom" %in% labels)
})

test_that("No argument value completion for non-character defaults", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Function with numeric default",
            "my_func <- function(x = 10, y = c(1, 2, 3)) {",
            "  x + y",
            "}",
            "",
            "my_func(x = a)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 12),
        retry_when = function(result) length(result) == 0
    )

    # Should not have argument_value completions for numeric defaults
    value_items <- result$items %>% keep(~ !is.null(.$data$type) && .$data$type == "argument_value")
    expect_length(value_items, 0)
})

test_that("Completion of argument values works with positional arguments", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "fun0 <- function(x, status = c('running', 'done', 'error')) {",
            "  status <- match.arg(status)",
            "  status",
            "}",
            "",
            "fun0(1, run)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 11),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # Substring 'run' matches 'running'
    expect_true("running" %in% labels)
    expect_false("done" %in% labels)
    expect_false("error" %in% labels)
})

test_that("Completion of argument values with positional partial match works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(mode = c('read', 'write', 'append')) {",
            "  mode <- match.arg(mode)",
            "  mode",
            "}",
            "",
            "my_func(r)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 9),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # Substring 'r' matches 'read' and 'write'
    expect_true("read" %in% labels)
    expect_true("write" %in% labels)
    expect_false("append" %in% labels)
})

test_that("Completion of argument values for positional in multi-parameter function", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_func <- function(x, mode = c('fast', 'slow'), style = c('plain', 'fancy')) {",
            "  mode <- match.arg(mode)",
            "  style <- match.arg(style)",
            "  list(x, mode, style)",
            "}",
            "",
            "# Should suggest values from both mode and style parameters",
            "test_func(1, fa)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(7, 14),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )

    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)

    # Substring 'fa' matches 'fast' and 'fancy' from both parameters
    expect_true("fast" %in% labels)
    expect_true("fancy" %in% labels)
    # Should not include values that don't contain 'fa'
    expect_false("slow" %in% labels)
    expect_false("plain" %in% labels)
})
