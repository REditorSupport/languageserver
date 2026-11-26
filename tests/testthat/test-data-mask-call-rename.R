test_that("Rename keeps an outer function separate from a callable in a data mask", {
    f <- provider_fixture(c("f <- function() 1", "base::with(list(f = function() 2), f())", "f()"))
    at <- function(line, col) list(row = line, col = col)
    for (pos in list(at(0L, 0L), at(2L, 0L))) {
        result <- rename_reply(1L, f$uri, f$workspace, f$document, pos, "outer")
        edits <- result$result$changes[[f$uri]]
        expect_equal(vapply(edits, function(edit) edit$range$start$line, numeric(1L)), c(0L, 2L))
    }
    column <- at(1L, 35L)
    expect_match(prepare_rename_reply(1L, f$uri, f$workspace, f$document, column)$error$message, "data.mask")
    expect_null(rename_reply(1L, f$uri, f$workspace, f$document, column, "outer")$result$changes)
})

test_that("ordinary calls skip known non-functions, while ambiguous call heads decline Rename", {
    src <- c("f <- function(x) x", "d <- data.table::data.table(f = 2L, v = 3L)",
        "d[, f(v)]", "base::with(list(f = NULL), f(1))", "base::with(list(x = 2L), f(1))")
    query <- function(src) {
        f <- provider_fixture(src)
        rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), "renamed")
    }
    result <- query(src)
    expect_null(result$error)
    expect_equal(vapply(result$result$changes[[1L]], function(edit) edit$range$start$line, numeric(1L)), c(0L, 2:4))
    for (use in c("base::with(list(f = candidate), f(1))", "dplyr::filter(unknown, f(value))",
            "base::with(list(f = function(x) x), base::with(list(), f(1)))",
            "base::with(list(), { f <- candidate; f(1) })")) {
        expect_match(query(c(src[[1L]], use))$error$message, "data.mask")
    }
})

test_that("qualified and nested local functions retain their own call sites", {
    src <- c("f <- function() 1", "d <- list(f = function() 2)",
        "with(d, { base::sum(1); obj$f(); f() })",
        "with(d, (function() { f <- function() 3; f() })())",
        "f()")
    f <- provider_fixture(src)
    # The separate member and namespace spellings never become bare calls.
    index <- f$document$parse_data$reference_index
    expect_equal(index$call_package[reference_indices(index, "sum")], "base")
    nested <- list(row = 3L, col = 22L)
    edits <- rename_reply(1L, f$uri, f$workspace, f$document, nested, "inner")$result$changes[[f$uri]]
    expect_length(edits, 2L)
    expect_true(all(vapply(edits, function(edit) edit$range$start$line == 3L, logical(1L))))
    result <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), "outer")
    expect_equal(vapply(result$result$changes[[f$uri]], function(edit) edit$range$start$line, numeric(1L)), c(0L, 4L))
})

test_that("call-mode lookup across a non-function lexical binding is guarded", {
    for (body in c("f <- 0L; with(list(x = 1), f())", "with(list(x = 1), f()); f <- 0L")) {
        line <- paste0("g <- function() { ", body, " }")
        f <- provider_fixture(c("f <- function() 1", line, "f()"))
        local <- regexpr("f <- 0L", line, fixed = TRUE)[[1L]] - 1L
        for (point in list(list(row = 0L, col = 0L), list(row = 1L, col = local))) {
            result <- rename_reply(1L, f$uri, f$workspace, f$document, point, "renamed")
            expect_match(result$error$message, "data.mask")
            expect_null(result$result$changes)
        }
    }
})

test_that("a renamed lexical call cannot acquire a callable member of its data mask", {
    f <- provider_fixture(c("g <- function() 1", "with(list(f = function() 2, x = 1L), g())"))
    rename <- function(name) {
        rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), name)
    }
    expect_null(rename("f")$result$changes)
    expect_length(rename("x")$result$changes[[f$uri]], 2L)
})
