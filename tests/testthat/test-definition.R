context("Test Definition")

test_that("Go to Definition works for functions in files", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("defn_file", "defn2_file", "query_file"), fileext = ".R")
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn_file)
    writeLines(c("my_fn"), query_file)

    client %>% did_save(defn_file)
    client %>% did_save(query_file)

    result <- client %>% respond_definition(query_file, c(0, 0))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 2, character = 1))

    # remove definition
    writeLines("", defn_file)
    client %>% did_save(defn_file)

    result <- client %>% respond_definition(query_file, c(0, 0),
        retry_when = function(result) {
            length(result) > 0
        })

    expect_length(result, 0)

    # move function into different file
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn2_file)
    client %>% did_save(defn2_file)

    result <- client %>% respond_definition(query_file, c(0, 0))

    expect_equal(result$uri, path_to_uri(defn2_file))
})

test_that("Go to Definition works for functions in packages", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("query_file"), fileext = ".R")
    writeLines(c("print"), query_file)

    client %>% did_save(query_file)

    result <- client %>% respond_definition(query_file, c(0, 0))

    expect_true(endsWith(result$uri, "print.R"))
})

test_that("Go to Definition works for in single file", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("single_file"), fileext = ".R")
    writeLines(
        c("my_fn <- function(x) {x + 1}", "my_fn", ".nonexistent"),
        single_file)

    client %>% did_save(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_definition(single_file, c(1, 0))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 28))

    # then query the missing function. The file is processed, don't need to retry
    result <- client %>% respond_definition(single_file, c(2, 0), retry = FALSE)

    expect_equal(length(result), 0)
})

test_that("Go to Definition works when package is specified", {
    skip_on_cran()
    # When there is a user-defined function with the same name
    # as a package function, but the package is specified,
    # test that the package version is used.

    client <- language_client()

    withr::local_tempfile(c("defn_file", "query_file"), fileext = ".R")
    writeLines(c("print <- function(x) {", "# Not base::print", "}"), defn_file)
    writeLines(c("print", "base::print"), query_file)

    client %>% did_save(defn_file)
    client %>% did_save(query_file)

    result <- client %>% respond_definition(query_file, c(0, 0),
        retry_when = function(result) {
            length(result) == 0 || !identical(result$uri, path_to_uri(defn_file))
        })

    expect_equal(result$uri, path_to_uri(defn_file))
    expect_equal(result$range$start, list(line = 0, character = 0))

    # The file is processed, don't need to retry
    result <- client %>% respond_definition(query_file, c(1, 9))
    expect_true(endsWith(result$uri, "print.R"))
})
