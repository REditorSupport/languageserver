context("Test Definition")

test_that("Go to Definition works for functions in files", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    defn2_file <- withr::local_tempfile(fileext = ".R")
    query_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn_file)
    writeLines(c("my_fn"), query_file)

    client %>% did_save(defn_file)
    client %>% did_save(query_file)

    # query at the beginning of token
    result <- client %>% respond_definition(query_file, c(0, 0))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 2, character = 1))

    # query in the middle of token
    result <- client %>% respond_definition(query_file, c(0, 3))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 2, character = 1))

    # query at the end of token
    result <- client %>% respond_definition(query_file, c(0, 5))

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

    query_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("print"), query_file)

    client %>% did_save(query_file)

    result <- client %>% respond_definition(query_file, c(0, 0))

    expect_true(endsWith(result$uri, "print.R"))
})

test_that("Go to Definition works in single file", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
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

test_that("Go to Definition works in scope with different assignment operators", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn <- function(var1) {",
        "  var2 <- 1",
        "  var3 = 2",
        "  3 -> var4",
        "  for (var5 in 1:10) {",
        "    var1 + var2 + var3 + var4 + var5",
        "  }",
        "}",
        "my_fn(1)"
    ), single_file)

    client %>% did_save(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_definition(single_file, c(8, 0))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 7, character = 1))

    result <- client %>% respond_definition(single_file, c(5, 5))

    expect_equal(result$range$start, list(line = 0, character = 18))
    expect_equal(result$range$end, list(line = 0, character = 22))

    result <- client %>% respond_definition(single_file, c(5, 12))

    expect_equal(result$range$start, list(line = 1, character = 2))
    expect_equal(result$range$end, list(line = 1, character = 11))

    result <- client %>% respond_definition(single_file, c(5, 20))

    expect_equal(result$range$start, list(line = 2, character = 2))
    expect_equal(result$range$end, list(line = 2, character = 10))

    result <- client %>% respond_definition(single_file, c(5, 26))

    expect_equal(result$range$start, list(line = 3, character = 2))
    expect_equal(result$range$end, list(line = 3, character = 11))

    result <- client %>% respond_definition(single_file, c(5, 34))

    expect_equal(result$range$start, list(line = 4, character = 7))
    expect_equal(result$range$end, list(line = 4, character = 11))
})


test_that("Go to Definition works on both sides of assignment", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "var1 <- 1",
        "var1 <- var1 + 1",
        "var2 = 2",
        "var2 = var2 + 2",
        "3 -> var3",
        "var3 + 3 -> var3"
    ), single_file)

    client %>% did_save(single_file)

    result <- client %>% respond_definition(single_file, c(0, 1))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 9))

    result <- client %>% respond_definition(single_file, c(1, 1))

    expect_equal(result$range$start, list(line = 1, character = 0))
    expect_equal(result$range$end, list(line = 1, character = 16))

    result <- client %>% respond_definition(single_file, c(1, 9))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 9))

    result <- client %>% respond_definition(single_file, c(2, 1))

    expect_equal(result$range$start, list(line = 2, character = 0))
    expect_equal(result$range$end, list(line = 2, character = 8))

    result <- client %>% respond_definition(single_file, c(3, 1))

    expect_equal(result$range$start, list(line = 3, character = 0))
    expect_equal(result$range$end, list(line = 3, character = 15))

    result <- client %>% respond_definition(single_file, c(3, 8))

    expect_equal(result$range$start, list(line = 2, character = 0))
    expect_equal(result$range$end, list(line = 2, character = 8))

    result <- client %>% respond_definition(single_file, c(4, 6))

    expect_equal(result$range$start, list(line = 4, character = 0))
    expect_equal(result$range$end, list(line = 4, character = 9))

    result <- client %>% respond_definition(single_file, c(5, 1))

    expect_equal(result$range$start, list(line = 4, character = 0))
    expect_equal(result$range$end, list(line = 4, character = 9))

    result <- client %>% respond_definition(single_file, c(5, 15))

    expect_equal(result$range$start, list(line = 5, character = 0))
    expect_equal(result$range$end, list(line = 5, character = 16))
})

test_that("Go to Definition works when package is specified", {
    skip_on_cran()
    # When there is a user-defined function with the same name
    # as a package function, but the package is specified,
    # test that the package version is used.

    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    query_file <- withr::local_tempfile(fileext = ".R")
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

test_that("Go to Definition in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "```{r}",
            "my_fn <- function(x) {x + 1}",
            "```",
            "",
            "```{r}",
            "my_fn",
            ".nonexistent",
            "```"
        ),
        single_file
    )

    client %>% did_save(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_definition(single_file, c(5, 0))

    expect_equal(result$range$start, list(line = 1, character = 0))
    expect_equal(result$range$end, list(line = 1, character = 28))

    # then query the missing function. The file is processed, don't need to retry
    result <- client %>% respond_definition(single_file, c(6, 0), retry = FALSE)

    expect_equal(length(result), 0)
})
