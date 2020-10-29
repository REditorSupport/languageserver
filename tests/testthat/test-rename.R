context("Test Rename")

test_that("Rename works for functions in files", {
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
    result <- client %>% respond_rename(
        query_file, c(0, 0), "new_fn", retry_when = function(result) length(result$changes) < 2)
    expect_length(result$changes, 2)

    result1 <- result$changes[[path_to_uri(defn_file)]]
    expect_length(result1, 1)
    expect_equal(result1[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result1[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result1[[1]]$newText, "new_fn")

    result2 <- result$changes[[path_to_uri(query_file)]]
    expect_length(result2, 1)
    expect_equal(result2[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result2[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result2[[1]]$newText, "new_fn")

    # query in the middle of token
    result <- client %>% respond_rename(query_file, c(0, 3), "new_fn")
    expect_length(result$changes, 2)

    result1 <- result$changes[[path_to_uri(defn_file)]]
    expect_length(result1, 1)
    expect_equal(result1[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result1[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result1[[1]]$newText, "new_fn")

    result2 <- result$changes[[path_to_uri(query_file)]]
    expect_length(result2, 1)
    expect_equal(result2[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result2[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result2[[1]]$newText, "new_fn")

    # query at the end of token
    result <- client %>% respond_rename(query_file, c(0, 5), "new_fn")
    expect_length(result$changes, 2)

    result1 <- result$changes[[path_to_uri(defn_file)]]
    expect_length(result1, 1)
    expect_equal(result1[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result1[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result1[[1]]$newText, "new_fn")

    result2 <- result$changes[[path_to_uri(query_file)]]
    expect_length(result2, 1)
    expect_equal(result2[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result2[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result2[[1]]$newText, "new_fn")

    # remove definition
    writeLines("", defn_file)
    client %>% did_save(defn_file)

    result <- client %>% respond_rename(query_file, c(0, 0), "new_fn",
        retry_when = function(result) {
            length(result$changes) > 0
        })

    expect_length(result$changes, 0)

    # move function into different file
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn2_file)
    client %>% did_save(defn2_file)

    result <- client %>% respond_rename(query_file, c(0, 0), "new_fn")
    expect_length(result$changes, 2)

    result1 <- result$changes[[path_to_uri(defn2_file)]]
    expect_length(result1, 1)
    expect_equal(result1[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result1[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result1[[1]]$newText, "new_fn")

    result2 <- result$changes[[path_to_uri(query_file)]]
    expect_length(result2, 1)
    expect_equal(result2[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result2[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result2[[1]]$newText, "new_fn")
})

test_that("Rename works in single file", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c("my_fn <- function(x) {x + 1}", "my_fn", ".nonexistent"),
        single_file)

    client %>% did_save(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_rename(single_file, c(1, 0), "new_fn")
    expect_length(result$changes, 1)

    result <- result$changes[[path_to_uri(single_file)]]
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result[[1]]$newText, "new_fn")

    expect_equal(result[[2]]$range$start, list(line = 1, character = 0))
    expect_equal(result[[2]]$range$end, list(line = 1, character = 5))
    expect_equal(result[[2]]$newText, "new_fn")

    # then query the missing function. The file is processed, don't need to retry
    result <- client %>% respond_rename(single_file, c(2, 0), "new_fn", retry = FALSE)

    expect_length(result$changes, 0)
})

test_that("Rename works in scope with different assignment operators", {
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
    result <- client %>% respond_rename(
        single_file, c(8, 0), "new_fn", retry_when = function(result) length(result$changes) == 0)
    expect_length(result$changes, 1)

    result <- result$changes[[path_to_uri(single_file)]]
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result[[1]]$newText, "new_fn")

    expect_equal(result[[2]]$range$start, list(line = 8, character = 0))
    expect_equal(result[[2]]$range$end, list(line = 8, character = 5))
    expect_equal(result[[2]]$newText, "new_fn")

    result <- client %>% respond_rename(single_file, c(5, 5), "new_fn")
    expect_length(result$changes, 1)

    result <- result$changes[[path_to_uri(single_file)]]
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 0, character = 18))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 22))
    expect_equal(result[[1]]$newText, "new_fn")

    expect_equal(result[[2]]$range$start, list(line = 5, character = 4))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 8))
    expect_equal(result[[2]]$newText, "new_fn")

    result <- client %>% respond_rename(single_file, c(5, 12), "new_fn")
    expect_length(result$changes, 1)

    result <- result$changes[[path_to_uri(single_file)]]
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 1, character = 2))
    expect_equal(result[[1]]$range$end, list(line = 1, character = 6))
    expect_equal(result[[1]]$newText, "new_fn")

    expect_equal(result[[2]]$range$start, list(line = 5, character = 11))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 15))
    expect_equal(result[[2]]$newText, "new_fn")

    result <- client %>% respond_rename(single_file, c(5, 20), "new_fn")
    expect_length(result$changes, 1)

    result <- result$changes[[path_to_uri(single_file)]]
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 2, character = 2))
    expect_equal(result[[1]]$range$end, list(line = 2, character = 6))
    expect_equal(result[[1]]$newText, "new_fn")

    expect_equal(result[[2]]$range$start, list(line = 5, character = 18))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 22))
    expect_equal(result[[2]]$newText, "new_fn")


    result <- client %>% respond_rename(single_file, c(5, 26), "new_fn")
    expect_length(result$changes, 1)

    result <- result$changes[[path_to_uri(single_file)]]
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 3, character = 7))
    expect_equal(result[[1]]$range$end, list(line = 3, character = 11))
    expect_equal(result[[1]]$newText, "new_fn")

    expect_equal(result[[2]]$range$start, list(line = 5, character = 25))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 29))
    expect_equal(result[[2]]$newText, "new_fn")


    result <- client %>% respond_rename(single_file, c(5, 34), "new_fn")
    expect_length(result$changes, 1)

    result <- result$changes[[path_to_uri(single_file)]]
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 4, character = 7))
    expect_equal(result[[1]]$range$end, list(line = 4, character = 11))
    expect_equal(result[[1]]$newText, "new_fn")

    expect_equal(result[[2]]$range$start, list(line = 5, character = 32))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 36))
    expect_equal(result[[2]]$newText, "new_fn")
})

test_that("Rename in Rmarkdown works", {
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
    result <- client %>% respond_rename(single_file, c(5, 0), "new_fn")
    expect_length(result$changes, 1)

    result <- result$changes[[path_to_uri(single_file)]]
    expect_length(result, 2)

    expect_equal(result[[1]]$range$start, list(line = 1, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 1, character = 5))
    expect_equal(result[[1]]$newText, "new_fn")

    expect_equal(result[[2]]$range$start, list(line = 5, character = 0))
    expect_equal(result[[2]]$range$end, list(line = 5, character = 5))
    expect_equal(result[[2]]$newText, "new_fn")

    # then query the missing function. The file is processed, don't need to retry
    result <- client %>% respond_rename(single_file, c(6, 0), "new_fn", retry = FALSE)
    expect_length(result$changes, 0)
})

test_that("Prepare rename works in single file", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c("my_fn <- function(x) {x + 123}", "my_fn", "new_fn"),
        single_file)

    client %>% did_save(single_file)

    # first make sure the file is processed
    result <- client %>% respond_references(
        single_file, c(0, 0), retry_when = function(result) length(result) < 2)
    expect_length(result, 2)

    result <- client %>% respond_prepare_rename(single_file, c(1, 0))
    expect_equal(result$start, list(line = 1, character = 0))
    expect_equal(result$end, list(line = 1, character = 5))

    result <- client %>% respond_prepare_rename(single_file, c(0, 0))
    expect_equal(result$start, list(line = 0, character = 0))
    expect_equal(result$end, list(line = 0, character = 5))

    error <- client %>% respond_prepare_rename(single_file, c(2, 0), allow_error = TRUE)
    expect_equal(error$message, "Cannot rename the symbol")

    error <- client %>% respond_prepare_rename(single_file, c(0, 27), allow_error = TRUE)
    expect_equal(error$message, "Cannot rename the symbol")
})
