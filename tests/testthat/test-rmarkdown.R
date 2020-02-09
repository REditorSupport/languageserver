context("Test Rmarkdown")

test_that("Completion in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "Title",
            "",
            "```{r}",
            "str",
            "file.c",
            "fs::path",
            "foo$sol",
            ".Mac",
            "```",
            "str"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(temp_file, c(3, 3))

    expect_length(result$items %>% keep(~.$label == "strsplit"), 1)
    expect_length(result$items %>% keep(~.$label == "strrep"), 1)

    result <- client %>% respond_completion(temp_file, c(4, 6))
    expect_length(result$items %>% keep(~.$label == "file.choose"), 1)
    expect_length(result$items %>% keep(~.$label == "file.create"), 1)

    result <- client %>% respond_completion(temp_file, c(5, 8))
    expect_true("path_real" %in% (result$items %>% map_chr(~.$label)))

    result <- client %>% respond_completion(temp_file, c(6, 7))
    expect_length(result$items, 0)

    result <- client %>% respond_completion(temp_file, c(7, 4))
    expect_length(result$items %>% keep(~.$label == ".Machine"), 1)

    result <- client %>% respond_completion(temp_file, c(8, 3))
    expect_length(result$items, 0)
})

test_that("Signature in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".Rmd")
    writeLines(
        c(
            "Title",
            "",
            "```{r}",
            "file.path(",
            "fs::path_home('foo', ",
            "```",
            "file.path("
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_signature(temp_file, c(3, 10))
    expect_length(result$signatures, 1)
    expect_match(result$signatures[[1]]$label, "file\\.path\\(.*")

    result <- client %>% respond_signature(temp_file, c(4, 21))
    expect_length(result$signatures, 1)
    expect_match(result$signatures[[1]]$label, "path_home\\(.*")

    result <- client %>% respond_signature(temp_file, c(5, 10))
    expect_length(result$signatures, 0)
})



test_that("Go to Definition works for in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("single_file"), fileext = ".Rmd")
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
        single_file)

    client %>% did_save(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_definition(single_file, c(5, 0))

    expect_equal(result$range$start, list(line = 1, character = 0))
    expect_equal(result$range$end, list(line = 1, character = 28))

    # then query the missing function. The file is processed, don't need to retry
    result <- client %>% respond_definition(single_file, c(6, 0), retry = FALSE)

    expect_equal(length(result), 0)
})


test_that("Formatting works for in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("single_file"), fileext = ".Rmd")
    writeLines(
        c(
            "```{r}",
            "my_fn= function(x) {x + 1; x}",
            "```"
        ),
        single_file)

    client %>% did_save(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_formatting(single_file)

    expect_length(result, 1)
    expect_equal(result[[1]]$range$start, list(line = 1, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 1, character = 29))
    expect_equal(result[[1]]$newText, "my_fn <- function(x) {\n    x + 1\n    x\n}")
})
