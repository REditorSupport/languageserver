context("Test Hover")

test_that("Simple hover works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "    strsplit",
            "fs::path_real"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 7))
    expect_length(result$contents, 1)
    expect_true(stringr::str_detect(result$contents[1], "strsplit"))
    expect_equal(result$range$end$character, 12)

    result <- client %>% respond_hover(temp_file, c(1, 7))
    expect_length(result$contents, 1)
    expect_true(stringr::str_detect(result$contents[1], "path_real"))
    expect_equal(result$range$end$character, 13)
})

test_that("Hover on user function works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "test1 <- function(x, y) x + 1",
            "test1"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(1, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest1(x, y)\n```")
    expect_equal(result$range$end$character, 5)
})

test_that("Hover on variable works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "var1 <- 1:10",
            "f(var1)",
            "local({",
            "   var2 <- 2:10",
            "   f(var1, var2)",
            "   var1 <- 0",
            "   f(var1, var2)",
            "})"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 1))
    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")
    
    result <- client %>% respond_hover(temp_file, c(1, 3))
    expect_equal(result$range$start, list(line = 1, character = 2))
    expect_equal(result$range$end, list(line = 1, character = 6))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(4, 6))
    expect_equal(result$range$start, list(line = 4, character = 5))
    expect_equal(result$range$end, list(line = 4, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(4, 13))
    expect_equal(result$range$start, list(line = 4, character = 11))
    expect_equal(result$range$end, list(line = 4, character = 15))
    expect_equal(result$contents, "```r\nvar2 <- 2:10\n```")

    result <- client %>% respond_hover(temp_file, c(6, 6))
    expect_equal(result$range$start, list(line = 6, character = 5))
    expect_equal(result$range$end, list(line = 6, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 0\n```")
})

test_that("Hover on function argument works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "unlist(list(a = 1, b = 2), recursive = FALSE)",
            "x <- list(var1 = 1, var2 = 2)"
        ),
        temp_file
    )
    
    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 30))
    expect_equal(result$range$start, list(line = 0, character = 27))
    expect_equal(result$range$end, list(line = 0, character = 36))
    expect_equal(result$contents, list(
        "```r\nunlist(x, recursive = TRUE, use.names = TRUE) \n```",
        "`recursive` - logical.  Should unlisting be applied to list components of `x` ?"
    ))

    result <- client %>% respond_hover(temp_file, c(1, 12))
    expect_equal(result$range$start, list(line = 1, character = 10))
    expect_equal(result$range$end, list(line = 1, character = 14))
    expect_equal(result$contents, list(
        "```r\nlist(...) \n```",
        "`...` - objects, possibly named."
    ))
})

test_that("Simple hover works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "Using strsplit",
            "```{r}",
            "    strsplit",
            "fs::path_real",
            "```"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(5, 7))
    expect_length(result$contents, 1)
    expect_true(stringr::str_detect(result$contents[1], "strsplit"))
    expect_equal(result$range$end$character, 12)

    result <- client %>% respond_hover(temp_file, c(6, 7))
    expect_length(result$contents, 1)
    expect_true(stringr::str_detect(result$contents[1], "path_real"))
    expect_equal(result$range$end$character, 13)
})

test_that("Hover on user function works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "some text here",
            "```{r}",
            "test1 <- function(x, y) x + 1",
            "test1",
            "```"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(6, 3))
    expect_length(result$contents, 1)
    expect_equal(result$contents[1], "```r\ntest1(x, y)\n```")
    expect_equal(result$range$end$character, 5)
})

test_that("Hover on variable works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "some text here",
            "```{r}",
            "var1 <- 1:10",
            "f(var1)",
            "local({",
            "   var2 <- 2:10",
            "   f(var1, var2)",
            "   var1 <- 0",
            "   f(var1, var2)",
            "})",
            "```"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(5, 1))
    expect_equal(result$range$start, list(line = 5, character = 0))
    expect_equal(result$range$end, list(line = 5, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")
    
    result <- client %>% respond_hover(temp_file, c(6, 3))
    expect_equal(result$range$start, list(line = 6, character = 2))
    expect_equal(result$range$end, list(line = 6, character = 6))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(9, 6))
    expect_equal(result$range$start, list(line = 9, character = 5))
    expect_equal(result$range$end, list(line = 9, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 1:10\n```")

    result <- client %>% respond_hover(temp_file, c(9, 13))
    expect_equal(result$range$start, list(line = 9, character = 11))
    expect_equal(result$range$end, list(line = 9, character = 15))
    expect_equal(result$contents, "```r\nvar2 <- 2:10\n```")

    result <- client %>% respond_hover(temp_file, c(11, 6))
    expect_equal(result$range$start, list(line = 11, character = 5))
    expect_equal(result$range$end, list(line = 11, character = 9))
    expect_equal(result$contents, "```r\nvar1 <- 0\n```")
})

test_that("Hover on function argument works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "some text here",
            "```{r}",
            "unlist(list(a = 1, b = 2), recursive = FALSE)",
            "x <- list(var1 = 1, var2 = 2)",
            "```"
        ),
        temp_file
    )
    
    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(5, 30))
    expect_equal(result$range$start, list(line = 5, character = 27))
    expect_equal(result$range$end, list(line = 5, character = 36))
    expect_equal(result$contents, list(
        "```r\nunlist(x, recursive = TRUE, use.names = TRUE) \n```",
        "`recursive` - logical.  Should unlisting be applied to list components of `x` ?"
    ))

    result <- client %>% respond_hover(temp_file, c(6, 12))
    expect_equal(result$range$start, list(line = 6, character = 10))
    expect_equal(result$range$end, list(line = 6, character = 14))
    expect_equal(result$contents, list(
        "```r\nlist(...) \n```",
        "`...` - objects, possibly named."
    ))
})
