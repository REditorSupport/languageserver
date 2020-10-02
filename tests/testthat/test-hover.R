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
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(0, 7))
    expect_length(result$contents, 1)
    expect_true(stringi::stri_detect_fixed(result$contents[1], "strsplit"))
    expect_equal(result$range$end$character, 12)

    result <- client %>% respond_hover(temp_file, c(1, 7))
    expect_length(result$contents, 1)
    expect_true(stringi::stri_detect_fixed(result$contents[1], "path_real"))
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
        temp_file
    )

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

test_that("Hover works in scope with different assignment operators", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
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
    ), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(8, 0))
    expect_equal(result$range$start, list(line = 8, character = 0))
    expect_equal(result$range$end, list(line = 8, character = 5))
    expect_equal(result$contents, "```r\nmy_fn(var1)\n```")

    result <- client %>% respond_hover(temp_file, c(5, 5))
    expect_equal(result$range$start, list(line = 5, character = 4))
    expect_equal(result$range$end, list(line = 5, character = 8))
    expect_equal(result$contents, "```r\nmy_fn <- function(var1) {\n```")

    result <- client %>% respond_hover(temp_file, c(5, 12))
    expect_equal(result$range$start, list(line = 5, character = 11))
    expect_equal(result$range$end, list(line = 5, character = 15))
    expect_equal(result$contents, "```r\nvar2 <- 1\n```")

    result <- client %>% respond_hover(temp_file, c(5, 20))
    expect_equal(result$range$start, list(line = 5, character = 18))
    expect_equal(result$range$end, list(line = 5, character = 22))
    expect_equal(result$contents, "```r\nvar3 = 2\n```")

    result <- client %>% respond_hover(temp_file, c(5, 26))
    expect_equal(result$range$start, list(line = 5, character = 25))
    expect_equal(result$range$end, list(line = 5, character = 29))
    expect_equal(result$contents, "```r\n3 -> var4\n```")

    result <- client %>% respond_hover(temp_file, c(5, 34))
    expect_equal(result$range$start, list(line = 5, character = 32))
    expect_equal(result$range$end, list(line = 5, character = 36))
    expect_equal(result$contents, "```r\nfor (var5 in 1:10) {\n```")
})

test_that("Hover works on both sides of assignment", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("single_file"), fileext = ".R")
    writeLines(c(
        "var1 <- 1",
        "var1 <- var1 + 1",
        "var2 = 2",
        "var2 = var2 + 2",
        "3 -> var3",
        "var3 + 3 -> var3"
    ), single_file)

    client %>% did_save(single_file)

    result <- client %>% respond_hover(single_file, c(0, 1))
    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- 1\n```")

    result <- client %>% respond_hover(single_file, c(1, 1))
    expect_equal(result$range$start, list(line = 1, character = 0))
    expect_equal(result$range$end, list(line = 1, character = 4))
    expect_equal(result$contents, "```r\nvar1 <- var1 + 1\n```")

    result <- client %>% respond_hover(single_file, c(1, 9))
    expect_equal(result$range$start, list(line = 1, character = 8))
    expect_equal(result$range$end, list(line = 1, character = 12))
    expect_equal(result$contents, "```r\nvar1 <- 1\n```")

    result <- client %>% respond_hover(single_file, c(2, 1))
    expect_equal(result$range$start, list(line = 2, character = 0))
    expect_equal(result$range$end, list(line = 2, character = 4))
    expect_equal(result$contents, "```r\nvar2 = 2\n```")

    result <- client %>% respond_hover(single_file, c(3, 1))
    expect_equal(result$range$start, list(line = 3, character = 0))
    expect_equal(result$range$end, list(line = 3, character = 4))
    expect_equal(result$contents, "```r\nvar2 = var2 + 2\n```")

    result <- client %>% respond_hover(single_file, c(3, 8))
    expect_equal(result$range$start, list(line = 3, character = 7))
    expect_equal(result$range$end, list(line = 3, character = 11))
    expect_equal(result$contents, "```r\nvar2 = 2\n```")

    result <- client %>% respond_hover(single_file, c(4, 6))
    expect_equal(result$range$start, list(line = 4, character = 5))
    expect_equal(result$range$end, list(line = 4, character = 9))
    expect_equal(result$contents, "```r\n3 -> var3\n```")

    result <- client %>% respond_hover(single_file, c(5, 1))
    expect_equal(result$range$start, list(line = 5, character = 0))
    expect_equal(result$range$end, list(line = 5, character = 4))
    expect_equal(result$contents, "```r\n3 -> var3\n```")

    result <- client %>% respond_hover(single_file, c(5, 15))
    expect_equal(result$range$start, list(line = 5, character = 12))
    expect_equal(result$range$end, list(line = 5, character = 16))
    expect_equal(result$contents, "```r\nvar3 + 3 -> var3\n```")
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

test_that("Hover works with local function", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "local({",
            "  #' test function",
            "  #' @param var1 a number",
            "  test <- function(var1, var2=1) {",
            "    var1 + var2",
            "  }",
            "  test(var1 = 1, var2 = 2)",
            "})"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(6, 4))
    expect_equal(result$range$start, list(line = 6, character = 2))
    expect_equal(result$range$end, list(line = 6, character = 6))
    expect_equal(result$contents, list(
        "```r\ntest(var1, var2 = 1)\n```",
        "test function  \n\n`@param` `var1` a number  \n"
    ))

    result <- client %>% respond_hover(temp_file, c(6, 9))
    expect_equal(result$range$start, list(line = 6, character = 7))
    expect_equal(result$range$end, list(line = 6, character = 11))
    expect_equal(result$contents, list(
        "```r\ntest(var1, var2 = 1)\n```",
        "`var1` - a number"
    ))
})

test_that("Hover works across multiple files", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("defn_file", "query_file"), fileext = ".R")
    writeLines(c("test <- 1"), defn_file)
    writeLines(c("test + 1"), query_file)

    client %>% did_save(defn_file)
    client %>% did_save(query_file)

    result <- client %>% respond_hover(query_file, c(0, 0))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 0, character = 4))
    expect_equal(result$contents, "```r\ntest <- 1\n```")
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
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_hover(temp_file, c(5, 7))
    expect_length(result$contents, 1)
    expect_true(stringi::stri_detect_fixed(result$contents[1], "strsplit"))
    expect_equal(result$range$end$character, 12)

    result <- client %>% respond_hover(temp_file, c(6, 7))
    expect_length(result$contents, 1)
    expect_true(stringi::stri_detect_fixed(result$contents[1], "path_real"))
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
        temp_file
    )

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
