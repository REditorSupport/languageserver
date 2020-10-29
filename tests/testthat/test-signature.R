context("Test Signature")

test_that("Simple signature works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "file.path(",
            "fs::path_home('foo', "
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_signature(temp_file, c(0, 10))
    expect_length(result$signatures, 1)
    expect_match(result$signatures[[1]]$label, "file\\.path\\(.*")
    expect_equal(result$signatures[[1]]$documentation$kind, "markdown")
    expect_match(result$signatures[[1]]$documentation$value,
        ".*Construct the path to a file from components in a platform-independent way.*")

    result <- client %>% respond_signature(temp_file, c(1, 21))
    expect_length(result$signatures, 1)
    expect_match(result$signatures[[1]]$label, "path_home\\(.*")
    expect_equal(result$signatures[[1]]$documentation$kind, "markdown")
    expect_match(result$signatures[[1]]$documentation$value,
        ".*`path_expand\\(\\)` performs tilde expansion on a path.*")
})

test_that("Signature of user function works", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    temp_file <- withr::local_tempfile(fileext = ".R")

    writeLines(c("foo <- function(x, y = 3) { x + y }"), defn_file)
    writeLines(c("foo(3, "), temp_file)

    client %>% did_save(defn_file) %>% did_save(temp_file)

    result <- client %>% respond_signature(
        temp_file, c(0, 7),
        retry_when = function(result) length(result) == 0 || length(result$signatures) == 0)
    expect_length(result$signatures, 1)
    expect_equal(result$signatures[[1]]$label, "foo(x, y = 3)")
})

test_that("Signature in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
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
        temp_file
    )

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
