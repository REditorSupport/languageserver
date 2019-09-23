context("Test Formatting")

test_that("Formatting document works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(c("my_fn <- function(x) {  x + 1 }"), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_formatting(temp_file)

    expect_length(result, 1)
    lines <- result[[1]]$newText %>% strsplit("\n") %>% extract2(1)
    expect_length(lines, 3)
})


test_that("Formatting selection works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(c("my_fn <- function(x) {", "    y =x+ 1", "    y+3", "}"), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(1, 0), c(2, 7))

    expect_length(result, 1)
    lines <- result[[1]]$newText %>% strsplit("\n") %>% extract2(1)
    expect_equal(lines[1], "    y = x + 1")
    expect_equal(lines[2], "    y + 3")
})
