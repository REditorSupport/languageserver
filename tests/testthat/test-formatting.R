context("Test Formatting")

test_that("Formatting document works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(c(
        "my_fn<-function(x){", 
        "f(x+1,x-1)+x", 
        "}"
    ), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_formatting(temp_file)

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_length(lines, 3)
    expect_equal(lines, c(
        "my_fn <- function(x) {",
        "    f(x + 1, x - 1) + x",
        "}"
    ))
})


test_that("Formatting selection works for complete line", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(c(
        "my_fn <- function(x) {",
        "    y =x+ 1",
        "    y+3",
        "}"
    ), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(1, 0), c(2, 7))

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_equal(lines[1], "    y <- x + 1")
    expect_equal(lines[2], "    y + 3")
})


test_that("Formatting selection works for partial line", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(c(
        "my_fn <- function(x) {", 
        "    y =x+ 1", 
        "    y+3", 
        "}"
    ), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(1, 4), c(2, 7))

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_equal(lines[1], "    y = x + 1")
    expect_equal(lines[2], "    y + 3")
})


test_that("On type formatting works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(c(
        "my_fn<-function(x){",
        "f(x+1,x-1)",
        "data[x,y]",
        "}"
    ), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_on_type_formatting(temp_file, c(1, 10), ")")
    expect_length(result, 1)
    expect_equal(result[[1]]$newText, "f(x + 1, x - 1)")

    result <- client %>% respond_on_type_formatting(temp_file, c(2, 9), ")")
    expect_length(result, 1)
    expect_equal(result[[1]]$newText, "data[x, y]")

    result <- client %>% respond_on_type_formatting(temp_file, c(3, 1), "}")
    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_length(lines, 4)
    expect_equal(lines, c(
        "my_fn <- function(x) {",
        "    f(x + 1, x - 1)",
        "    data[x, y]",
        "}"
    ))
})
