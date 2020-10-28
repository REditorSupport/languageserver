context("Test Formatting")

test_that("Formatting document works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn<-function(x){",  # nolint
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

    temp_file <- withr::local_tempfile(fileext = ".R")
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

    temp_file <- withr::local_tempfile(fileext = ".R")
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

test_that("Formatting selection preserves initial indentation", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "  my_fn <- function(x) {",
        "      y =x+ 1",
        "      y+3",
        "  }"
    ), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(0, 0), c(3, 3))

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_equal(lines[1], "  my_fn <- function(x) {")
    expect_equal(lines[2], "      y <- x + 1")
    expect_equal(lines[3], "      y + 3")
    expect_equal(lines[4], "  }")
})

test_that("Formatting selection does not add indentation to multi-line string", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fun <- function() {",
        "  query(con,\"select group, date, time",
        "    from some_table",
        "    where group > 10\")",
        "}"
    ), temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(1, 0), c(3, 23))

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_equal(lines[1], "  query(con, \"select group, date, time")
    expect_equal(lines[2], "    from some_table")
    expect_equal(lines[3], "    where group > 10\")")
})

test_that("On type formatting works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn<-function(x){",  # nolint
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

test_that("Formatting in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "```{r}",
            "my_fn= function(x) {x + 1; x}",
            "```"
        ),
        single_file
    )

    client %>% did_save(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_formatting(single_file)

    expect_length(result, 1)
    expect_equal(result[[1]]$range$start, list(line = 1, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 1, character = 29))
    expect_equal(result[[1]]$newText, "my_fn <- function(x) {\n    x + 1\n    x\n}")
})
