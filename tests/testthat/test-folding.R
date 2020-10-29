context("Test Folding Range")

test_that("Expression folding rage works", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "f <- function(x) {",
        "  x + 1",
        "}",
        "g <- function(x) { x - 1 }"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>% respond_document_folding_range(defn_file)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 1)
    expect_equivalent(result[[1]]$startLine, 0)
    expect_equivalent(result[[1]]$endLine, 1)
})

test_that("Section folding range works", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "# section1 ####",
        "f <- function(x) {",
        "  ## step1 ====",
        "  x + 1",
        "  ## step2 ====",
        "  x + 2",
        "}",
        "# section2 ####",
        "g <- function(x) { x - 1 }"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>%
        respond_document_folding_range(defn_file) %>%
        keep(~ .$kind == FoldingRangeKind$Region)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 3)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 6)
    expect_equal(result[[2]]$startLine, 1)
    expect_equal(result[[2]]$endLine, 5)
    expect_equal(result[[3]]$startLine, 7)
    expect_equal(result[[3]]$endLine, 8)
})

test_that("Comment folding range works", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "# test 1",
        "# test 2",
        "f <- function(x) {",
        "  # step1",
        "  # step1",
        "  x + 1",
        "}",
        "# test 3",
        "# test 4",
        "g <- function(x) { x - 1 }"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>%
        respond_document_folding_range(defn_file) %>%
        keep(~ .$kind == FoldingRangeKind$Comment)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 3)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 1)
    expect_equal(result[[2]]$startLine, 3)
    expect_equal(result[[2]]$endLine, 4)
    expect_equal(result[[3]]$startLine, 7)
    expect_equal(result[[3]]$endLine, 8)
})

test_that("Folding range works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(c(
        "## section1",
        "Some text here",
        "### subsection1",
        "```{r}",
        "f <- function(x) {",
        "  x + 1",
        "}",
        "# title",
        "# description",
        "g <- function(x) {",
        "  x - 1",
        "}",
        "```"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>% respond_document_folding_range(defn_file)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 6)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 12)
    expect_equal(result[[2]]$startLine, 2)
    expect_equal(result[[2]]$endLine, 12)
    expect_equal(result[[3]]$startLine, 3)
    expect_equal(result[[3]]$endLine, 12)
    expect_equal(result[[4]]$startLine, 4)
    expect_equal(result[[4]]$endLine, 5)
    expect_equal(result[[5]]$startLine, 7)
    expect_equal(result[[5]]$endLine, 8)
    expect_equal(result[[6]]$startLine, 9)
    expect_equal(result[[6]]$endLine, 10)
})
