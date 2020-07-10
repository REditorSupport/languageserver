context("Test Folding Range")

test_that("Document folding rage works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("defn_file"), fileext = ".R")
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
    expect_equivalent(result[[1]]$endLine, 2)
})

test_that("Document section folding range works", {
    skip_on_cran()
    client <- language_client(capabilities = list(
        textDocument = list(
            documentSymbol = list(
                hierarchicalDocumentSymbolSupport = TRUE
            )
        )
    ))

    withr::local_tempfile(c("defn_file"), fileext = ".R")
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
    expect_equal(result[[2]]$endLine, 6)
    expect_equal(result[[3]]$startLine, 7)
    expect_equal(result[[3]]$endLine, 8)
})

test_that("Document comment folding range works", {
    skip_on_cran()
    client <- language_client(capabilities = list(
        textDocument = list(
            documentSymbol = list(
                hierarchicalDocumentSymbolSupport = TRUE
            )
        )
    ))

    withr::local_tempfile(c("defn_file"), fileext = ".R")
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

test_that("Document folding range works in Rmarkdown", {
    skip_on_cran()
    client <- language_client(capabilities = list(
        textDocument = list(
            documentSymbol = list(
                hierarchicalDocumentSymbolSupport = TRUE
            )
        )
    ))

    withr::local_tempfile(c("defn_file"), fileext = ".Rmd")
    writeLines(c(
        "## section1",
        "Some text here",
        "### subsection1",
        "```{r}",
        "f <- function(x) {",
        "  x + 1",
        "}",
        "```"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>% respond_document_folding_range(defn_file)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 4)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 7)
    expect_equal(result[[2]]$startLine, 2)
    expect_equal(result[[2]]$endLine, 7)
    expect_equal(result[[3]]$startLine, 3)
    expect_equal(result[[3]]$endLine, 7)
    expect_equal(result[[4]]$startLine, 4)
    expect_equal(result[[4]]$endLine, 6)
})
