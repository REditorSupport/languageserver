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
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 1)
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

    expect_equal(length(result), 5)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 6)
    expect_equal(result[[2]]$startLine, 1)
    expect_equal(result[[2]]$endLine, 5)
    expect_equal(result[[3]]$startLine, 2)
    expect_equal(result[[3]]$endLine, 3)
    expect_equal(result[[4]]$startLine, 4)
    expect_equal(result[[4]]$endLine, 5)
    expect_equal(result[[5]]$startLine, 7)
    expect_equal(result[[5]]$endLine, 8)
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

test_that("Multiple-level Folding range works", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "# section one ----",
        "one_a <- 1:10",
        "## sub-section for section one ----",
        "one_b <- letters",
        "# section two ----",
        "two_a <- 1:2",
        "### sub-section for section two ----",
        "two_b <- 1:2",
        "## sub-section for section two ----",
        "two_c <- 1:2"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>% respond_document_folding_range(defn_file)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 5)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 3)
    expect_equal(result[[2]]$startLine, 2)
    expect_equal(result[[2]]$endLine, 3)
    expect_equal(result[[3]]$startLine, 4)
    expect_equal(result[[3]]$endLine, 9)
    expect_equal(result[[4]]$startLine, 6)
    expect_equal(result[[4]]$endLine, 7)
    expect_equal(result[[5]]$startLine, 8)
    expect_equal(result[[5]]$endLine, 9)
})

test_that("Multiple-level Folding range nested in blocks works well", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "f1 <- function(x) {f2 <- function() {",
        "        # section for function f2 ------",
        "        x <- 1:2",
        "        x",
        "    }",
        "    # section for f1 ----",
        "    a <- 1:10",
        "    f3 <- function() {",
        "        LETTERS",
        "    }",
        "    ## sub-section for f1 ----",
        "    b <- letters",
        "    # section two for f1 ----",
        "    d <- 1:2",
        "    f4 <- function() {",
        "        # section for function f4 -----",
        "        c(a, b, d)",
        "    }} # end of both f1 and f4"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>% respond_document_folding_range(defn_file)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 9)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 16)
    # the second result is for function f2, although this will not work actually
    # as f1 and f2 have the same start line
    expect_equal(result[[2]]$startLine, 0)
    expect_equal(result[[2]]$endLine, 3)
    expect_equal(result[[3]]$startLine, 1)
    expect_equal(result[[3]]$endLine, 3)
    expect_equal(result[[4]]$startLine, 5)
    expect_equal(result[[4]]$endLine, 11)
    expect_equal(result[[5]]$startLine, 7)
    expect_equal(result[[5]]$endLine, 8)
    expect_equal(result[[6]]$startLine, 10)
    expect_equal(result[[6]]$endLine, 11)
    # all nested section in f1 will not exceed the close bracket of f1
    expect_equal(result[[7]]$startLine, 12)
    expect_equal(result[[7]]$endLine, 16)
    expect_equal(result[[8]]$startLine, 14)
    expect_equal(result[[8]]$endLine, 16)
    expect_equal(result[[9]]$startLine, 15)
    expect_equal(result[[9]]$endLine, 16)
})

test_that("two or more blank lines breaking section succession works well", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "# section one ----",
        "a <- 1:10",
        "",
        "",
        "## section two ----",
        "b <- letters",
        "",
        "",
        "",
        "### section three ----",
        "d <- 1:2"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>% respond_document_folding_range(defn_file)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 3)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 1)
    expect_equal(result[[2]]$startLine, 4)
    expect_equal(result[[2]]$endLine, 5)
    expect_equal(result[[3]]$startLine, 9)
    expect_equal(result[[3]]$endLine, 10)
})

test_that("Folding range of binary opts works well", {
    skip_on_cran()
    client <- language_client()

    defn_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c( # mix section with binary opts
        "# section one ----",
        "a <- letters %>% ",
        "",
        "paste0()",
        "",
        "# section two ----",
        "b <- 1:10 +",
        "",
        "    1L",
        "",
        "## sub-section ----",
        "d <- 1:2 +",
        "    1 +", # two blank lines will break the succession
        "",
        "",
        "1"
    ), defn_file)

    client %>% did_save(defn_file)
    result <- client %>% respond_document_folding_range(defn_file)
    result <- result[order(sapply(result, "[[", "startLine"))]

    expect_equal(length(result), 6)
    expect_equal(result[[1]]$startLine, 0)
    expect_equal(result[[1]]$endLine, 4)
    expect_equal(result[[2]]$startLine, 1)
    expect_equal(result[[2]]$endLine, 3)
    expect_equal(result[[3]]$startLine, 5)
    expect_equal(result[[3]]$endLine, 12)
    expect_equal(result[[4]]$startLine, 6)
    expect_equal(result[[4]]$endLine, 8)
    expect_equal(result[[5]]$startLine, 10)
    expect_equal(result[[5]]$endLine, 12)
    expect_equal(result[[6]]$startLine, 11)
    expect_equal(result[[6]]$endLine, 12)
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
