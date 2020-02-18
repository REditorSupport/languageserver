context("Test Symbol")

test_that("Document Symbol works", {
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
    result <- client %>% respond_document_symbol(defn_file)

    expect_equal(result %>% map_chr(~ .$name) %>% sort(), c("f", "g"))
    expect_equivalent(
        result %>% detect(~ .$name == "f") %>% pluck("location", "range"),
        range(position(0, 0), position(2, 1))
    )
    expect_equivalent(
        result %>% detect(~ .$name == "g") %>% pluck("location", "range"),
        range(position(3, 0), position(3, 26))
    )
})

test_that("Document section symbol works", {
    skip_on_cran()
    client <- language_client()
    client$ClientCapabilities <- list(
        textDocument = list(
            documentSymbol = list(
                hierarchicalDocumentSymbolSupport = TRUE
            )
        )
    )

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
    result <- client %>% respond_document_symbol(defn_file)

    print(str(result))

    expect_equal(
        result %>% map_chr(~ .$name) %>% sort(),
        c("section1", "f", "step1", "step2", "section2", "g") %>% sort()
    )
    expect_equivalent(
        result %>% detect(~ .$name == "section1") %>% pluck("location", "range"),
        range(position(0, 0), position(6, 1))
    )
    expect_equivalent(
        result %>% detect(~ .$name == "f") %>% pluck("location", "range"),
        range(position(1, 0), position(6, 1))
    )
    expect_equivalent(
        result %>% detect(~ .$name == "step1") %>% pluck("location", "range"),
        range(position(2, 0), position(2, 15))
    )
    expect_equivalent(
        result %>% detect(~ .$name == "step2") %>% pluck("location", "range"),
        range(position(4, 0), position(4, 15))
    )
    expect_equivalent(
        result %>% detect(~ .$name == "section2") %>% pluck("location", "range"),
        range(position(7, 0), position(8, 26))
    )
    expect_equivalent(
        result %>% detect(~ .$name == "g") %>% pluck("location", "range"),
        range(position(8, 0), position(8, 26))
    )
})

test_that("Workspace Symbol works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("defn_file", "defn2_file"), fileext = ".R")
    writeLines(c(
        "f1 <- function(x) {",
        "  x + 1",
        "}",
        "g <- function(x) { x - 1 }"
    ), defn_file)
    writeLines(c(
        "f2 <- function(x) {", 
        "  x + 1", 
        "}"
    ), defn2_file)

    client %>% did_save(defn_file)
    client %>% did_save(defn2_file)

    expected_names <- c("f1", "f2")
    result <- client %>% respond_workspace_symbol(
        query = "f",
        retry_when = function(result) length(result) < 2
    )

    result_names <- result %>% map_chr(~ .$name) %>% sort()
    expect_equal(result_names, expected_names)
})
