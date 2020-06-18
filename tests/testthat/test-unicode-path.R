context("Test Unicode path")

test_that("Works with unicode path", {
    skip_on_cran()

    dir <- file.path(tempdir(), "中文 にほんご 한국어")
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    client <- language_client(dir)

    withr::local_tempfile("defn_file", pattern = "中文", tmpdir = dir, fileext = ".R")
    withr::local_tempfile("defn2_file", pattern = "にほんご", tmpdir = dir, fileext = ".R")
    withr::local_tempfile("query_file", pattern = "한국어", tmpdir = dir, fileext = ".R")

    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn_file)
    writeLines(c("my_fn"), query_file)

    client %>% did_save(defn_file)
    client %>% did_save(query_file)

    result <- client %>% respond_definition(query_file, c(0, 0))

    expect_equal(result$range$start, list(line = 0, character = 0))
    expect_equal(result$range$end, list(line = 2, character = 1))

    # remove definition
    writeLines("", defn_file)
    client %>% did_save(defn_file)

    result <- client %>% respond_definition(query_file, c(0, 0),
        retry_when = function(result) {
            length(result) > 0
        })

    expect_length(result, 0)

    # move function into different file
    writeLines(c("my_fn <- function(x) {", "  x + 1", "}"), defn2_file)
    client %>% did_save(defn2_file)

    result <- client %>% respond_definition(query_file, c(0, 0))

    expect_equal(result$uri, path_to_uri(defn2_file))

})
