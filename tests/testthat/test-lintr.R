context("Test Diagnostics")

test_that("lintr works", {
    skip_on_cran()

    dir <- tempdir()
    client <- language_client(working_dir = dir, diagnostics = TRUE)

    temp_file <- withr::local_tempfile(tmpdir = dir, fileext = ".R")
    writeLines("a = 1", temp_file)

    client %>% did_open(temp_file)
    data <- client %>% wait_for("textDocument/publishDiagnostics")

    expect_equal(client$diagnostics$size(), 1)
    expect_equal(client$diagnostics$get(data$uri), data$diagnostics)
    expect_equal(data$diagnostics[[1]]$message, "Use <-, not =, for assignment.")
})

test_that("lintr is disabled", {
    skip_on_cran()
    client <- language_client(diagnostics = FALSE)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines("a = 1", temp_file)

    client %>% did_open(temp_file)
    data <- client %>% wait_for("textDocument/publishDiagnostics", timeout = runif(1, 1, 3))
    expect_null(data)
})
