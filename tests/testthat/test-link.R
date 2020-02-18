context("Test Link")

test_that("Document link works", {
    skip_on_cran()

    dir <- tempdir()
    client <- language_client(dir)

    withr::local_tempfile(c("source_file", "temp_file"), tmpdir = dir, fileext = ".R")
    writeLines(
        c(
            "x <- 1"
        ),
        source_file
    )
    client %>% did_save(source_file)

    writeLines(
        c(
            "source('source_file.R')",
            "source('non_exist_file.R')"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_document_link(temp_file)

    expect_length(result, 1)
    expect_equal(result[[1]]$range$start, list(line = 0, character = 8))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 21))
    expect_equal(result[[1]]$target, path_to_uri(file.path(dir, temp_file)))
})
