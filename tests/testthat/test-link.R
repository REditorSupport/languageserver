context("Test Link")

test_that("Document link works", {
    skip_on_cran()

    dir <- tempdir()
    client <- language_client(dir)

    withr::local_tempfile(c("src_file", "temp_file"), tmpdir = dir, fileext = ".R")
    writeLines(
        c(
            "x <- 1"
        ),
        src_file
    )

    writeLines(
        c(
            sprintf("source('%s')", src_file),
            sprintf("source('%s')", basename(src_file)),
            "source('non_exist_file.R')"
        ),
        temp_file
    )

    client %>% did_save(src_file)
    client %>% did_save(temp_file)

    result <- client %>% respond_document_link(temp_file)

    expect_length(result, 2)
    expect_equal(result[[1]]$range$start, list(line = 0, character = 8))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 8 + nchar(src_file)))
    expect_equal(result[[1]]$target, path_to_uri(normalizePath(src_file, "/", mustWork = FALSE)))

    expect_equal(result[[2]]$range$start, list(line = 1, character = 8))
    expect_equal(result[[2]]$range$end, list(line = 1, character = 8 + nchar(basename(src_file))))
    expect_equal(result[[2]]$target, path_to_uri(src_file))
})

test_that("Document link works in Rmarkdown", {
    skip_on_cran()

    dir <- tempdir()
    client <- language_client(working_dir = dir)

    withr::local_tempfile("src_file", tmpdir = dir, fileext = ".R")
    withr::local_tempfile("temp_file", tmpdir = dir, fileext = ".Rmd")

    writeLines(
        c(
            "x <- 1"
        ),
        src_file
    )

    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            sprintf("will source file '%s'", src_file),
            "```{r}",
            sprintf("source('%s')", src_file),
            sprintf("source('%s')", basename(src_file)),
            "source('non_exist_file.R')",
            "```"
        ),
        temp_file
    )

    client %>% did_save(src_file)
    client %>% did_save(temp_file)

    result <- client %>% respond_document_link(temp_file)

    expect_length(result, 2)
    expect_equal(result[[1]]$range$start, list(line = 5, character = 8))
    expect_equal(result[[1]]$range$end, list(line = 5, character = 8 + nchar(src_file)))
    expect_equal(result[[1]]$target, path_to_uri(normalizePath(src_file, "/", mustWork = FALSE)))

    expect_equal(result[[2]]$range$start, list(line = 6, character = 8))
    expect_equal(result[[2]]$range$end, list(line = 6, character = 8 + nchar(basename(src_file))))
    expect_equal(result[[2]]$target, path_to_uri(src_file))
})
