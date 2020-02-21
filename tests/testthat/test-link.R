context("Test Link")

test_that("Document link works", {
    skip_on_cran()

    dir <- tempdir()
    client <- language_client(dir)

    withr::local_tempfile(c("src_file1", "src_file2", "temp_file"), tmpdir = dir, fileext = ".R")
    src_file1 <- fs::path(src_file1)
    src_file2 <- fs::path(src_file2)

    writeLines(
        c(
            "x <- 1"
        ),
        src_file1
    )

    writeLines(
        c(
            "x <- 2"
        ),
        src_file2
    )

    writeLines(
        c(
            sprintf("source('%s')", src_file1),
            sprintf("source('%s')", basename(src_file1)),
            sprintf("source('%s')", src_file2),
            sprintf("source('%s')", basename(src_file2)),
            "source('non_exist_file.R')"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_document_link(temp_file)

    expect_length(result, 4)
    expect_equal(result[[1]]$range$start, list(line = 0, character = 8))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 8 + nchar(src_file1)))
    expect_equal(result[[1]]$target, path_to_uri(src_file1))

    expect_equal(result[[2]]$range$start, list(line = 1, character = 8))
    expect_equal(result[[2]]$range$end, list(line = 1, character = 8 + nchar(basename(src_file1))))
    expect_equal(result[[2]]$target, path_to_uri(src_file1))

    expect_equal(result[[3]]$range$start, list(line = 2, character = 8))
    expect_equal(result[[3]]$range$end, list(line = 2, character = 8 + nchar(src_file2)))
    expect_equal(result[[3]]$target, path_to_uri(src_file2))
    
    expect_equal(result[[4]]$range$start, list(line = 3, character = 8))
    expect_equal(result[[4]]$range$end, list(line = 3, character = 8 + nchar(basename(src_file2))))
    expect_equal(result[[4]]$target, path_to_uri(src_file2))
})

test_that("Document link works in Rmarkdown", {
    skip_on_cran()

    dir <- tempdir()
    client <- language_client(working_dir = dir)

    withr::local_tempfile(c("src_file1", "src_file2"), tmpdir = dir, fileext = ".R")
    withr::local_tempfile("temp_file", tmpdir = dir, fileext = ".Rmd")
    src_file1 <- fs::path(src_file1)
    src_file2 <- fs::path(src_file2)

    writeLines(
        c(
            "x <- 1"
        ),
        src_file1
    )
    
    writeLines(
        c(
            "x <- 2"
        ),
        src_file2
    )

    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            sprintf("will source file '%s' and '%s'", src_file1, src_file2),
            "```{r}",
            sprintf("source('%s')", src_file1),
            sprintf("source('%s')", basename(src_file1)),
            sprintf("source('%s')", src_file2),
            sprintf("source('%s')", basename(src_file2)),
            "source('non_exist_file.R')",
            "```"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_document_link(temp_file)

    expect_length(result, 4)
    expect_equal(result[[1]]$range$start, list(line = 5, character = 8))
    expect_equal(result[[1]]$range$end, list(line = 5, character = 8 + nchar(src_file1)))
    expect_equal(result[[1]]$target, path_to_uri(src_file1))

    expect_equal(result[[2]]$range$start, list(line = 6, character = 8))
    expect_equal(result[[2]]$range$end, list(line = 6, character = 8 + nchar(basename(src_file1))))
    expect_equal(result[[2]]$target, path_to_uri(src_file1))

    expect_equal(result[[3]]$range$start, list(line = 7, character = 8))
    expect_equal(result[[3]]$range$end, list(line = 7, character = 8 + nchar(src_file2)))
    expect_equal(result[[3]]$target, path_to_uri(src_file2))
    
    expect_equal(result[[4]]$range$start, list(line = 8, character = 8))
    expect_equal(result[[4]]$range$end, list(line = 8, character = 8 + nchar(basename(src_file2))))
    expect_equal(result[[4]]$target, path_to_uri(src_file2))
})
