context("Test Highlight")

test_that("Document highlight works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "my_fn <- 3",
            "my_fn",
            "ggplot2::ggplot",
            "ggplot2::geom_abline"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_document_highlight(temp_file, c(1, 0))

    expect_length(result, 2)
    expect_equal(result[[1]]$range$start, list(line = 0, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 5))
    expect_equal(result[[2]]$range$start, list(line = 1, character = 0))
    expect_equal(result[[2]]$range$end, list(line = 1, character = 5))


    result <- client %>% respond_document_highlight(temp_file, c(3, 0))

    expect_length(result, 2)
    expect_equal(result[[1]]$range$start, list(line = 2, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 2, character = 7))
    expect_equal(result[[2]]$range$start, list(line = 3, character = 0))
    expect_equal(result[[2]]$range$end, list(line = 3, character = 7))

})