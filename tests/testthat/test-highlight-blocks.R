test_that("Document highlight works for symbols in top-level blocks", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "{",
            "  x <- 1",
            "  x",
            "}",
            "",
            "if (TRUE) {",
            "  y <- 2",
            "  y",
            "}",
            "",
            "while (FALSE) {",
            "  z <- 3",
            "  z",
            "}"
        ),
        temp_file)

    client %>% did_save(temp_file)

    # Test highlighting x (inside plain braces)
    result <- client %>% respond_document_highlight(temp_file, c(1, 2))
    expect_length(result, 2)
    expect_equal(result[[1]]$range$start, list(line = 1, character = 2))
    expect_equal(result[[1]]$range$end, list(line = 1, character = 3))
    expect_equal(result[[2]]$range$start, list(line = 2, character = 2))
    expect_equal(result[[2]]$range$end, list(line = 2, character = 3))

    # Test highlighting y (inside if block)
    result <- client %>% respond_document_highlight(temp_file, c(6, 2))
    expect_length(result, 2)
    expect_equal(result[[1]]$range$start, list(line = 6, character = 2))
    expect_equal(result[[1]]$range$end, list(line = 6, character = 3))
    expect_equal(result[[2]]$range$start, list(line = 7, character = 2))
    expect_equal(result[[2]]$range$end, list(line = 7, character = 3))

    # Test highlighting z (inside while block)
    result <- client %>% respond_document_highlight(temp_file, c(11, 2))
    expect_length(result, 2)
    expect_equal(result[[1]]$range$start, list(line = 11, character = 2))
    expect_equal(result[[1]]$range$end, list(line = 11, character = 3))
    expect_equal(result[[2]]$range$start, list(line = 12, character = 2))
    expect_equal(result[[2]]$range$end, list(line = 12, character = 3))
})
