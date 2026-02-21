test_that("Go to Definition works for symbols in top-level blocks", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "{",
        "  x <- 1",
        "}",
        "x",
        "",
        "if (TRUE) {",
        "  y <- 2",
        "}",
        "y",
        "",
        "while (FALSE) {",
        "  z <- 3",
        "}",
        "z"
    ), single_file)

    client %>% did_open(single_file)

    # Test definition of x (inside plain braces)
    result <- client %>% respond_definition(single_file, c(3, 0))
    expect_equal(result$range$start, list(line = 1, character = 2))
    expect_equal(result$range$end, list(line = 1, character = 8))

    # Test definition of y (inside if block)
    result <- client %>% respond_definition(single_file, c(8, 0))
    expect_equal(result$range$start, list(line = 6, character = 2))
    expect_equal(result$range$end, list(line = 6, character = 8))

    # Test definition of z (inside while block)
    result <- client %>% respond_definition(single_file, c(13, 0))
    expect_equal(result$range$start, list(line = 11, character = 2))
    expect_equal(result$range$end, list(line = 11, character = 8))
})
