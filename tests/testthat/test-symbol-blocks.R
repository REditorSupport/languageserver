test_that("Document symbols work for symbols in top-level blocks", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "{",
            "  x <- 1",
            "}",
            "",
            "if (TRUE) {",
            "  y <- 2",
            "}",
            "",
            "while (FALSE) {",
            "  z <- 3",
            "}"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_document_symbol(temp_file)

    # Find the symbols x, y, z
    x_symbol <- Find(function(s) s$name == "x", result)
    y_symbol <- Find(function(s) s$name == "y", result)
    z_symbol <- Find(function(s) s$name == "z", result)

    expect_false(is.null(x_symbol))
    expect_false(is.null(y_symbol))
    expect_false(is.null(z_symbol))

    # Test x symbol range (inside plain braces)
    expect_equal(x_symbol$location$range$start, list(line = 1, character = 2))
    expect_equal(x_symbol$location$range$end, list(line = 1, character = 8))

    # Test y symbol range (inside if block)
    expect_equal(y_symbol$location$range$start, list(line = 5, character = 2))
    expect_equal(y_symbol$location$range$end, list(line = 5, character = 8))

    # Test z symbol range (inside while block)
    expect_equal(z_symbol$location$range$start, list(line = 9, character = 2))
    expect_equal(z_symbol$location$range$end, list(line = 9, character = 8))
})
