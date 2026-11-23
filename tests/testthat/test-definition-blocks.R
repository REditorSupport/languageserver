test_that("equals-assigned functions retain their enclosing parameters", {
    fixture <- provider_fixture(c("serve = function(dir = '.') {",
            "  dir = normalizePath(dir)", "}"))
    parsed <- fixture$document$parse_data
    xml <- parse_data_xml(parsed)
    for (mode in c("syntax", "indexed XML", "XML")) {
        if (mode != "syntax") {
            parsed$syntax_data <- NULL
            attr(xml, "top_level_index") <- if (mode == "indexed XML") xdoc_top_level_index(xml)
            parsed$xml_doc <- xml
        }
        result <- symbol_definition_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 1L, col = 23L), rootPath = tempdir())$result
        expect_identical(result$uri, fixture$uri)
        expect_equal(result$range, range(position(0L, 17L), position(0L, 20L)), info = mode)
    }
})

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
