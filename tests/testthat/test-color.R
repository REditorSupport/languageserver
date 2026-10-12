get_color <- function(color) {
    rgba <- grDevices::col2rgb(color, alpha = TRUE) / 255
    as.list(rgba[, 1])
}

test_that("indexed colors preserve literals and cache against original content", {
    fixture <- provider_fixture(c('label <- "😀"; colors <- c("red", "#2a3b6af2", "not-a-color")',
        'raw <- r"(red)"; escaped <- "r\\145d"'))
    parsed <- fixture$document$parse_data
    indexed <- parsed$range_data
    actual <- document_color_reply(1L, fixture$uri, fixture$workspace, fixture$document)$result
    parsed$range_data <- NULL
    parsed$colors_cache <- NULL
    expected <- document_color_reply(2L, fixture$uri, fixture$workspace, fixture$document)$result
    expect_identical(actual, expected)
    parsed$range_data <- indexed
    parsed$colors_cache <- NULL
    parsed$xml_doc <- parsed$xml_data <- NULL
    expect_identical(document_color_reply(3L, fixture$uri, fixture$workspace, fixture$document)$result, expected)
    expect_identical(document_color_reply(4L, fixture$uri, fixture$workspace, fixture$document)$result, expected)
    expect_null(parsed$xml_doc)
})

test_that("color presentations preserve opaque and translucent alpha", {
    fixture <- provider_fixture("value <- 1")
    opaque <- color_presentation_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(red = 1, green = 0, blue = 0, alpha = 1)
    )
    translucent <- color_presentation_reply(
        2L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(red = 1, green = 0, blue = 0, alpha = 0.5)
    )

    expect_equal(opaque$result[[1L]]$label, "#ff0000")
    expect_equal(translucent$result[[1L]]$label, "#ff000080")
})

test_that("Document color works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "plot(rnorm(100), col = \"red\")",
            "plot(rnorm(100), col = \"green\")",
            "plot(rnorm(100), main = \"color\")",
            "list(color1 = 'green', color2 = '#2a3b6a', color3 = '#2a3b6af2')"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_document_color(temp_file)
    expect_length(result, 5)

    expect_equal(result[[1]]$range$start, list(line = 0, character = 24))
    expect_equal(result[[1]]$range$end, list(line = 0, character = 27))
    expect_equal(result[[1]]$color, get_color("red"))

    expect_equal(result[[2]]$range$start, list(line = 1, character = 24))
    expect_equal(result[[2]]$range$end, list(line = 1, character = 29))
    expect_equal(result[[2]]$color, get_color("green"))

    expect_equal(result[[3]]$range$start, list(line = 3, character = 15))
    expect_equal(result[[3]]$range$end, list(line = 3, character = 20))
    expect_equal(result[[3]]$color, get_color("green"))

    expect_equal(result[[4]]$range$start, list(line = 3, character = 33))
    expect_equal(result[[4]]$range$end, list(line = 3, character = 40))
    expect_equal(result[[4]]$color, get_color("#2a3b6a"), tolerance = 1e-3)

    expect_equal(result[[5]]$range$start, list(line = 3, character = 53))
    expect_equal(result[[5]]$range$end, list(line = 3, character = 62))
    expect_equal(result[[5]]$color, get_color("#2a3b6af2"), tolerance = 1e-3)
})

test_that("Document color works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "---",
            "title: r markdown",
            "---",
            "some \"red\" color in text",
            "```{r}",
            "plot(rnorm(100), col = \"red\")",
            "plot(rnorm(100), col = \"green\")",
            "plot(rnorm(100), main = \"color\")",
            "list(color1 = 'green', color2 = '#2a3b6a', color3 = '#2a3b6af2')",
            "```"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_document_color(temp_file)
    expect_length(result, 5)

    expect_equal(result[[1]]$range$start, list(line = 5, character = 24))
    expect_equal(result[[1]]$range$end, list(line = 5, character = 27))
    expect_equal(result[[1]]$color, get_color("red"))

    expect_equal(result[[2]]$range$start, list(line = 6, character = 24))
    expect_equal(result[[2]]$range$end, list(line = 6, character = 29))
    expect_equal(result[[2]]$color, get_color("green"))

    expect_equal(result[[3]]$range$start, list(line = 8, character = 15))
    expect_equal(result[[3]]$range$end, list(line = 8, character = 20))
    expect_equal(result[[3]]$color, get_color("green"))

    expect_equal(result[[4]]$range$start, list(line = 8, character = 33))
    expect_equal(result[[4]]$range$end, list(line = 8, character = 40))
    expect_equal(result[[4]]$color, get_color("#2a3b6a"), tolerance = 1e-3)

    expect_equal(result[[5]]$range$start, list(line = 8, character = 53))
    expect_equal(result[[5]]$range$end, list(line = 8, character = 62))
    expect_equal(result[[5]]$color, get_color("#2a3b6af2"), tolerance = 1e-3)
})
