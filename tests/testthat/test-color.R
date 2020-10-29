context("Test Color")

get_color <- function(color) {
    rgba <- grDevices::col2rgb(color, alpha = TRUE) / 255
    as.list(rgba[, 1])
}

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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
