skip_if_not_installed("styler", "1.5.1")

newline_formatting_result <- function(content, row = length(content) - 1L,
    options = list(tabSize = 4L, insertSpaces = TRUE), uri = "file:///newline.R", style = NULL) {
    withr::local_options(languageserver.formatting_style = style, styler.cache_name = NULL)
    document <- Document$new(uri, language = if (is_rmarkdown(uri)) "rmd" else "r",
        version = 1L, content = content)
    point <- list(row = row, col = nchar(content[[row + 1L]]))
    edits <- styler_on_type_edits(uri, document, point, "\n", options)
    for (edit in rev(edits)) {
        document$apply_content_changes(2L, list(list(range = edit$range, text = edit$newText)))
    }
    list(content = document$content, edits = edits)
}

test_that("Enter indents bare headers without introducing delimiters", {
    withr::local_options(languageserver.formatting_style = NULL)
    headers <- c("if (TRUE)", "while (TRUE)", "for (i in 1:3)", "f <- function(x)")
    for (header in headers) {
        result <- newline_formatting_result(c(header, ""))
        expect_length(result$edits, 1L)
        expect_equal(result$content, c(header, "    "))
    }
})

test_that("Enter preserves manual line breaks in complete and incomplete calls", {
    complete <- newline_formatting_result(c("tibble(a = 1:10, b = 2:11,", " )"))
    expect_equal(complete$content, c("tibble(a = 1:10, b = 2:11,", ")"))
    incomplete <- newline_formatting_result(c("f(", "", " a=1,", ""))
    expect_equal(incomplete$content, c("f(", "", "    a = 1,", "    "))
})

test_that("Enter preserves literal tokens and semicolons", {
    quoted <- newline_formatting_result(c("f('quoted',", ""))
    expect_equal(quoted$content, c("f('quoted',", "    "))
    semicolon <- newline_formatting_result(c("x=1;y=2", ""))
    expect_equal(semicolon$content, c("x = 1;y = 2", ""))
})

test_that("Enter keeps multiline literals and quoted names unchanged", {
    literals <- list(
        c('f("first', '  second",', ""),
        c('f(r"(first', '  second)",', ""),
        c("f(`名", "  字`,", "")
    )
    for (content in literals) {
        result <- newline_formatting_result(content)
        expect_equal(result$content, c(content[-length(content)], "    "))
    }
})

test_that("Enter respects tabs without replacing preceding source", {
    options <- list(tabSize = 4L, insertSpaces = FALSE)
    for (header in c("\tif (TRUE)", "\tif (TRUE) {")) {
        result <- newline_formatting_result(c(header, "\t"), options = options)
        expect_equal(result$content, c(header, "\t\t"))
    }
})

test_that("Enter preserves literate fences and adjacent prose", {
    content <- c("intro", "```{r}", "if (TRUE)", "", "```", "tail")
    result <- newline_formatting_result(content, row = 3L, uri = "file:///newline.Rmd")
    expect_equal(result$content, c("intro", "```{r}", "if (TRUE)", "    ", "```", "tail"))
})

test_that("Enter preserves leading blank lines and nested headers", {
    result <- newline_formatting_result(c("", "f <- function() {", "    if (TRUE)", ""))
    expect_equal(result$content, c("", "f <- function() {", "    if (TRUE)", "        "))
})

test_that("Enter ignores token and line-break transforms in a custom style", {
    style <- function(options) {
        result <- styler::tidyverse_style(indent_by = options$tabSize)
        result$token <- result$line_break <- list(unexpected = function(data) {
            stop("Enter should not run token or line-break transforms")
        })
        result
    }
    result <- newline_formatting_result(c("if (TRUE)", ""), style = style)
    expect_equal(result$content, c("if (TRUE)", "    "))
})

test_that("Enter rejects custom whitespace transforms that change source", {
    cases <- list(
        list(content = c("f('quoted',", ""), transform = function(data) {
            data$text[data$text == "'quoted'"] <- '\"quoted\"'
            data
        }),
        list(content = c("if (TRUE) {", ""), transform = function(data) {
            data$lag_newlines[data$text == "TRUE"] <- 1L
            data
        })
    )
    for (case in cases) {
        style <- function(options) {
            result <- styler::tidyverse_style(indent_by = options$tabSize)
            result$space$unexpected <- case$transform
            result
        }
        result <- newline_formatting_result(case$content, style = style)
        expect_equal(result$content, c(case$content[[1L]], "    "))
    }
})

test_that("Enter uses a distinct styler cache policy", {
    withr::local_options(languageserver.formatting_style = NULL,
        styler.cache_name = "newline-regression", R.cache.rootPath = withr::local_tempdir())
    options <- list(tabSize = 4L, insertSpaces = TRUE)
    content <- c("if (TRUE)", "")
    completion <- complete_incomplete_expression(content)
    styled <- style_text(completion$text, get_style(options))
    expect_true(grepl("if (TRUE) {", styled, fixed = TRUE))

    document <- Document$new("file:///cached-newline.R", language = "r", content = content)
    for (request in 1:2) {
        edits <- styler_on_type_edits(document$uri, document, list(row = 1L, col = 0L), "\n", options)
        expect_length(edits, 1L)
        expect_equal(edits[[1L]]$newText, "if (TRUE)\n    ")
    }
})

test_that("LSP Enter formatting preserves bare headers and split calls", {
    skip_on_cran()
    withr::local_options(languageserver.formatting_style = NULL, styler.cache_name = NULL)
    root <- withr::local_tempdir()
    client <- language_client(working_dir = root)
    cases <- list(
        list(content = c("if (TRUE)", ""), pos = c(1L, 0L), expected = "if (TRUE)\n    "),
        list(content = c("tibble(a = 1:10, b = 2:11,", " )"), pos = c(1L, 1L),
            expected = "tibble(a = 1:10, b = 2:11,\n)")
    )
    for (index in seq_along(cases)) {
        case <- cases[[index]]
        path <- file.path(root, paste0("newline-", index, ".R"))
        writeLines(case$content, path)
        did_open(client, path, text = case$content)
        edits <- respond_on_type_formatting(client, path, case$pos, "\n", retry = FALSE)
        expect_length(edits, 1L)
        expect_equal(edits[[1L]]$newText, case$expected)
    }
})
