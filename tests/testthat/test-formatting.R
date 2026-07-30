test_that("Formatting document works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn<-function(x){",  # nolint
        "f(x+1,x-1)+x",
        "}"
    ), temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_formatting(temp_file)

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_length(lines, 3)
    expect_equal(lines, c(
        "my_fn <- function(x) {",
        "    f(x + 1, x - 1) + x",
        "}"
    ))
})


test_that("Formatting selection works for complete line", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn <- function(x) {",
        "    y =x+ 1",
        "    y+3",
        "}"
    ), temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(1, 0), c(2, 7))

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_equal(lines[1], "    y <- x + 1")
    expect_equal(lines[2], "    y + 3")
})


test_that("Formatting selection works for partial line", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn <- function(x) {",
        "    y =x+ 1",
        "    y+3",
        "}"
    ), temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(1, 4), c(2, 7))

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_equal(lines[1], "    y = x + 1")
    expect_equal(lines[2], "    y + 3")
})

test_that("Formatting selection preserves initial indentation", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "  my_fn <- function(x) {",
        "      y =x+ 1",
        "      y+3",
        "  }"
    ), temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(0, 0), c(3, 3))

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_equal(lines[1], "  my_fn <- function(x) {")
    expect_equal(lines[2], "      y <- x + 1")
    expect_equal(lines[3], "      y + 3")
    expect_equal(lines[4], "  }")
})

test_that("Formatting selection does not add indentation to multi-line string", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fun <- function() {",
        "  query(con,\"select group, date, time",
        "    from some_table",
        "    where group > 10\")",
        "}"
    ), temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_range_formatting(temp_file, c(1, 0), c(3, 23))

    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_equal(lines[1], "  query(con, \"select group, date, time")
    expect_equal(lines[2], "    from some_table")
    expect_equal(lines[3], "    where group > 10\")")
})

test_that("On type formatting works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_fn<-function(x){",  # nolint
        "f(x+1,x-1)# call function",
        "data[x,y]",
        "}",
        "",
        "data%>%",
        " mutate(a=1,b=2)%>%# change data",
        "  filter(a>=2)%>%",
        "select(a,b)"
    ), temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_on_type_formatting(temp_file, c(1, 10), ")")
    expect_length(result, 1)
    expect_equal(result[[1]]$newText, "f(x + 1, x - 1) # call function")

    result <- client %>% respond_on_type_formatting(temp_file, c(2, 9), ")")
    expect_length(result, 1)
    expect_equal(result[[1]]$newText, "data[x, y]")

    result <- client %>% respond_on_type_formatting(temp_file, c(3, 1), "}")
    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_length(lines, 4)
    expect_equal(lines, c(
        "my_fn <- function(x) {",
        "    f(x + 1, x - 1) # call function",
        "    data[x, y]",
        "}"
    ))

    result <- client %>% respond_on_type_formatting(temp_file, c(3, 1), "\n")
    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_length(lines, 4)
    expect_equal(lines, c(
        "my_fn <- function(x) {",
        "    f(x + 1, x - 1) # call function",
        "    data[x, y]",
        "}"
    ))

    result <- client %>% respond_on_type_formatting(temp_file, c(8, 13), "\n")
    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_length(lines, 4)
    expect_equal(lines, c(
        "data %>%",
        "    mutate(a = 1, b = 2) %>% # change data",
        "    filter(a >= 2) %>%",
        "    select(a, b)"
    ))
})

test_that("On type formatting safely completes incomplete expressions", {
    withr::local_options(languageserver.formatting_style = NULL)
    options <- list(tabSize = 4L, insertSpaces = TRUE)
    cases <- list(
        call = list(
            content = c("foo(", "  a=1,", ""),
            expected = "foo(\n    a = 1,\n    "
        ),
        pipeline = list(
            content = c("data%>%", " mutate(a=1)%>%", ""),
            expected = "data %>%\n    mutate(a = 1) %>%\n    "
        ),
        block = list(
            content = c("if(x){", ""),
            expected = "if (x) {\n    "
        ),
        for_loop = list(
            content = c("for(", ""),
            expected = "for (\n    "
        )
    )

    for (case in cases) {
        document <- Document$new(
            "file:///incomplete.R",
            language = "r",
            content = case$content
        )
        point <- list(
            row = length(case$content) - 1L,
            col = nchar(case$content[[length(case$content)]])
        )
        reply <- on_type_formatting_reply(
            1L, document$uri, document, point, "\n", options
        )

        expect_length(reply$result, 1L)
        expect_equal(reply$result[[1L]]$newText, case$expected)
        expect_false(grepl("languageserver_formatting_sentinel", reply$result[[1L]]$newText))
    }
})

test_that("On type formatting falls back to indentation for unsafe syntax", {
    withr::local_options(languageserver.formatting_style = NULL)
    document <- Document$new(
        "file:///incomplete.R",
        language = "r",
        content = c(
            "f <- function() {",
            "    x <- \"unterminated",
            ""
        )
    )
    reply <- on_type_formatting_reply(
        1L,
        document$uri,
        document,
        list(row = 2L, col = 0L),
        "\n",
        list(tabSize = 4L, insertSpaces = TRUE)
    )

    expect_length(reply$result, 1L)
    expect_equal(
        unclass(reply$result[[1L]]$range$start),
        list(line = 2L, character = 0L)
    )
    expect_equal(
        unclass(reply$result[[1L]]$range$end),
        list(line = 2L, character = 0L)
    )
    expect_equal(reply$result[[1L]]$newText, "    ")
})

test_that("Formatting in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "```{r}",
            "my_fn= function(x) {x + 1; x}",
            "```"
        ),
        single_file
    )

    client %>% did_open(single_file)

    # first query a known function to make sure the file is processed
    result <- client %>% respond_formatting(single_file)

    expect_length(result, 1)
    expect_equal(result[[1]]$range$start, list(line = 1, character = 0))
    expect_equal(result[[1]]$range$end, list(line = 1, character = 29))
    expect_equal(result[[1]]$newText, "my_fn <- function(x) {\n    x + 1\n    x\n}")
})

test_that("On type formatting works in Rmarkdown", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(c(
        "---",
        "title: 1+1",
        "---",
        "",
        "1+1",
        "",
        "```{r}",
        "my_fn<-function(x){",  # nolint
        "f(x+1,x-1)",
        "data[x,y]",
        "}",
        "```"
    ), temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_on_type_formatting(temp_file, c(1, 10), ")", retry = FALSE)
    expect_length(result, 0)

    result <- client %>% respond_on_type_formatting(temp_file, c(4, 3), ")", retry = FALSE)
    expect_length(result, 0)

    result <- client %>% respond_on_type_formatting(temp_file, c(10, 1), "}")
    expect_length(result, 1)
    lines <- strsplit(result[[1]]$newText, "\n")[[1]]
    expect_length(lines, 4)
    expect_equal(lines, c(
        "my_fn <- function(x) {",
        "    f(x + 1, x - 1)",
        "    data[x, y]",
        "}"
    ))
})

test_that("Formatting helpers handle invalid and synthesized source", {
    withr::local_options(languageserver.formatting_style = NULL)
    options <- list(tabSize = 2L, insertSpaces = TRUE)

    expect_null(style_text("x <-", get_style(options)))
    expect_equal(missing_closing_delimiters(character()), "")
    expect_equal(missing_closing_delimiters(c("foo(", "bar[")), "])")
    expect_equal(missing_closing_delimiters("value <- 1"), "")
    expect_null(complete_incomplete_expression("   "))
    expect_null(complete_incomplete_expression("value <- 1"))

    completed <- complete_incomplete_expression(c(
        "foo(", ".__languageserver_formatting_sentinel__ = 1,"
    ))
    expect_false(is.null(completed))
    expect_match(completed$sentinel, "sentinel__+", perl = TRUE)
    expect_equal(remove_formatting_sentinel(
        paste0("prefix", completed$sentinel), completed$sentinel
    ), "prefix")
    expect_null(remove_formatting_sentinel("without sentinel", "missing"))
    expect_null(remove_formatting_sentinel("marker marker", "marker"))

    called <- FALSE
    withr::local_options(languageserver.formatting_style = function(options) {
        called <<- TRUE
        styler::tidyverse_style(indent_by = options$tabSize * 2L)
    })
    expect_type(get_style(options), "list")
    expect_true(called)
})

test_that("Formatting handles empty R Markdown and invalid blocks", {
    options <- list(tabSize = 2L, insertSpaces = TRUE)
    plain <- Document$new(
        "file:///plain.Rmd", language = "rmd", content = "plain prose"
    )
    expect_identical(
        formatting_reply(1L, plain$uri, plain, options)$result,
        list()
    )

    invalid <- Document$new(
        "file:///invalid.Rmd", language = "rmd",
        content = c("```{r}", "x <-", "```")
    )
    reply <- formatting_reply(1L, invalid$uri, invalid, options)
    expect_length(reply$result, 1L)
    expect_equal(reply$result[[1L]]$newText, "x <-")
})

test_that("Range formatting handles empty and line-ending selections", {
    options <- list(tabSize = 2L, insertSpaces = TRUE)
    document <- Document$new(
        "file:///ranges.R", language = "r",
        content = c("x<-1", "y<-2", "z<-3")
    )

    empty <- range_formatting_reply(
        1L, document$uri, document,
        list(start = list(row = 0L, col = 2L),
            end = list(row = 0L, col = 2L)),
        options
    )
    expect_identical(empty$result, list())

    full_line <- range_formatting_reply(
        1L, document$uri, document,
        list(start = list(row = 0L, col = 0L),
            end = list(row = 1L, col = 0L)),
        options
    )
    expect_length(full_line$result, 1L)
    expect_equal(full_line$result[[1L]]$newText, "x <- 1")

    merged <- ranges_formatting_reply(
        1L, document$uri, document,
        list(
            list(start = list(row = 1L, col = 0L),
                end = list(row = 2L, col = 0L)),
            list(start = list(row = 0L, col = 0L),
                end = list(row = 1L, col = 4L))
        ),
        options
    )
    expect_length(merged$result, 1L)
    expect_equal(merged$result[[1L]]$newText, "x <- 1\ny <- 2")
    expect_identical(
        ranges_formatting_reply(
            1L, document$uri, document, list(), options
        )$result,
        list()
    )
})

test_that("Indentation fallback handles tabs, invalid points, and blank context", {
    options <- list(tabSize = NA_integer_, insertSpaces = FALSE)
    document <- Document$new(
        "file:///indent.R", language = "r",
        content = c("if (TRUE) {", "", "  ", "value <- 1")
    )

    expect_null(indentation_only_reply(
        1L, document, list(row = -1L, col = 0L), options
    )$result)
    expect_null(indentation_only_reply(
        1L, document, list(row = 3L, col = 0L), options
    )$result)

    nested <- indentation_only_reply(
        1L, document, list(row = 1L, col = 0L), options
    )
    expect_equal(nested$result[[1L]]$newText, "\t")

    previous <- Document$new(
        "file:///previous.R", language = "r",
        content = c("  value +", "", "")
    )
    spaced <- indentation_only_reply(
        1L, previous, list(row = 2L, col = 0L),
        list(tabSize = 2L, insertSpaces = TRUE)
    )
    expect_equal(spaced$result[[1L]]$newText, "    ")

    blank <- Document$new("file:///blank.R", language = "r", content = "")
    expect_null(indentation_only_reply(
        1L, blank, list(row = 0L, col = 0L), options
    )$result)
})

test_that("On-type formatting ignores out-of-scope and leading newlines", {
    options <- list(tabSize = 2L, insertSpaces = TRUE)
    markdown <- Document$new(
        "file:///scope.Rmd", language = "rmd",
        content = c("prose", "", "```{r}", "x <- 1", "```")
    )
    expect_null(on_type_formatting_reply(
        1L, markdown$uri, markdown, list(row = 0L, col = 5L),
        "\n", options
    )$result)

    document <- Document$new(
        "file:///leading.R", language = "r", content = c("", "# comment")
    )
    expect_null(on_type_formatting_reply(
        1L, document$uri, document, list(row = 0L, col = 0L),
        "\n", options
    )$result)
    expect_null(on_type_formatting_reply(
        1L, document$uri, document, list(row = 1L, col = 9L),
        "\n", options
    )$result)
})
