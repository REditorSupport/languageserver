test_that("Quarto files and language identifiers are literate documents", {
    expect_true(is_rmarkdown("file:///analysis.qmd"))
    expect_true(is_rmarkdown("file:///analysis.txt", "quarto"))
    expect_true(is_rmarkdown("file:///analysis.txt", "quarto-markdown"))
    expect_true(is_rmarkdown("file:///analysis.txt", "quartoMarkdown"))
    expect_true(is_rmarkdown("file:///analysis.txt", "qmd"))
    expect_false(is_rmarkdown("file:///analysis.md", "markdown"))

    quarto <- Document$new(
        "file:///analysis.txt", language = "quarto-markdown", content = "text"
    )
    expect_true(quarto$is_rmarkdown)
    expect_identical(quarto$regions$line_type, "markdown")
})

test_that("Quarto outlines defer R definitions to embedded cell providers", {
    content <- c(
        "## Air Quality",
        "```{r}",
        "# test function",
        "fun1 <- function(x) {",
        "  x + 1",
        "}",
        "```",
        "```{r}",
        "fun1(1)",
        "```"
    )
    uri <- "file:///outline.qmd"
    parse_data <- parse_document(uri, content, is_rmarkdown = TRUE)
    parse_data$version <- 1L
    parse_data$xml_doc <- xml2::read_xml(parse_data$xml_data)

    workspace <- new.env(parent = baseenv())
    workspace$get_parse_data <- function(uri) parse_data
    workspace$get_definitions_for_uri <- function(uri) parse_data$definitions

    quarto <- Document$new(
        uri, language = "quarto", version = 1L, content = content
    )
    quarto_symbols <- document_symbol_reply(
        1L, uri, workspace, quarto,
        list(hierarchicalDocumentSymbolSupport = TRUE)
    )$result
    quarto_names <- vapply(quarto_symbols, `[[`, character(1L), "name")
    expect_setequal(
        quarto_names,
        c("Air Quality", "unnamed-chunk-1", "unnamed-chunk-2")
    )
    expect_false("fun1" %in% quarto_names)

    standalone <- Document$new(
        uri, language = "rmd", version = 1L, content = content
    )
    standalone_symbols <- document_symbol_reply(
        2L, uri, workspace, standalone,
        list(hierarchicalDocumentSymbolSupport = TRUE)
    )$result
    standalone_names <- vapply(
        standalone_symbols, `[[`, character(1L), "name"
    )
    expect_identical(sum(standalone_names == "fun1"), 1L)
})

test_that("literate regions distinguish YAML Markdown divs and cells", {
    content <- c(
        "---",
        "title: Test",
        "# not a heading",
        "---",
        "# Heading",
        ":::: {.outer}",
        "::: {.inner}",
        "```{python}",
        "r_like <- 1",
        ":::",
        "```",
        ":::",
        "~~~{r}",
        "#| label: quoted-cell",
        "value <- 1",
        "~~~",
        "```{.r}",
        "static <- TRUE",
        "```",
        "::::"
    )

    regions <- parse_literate_regions(content)
    expect_identical(regions$line_type[1:4], rep("yaml", 4L))
    expect_identical(regions$line_type[[5L]], "markdown")
    expect_identical(regions$line_type[[9L]], "cell")
    expect_identical(regions$line_type[[10L]], "cell")
    expect_identical(regions$line_type[[14L]], "yaml")
    expect_identical(regions$line_type[[15L]], "r")
    expect_equal(vapply(regions$divs, `[[`, integer(1L), "depth"), c(1L, 2L))
    expect_equal(vapply(regions$divs, `[[`, integer(1L), "end_line"), c(20L, 12L))
    expect_equal(
        vapply(regions$cells[1:2], `[[`, character(1L), "engine"),
        c("python", "r")
    )
    expect_false(regions$cells[[3L]]$executable)
    expect_null(regions$cells[[3L]]$engine)
    expect_identical(regions$cells[[2L]]$label, "quoted-cell")
})

test_that("scope is limited to R code rather than executable-cell metadata", {
    content <- c(
        "---", "title: value", "---",
        "value in prose",
        "```{python}", "value = 1", "```",
        "```{r}", "#| label: calculation", "value <- 1", "```"
    )
    document <- Document$new(
        "file:///scope.qmd", language = "quarto", content = content
    )

    scoped <- vapply(seq_along(content) - 1L, function(row) {
        check_scope(document$uri, document, list(row = row, col = 1L))
    }, logical(1L))
    expect_identical(which(scoped), 10L)
})

test_that("R cells are validated independently with original line positions", {
    content <- c(
        "# First",
        "```{r}",
        "first <- 1",
        "```",
        "# Broken",
        "```{r}",
        "#| eval: false",
        "broken <- function(",
        "```",
        "# Python",
        "```{python}",
        "leaked <- 2",
        "```",
        "# Last",
        "```{r}",
        "last <- first + 1",
        "```"
    )

    normalized <- normalize_parse_content(content, is_rmarkdown = TRUE)
    expect_identical(which(nzchar(normalized)), c(3L, 16L))

    parsed <- parse_document(
        "file:///independent.qmd", content, is_rmarkdown = TRUE)
    expect_false(parsed$parse_error)
    expect_setequal(names(parsed$definitions), c("first", "last"))
    expect_true(all(parsed$semantic_data$lines %in% c(2L, 15L)))

    sections <- get_rmd_document_sections(content)
    expect_setequal(
        vapply(sections, `[[`, character(1L), "name"),
        c(
            "First", "Broken", "Python", "Last",
            "unnamed-chunk-1", "unnamed-chunk-2", "unnamed-chunk-3",
            "unnamed-chunk-4"
        )
    )
})

test_that("structural ranges include nested divs and ignore headings in cells", {
    content <- c(
        "---", "title: Test", "---",
        "# Top",
        ":::: {.outer}",
        "::: #inner",
        "```{r named}",
        "# not a heading",
        "x <- 1",
        "```",
        ":::",
        "::::"
    )
    regions <- get_literate_document_sections(
        content, c("section", "chunk", "yaml", "div", "code")
    )

    expect_setequal(
        vapply(regions, `[[`, character(1L), "name"),
        c("Top", "named", "YAML", "outer", "inner")
    )
    divs <- Filter(function(region) region$type == "div", regions)
    expect_equal(vapply(divs, `[[`, integer(1L), "start_line"), c(5L, 6L))
    expect_equal(vapply(divs, `[[`, integer(1L), "end_line"), c(12L, 11L))
})

test_that("point providers do not leak from R cells into Markdown", {
    content <- c(
        "value in prose",
        "```{r}",
        "value <- 1",
        "value",
        "```"
    )
    uri <- "file:///provider.qmd"
    document <- Document$new(uri, language = "quarto", version = 1L, content = content)
    parse_data <- parse_document(uri, content, is_rmarkdown = TRUE)
    parse_data$version <- 1L
    parse_data$xml_doc <- xml2::read_xml(parse_data$xml_data)
    document$update_parse_data(parse_data)

    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- new.env(parent = baseenv())
    workspace$documents <- documents
    workspace$get_parse_data <- function(...) parse_data

    prose <- document_highlight_reply(
        1L, uri, workspace, document, list(row = 0L, col = 1L)
    )
    code <- document_highlight_reply(
        2L, uri, workspace, document, list(row = 3L, col = 1L)
    )
    expect_null(prose$result)
    expect_length(code$result, 2L)
})

test_that("range formatting rejects Markdown and cross-cell ranges", {
    document <- Document$new(
        "file:///format.qmd", language = "quarto",
        content = c(
            "plain text", "```{r}", "x<-1", "```",
            "more text", "```{r}", "y<-2", "```"
        )
    )
    options <- list(tabSize = 2L, insertSpaces = TRUE)

    prose <- range_formatting_reply(
        1L, document$uri, document,
        list(start = list(row = 0L, col = 0L), end = list(row = 0L, col = 4L)),
        options
    )
    cross_cell <- range_formatting_reply(
        2L, document$uri, document,
        list(start = list(row = 2L, col = 0L), end = list(row = 6L, col = 4L)),
        options
    )
    expect_identical(prose$result, list())
    expect_identical(cross_cell$result, list())
})

test_that("Quarto documents work end to end with isolated cells", {
    skip_on_cran()
    client <- language_client(capabilities = list(
        textDocument = list(
            documentSymbol = list(hierarchicalDocumentSymbolSupport = TRUE)
        )
    ))
    path <- withr::local_tempfile(fileext = ".qmd")
    writeLines(c(
        "---",
        "title: Quarto",
        "---",
        "# Analysis",
        "value in prose",
        "```{python}",
        "leaked <- 1",
        "```",
        "```{r}",
        "#| label: broken-cell",
        "#| eval: false",
        "broken <- function(",
        "```",
        "## Results",
        "```{r valid-cell}",
        "good <- function(x) x + 1",
        "good(1)",
        "```"
    ), path)

    client %>% did_open(path, languageId = "quarto")
    symbols <- client %>% respond_document_symbol(path)
    expect_setequal(
        map_chr(symbols, ~ .x$name),
        c(
            "Analysis", "Results", "unnamed-chunk-1", "broken-cell",
            "valid-cell"
        )
    )

    tokens <- client %>% respond_semantic_tokens_full(path)
    encoded <- matrix(tokens$data, ncol = 5L, byrow = TRUE)
    token_lines <- cumsum(encoded[, 1L])
    expect_true(length(token_lines) > 0L)
    expect_true(all(token_lines %in% c(15L, 16L)))
})
