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

test_that("editing an indented fence updates the surrounding R and Markdown lines", {
    content <- c(
        "    ~~~~{r}", "\t:::: ignored", "   :::: {.outer}", "   ````{r}",
        "#| label: example", "x <- 1", "  ```", "    ````", "  ~~~~",
        "::: inner text", "  ```` extra", "   ````\t", "::::", "prose"
    )
    doc <- Document$new("file:///fences.qmd", content = content)
    expect_identical(doc$regions$line_type,
        c("markdown", "markdown", "div", "cell", "yaml", rep("r", 6L),
            "cell", "div", "markdown"))
    expect_identical(doc$regions$cells[[1L]]$label, "example")
    expect_true(doc$regions$divs[[1L]]$closed)

    # A four-space indent makes the former closer part of the unclosed cell.
    doc$apply_content_changes(2L, list(list(
        range = range(position(11L, 0L), position(11L, 0L)), text = " ")))
    expect_false(doc$regions$cells[[1L]]$closed)
    expect_false(doc$regions$divs[[1L]]$closed)
    expect_identical(tail(doc$regions$line_type, 3L), rep("r", 3L))
})

test_that("edits to ordinary headings, list items and R comments reuse literate regions", {
    doc <- Document$new("file:///report.qmd", content = c("# Title", "- item", "```{r}", "# comment", "```"))
    regions <- doc$regions
    local_mocked_bindings(parse_literate_regions = function(...) stop("Unexpected full region scan"))
    for (row in c(0L, 1L, 3L)) {
        edit <- list(range = range(position(row, 2L), position(row, 3L)), text = "X")
        doc$apply_content_changes(2L, list(edit))
    }
    expect_identical(doc$content, c("# Xitle", "- Xtem", "```{r}", "# Xomment", "```"))
    expect_identical(doc$regions, regions)
})

test_that("incremental literate regions agree with a full scan across structural edits", {
    content <- c("title", "text", "---", "```{r}", "#| label: old", "x <- 1", "```", "end")
    doc <- Document$new("file:///report.qmd", content = content)
    replace <- function(row, length, text) {
        list(range = range(position(row, 0L), position(row, length)), text = text)
    }
    change <- function(...) {
        doc$apply_content_changes(2L, list(...))
        expect_identical(doc$regions, parse_literate_regions(doc$content))
    }
    change(replace(0L, 5L, "\ufeff---"), replace(1L, 4L, "title: new"))
    expect_true(doc$regions$yaml[[1L]]$closed)
    change(replace(2L, 3L, "ordinary"))
    expect_false(doc$regions$yaml[[1L]]$closed)
    change(replace(2L, 8L, "..."), replace(4L, 12L, "value"),
        replace(5L, 6L, "\t#| label: new"))
    expect_identical(doc$regions$cells[[1L]]$label, "new")
    change(replace(6L, 3L, "`\n``"))
    expect_false(doc$regions$cells[[1L]]$closed)
    change(replace(4L, 5L, "new_value"), replace(7L, 2L, "```"))
    expect_true(doc$regions$cells[[1L]]$closed)
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
    expect_setequal(parsed$semantic_data$lines, c(2L, 7L, 15L))

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

test_that("incomplete R cells preserve complete statements at their original positions", {
    content <- c(
        "prose <- 1",
        "```{r}", "#| label: first", "first <- function(x) x", "stats::", "```",
        "```{python}", "python <- 2", "```",
        "```{r}", "second <- first(2)", 'raw <- r"(unfinished', "hidden <- 3", "```",
        "```{r}", "last <- second", "```",
        "```{r}", 'palette("#ff0000", "target.R",', "```"
    )
    for (ext in c("Rmd", "qmd")) {
        parsed <- parse_document(paste0("file:///cells.", ext), content, is_rmarkdown = TRUE)
        expect_setequal(names(parsed$definitions), c("first", "second", "last"))
        token_lines <- unique(parsed$semantic_data$lines)
        expect_true(all(c(3L, 10L, 15L) %in% token_lines))
        expect_false(any(c(0L, 2L, 7L, 12L) %in% token_lines))
        expect_true(parsed$inlay_hint_incomplete)
        strings <- parsed$range_data$strings
        expect_identical(substr(content[strings$line], strings$col1, strings$col2),
            c('"#ff0000"', '"target.R"'))
    }
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
    skip_if_not_installed("styler", "1.5.1")
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
    expect_equal(map_chr(symbols, "name"), "Analysis")
    symbols <- flatten_document_symbols(symbols)
    expect_setequal(
        map_chr(symbols, ~ .x$name),
        c(
            "Analysis", "Results", "unnamed-chunk-1", "broken-cell",
            "valid-cell", "good"
        )
    )

    tokens <- client %>% respond_semantic_tokens_full(path)
    encoded <- matrix(tokens$data, ncol = 5L, byrow = TRUE)
    token_lines <- cumsum(encoded[, 1L])
    expect_true(length(token_lines) > 0L)
    expect_setequal(token_lines, c(11L, 15L, 16L))
})

test_that("line directives stay inside their R cell and preserve physical definition positions", {
    content <- c("```{r}", '#line 1000 "generated.R"', 'header <- "start',
        '#line 50 inside a string"', "```", "```{r}", "before <- 2", "pkg::", "after <- 3", "```")
    parsed <- parse_document("file:///physical.qmd", content, is_rmarkdown = TRUE)
    expect_identical(names(parsed$definitions), c("header", "before", "after"))
    expect_equal(parsed$definitions$header$range$start, position(2L, 0L))
    expect_equal(parsed$definitions$after$range$start, position(8L, 0L))
    expect_identical(parsed$member_data$items[[1L]]$expr[[3L]], "start\n#line 50 inside a string")
})
