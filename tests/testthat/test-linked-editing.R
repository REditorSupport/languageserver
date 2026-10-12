test_that("linked editing connects roxygen parameters and R formals", {
    expect_true(ServerCapabilities$linkedEditingRangeProvider)
    fixture <- provider_fixture(c(
        "#' Add one",
        "#' @param x A value.",
        "foo <- function(x) x + 1"
    ))
    reply <- linked_editing_range_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(line = 1L, character = 10L)
    )

    expect_length(reply$result$ranges, 2L)
    expect_equal(reply$result$ranges[[1L]]$start$line, 2L)
    expect_equal(reply$result$ranges[[2L]]$start$line, 1L)
})

test_that("linked editing works through the language server", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "#' Add one",
        "#' @param x A value.",
        "foo <- function(x) x + 1"
    ), path)
    client %>% did_open(path)

    result <- respond(
        client,
        "textDocument/linkedEditingRange",
        list(
            textDocument = list(uri = path_to_uri(path)),
            position = list(line = 1L, character = 10L)
        )
    )
    expect_length(result$ranges, 2L)
})

test_that("linked editing rejects incomplete definitions and documentation", {
    point <- list(line = 0L, character = 0L)
    fixture <- provider_fixture("value <- 1")
    fixture$document$parse_data$xml_doc <- NULL
    expect_null(linked_editing_range_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, point
    )$result)

    cases <- list(
        list(content = "value <- 1", definitions = list(value = list(
            type = "double", range = range(position(0L, 0L), position(0L, 5L))
        ))),
        list(content = "value <- 1", definitions = list(missing = list(
            type = "function", range = range(position(0L, 0L), position(0L, 5L))
        ))),
        list(content = "foo <- function() 1", definitions = NULL),
        list(content = c("#' @param other docs", "foo <- function(x) x"), definitions = NULL)
    )
    for (case in cases) {
        item <- provider_fixture(case$content)
        if (!is.null(case$definitions)) {
            item$document$parse_data$definitions <- case$definitions
        }
        expect_null(linked_editing_range_reply(
            2L, item$uri, item$workspace, item$document, point
        )$result)
    }

    documented <- Document$new(
        "file:///empty-param.R",
        content = c("#' @param x,,y docs", "foo <- function(x, y) NULL")
    )
    ranges <- roxygen_parameter_ranges(documented, 1L)
    expect_setequal(names(ranges), c("x", "y"))
})

test_that("indexed linked editing preserves definition context and cached ranges without XML", {
    fixture <- provider_fixture(c(
        "#' @param argument First documentation.",
        "first <- function(argument) argument",
        "#' @param argument Second documentation.",
        "second <- function(argument) {",
        "  # nested function must not replace its containing definition",
        "  first <- function(argument) argument + 1",
        "  argument",
        "}",
        "#' @param argument Right-assignment documentation.",
        "function(argument) argument -> right"
    ))
    parsed <- fixture$document$parse_data
    legacy <- list2env(as.list(parsed), parent = emptyenv())
    legacy$syntax_data <- NULL
    for (line in seq_along(fixture$document$content)) {
        for (col in c(0L, 10L, 18L, 26L)) {
            point <- fixture$document$to_lsp_position(line - 1L, col)
            fixture$document$parse_data <- legacy
            expected <- linked_editing_range_reply(1L, fixture$uri, fixture$workspace,
                fixture$document, point)$result
            fixture$document$parse_data <- parsed
            parsed$xml_doc <- NULL
            actual <- linked_editing_range_reply(1L, fixture$uri, fixture$workspace,
                fixture$document, point)$result
            expect_equal(actual, expected, info = paste(line, col))
            expect_null(parsed$xml_doc)
        }
    }
    fixture$document$parse_data <- parsed
    expect_equal(linked_editing_range_reply(2L, fixture$uri, fixture$workspace,
        fixture$document, list(line = 0L, character = 13L))$result,
        linked_editing_range_reply(3L, fixture$uri, fixture$workspace,
            fixture$document, list(line = 0L, character = 13L))$result)
    expect_null(parsed$xml_doc)
})
