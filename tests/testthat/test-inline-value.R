test_that("inline values request debugger lookups for R variables", {
    expect_true(ServerCapabilities$inlineValueProvider)
    fixture <- provider_fixture(c(
        "foo <- function(x) {",
        "  y <- x + 1",
        "  y",
        "}"
    ))
    reply <- inline_value_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 3L, character = 1L)
        )
    )

    variable_names <- vapply(reply$result, `[[`, character(1L), "variableName")
    expect_true(all(c("x", "y") %in% variable_names))
    expect_true(all(vapply(
        reply$result, `[[`, logical(1L), "caseSensitiveLookup")))
})

test_that("inline values work through the language server", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "foo <- function(x) {",
        "  y <- x + 1",
        "  y",
        "}"
    ), path)
    client %>% did_open(path)

    result <- respond(
        client,
        "textDocument/inlineValue",
        list(
            textDocument = list(uri = path_to_uri(path)),
            range = list(
                start = list(line = 0L, character = 0L),
                end = list(line = 3L, character = 1L)
            ),
            context = list(
                frameId = 1L,
                stoppedLocation = list(
                    start = list(line = 1L, character = 0L),
                    end = list(line = 1L, character = 12L)
                )
            )
        )
    )
    expect_true("x" %in% vapply(
        result, `[[`, character(1L), "variableName"))
})

test_that("inline values handle empty XML, ranges, and duplicate variables", {
    request <- list(
        start = list(line = 0L, character = 0L),
        end = list(line = 1L, character = 0L)
    )
    fixture <- provider_fixture("value <- 1")
    fixture$document$parse_data$xml_doc <- NULL
    expect_length(inline_value_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, request
    )$result, 0L)

    no_symbols <- provider_fixture(c("# comment", "value <- 1"))
    expect_length(inline_value_reply(
        2L,
        no_symbols$uri,
        no_symbols$workspace,
        no_symbols$document,
        request
    )$result, 0L)

    repeated <- provider_fixture("function(...) value + value")
    full_range <- list(
        start = list(line = 0L, character = 0L),
        end = list(line = 0L, character = 30L)
    )
    values <- inline_value_reply(
        3L,
        repeated$uri,
        repeated$workspace,
        repeated$document,
        full_range
    )$result
    expect_equal(
        sum(vapply(values, `[[`, character(1L), "variableName") == "value"),
        1L
    )

    after_first <- full_range
    after_first$start$character <- 20L
    expect_length(inline_value_reply(
        4L,
        repeated$uri,
        repeated$workspace,
        repeated$document,
        after_first
    )$result, 1L)
})
