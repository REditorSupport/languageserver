test_that("code lenses lazily resolve R function call counts", {
    expect_true(ServerCapabilities$codeLensProvider$resolveProvider)
    fixture <- provider_fixture(c(
        "foo <- function(x) x + 1",
        "foo(1)",
        "foo(2)",
        "other::foo(3)"
    ))
    capabilities <- list(textDocument = list(codeLens = list(
        resolveSupport = list(properties = list("command"))
    )))

    reply <- code_lens_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, capabilities)
    expect_length(reply$result, 1L)
    expect_null(reply$result[[1L]]$command)

    resolved <- code_lens_resolve_reply(
        2L, fixture$workspace, reply$result[[1L]])$result
    expect_equal(resolved$command$title, "2 calls")
    expect_equal(
        resolved$command$command,
        "editor.action.peekLocations"
    )
    expect_length(resolved$command$arguments, 3L)
    expect_equal(resolved$command$arguments[[1L]]$`$mid`, 1L)
    expect_equal(resolved$command$arguments[[1L]]$scheme, "file")
    expect_equal(
        resolved$command$arguments[[2L]],
        list(lineNumber = 1L, column = 1L)
    )
    expect_length(resolved$command$arguments[[3L]], 2L)
    expect_true(all(vapply(
        resolved$command$arguments[[3L]],
        function(value) identical(value$uri$`$mid`, 1L),
        logical(1L)
    )))
})

test_that("code lenses work through the language server after incremental edits", {
    skip_on_cran()
    client <- language_client(capabilities = list(textDocument = list(
        codeLens = list(resolveSupport = list(properties = list("command")))
    )))
    path <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "foo <- function(x) x + 1",
        "foo(1)"
    ), path)
    client %>% did_open(path)
    uri <- path_to_uri(path)

    lenses <- respond(
        client,
        "textDocument/codeLens",
        list(textDocument = list(uri = uri))
    )
    expect_length(lenses, 1L)
    resolved <- respond(client, "codeLens/resolve", lenses[[1L]])
    expect_equal(resolved$command$title, "1 call")
    expect_equal(resolved$command$command, "editor.action.peekLocations")
    expect_length(resolved$command$arguments[[3L]], 1L)

    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(
            range = list(
                start = list(line = 0L, character = 0L),
                end = list(line = 0L, character = 3L)
            ),
            rangeLength = 3L,
            text = "bar"
        ))
    ))
    changed_lenses <- respond(
        client,
        "textDocument/codeLens",
        list(textDocument = list(uri = uri))
    )
    expect_equal(changed_lenses[[1L]]$data$symbol, "bar")
})

test_that("code lenses cover XML fallback and non-resolvable definitions", {
    fixture <- provider_fixture(c(
        "foo <- function(x) x",
        "foo(1)",
        "pkg::foo(2)",
        "value <- 3"
    ))
    fixture$document$parse_data$reference_index <- NULL

    locations <- function_call_locations(fixture$workspace, "foo")
    expect_length(locations, 1L)
    expect_equal(locations[[1L]]$range$start$line, 1L)

    expect_length(function_call_locations(fixture$workspace, "absent"), 0L)
    saved_xml <- fixture$document$parse_data$xml_doc
    fixture$document$parse_data$xml_doc <- NULL
    expect_length(function_call_locations(fixture$workspace, "foo"), 0L)
    fixture$document$parse_data$xml_doc <- saved_xml

    incomplete <- list(data = list(uri = fixture$uri))
    expect_identical(
        resolve_function_code_lens(fixture$workspace, incomplete),
        incomplete
    )

    fixture$document$parse_data$definitions <- list()
    empty <- code_lens_reply(
        1L, fixture$uri, fixture$workspace, fixture$document)$result
    expect_length(empty, 0L)

    definition_range <- range(position(0L, 0L), position(0L, 5L))
    fixture$document$parse_data$definitions <- list(
        value = list(type = "double", range = definition_range),
        foo = list(type = "function", range = definition_range)
    )
    eager <- code_lens_reply(
        2L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(textDocument = list(codeLens = list(
            resolveSupport = list(properties = "range")
        )))
    )$result
    expect_length(eager, 1L)
    expect_equal(eager[[1L]]$command$title, "1 call")
})
