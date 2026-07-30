test_that("general LSP 3.18 capabilities are advertised", {
    expect_equal(ServerCapabilities$positionEncoding, "utf-16")
    expect_equal(
        ServerCapabilities$textDocumentSync$change,
        TextDocumentSyncKind$Incremental
    )
    expect_true(ServerCapabilities$documentRangeFormattingProvider$rangesSupport)
    expect_true(ServerCapabilities$semanticTokensProvider$full$delta)
    expect_equal(tail(ServerCapabilities$semanticTokensProvider$legend$tokenTypes, 1), "label")
})

test_that("rename prepare capability follows client support", {
    capabilities <- update_server_capabilities(
        ServerCapabilities,
        list(textDocument = list(rename = list(prepareSupport = TRUE)))
    )
    expect_equal(capabilities$renameProvider, RenameOptions)
})

test_that("incremental document changes are sequential and UTF-16 aware", {
    document <- Document$new(
        "file:///incremental.R",
        version = 1L,
        content = c("a\U0001f600b", "second")
    )
    document$apply_content_changes(2L, list(
        list(
            range = list(
                start = list(line = 0L, character = 1L),
                end = list(line = 0L, character = 3L)
            ),
            text = "X"
        ),
        list(
            range = list(
                start = list(line = 0L, character = 2L),
                end = list(line = 0L, character = 2L)
            ),
            text = "\nnew"
        )
    ))

    expect_equal(document$content, c("aX", "newb", "second"))
    expect_equal(document$version, 2L)
    expect_equal(document$nline, 3L)
})

test_that("LSP 3.18 multiple-range formatting returns non-overlapping edits", {
    document <- Document$new(
        "file:///format.R",
        language = "r",
        version = 1L,
        content = c("x<-1", "", "y<-2")
    )
    reply <- ranges_formatting_reply(
        1L,
        document$uri,
        document,
        list(
            list(start = list(row = 0L, col = 0L), end = list(row = 0L, col = 4L)),
            list(start = list(row = 2L, col = 0L), end = list(row = 2L, col = 4L))
        ),
        list(tabSize = 2L, insertSpaces = TRUE)
    )

    expect_length(reply$result, 2L)
    expect_equal(vapply(
        reply$result,
        function(edit) edit$range$start$line,
        numeric(1L)
    ), c(0, 2))
})
