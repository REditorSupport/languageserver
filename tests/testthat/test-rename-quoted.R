test_that("quoted identifiers retain their complete ranges and lexical scope", {
    fixture <- provider_fixture(c(
        "`foo bar` <- 1",
        "`foo bar`",
        "f <- function(`foo bar`) `foo bar`"
    ))
    point <- list(row = 1L, col = 5L)
    expected <- range(position(1L, 0L), position(1L, 9L))
    for (indexed in c(TRUE, FALSE)) {
        if (!indexed) fixture$document$parse_data$reference_index <- NULL
        prepared <- prepare_rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)
        expect_equal(prepared$result, unclass(expected))
        references <- references_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)$result
        expect_length(references, 2L)
        expect_equal(vapply(references, function(ref) ref$range$start$line, numeric(1L)), c(0L, 1L))
        expect_equal(references[[2L]]$range, expected)
    }
})

test_that("equivalent quoted spellings share navigation and reference identities", {
    fixture <- provider_fixture(c("`value` <- 1", "value", "`value`"))
    for (point in list(list(row = 1L, col = 2L), list(row = 2L, col = 3L))) {
        references <- references_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)$result
        expect_length(references, 3L)
        highlights <- document_highlight_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)$result
        expect_length(highlights, 3L)
    }
})

test_that("multiline quoted identifiers convert each endpoint on its own line", {
    text <- paste0(intToUtf8(0x1f600), "b`")
    fixture <- provider_fixture(c("`a", paste0(text, " <- 1"), "`a", text))
    point <- list(row = 2L, col = 1L)
    expected <- list(range(position(0L, 0L), position(1L, 4L)),
        range(position(2L, 0L), position(3L, 4L)))
    refs <- references_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)$result
    expect_equal(lapply(refs, function(ref) ref$range), expected)
    prepared <- prepare_rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)
    expect_equal(prepared$result, unclass(expected[[2L]]))
    renamed <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, "renamed")
    expect_equal(lapply(renamed$result$changes[[fixture$uri]], function(edit) edit$range), expected)
})

test_that("renaming quotes names and escapes their literal characters", {
    fixture <- provider_fixture(c("value <- 1", "value"))
    point <- list(row = 1L, col = 2L)
    for (name in c("new_value", "new name", "if", "+", "a`b", "a\\b", intToUtf8(0x03b1))) {
        reply <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, name)
        edits <- reply$result$changes[[fixture$uri]]
        expect_length(edits, 2L)
        replacement <- edits[[1L]]$newText
        expect_identical(as.character(str2lang(replacement)), name)
        expect_identical(edits[[2L]]$newText, replacement)
        expression <- str2lang(paste0(replacement, " <- 1"))
        expect_identical(as.character(expression[[2L]]), name)
    }
    for (name in list("", NA_character_, NULL, c("a", "b"), 1L)) {
        reply <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, name)
        expect_equal(reply$error$code, ErrorCodes$InvalidParams)
        expect_null(reply$result)
    }
})

test_that("rename groups edits once while preserving URI and occurrence order", {
    fixture <- provider_fixture(c("value <- 1", "value"))
    refs <- lapply(seq_len(1000L), function(i) {
        location(if (i %% 3L == 0L) "file:///second.R" else fixture$uri,
            range(position(i, 0L), position(i, 5L)))
    })
    mockery::stub(rename_reply, "references_reply", function(...) Response$new(1L, result = refs))
    changes <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 1L, col = 2L), "renamed")$result$changes
    expect_identical(names(changes), c(fixture$uri, "file:///second.R"))
    for (uri in names(changes)) {
        selected <- refs[vapply(refs, function(ref) identical(ref$uri, uri), logical(1L))]
        expect_identical(changes[[uri]], lapply(selected, function(ref) {
            text_edit(ref$range, "renamed")
        }))
    }
})

test_that("rename uses complete lexical scopes in partial documents", {
    fixture <- provider_fixture(c(
        "first <- function(arg) arg + 1", "stats::", "second <- function(arg) arg - 1"
    ))
    rename <- function(row, col) {
        rename_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = row, col = col), "updated")
    }
    fixture$workspace$document_uris_for_references <- function(...) stop("unexpected cross-file lookup")
    for (row in c(0L, 2L)) {
        prepared <- prepare_rename_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = row, col = 20L))
        expect_equal(prepared$result$start$line, row)
        reply <- rename(row, 20L)
        edits <- reply$result$changes[[fixture$uri]]
        expect_length(edits, 2L)
        expect_equal(vapply(edits, function(edit) edit$range$start$line, integer(1L)), rep(row, 2L))
    }
    fixture$workspace$document_uris_for_references <- NULL

    # A second document can hide a use of the global, even if no indexed
    # occurrence from that file is among the locations to edit.
    uri <- "file:///uses.R"
    content <- c("stats::", "first(")
    doc <- Document$new(uri, version = 1L, content = content)
    parsed <- parse_document(uri, content)
    parsed$version <- doc$version
    doc$update_parse_data(parsed)
    fixture$workspace$documents$set(uri, doc)
    expect_length(references_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 0L, col = 1L))$result, 1L)
    expect_equal(rename(0L, 1L)$error$code, ErrorCodes$RequestFailed)
    prepared <- prepare_rename_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 0L, col = 1L))
    expect_equal(prepared$error$code, ErrorCodes$RequestFailed)
    expect_length(rename(0L, 19L)$result$changes[[fixture$uri]], 2L)

    fixture$workspace$documents$remove(uri)
    expect_length(rename(0L, 1L)$result$changes[[fixture$uri]], 1L)

    content <- c("first <- function(arg) {", "  stats::", "  arg", "}")
    fixture <- provider_fixture(content)
    expect_length(rename(0L, 20L)$result$changes[[fixture$uri]], 2L)
    fixture <- provider_fixture(c(content[[1L]], "  damaged <- arg +", "}"))
    expect_equal(rename(0L, 20L)$error$code, ErrorCodes$RequestFailed)
})

test_that("an incomplete longer identifier does not block renaming an unrelated name", {
    point <- list(row = 1L, col = 1L)
    for (tail in c("stats::", "argument +", "data_item(")) {
        fixture <- provider_fixture(c("a <- 1", "a", tail))
        prepared <- prepare_rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)
        expect_equal(prepared$result$start, position(1L, 0L))
        reply <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, "renamed")
        expect_length(reply$result$changes[[fixture$uri]], 2L)
    }
    for (tail in c("bad(a +", "bad(`a` +", "bad(1La", "bad(`\\141` +")) {
        fixture <- provider_fixture(c("a <- 1", "a", tail))
        reply <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, "renamed")
        expect_equal(reply$error$code, ErrorCodes$RequestFailed, info = tail)
    }
})
