test_that("Rename keeps the outer binding, data.table columns and data-mask pronouns separate", {
    f <- provider_fixture(c(
        "v <- 99L",
        "DT <- data.table::data.table(v = 1L)",
        "DT[, v]",
        "B <- data.table::data.table(v = 2L)",
        "B[, v]",
        "df <- data.frame(v = 1L)",
        "with(df, v)",
        "base::subset(df, v < 2L)",
        "dplyr::filter(df, v < 2L)",
        "dplyr::filter(df, .data$v == .env$v)",
        "print(v)"
    ))
    f$workspace$get_definition <- function(...) NULL
    at <- function(row, col) list(row = row, col = col)
    expected <- list(range(position(0, 0), position(0, 1)),
        range(position(9, 34), position(9, 35)), range(position(10, 6), position(10, 7)))
    for (indexed in c(TRUE, FALSE)) {
        if (!indexed) f$document$parse_data$reference_index <- NULL
        for (cursor in list(at(0, 0), at(9, 34), at(9, 35), at(10, 6))) {
            edits <- rename_reply(1L, f$uri, f$workspace, f$document, cursor, "outer")$result$changes[[f$uri]]
            expect_equal(lapply(edits, "[[", "range"), expected)
            expect_equal(vapply(edits, "[[", "", "newText"), rep("outer", 3L))
            refs <- references_reply(1L, f$uri, f$workspace, f$document, cursor, include_declaration = FALSE)$result
            expect_equal(lapply(refs, "[[", "range"), expected[-1L])
        }
        prepared <- prepare_rename_reply(1L, f$uri, f$workspace, f$document, at(9, 34))
        expect_equal(prepared$result, unclass(expected[[2L]]))
        for (cursor in list(at(2, 5), at(4, 4), at(6, 9), at(7, 17), at(8, 18), at(9, 24))) {
            expect_match(prepare_rename_reply(1L, f$uri, f$workspace, f$document, cursor)$error$message, "data.mask")
            expect_null(rename_reply(1L, f$uri, f$workspace, f$document, cursor, "outer")$result$changes)
            expect_length(references_reply(1L, f$uri, f$workspace, f$document, cursor)$result, 0L)
        }
    }
})

test_that("a quoted .env reference resolves to its enclosing formal with exact LSP ranges", {
    f <- provider_fixture(c(
        "`outer value` <- 1L",
        "f <- function(`outer value`) {",
        "  dplyr::filter(df, '\U0001f600' == .env$`outer value`, .data$`outer value` > 0)",
        "}",
        "other <- list(`outer value` = 1)",
        "other$`outer value`"
    ))
    point <- list(row = 2L, col = 35L)
    expected <- list(range(position(1, 14), position(1, 27)), range(position(2, 33), position(2, 46)))
    rename <- rename_reply(1L, f$uri, f$workspace, f$document, point, "new value")
    edits <- rename$result$changes[[f$uri]]
    expect_equal(lapply(edits, "[[", "range"), expected)
    expect_equal(vapply(edits, "[[", "", "newText"), rep("`new value`", 2L))
    expect_equal(definition_reply(1L, f$uri, f$workspace, f$document, point)$result$range, expected[[1L]])
    highlights <- document_highlight_reply(1L, f$uri, f$workspace, f$document, point)$result
    expect_equal(lapply(highlights, "[[", "range"), expected)
})

test_that("member and glue Rename cannot bypass data-mask lookup", {
    f <- provider_fixture(c(
        "v <- 9", "df <- data.frame(v = 1)", "with(df, v)", "df$v",
        ".env <- list(v = 3)", ".env$v", "dplyr::mutate(df, v = .env$v)"
    ))
    for (point in list(list(row = 1L, col = 17L), list(row = 3L, col = 3L))) {
        expect_null(rename_reply(1L, f$uri, f$workspace, f$document, point, "other")$result$changes)
    }
    edits <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), "outer")$result$changes[[f$uri]]
    expect_equal(vapply(edits, function(edit) edit$range$start$line, numeric(1L)), c(0L, 6L))

    f <- provider_fixture(c("v <- 9", "df <- data.frame(v = 1)", "dplyr::mutate(df, label = glue::glue('{v}'))"))
    reply <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), "other")
    expect_match(reply$error$message, "glue expression")
    expect_null(reply$result$changes)

    f <- provider_fixture(c("v <- 9", "glue::glue('{dplyr::filter(df, .data$v > .env$v)}')"))
    edits <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), "other")$result$changes[[f$uri]]
    expect_equal(lapply(edits, "[[", "range"), list(range(position(0, 0), position(0, 1)),
            range(position(1, 46), position(1, 47))))
    f <- provider_fixture(c("v <- 9", "glue::glue('{dplyr::filter(df, v > 0)}')"))
    reply <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), "other")
    expect_match(reply$error$message, "glue expression")
})

test_that("lexical Rename crosses documents through .env and leaves column names available", {
    f <- provider_fixture("v <- 1")
    peer <- "file:///caller.R"
    content <- c("dplyr::filter(data.frame(new = 1L), new > .env$v)", "v",
        "f <- function(v) dplyr::filter(data.frame(new = 1L), new > .env$v)")
    doc <- Document$new(peer, language = "r", version = 1L, content = content)
    parsed <- parse_document(peer, content)
    parsed$version <- doc$version
    doc$update_parse_data(parsed)
    f$workspace$documents$set(peer, doc)
    f$workspace$get_definition <- function(...) location(f$uri, f$document$parse_data$definitions$v$range)
    f$workspace$document_uris_for_references <- function(...) c(f$uri, peer)
    for (selection in list(list(doc = f$document, row = 0L, col = 0L), list(doc = doc, row = 0L, col = 47L))) {
        edits <- rename_reply(1L, selection$doc$uri, f$workspace, selection$doc,
            selection[c("row", "col")], "new")$result$changes
        expect_length(edits[[f$uri]], 1L)
        expect_equal(lapply(edits[[peer]], "[[", "range"), list(range(position(0, 47), position(0, 48)),
                range(position(1, 0), position(1, 1))))
    }
})

test_that("lexical Rename cannot capture a lookup that can fall through a mask", {
    f <- provider_fixture(c("old <- 1L", "df <- data.frame(x = 1L)", "dplyr::filter(df, x > threshold)"))
    point <- list(row = 0L, col = 1L)
    rename <- rename_reply(1L, f$uri, f$workspace, f$document, point, "threshold")
    expect_null(rename$result$changes)
    expect_match(rename$error$message, "lexical binding")
})

test_that("a masked assignment cannot become an enclosing lexical declaration", {
    f <- provider_fixture(c("v <- 1", "DT <- data.table::data.table(v = 2L)",
            "DT[, v := 3L]", "with(DT, {v <- 4L; v})", "v",
            "DT[, new_col := 1L]", "new_col"))
    f$workspace$get_definition <- function(...) NULL
    p <- list(row = 4L, col = 1L)
    expect_equal(definition_reply(1L, f$uri, f$workspace, f$document, p)$result$range,
        range(position(0L, 0L), position(0L, 6L)))
    edits <- rename_reply(1L, f$uri, f$workspace, f$document, p, "outer")$result$changes[[f$uri]]
    expect_equal(vapply(edits, function(edit) edit$range$start$line, numeric(1L)), c(0L, 4L))
    p <- list(row = 6L, col = 3L)
    expect_null(definition_reply(1L, f$uri, f$workspace, f$document, p)$result)
    expect_null(rename_reply(1L, f$uri, f$workspace, f$document, p, "other")$result$changes)
})

test_that("indirect data-mask lookups guard the matching lexical scope", {
    f <- provider_fixture(c("v <- 1L", "DT <- data.table::data.table(x = 1L)",
            "f <- function(v) DT[, ..v]", "g <- function(v) dplyr::filter(DT, .env[['v']] > 0L)", "print(v)"))
    for (row in c(2L, 3L)) {
        point <- list(row = row, col = 14L)
        expect_match(prepare_rename_reply(1L, f$uri, f$workspace, f$document, point)$error$message, "data-mask lookup")
        expect_match(rename_reply(1L, f$uri, f$workspace, f$document, point, "other")$error$message, "data-mask lookup")
    }
    edits <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), "outer")$result$changes[[f$uri]]
    expect_equal(vapply(edits, function(edit) edit$range$start$line, numeric(1L)), c(0L, 4L))
})

test_that("LSP Rename from .env includes the closed source file", {
    skip_on_cran()
    root <- withr::local_tempdir()
    owner <- file.path(root, "owner.R")
    caller <- file.path(root, "caller.R")
    writeLines("value <- 9", owner)
    writeLines(c("source('owner.R')", "DT <- data.table::data.table(value = 1)",
            "DT[, value]", "dplyr::filter(DT, value < .env$value)", "print(value)"), caller)
    client <- language_client(working_dir = root)
    did_open(client, caller)
    renamed <- respond_rename(client, caller, c(3, 33), "outer")
    expect_setequal(names(renamed$changes), path_to_uri(c(owner, caller)))
    expect_equal(renamed$changes[[path_to_uri(owner)]][[1L]]$range$start, list(line = 0, character = 0))
    edits <- renamed$changes[[path_to_uri(caller)]]
    expect_equal(lapply(edits, function(edit) edit$range$start),
        list(list(line = 3, character = 31), list(line = 4, character = 6)))
    column <- respond_rename(client, caller, c(2, 6), "outer", retry = FALSE, allow_error = TRUE)
    expect_null(column$changes)
})
