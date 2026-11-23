glue_rename_lines <- function(content, line, col, new_name) {
    fixture <- provider_fixture(content)
    point <- list(row = line, col = col)
    reply <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, new_name)
    expect_null(reply$error)
    edits <- reply$result$changes[[fixture$uri]]
    expect_gt(length(edits), 0L)
    rows <- vapply(edits, function(e) e$range$start$line, numeric(1L))
    cols <- vapply(edits, function(e) e$range$start$character, numeric(1L))
    changes <- lapply(edits[order(rows, cols, decreasing = TRUE)], function(e) list(range = e$range, text = e$newText))
    fixture$document$apply_content_changes(2L, changes)
    fixture$document$content
}

test_that("glue rename follows lexical, named and data bindings in both directions", {
    lines <- c("x <- 42", 'glue::glue("{x}", x = x)', 'glue::glue_data(data.frame(x = 7), "{x}")',
        'glue::glue("prose x, literal {{x}}, use {x}")')
    lexical <- lines
    lexical[c(1L, 2L, 4L)] <- c("total <- 42", 'glue::glue("{x}", x = total)', 'glue::glue("prose x, literal {{x}}, use {total}")')
    expect_identical(glue_rename_lines(lines, 0L, 0L, "total"), lexical)
    expect_identical(glue_rename_lines(lines, 3L, 41L, "total"), lexical)
    named <- lines
    named[[2L]] <- 'glue::glue("{total}", total = x)'
    for (col in c(13L, 18L)) expect_identical(glue_rename_lines(lines, 1L, col, "total"), named)
    column <- lines
    column[[3L]] <- 'glue::glue_data(data.frame(total = 7), "{total}")'
    for (col in c(27L, 37L)) expect_identical(glue_rename_lines(lines, 2L, col, "total"), column)
    fixture <- provider_fixture(lines)
    prepared <- prepare_rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 1L, col = 13L))
    expect_equal(prepared$result, unclass(range(position(1L, 13L), position(1L, 14L))))
})

test_that("glue rename escapes the outer quote and respects named RHS order", {
    lines <- c("x <- 42", 'glue::glue("{x} {y}", x = x, y = x)')
    new <- 'a"b\\c'
    edited <- glue_rename_lines(lines, 1L, 13L, new)
    call <- str2lang(edited[[2L]])
    expect_equal(names(as.list(call))[-(1L:2L)], c(new, "y"))
    expect_identical(as.character(call[[3L]]), "x")
    expect_identical(as.character(call[[4L]]), new)
    expr <- sub("^\\{(.*)\\} \\{y\\}$", "\\1", call[[2L]])
    expect_identical(as.character(str2lang(expr)), new)
    expect_identical(edited[[1L]], lines[[1L]])
})

test_that("glue rename refuses capture, uneditable expressions and unknown masks", {
    cases <- list(
        list(c("x <- 42", 'glue::glue("{x}", taken = 7)'), "taken"),
        list(c("x <- 42", 'glue::glue("{x} {free}")'), "free"),
        list(c("x <- 42", 'glue::glue_data(get_data(), "{x}")'), "other"),
        list(c("name <- 42", 'glue::glue("{na", "me}")'), "other"),
        list(c("x <- 42", 'glue::glue("{x <- 2}{x}")'), "other"))
    for (case in cases) {
        fixture <- provider_fixture(case[[1L]])
        reply <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 0L, col = 0L), case[[2L]])
        expect_match(reply$error$message, "glue")
        expect_null(reply$result$changes)
    }
    fixture <- provider_fixture('glue::glue("{x}", x = 42)')
    reply <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 0L, col = 13L), ".envir")
    expect_match(reply$error$message, "control argument")
})

test_that("workspace rename reaches an unopened glue caller and uses its unsaved buffer", {
    skip_on_cran()
    dir <- withr::local_tempdir()
    owner <- file.path(dir, "owner.R")
    caller <- file.path(dir, "caller.R")
    writeLines("total <- 42", owner)
    writeLines(c('source("owner.R")', 'glue::glue("{total}")'), caller)
    client <- language_client(working_dir = dir)
    did_open(client, owner)
    first <- respond_rename(client, owner, c(0L, 2L), "count")
    expect_equal(first$changes[[path_to_uri(caller)]][[1L]], list(
        range = list(start = list(line = 1, character = 13), end = list(line = 1, character = 18)), newText = "count"))
    did_open(client, caller, text = c('source("owner.R")', 'glue::glue("{total}", total = total)'))
    outer <- respond_rename(client, owner, c(0L, 2L), "count")
    expect_equal(outer$changes[[path_to_uri(caller)]][[1L]]$range$start, list(line = 1, character = 30))
    inner <- respond_rename(client, caller, c(1L, 14L), "units")
    expect_named(inner$changes, path_to_uri(caller))
    expect_equal(sort(vapply(inner$changes[[1L]], function(e) e$range$start$character, numeric(1L))), c(13, 22))
})

test_that("glue formal rename checks named calls and preserves unrelated argument tags", {
    lines <- c("f <- function(long_name) long_name", 'glue::glue("{f(long_name = 1)}")')
    expected <- gsub("long_name", "total", lines, fixed = TRUE)
    expect_identical(glue_rename_lines(lines, 0L, 17L, "total"), expected)
    expect_identical(glue_rename_lines(lines, 1L, 18L, "total"), expected)
    abbreviated <- provider_fixture(c(lines[[1L]], 'glue::glue("{f(long_n = 1)}")'))
    refused <- rename_reply(1L, abbreviated$uri, abbreviated$workspace, abbreviated$document, list(row = 0L, col = 17L), "total")
    expect_match(refused$error$message, "argument matching")
    lines <- c("outer <- 42", 'glue::glue("{stats::median(1, outer = outer)}")')
    expect_identical(glue_rename_lines(lines, 0L, 1L, "total"),
        c("total <- 42", 'glue::glue("{stats::median(1, outer = total)}")'))
})

test_that("renaming a shared data field reaches ordinary member accesses and glue masks", {
    lines <- c("d <- list(value = 4)", "d$value", 'd %>% glue::glue_data("{value}")',
        'glue::glue_data(d, "{value}", value = 8)')
    expected <- lines
    expected[1L:3L] <- gsub("value", "total", expected[1L:3L], fixed = TRUE)
    for (p in list(c(0L, 11L), c(1L, 3L), c(2L, 26L))) {
        expect_identical(glue_rename_lines(lines, p[[1L]], p[[2L]], "total"), expected)
    }
    f <- provider_fixture(c(lines[1L:2L], 'glue::glue_data(identity(d), "{value}")'))
    reply <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 1L, col = 3L), "total")
    expect_match(reply$error$message, "cannot be renamed precisely")
    expect_null(reply$result)
})
