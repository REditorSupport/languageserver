test_that("Rename follows caller lookups in .SDcols and preserves column ranges", {
    content <- c("cols <- 'x'",
        "DT <- data.table::data.table(cols = 1L, x = 2L)",
        "DT[, lapply(.SD, sum), .SDcols = cols]",
        "DT[, .SD, .SDcols = !(cols)]",
        "DT[, .SD, .SDcols = patterns(cols)]",
        "DT[, .SD, .SDcols = c(cols, 'x')]",
        "DT[, .SD, .SDcols = cols:x]",
        "DT[, .SD, .SDcols = -((cols:x))]",
        "DT[, .SD, .SDcols = `:`(cols, x)]",
        "DT[, .SD, .SDcols = (cols):x]",
        "DT[, .SD, .SDcols = c(cols:x)]",
        "DT[, .SD, .SDc = cols:x]",
        "DT[, .SD,,,,,,,,, cols:x]")
    f <- provider_fixture(content)
    f$workspace$get_definition <- function(...) NULL
    lexical <- c(1L, 3L:6L, 10L:11L)
    ranges <- lapply(lexical, function(line) {
        col <- regexpr("\\bcols\\b", content[[line]])[[1L]] - 1L
        range(position(line - 1L, col), position(line - 1L, col + 4L))
    })
    for (i in c(1L, 2L)) {
        cursor <- f$document$from_lsp_position(ranges[[i]]$start)
        result <- rename_reply(1L, f$uri, f$workspace, f$document, cursor, "selected")
        expect_null(result$error)
        expect_equal(lapply(result$result$changes[[f$uri]], "[[", "range"), ranges)
    }
    for (i in c(7L:9L, 12L:13L)) {
        col <- regexpr("\\bcols\\b", content[[i]])[[1L]] - 1L
        cursor <- list(row = i - 1L, col = col)
        expect_match(prepare_rename_reply(1L, f$uri, f$workspace, f$document, cursor)$error$message, "data.mask")
    }
})

test_that(".SDcols retains an enclosing mask or an uncertain bracket dispatch", {
    f <- provider_fixture(c("cols <- 'x'", "DT <- data.table::data.table(cols = 1L, x = 2L)",
            "outer <- data.frame(cols = 'x')", "with(outer, DT[, .SD, .SDcols = cols])", "print(cols)"))
    f$workspace$get_definition <- function(...) NULL
    lexical <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 1L), "outer_cols")
    expect_null(lexical$error)
    expect_equal(vapply(lexical$result$changes[[f$uri]], function(edit) edit$range$start$line, numeric(1L)), c(0L, 4L))
    f <- provider_fixture(c("cols <- 'x'", "unknown[, .SD, .SDcols = cols]"))
    f$workspace$get_definition <- function(...) NULL
    expect_match(rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 1L),
            "outer_cols")$error$message, "data.mask")
})
