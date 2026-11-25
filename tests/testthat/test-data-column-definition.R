data_column_lookup <- function(lines) {
    fixture <- provider_fixture(lines)
    fixture$workspace$get_definition <- function(...) NULL
    function(line, needle = "value") {
        col <- regexpr(needle, lines[[line]], fixed = TRUE)[[1L]]
        stopifnot(col > 0L)
        definition_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = line - 1L, col = col - 1L), getwd())$result
    }
}

test_that("masked reads navigate to the named column of the current constructor", {
    lookup <- data_column_lookup(c(
        "value <- 99L",
        "DT <- data.table::data.table(value = 1:3)",
        "other <- data.table::data.table(value = 4L)",
        "DT[, value]",
        "other[, value]",
        "DT[, value]",
        "DT <- data.table::data.table(value = c(5L, 6L))",
        "DT[, value]",
        "df <- data.frame(value = 1L)",
        "with(df, value)",
        "base::subset(df, value < 2L)",
        "dplyr::filter(df, .data$value > 0)",
        "dplyr::filter(tibble::tibble(value = 1L), .data$value > 0)"
    ))
    expect_equal(lookup(4L)$range, range(position(1L, 29L), position(1L, 34L)))
    expect_equal(lookup(5L)$range$start, position(2L, 32L))
    expect_equal(lookup(6L)$range$start, position(1L, 29L))
    expect_equal(lookup(8L)$range$start, position(6L, 29L))
    for (line in 10L:12L) expect_equal(lookup(line)$range$start, position(8L, 17L))
    expect_equal(lookup(13L, "value >")$range$start, position(12L, 29L))
})

test_that("column navigation stops at ambiguous bindings and effects", {
    lookup <- data_column_lookup(c(
        "value <- 99L",
        "DT <- data.table::data.table(value = 1L)",
        "data.table::setnames(DT, 'value', 'other')",
        "DT[, value]",
        "DT <- data.table::data.table(value = 2L)",
        "DT[, value := NULL]",
        "DT[, value]",
        "DT <- data.table::data.table(value = 3L)",
        "alias <- DT",
        "alias[, value := NULL]",
        "DT[, value]",
        "if (condition) DT <- data.table::data.table(value = 4L)",
        "DT[, value]",
        "dplyr::filter(unknown, value > 0)",
        "df <- data.frame(value = 1L, value = 2L)",
        "dplyr::filter(df, value > 0)",
        "df <- tibble::tibble(value = NULL, x = 1)",
        "dplyr::filter(df, value > 0)",
        "f <- function(DT) dplyr::filter(DT, value > 0)",
        "DT <- data.table::data.table(value = 1L)",
        "assign('DT', data.table::data.table(other = 1L))",
        "DT[, value]"
    ))
    for (line in c(4L, 7L, 11L, 13L, 14L, 16L, 18L, 19L, 22L)) expect_null(lookup(line))
})

test_that("a local constructor in the executing block owns its masked column", {
    lookup <- data_column_lookup(c(
        "f <- function() {",
        "  d <- data.frame(value = 1L)",
        "  dplyr::filter(d, value > 0)",
        "}",
        "data.frame <- function(...) list(other = 1L)",
        "d <- data.frame(value = 1L)",
        "dplyr::filter(d, value > 0)"
    ))
    expect_equal(lookup(3L)$range$start, position(1L, 18L))
    expect_null(lookup(7L))
})

test_that("a shadowed range operator cannot establish a constructor column", {
    f <- provider_fixture(c(
        "value <- 99L",
        "`:` <- function(a, b) base::data.frame(other = 1L)",
        "df <- base::data.frame(value = 1:3)",
        "with(df, value)",
        "value"
    ))
    f$workspace$get_definition <- function(...) NULL
    definition <- definition_reply(1L, f$uri, f$workspace, f$document, list(row = 3L, col = 10L), getwd())
    expect_null(definition$result)
    renamed <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 1L), "outer")
    expect_match(renamed$error$message, "data.mask")
})

test_that("a column declaration survives a long sequence of read-only queries", {
    lookup <- data_column_lookup(c("DT <- data.table::data.table(value = 1)", rep("DT[, value]", 100L)))
    expect_equal(lookup(101L)$range$start, position(0L, 29L))
})

test_that("field Rename checks same-named mask columns on both sides of a reassignment", {
    f <- provider_fixture(c("unrelated <- data.frame(v = 1)", "df <- data.frame(v = 2)",
            "with(df, v)", "df <- data.frame(v = 3)", rep("with(df, v)", 10L), "unrelated$v"))
    rename <- function(row, col) rename_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = col), "z")
    edits <- rename(14L, 10L)$result$changes[[f$uri]]
    expect_equal(vapply(edits, function(edit) edit$range$start$line, numeric(1L)), c(0L, 14L))
    for (row in c(1L, 3L)) expect_match(rename(row, 17L)$error$message, "data.mask")
})
