test_that("formal Rename follows named arguments only at verified caller functions", {
    lines <- c("f <- function(arg) arg", "base::with(list(f = function(arg) arg + 1), f(arg = 2))",
        "base::with(list(f = 1L), f(arg = 3))", "f(arg = 4)")
    f <- provider_fixture(lines)
    p <- function(line, needle) list(row = line - 1L, col = regexpr(needle, lines[[line]], fixed = TRUE)[[1L]] - 1L)
    for (point in list(p(1L, "arg"), p(3L, "arg = 3"), p(4L, "arg = 4"))) {
        edits <- rename_reply(1L, f$uri, f$workspace, f$document, point, "renamed")$result$changes[[f$uri]]
        expect_equal(sort(vapply(edits, function(edit) edit$range$start$line, numeric(1L))), c(0L, 0L, 2L, 3L))
    }
    tag <- p(2L, "arg = 2")
    expect_null(prepare_rename_reply(1L, f$uri, f$workspace, f$document, tag)$result)
    expect_null(rename_reply(1L, f$uri, f$workspace, f$document, tag, "renamed")$result$changes)
    inner <- p(2L, "arg)")
    expect_match(rename_reply(1L, f$uri, f$workspace, f$document, inner, "renamed")$error$message, "data mask")
})

test_that("an ambiguous masked callee prevents a partial formal Rename", {
    for (tag in c("arg", "ar", "ren")) {
        lines <- c("f <- function(arg, ...) arg", paste0("base::with(unknown, f(", tag, " = 2))"), "f(arg = 4)")
        f <- provider_fixture(lines)
        point <- list(row = 0L, col = 15L)
        expect_match(rename_reply(1L, f$uri, f$workspace, f$document, point, "renamed")$error$message, "data mask")
        tag_point <- list(row = 1L, col = regexpr(paste0(tag, " ="), lines[[2L]], fixed = TRUE)[[1L]] - 1L)
        expect_null(prepare_rename_reply(1L, f$uri, f$workspace, f$document, tag_point)$result)
    }
})

test_that("a separate local function owns the caller fallback at a masked tag", {
    f <- provider_fixture(c("f <- function(arg) arg", "f(arg = 4)", "g <- function() {",
            "  f <- function(arg) arg", "  base::with(unknown, f(arg = 2))", "}"))
    rename <- function(row, col) rename_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = col), "changed")
    edits <- rename(0L, 15L)$result$changes[[f$uri]]
    expect_equal(sort(vapply(edits, function(edit) edit$range$start$line, numeric(1L))), c(0L, 0L, 1L))
    expect_match(rename(3L, 17L)$error$message, "data mask")
})
