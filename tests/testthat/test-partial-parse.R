test_that("partial structure preserves physical ranges across an isolated syntax error", {
    content <- c('#line 500 "elsewhere.R"', "first = function(x) x", "broken <- )", "after <- 2")
    parsed <- parse_document("file:///partial.R", content)
    expect_true(parsed$parse_error)
    expect_identical(names(parsed$definitions), c("first", "after"))
    expect_identical(parsed$partial_incomplete_lines, 2L)
    expect_equal(parsed$definitions$first$range$start, position(1L, 0L))
    expect_equal(parsed$definitions$after$range$start, position(3L, 0L))
    expect_identical(parsed$syntax_data$line1[parsed$partial_complete_roots], c(2L, 4L))
})

test_that("discarding partial syntax preserves ownership of the following statement", {
    # Removing only pkg:: would parse the next assignment as stray's value.
    joined <- parse_document("file:///partial.R", c("known <- 1", "stray <-", "pkg::", "other <- 2"))
    expect_identical(names(joined$definitions), c("known", "other"))

    # The parser stopped inside an unclosed function. A statement that is
    # valid when parsed as a standalone suffix still belongs to that function.
    nested <- parse_document("file:///partial.R", c("known <- 1", "outer <- function() {",
            "  broken <- )", "  local_only <- 2"))
    expect_identical(names(nested$definitions), "known")
    expect_false("local_only" %in% nested$syntax_data$text)
})

test_that("a stray closer in a header preserves ownership of the following body", {
    headers <- c("outer <- function(x))", "if (x))", "while (x))", "for (x in xs))", "repeat )")
    if (getRversion() >= "4.1.0") headers <- c(headers, "outer <- \\(x))")
    for (header in headers) {
        content <- c("before <- 1", header, "    inside <- x", "tail <- 2")
        parsed <- parse_document("file:///headers.R", content)
        syntax <- parsed$syntax_data
        expect_true(parsed$parse_error, info = header)
        expect_identical(syntax$line1[parsed$partial_complete_roots], c(1L, 4L), info = header)
        roots <- syntax$roots[syntax$token[syntax$roots] %in% c("expr", "expr_or_assign_or_help")]
        expect_true(any(syntax$line1[roots] == 2L & syntax$line2[roots] == 3L), info = header)
    }
    # If removing a single closer cannot restore the header, do not publish
    # its unbraced body as a complete global statement.
    parsed <- parse_document("file:///headers.R", c("before <- 1", "outer <- function(x)) )",
            "    inside <- x", "tail <- 2"))
    expect_false("inside" %in% names(parsed$definitions))
})

test_that("discarding malformed syntax cannot reopen a completed function body", {
    for (first in list("f <- function(x) { 1 } bad", "f <- function(x) { 1; 2 } bad",
            c("f <- function(x) {", "  1 } bad"))) {
        content <- c(first, "shared <- x", "}", "tail <- 2")
        parsed <- parse_document("file:///closed-body.R", content)
        # Any returned function must end at the original closing brace.
        f <- parsed$definitions$f
        if (!is.null(f)) expect_lte(f$range$end$line, length(first) - 1L)
        expect_false(any(parsed$syntax_data$token == "expr" & parsed$syntax_data$line1 == 1L &
                    parsed$syntax_data$line2 > length(first)))
    }
})
