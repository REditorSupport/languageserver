test_that("Formal references include named arguments only at calls to the declaring function", {
    code <- c("box <- list(run = (function(value) value))",
        "other <- list(run = function(value) value)",
        "box$run(value = 1)", "other$run(value = 2)")
    f <- provider_fixture(code)
    point <- list(row = 0L, col = regexpr("value", code[[1L]], fixed = TRUE)[[1L]])
    expected <- list(range(position(0L, 28L), position(0L, 33L)),
        range(position(0L, 35L), position(0L, 40L)), range(position(2L, 8L), position(2L, 13L)))
    refs <- references_reply(1L, f$uri, f$workspace, f$document, point)$result
    expect_equal(lapply(refs, `[[`, "range"), expected)
    renamed <- rename_reply(1L, f$uri, f$workspace, f$document, point, "input")$result$changes
    expect_equal(lapply(renamed[[f$uri]], `[[`, "range"), expected)
    expect_identical(vapply(renamed[[f$uri]], `[[`, "", "newText"), rep("input", 3L))
})

test_that("Formal lookup classifies nested, rightward and escaping function declarations", {
    code <- c("f <- function(x) x", "(function(x) x) -> other",
        "outer <- function() { local <- function(x) x }",
        "outer <- function() { escape <<- function(x) x }",
        "obj$member <- function(x) x", "list(member = (function(x) x))",
        "callback(handler = function(x) x)", "function(x) x")
    f <- provider_fixture(code)
    expected <- list("workspace", "workspace", "document", "workspace", "workspace", "workspace", NULL, NULL)
    for (i in seq_along(code)) {
        col <- regexpr("function(x)", code[[i]], fixed = TRUE)[[1L]] + nchar("function(") - 1L
        formal <- location(f$uri, range(position(i - 1L, col), position(i - 1L, col + 1L)))
        expect_identical(reference_formal_call_scope(f$document$parse_data, f$document, formal), expected[[i]])
    }
})

test_that("References recognize exponent registrations in both R operator spellings", {
    code <- c("methods::setMethod('^', c('Record', 'numeric'), function(e1, e2) e1)",
        "x ^ 2", "x ** 2", "x * 2", "base::`^`(x, 2)")
    f <- provider_fixture(code)
    find <- function(row, col, include = TRUE) {
        refs <- references_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = col),
            include_declaration = include)$result
        lapply(refs, `[[`, "range")
    }
    expect_equal(find(0L, 20L, FALSE), list(range(position(1L, 2L), position(1L, 3L)),
            range(position(2L, 2L), position(2L, 4L))))
    expect_equal(find(1L, 2L), find(2L, 3L))
})

test_that("Rename skips malformed roxygen parameter lists while updating valid tags and R uses", {
    code <- c("#' @param value, trailing comma", "#' @param other,,value empty name",
        "#' @param value,other Both parameters", "#' @param `value Unclosed name", "f <- function(value, other) value")
    f <- provider_fixture(code)
    renamed <- rename_reply(1L, f$uri, f$workspace, f$document, list(row = 4L, col = 16L), "input")
    expect_null(renamed$error)
    edits <- renamed$result$changes[[f$uri]]
    expect_equal(sort(vapply(edits, function(x) x$range$start$line, integer(1L))), c(2L, 4L, 4L))
    expect_identical(vapply(edits, `[[`, "", "newText"), rep("input", 3L))
})

test_that("Prose and foreign cells have no references or rename edits", {
    lines <- c("A prose mention of value.", "```{r}", "value <- 1", "```",
        "```{python}", "value = 2", "```")
    doc <- Document$new("file:///references.qmd", language = "quarto", version = 1L, content = lines)
    for (row in c(0L, 5L)) {
        point <- list(row = row, col = regexpr("value", lines[[row + 1L]])[[1L]])
        expect_length(references_reply(1L, doc$uri, list(), doc, point)$result, 0L)
        expect_null(rename_reply(1L, doc$uri, list(), doc, point, "updated")$result)
        prepare <- prepare_rename_reply(1L, doc$uri, list(), doc, point)
        expect_equal(prepare$error$code, ErrorCodes$RequestCancelled)
    }
})
