rename_roxygen_fixture <- function(content, row, token, new_name) {
    fixture <- provider_fixture(content)
    col <- regexpr(token, content[[row + 1L]], fixed = TRUE)[[1L]] - 1L + nchar(token)
    stopifnot(col > 0L)
    reply <- rename_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = row, col = col), new_name)
    list(fixture = fixture, edits = reply$result$changes[[fixture$uri]])
}

rename_roxygen_apply <- function(case) {
    edits <- case$edits
    rows <- vapply(edits, function(edit) edit$range$start$line, numeric(1L))
    cols <- vapply(edits, function(edit) edit$range$start$character, numeric(1L))
    changes <- lapply(edits[order(rows, cols, decreasing = TRUE)], function(edit) {
        list(range = edit$range, text = edit$newText)
    })
    case$fixture$document$apply_content_changes(2L, changes)
    case$fixture$document$content
}

test_that("parameter Rename updates only explicit attached param names", {
    content <- c(
        "#' Add two arguments",
        "#' @param argument First argument; argument is used below.",
        "#' @param other Second argument.",
        "#' @returns The sum of argument and other.",
        "f <- function(argument, other) {",
        "  argument + other",
        "}"
    )
    expected <- content
    expected[c(2L, 5L, 6L)] <- c(
        "#' @param renamed First argument; argument is used below.",
        "f <- function(renamed, other) {",
        "  renamed + other"
    )
    for (row in c(4L, 5L)) {
        case <- rename_roxygen_fixture(content, row, "argument", "renamed")
        expect_length(case$edits, 3L)
        expect_identical(rename_roxygen_apply(case), expected)
    }
})

test_that("parameter Rename preserves comma-separated names and wrapped descriptions", {
    content <- c(
        "#' @param x,y Shared description mentions x,y.",
        "#' @param z,x Another description.",
        "#'   This continuation mentions x without renaming it.",
        "f <- function(x, y, z) { x + y + z }"
    )
    case <- rename_roxygen_fixture(content, 3L, "x", "new_x")
    expect_length(case$edits, 4L)
    expect_identical(rename_roxygen_apply(case), c(
        "#' @param new_x,y Shared description mentions x,y.",
        "#' @param z,new_x Another description.",
        content[[3L]],
        "f <- function(new_x, y, z) { new_x + y + z }"
    ))
})

test_that("parameter Rename follows a multiline declaration and tag", {
    content <- c(
        "#' @param",
        "#'   argument A description wrapped below.",
        "#'   More information about argument.",
        "#' @inheritParams other_function",
        "f <-",
        "  function(argument) {",
        "    argument",
        "  }"
    )
    case <- rename_roxygen_fixture(content, 6L, "argument", "renamed")
    expected <- content
    expected[c(2L, 6L, 7L)] <- c(
        "#'   renamed A description wrapped below.",
        "  function(renamed) {",
        "    renamed"
    )
    expect_identical(rename_roxygen_apply(case), expected)
})

test_that("parameter Rename keeps nested and peer function documentation separate", {
    content <- c(
        "#' @param x Outer input.",
        "outer <- function(x) {",
        "  #' @param x Inner input.",
        "  inner <- function(x) x",
        "  x",
        "}",
        "#' @param x Peer input.",
        "peer <- function(x) x"
    )
    outer <- rename_roxygen_fixture(content, 4L, "x", "outer_x")
    expected <- content
    expected[c(1L, 2L, 5L)] <- c(
        "#' @param outer_x Outer input.",
        "outer <- function(outer_x) {",
        "  outer_x"
    )
    expect_identical(rename_roxygen_apply(outer), expected)
    inner <- rename_roxygen_fixture(content, 3L, "x", "inner_x")
    expected <- content
    expected[c(3L, 4L)] <- c(
        "  #' @param inner_x Inner input.",
        "  inner <- function(inner_x) inner_x"
    )
    expect_identical(rename_roxygen_apply(inner), expected)
})

test_that("parameter Rename does not borrow unrelated or computed documentation", {
    for (separator in c("", "# An ordinary comment", "other <- 1")) {
        content <- c("#' @param x Unrelated input.", separator, "f <- function(x) x")
        case <- rename_roxygen_fixture(content, 2L, "x", "renamed")
        expect_length(case$edits, 2L)
        expect_identical(rename_roxygen_apply(case), c(content[1:2], "f <- function(renamed) renamed"))
    }
    content <- c(
        "#' @inheritParams elsewhere",
        "#' @eval some_param_tags()",
        "f <- function(x = function(y) y) x"
    )
    case <- rename_roxygen_fixture(content, 2L, "y", "renamed")
    expect_length(case$edits, 2L)
    expect_identical(rename_roxygen_apply(case), c(content[1:2], "f <- function(x = function(renamed) renamed) x"))
    variable <- rename_roxygen_fixture(c("#' @param value Input.", "value <- 1", "value"), 2L, "value", "renamed")
    expect_length(variable$edits, 2L)
    expect_identical(rename_roxygen_apply(variable), c("#' @param value Input.", "renamed <- 1", "renamed"))
    tag <- rename_roxygen_fixture(c("#' @param", "#' @return A result.",
            "f <- function(`@return`) `@return`"), 2L, "`@return`", "renamed")
    expect_length(tag$edits, 2L)
    expect_identical(rename_roxygen_apply(tag), c("#' @param", "#' @return A result.",
            "f <- function(renamed) renamed"))
})

test_that("parameter Rename handles quoted names and UTF-16 documentation ranges", {
    name <- paste0(intToUtf8(0x1f600), " input")
    content <- c(
        paste0("#' @param `", name, "` Description."),
        paste0("f <- function(`", name, "`) {"),
        paste0("  `", name, "`"),
        "}"
    )
    case <- rename_roxygen_fixture(content, 2L, paste0("`", name, "`"), "new name")
    documentation <- case$edits[[3L]]
    expect_equal(documentation$range, range(position(0L, 10L), position(0L, 20L)))
    expect_identical(rename_roxygen_apply(case), c(
        "#' @param `new name` Description.",
        "f <- function(`new name`) {",
        "  `new name`",
        "}"
    ))
    unicode <- rename_roxygen_fixture(c("#' @param β Input.", "f <- function(β) β"), 1L, "β", "γ")
    expect_identical(rename_roxygen_apply(unicode), c("#' @param γ Input.", "f <- function(γ) γ"))
})
