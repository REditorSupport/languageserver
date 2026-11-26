type_rename_fixture <- function(lines, uri = "file:///typed-rename.R", workspace = Workspace$new(NULL)) {
    doc <- Document$new(uri, content = lines, version = 1L)
    workspace$documents$set(uri, doc)
    parsed <- parse_document(uri, lines)
    parsed$version <- doc$version
    workspace$update_parse_data(uri, parsed)
    if (isTRUE(workspace$index$enabled)) workspace$index$update_content(uri, lines, parse_data = parsed)
    list(document = doc, uri = uri, workspace = workspace)
}

type_rename <- function(f, row, col, new_name) {
    rename_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = col), new_name)
}

test_that("renaming a generator or its alias updates only its bound type names", {
    lines <- c('Maker <- R6::R6Class("Record", public = list(answer = 42))',
        "Alias <- Maker", "#' @param x (`Maker | Alias`) a record", "#' @return (`Maker`) a record",
        "id <- function(x) x", "id(Maker$new())$answer", "id(opaque())$answer",
        "#' @return (`other::Maker`) unrelated", "factory <- function() opaque()")
    f <- type_rename_fixture(lines)
    edits <- type_rename(f, 0L, 2L, "Builder")$result$changes[[f$uri]]
    rows <- vapply(edits, function(e) e$range$start$line, integer(1L))
    expect_setequal(rows, c(0L, 1L, 2L, 3L, 5L))
    for (e in edits) {
        row <- e$range$start$line + 1L
        col <- e$range$start$character + 1L
        expect_identical(substr(lines[[row]], col, e$range$end$character), "Maker")
        lines[[row]] <- paste0(substr(lines[[row]], 1L, col - 1L), e$newText, substring(lines[[row]], e$range$end$character + 1L))
    }
    after <- type_rename_fixture(lines)
    offered <- member_completion(after$uri, after$workspace, after$document,
        list(row = 6L, col = nchar("id(opaque())$")), TRUE, 100L)
    expect_true("answer" %in% vapply(offered, `[[`, "", "label"))
    alias <- type_rename(f, 1L, 1L, "Handle")$result$changes[[f$uri]]
    expect_setequal(vapply(alias, function(e) e$range$start$line, integer(1L)), c(1L, 2L))
    expect_match(type_rename(f, 0L, 1L, "with spaces")$error$message, "documented type")
    unicode <- type_rename(f, 0L, 1L, "\u03a0")$result$changes[[f$uri]]
    expect_equal(vapply(unicode, `[[`, "", "newText"), rep("\u03a0", 5L))
})

test_that("local classes, nominal names and capture are distinguished during rename", {
    code <- c('Maker <- R6::R6Class("Global")', "outer <- function() {",
        '    Maker <- R6::R6Class("Private")', "    #' @return (`Maker`)",
        "    local <- function() opaque()", "}", "#' @return (`Maker`)", "global <- function() opaque()")
    f <- type_rename_fixture(code)
    rows <- function(row, col) {
        edits <- type_rename(f, row, col, "Builder")$result$changes[[f$uri]]
        vapply(edits, function(e) e$range$start$line, integer(1L))
    }
    expect_setequal(rows(0L, 2L), c(0L, 6L))
    expect_setequal(rows(2L, 6L), c(2L, 3L))
    masked <- type_rename_fixture(c('Maker <- R6::R6Class("Maker")', "outer <- function(Builder) {",
            "  #' @param x (`Maker`)", "  f <- function(x) x", "}"))
    expect_match(type_rename(masked, 0L, 2L, "Builder")$error$message, "binding")
    nominal <- type_rename_fixture(c("widget <- 1", "#' @param x (`widget`)", "f <- function(x) x"))
    edits <- type_rename(nominal, 0L, 2L, "number")$result$changes[[nominal$uri]]
    expect_identical(vapply(edits, function(e) e$range$start$line, integer(1L)), 0L)
})

test_that("current-package qualifications refer to the namespace across local shadowing", {
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines("Package: hintfixture", file.path(root, "DESCRIPTION"))
    writeLines("", file.path(root, "NAMESPACE"))
    workspace <- Workspace$new(root)
    open <- function(name, lines) {
        path <- file.path(root, "R", name)
        writeLines(lines, path)
        type_rename_fixture(lines, path_to_uri(path), workspace)
    }
    owner <- open("classes.R", 'Maker <- R6::R6Class("Record", public = list(keep = 1))')
    user <- open("user.R", c("outer <- function(Maker) {", "  #' @param x (`hintfixture::Maker`)",
            "  inner <- function(x) x$keep", "  #' @param x (`Maker`)", "  hidden <- function(x) x$missing", "}"))
    items <- member_completion(user$uri, workspace, user$document, list(row = 2L, col = nchar("  inner <- function(x) x$")), TRUE, 100L)
    expect_true("keep" %in% vapply(items, `[[`, "", "label"))
    changes <- type_rename(owner, 0L, 1L, "New")$result$changes
    expect_setequal(names(changes), c(owner$uri, user$uri))
    expect_equal(changes[[user$uri]][[1L]]$range, range(position(1L, 29L), position(1L, 34L)))
    expect_length(changes[[user$uri]], 1L)
})

test_that("sourcing a second generator changes which annotations rename edits", {
    root <- index_normalize_path(withr::local_tempdir())
    workspace <- Workspace$new(root)
    open <- function(name, code) {
        path <- file.path(root, name)
        writeLines(code, path)
        type_rename_fixture(code, path_to_uri(path), workspace)
    }
    first <- open("first.R", 'Maker <- R6::R6Class("First")')
    second <- open("second.R", 'Maker <- R6::R6Class("Second")')
    caller <- open("main.R", c('source("first.R")', "#' @param x (`Maker`)", "one <- function(x) x",
            'source("second.R")', "#' @param x (`Maker`)", "two <- function(x) x"))
    caller_rows <- function(target) {
        changes <- type_rename(target, 0L, 1L, "Renamed")$result$changes
        vapply(changes[[caller$uri]], function(e) e$range$start$line, integer(1L))
    }
    expect_identical(caller_rows(first), 1L)
    expect_identical(caller_rows(second), 4L)
})

test_that("rename updates recoverable documented types and declines partial edits", {
    declaration <- 'Maker <- R6::R6Class("Maker")'
    parameter <- "#' @param x (`Maker`)"
    for (body in list("f <- function(x) x$", c("f <- function(x) {", "  x$", "}"))) {
        f <- type_rename_fixture(c(declaration, parameter, body))
        changed <- type_rename(f, 0L, 2L, "Builder")$result$changes[[f$uri]]
        expect_setequal(vapply(changed, function(e) e$range$start$line, integer(1L)), c(0L, 1L))
    }
    incomplete <- type_rename_fixture(c(declaration, parameter, "f <- function(x) {", "  x$"))
    expect_match(type_rename(incomplete, 0L, 2L, "Builder")$error$message, "documented type")
    unrelated <- type_rename_fixture(c(declaration, parameter, "other <- 1", "broken <- ("))
    edits <- type_rename(unrelated, 0L, 2L, "Builder")$result$changes[[unrelated$uri]]
    expect_identical(vapply(edits, function(e) e$range$start$line, integer(1L)), 0L)
})

test_that("annotations in a sourced consumer use the binding present when it was sourced", {
    root <- index_normalize_path(withr::local_tempdir())
    workspace <- Workspace$new(root)
    open <- function(name, code) {
        path <- file.path(root, name)
        writeLines(code, path)
        type_rename_fixture(code, path_to_uri(path), workspace)
    }
    first <- open("first.R", 'Maker <- R6::R6Class("First")')
    consumer <- open("consumer.R", c("#' @param x (`Maker`)", "identity <- function(x) x"))
    open("main.R", c('source("first.R")', 'source("consumer.R")'))
    rename <- function(target) type_rename(target, 0L, 1L, "Builder")
    expect_equal(rename(first)$result$changes[[consumer$uri]][[1L]]$range, range(position(0L, 14L), position(0L, 19L)))

    second <- open("second.R", 'Maker <- R6::R6Class("Second")')
    open("main.R", c('source("first.R")', 'source("consumer.R")', 'source("second.R")'))
    expect_length(rename(first)$result$changes[[consumer$uri]], 1L)
    expect_null(rename(second)$result$changes[[consumer$uri]])

    open("main.R", c('source("first.R")', 'source("consumer.R")', 'source("second.R")', 'source("consumer.R")'))
    expect_null(rename(first)$result)
    expect_match(rename(first)$error$message, "documented type|sourced declarations")
})
