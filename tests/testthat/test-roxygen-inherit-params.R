inherit_fixture <- function(code) {
    f <- provider_fixture(code)
    hints <- f$document$parse_data$member_data$type_hints
    topics <- attr(hints, "topics", exact = TRUE)
    function(name) {
        key <- names(Filter(function(topic) identical(topic$topic, name), topics))
        stopifnot(length(key) == 1L)
        resolved <- roxygen_effective_params(hints[[key]], f$workspace, f$document)
        lapply(resolved, function(x) if (is.character(x)) x else x$types)
    }
}

test_that("parameter inheritance respects source groups, prose overrides and donor order", {
    probe <- inherit_fixture(c(
        "#' @inheritParams first", "#' @inheritParams second", "mix <- function(.x, y, n, flag) NULL",
        "#' @inheritParams first", "#' @param y My own description.", "mask <- function(x, y, n) NULL",
        "#' @inheritParams first", "part <- function(x, n) NULL",
        "#' @inheritParams mix", "chain <- function(x, y, n, flag) NULL",
        "#' @param x,y (`Left | Right`) Joint argument.", "#' @param n Local prose.",
        "#' @return (`Left`) Returned value.", "first <- function(x, y, n) NULL",
        "#' @param x (`Other`) Alternative.", "#' @param n (`integer`) Count.", "#' @param flag (`logical`) Switch.",
        "second <- function(x, n, flag) NULL"))
    types <- c("Left", "Right")
    expect_identical(probe("mix"), list(.x = types, y = types, flag = "logical"))
    expect_identical(probe("chain"), list(x = types, y = types, flag = "logical"))
    expect_length(probe("mask"), 0L)
    expect_length(probe("part"), 0L)
})

test_that("literal inheritance selectors filter documentation without evaluating code", {
    probe <- inherit_fixture(c(
        "#' @param a (`A`) A.", "#' @param b (`B`) B.", "#' @param c (`C`) C.",
        "donor <- function(a, b, c) NULL", "#' @inheritParams donor b:c", "one <- function(a,b,c) NULL",
        "#' @inheritParams donor -a -c", "two <- function(a,b,c) NULL",
        "#' @inheritParams donor -(a:b)", "three <- function(a,b,c) NULL",
        "#' @inheritParams donor a", "#' @inheritParams donor c", "multi <- function(a,b,c) NULL",
        "#' @inheritParams donor stop('unexpected')", "bad <- function(a,b,c) NULL"))
    expect_identical(probe("one"), list(b = "B", c = "C"))
    expect_identical(probe("two"), list(b = "B"))
    expect_identical(probe("three"), list(c = "C"))
    expect_identical(probe("multi"), list(a = "A", c = "C"))
    expect_length(probe("bad"), 0L)
})

test_that("only unique, visible source topics donate, including explicitly named NULL topics", {
    probe <- inherit_fixture(c(
        "#' @name model-input", "#' @param x (`Box`) A box.", "NULL",
        "#' @inheritParams model-input", "ok <- function(x) NULL",
        "#' @param x (`Hidden`) A box.", "#' @noRd", "hidden <- function(x) NULL",
        "#' @inheritParams hidden", "not_public <- function(x) NULL",
        "#' @param x (`Uncertain`) A box.", "#' @rdname common", "first <- function(x) NULL",
        "#' @rdname common", "shared <- function(x) NULL", "#' @inheritParams first", "ambiguous <- function(x) NULL",
        "#' @inheritParams elsewhere::model-input", "external <- function(x) NULL",
        "#' @inheritParams cycleB", "cycleA <- function(x) NULL", "#' @inheritParams cycleA", "cycleB <- function(x) NULL"))
    expect_identical(probe("ok"), list(x = "Box"))
    for (name in c("not_public", "ambiguous", "external", "cycleA", "cycleB")) expect_length(probe(name), 0L)
})

test_that("generator rename edits a standalone donor's physical annotation only", {
    code <- c('Box <- R6::R6Class("Box", public = list(value = 1L))',
        "#' @name input-box", "#' @param x (`Box`) Input.", "NULL",
        "#' @inheritParams input-box", "read_value <- function(x) x$value")
    f <- provider_fixture(code)
    w <- Workspace$new(NULL)
    w$documents$set(f$uri, f$document)
    w$update_parse_data(f$uri, f$document$parse_data)
    result <- rename_reply(1L, f$uri, w, f$document, list(row = 0L, col = 1L), "Packet")
    expect_null(result$error)
    edits <- result$result$changes[[f$uri]]
    expect_setequal(vapply(edits, function(edit) edit$range$start$line, integer(1L)), c(0L, 2L))
    items <- member_completion(f$uri, w, f$document, list(row = 5L, col = nchar("read_value <- function(x) x$")), FALSE, 100L)
    expect_true("value" %in% vapply(items, `[[`, "", "label"))
})

test_that("package topic lookup observes duplicate, renamed and removed source files", {
    root <- index_normalize_path(withr::local_tempdir())
    for (pkg in c("homepkg", "neighbor")) {
        dir.create(file.path(root, pkg, "R"), recursive = TRUE)
        writeLines(paste("Package:", pkg), file.path(root, pkg, "DESCRIPTION"))
        writeLines("", file.path(root, pkg, "NAMESPACE"))
    }
    workspace <- Workspace$new(root)
    open <- function(pkg, name, code) {
        path <- file.path(root, pkg, "R", name)
        writeLines(code, path)
        uri <- path_to_uri(path)
        doc <- Document$new(uri, version = 1L, content = code)
        doc$did_open()
        workspace$documents$set(uri, doc)
        update(doc, code)
        doc
    }
    update <- function(doc, code) {
        doc$set_content(doc$version + 1L, code)
        parsed <- parse_document(doc$uri, code)
        parsed$version <- doc$version
        workspace$update_parse_data(doc$uri, parsed)
    }
    donor <- c("#' @param x (`integer`) A number.", "parent <- function(x) x")
    extra <- open("neighbor", "neighbor.R", donor)
    parent <- open("homepkg", "parent.R", donor)
    child <- open("homepkg", "child.R", c("#' @inheritParams homepkg::parent", "child <- function(x) x"))
    hints <- child$parse_data$member_data$type_hints[[1L]]
    effective <- function() roxygen_effective_params(hints, workspace, child)$x
    expect_identical(effective()$uri, parent$uri)
    expect_identical(effective()$types, "integer")
    duplicate <- open("homepkg", "second.R", c("#' @name parent", "#' @param x (`Other`) Another type.", "NULL"))
    expect_null(effective())
    workspace$index$remove(duplicate$uri)
    workspace$documents$remove(duplicate$uri)
    expect_identical(effective()$types, "integer")
    update(parent, c("#' @name changed", donor))
    expect_null(effective())
    update(parent, readLines(path_from_uri(parent$uri)))
    expect_identical(effective()$types, "integer")
    workspace$index$remove(parent$uri)
    workspace$documents$remove(parent$uri)
    expect_null(effective())
    expect_true(workspace$index$has_definition("parent", extra$uri))
})
