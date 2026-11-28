topic_rename_doc <- function(code, uri = "file:///topic-rename.R", workspace = Workspace$new(NULL)) {
    doc <- Document$new(uri, version = 1L, content = code)
    doc$did_open()
    workspace$documents$set(uri, doc)
    data <- parse_document(uri, code)
    data$version <- doc$version
    workspace$update_parse_data(uri, data)
    if (isTRUE(workspace$index$enabled)) workspace$index$update_content(uri, code, parse_data = data)
    list(doc = doc, w = workspace, uri = uri)
}

topic_rename_call <- function(f, line, prefix, new_name) {
    p <- list(row = line - 1L, col = nchar(prefix))
    rename_reply(1L, f$uri, f$w, f$doc, p, new_name)
}

topic_edit_rows <- function(reply, uri) {
    vapply(reply$result$changes[[uri]], function(e) e$range$start$line + 1L, integer(1L))
}

test_that("renaming an implicit donor updates just its documented topic spellings", {
    lines <- c("#' @param x (`Model`) Input.", "original <- function(x) x", "#' @inheritParams original x",
        "#' @inheritParams", "#'     original -y", "#' @inheritParams external::original x", "wrapper <- function(x) x",
        "other <- function() {", "  original <- function(x) x", "  original(1)", "}")
    f <- topic_rename_doc(lines)
    for (enabled in c(TRUE, FALSE)) {
        old <- lsp_settings$get("roxygen_type_hints")
        withr::defer(lsp_settings$set("roxygen_type_hints", old))
        lsp_settings$set("roxygen_type_hints", enabled)
        res <- topic_rename_call(f, 2L, "ori", "new-name")
        expect_null(res$error)
        expect_setequal(topic_edit_rows(res, f$uri), c(2L, 3L, 5L))
        for (edit in res$result$changes[[f$uri]]) {
            r <- edit$range
            expect_identical(substr(lines[[r$start$line + 1L]], r$start$character + 1L, r$end$character), "original")
            expect_identical(edit$newText, if (r$start$line == 1L) "`new-name`" else "new-name")
        }
        local <- topic_rename_call(f, 9L, "  ori", "nested")
        expect_setequal(topic_edit_rows(local, f$uri), c(9L, 10L))
    }
})

test_that("explicit topic names are independent and output filenames do not name donors", {
    for (name in c("original", "stable")) {
        lines <- c(paste("#' @name", name), "#' @param x Input.", "original <- function(x) x",
            paste("#' @inheritParams", name), "wrapper <- function(x) x")
        f <- topic_rename_doc(lines)
        expect_identical(topic_edit_rows(topic_rename_call(f, 3L, "ori", "fresh"), f$uri), 3L)
    }
    f <- topic_rename_doc(c("#' @rdname stable-page", "#' @param x Input.", "original <- function(x) x",
            "#' @inheritParams original x", "#' @inheritParams stable-page", "wrapper <- function(x) x"))
    expect_setequal(topic_edit_rows(topic_rename_call(f, 3L, "ori", "fresh"), f$uri), c(3L, 4L))
})

test_that("topic rename rejects ambiguity, an unwritable name, capture and affected formal selectors", {
    base <- c("#' @param x,y (`Model`) Inputs.", "original <- function(x, y) x")
    f <- topic_rename_doc(c(base, "#' @inheritParams original y", "wrapper <- function(x, y) x"))
    expect_match(topic_rename_call(f, 2L, "ori", "two names")$error$message, "@inheritParams")
    expect_match(topic_rename_call(f, 2L, "original <- function(x, y", "next_y")$error$message, "selector")
    expect_match(topic_rename_call(f, 2L, "original <- function(x", "next_x")$error$message, "@inheritParams")
    expect_match(topic_rename_call(f, 4L, "wrapper <- function(x", "next_x")$error$message, "@inheritParams")
    g <- topic_rename_doc(c(base, "#' @inheritParams fresh", "wrapper <- function(x) x"))
    expect_match(topic_rename_call(g, 2L, "ori", "fresh")$error$message, "capture")
    shared <- topic_rename_doc(c(base, "#' @rdname original", "peer <- function() 1",
            "#' @inheritParams original", "use <- function(x) x"))
    expect_match(topic_rename_call(shared, 2L, "ori", "other")$error$message, "ambiguous")
    stale <- topic_rename_doc(c(base, "#' @inheritParams original", "unfinished <- function(x) {", "  x$"))
    expect_match(topic_rename_call(stale, 2L, "ori", "fresh")$error$message, "[Ii]ncomplete")
})

test_that("a package rename reaches matching cold consumers and invalidates changed files", {
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines("Package: fixture", file.path(root, "DESCRIPTION"))
    writeLines("", file.path(root, "NAMESPACE"))
    w <- Workspace$new(root)
    owner <- file.path(root, "R", "donor.R")
    definition <- c("#' @param x (`Model`) Input.", "private_donor <- function(x) x")
    writeLines(definition, owner)
    f <- topic_rename_doc(definition, path_to_uri(owner), w)
    path <- file.path(root, "R", "borrow.R")
    uri <- path_to_uri(path)
    lines <- c("#' @inheritParams fixture::private_donor x", "consume <- function(x) x",
        "#' @inheritParams external::private_donor", "ignore <- function(x) x")
    writeLines(lines, path)
    w$index$update_path(path)
    expect_false(w$documents$has(uri))
    rename <- function() topic_rename_call(f, 2L, "private_", "fresh")
    res <- rename()
    expect_null(res$error)
    expect_identical(topic_edit_rows(res, uri), 1L)
    edit <- res$result$changes[[uri]][[1L]]
    r <- edit$range
    expect_identical(substr(lines[[1L]], r$start$character + 1L, r$end$character), "private_donor")
    expect_identical(edit$newText, "fresh")
    writeLines(c("# newer text", lines), path)
    expect_match(rename()$error$message, "current package source")
    w$index$update_path(path)
    expect_identical(topic_edit_rows(rename(), uri), 2L)
    expect_true(file.remove(path))
    w$index$remove(uri)
    expect_false(uri %in% w$index$roxygen_inherit_uris("fixture::private_donor"))
    expect_length(topic_edit_rows(rename(), uri), 0L)
})

test_that("open edited consumers win and closing restores the indexed file", {
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines("Package: fixture", file.path(root, "DESCRIPTION"))
    writeLines("", file.path(root, "NAMESPACE"))
    w <- Workspace$new(root)
    x <- c("#' A donor.", "donor <- function(x) x")
    owner <- file.path(root, "R", "donor.R")
    writeLines(x, owner)
    f <- topic_rename_doc(x, path_to_uri(owner), w)
    path <- file.path(root, "R", "consumer.R")
    disk <- c("#' @inheritParams donor", "consumer <- function(x) x")
    writeLines(disk, path)
    uri <- path_to_uri(path)
    g <- topic_rename_doc(c("# unsaved", disk), uri, w)
    rename <- function() topic_rename_call(f, 2L, "don", "future")
    expect_identical(topic_edit_rows(rename(), uri), 2L)
    g$doc$did_close()
    expect_match(rename()$error$message, "current package source")
    w$index$update_path(path)
    expect_identical(topic_edit_rows(rename(), uri), 1L)

    collision <- file.path(root, "R", "another.R")
    writeLines(c("#' @name future", "#' @param y Input.", "NULL"), collision)
    w$index$update_path(collision)
    expect_match(rename()$error$message, "conflicts")
    expect_true(file.rename(collision, paste0(collision, ".bak")))
    w$index$remove(path_to_uri(collision))
    expect_identical(topic_edit_rows(rename(), uri), 1L)
})

test_that("formal rename requires a complete set of potentially inherited parameters", {
    f <- topic_rename_doc(c("#' @inheritParams stats::lm", "fit <- function(data) data"))
    result <- topic_rename_call(f, 2L, "fit <- function(da", "observations")
    expect_null(result$result)
    expect_match(result$error$message, "@inheritParams")

    f <- topic_rename_doc(c("#' @param data A data frame.", "donor <- function(data) data",
            "#' @inheritParams donor", "fit <- function(keep) keep"))
    expect_null(topic_rename_call(f, 4L, "fit <- function(ke", "unrelated")$error)
    expect_match(topic_rename_call(f, 4L, "fit <- function(ke", "data")$error$message, "@inheritParams")
})

test_that("formal rename protects positional inheritance selectors", {
    for (selector in c("2L", "1:2", "(2)")) {
        f <- topic_rename_doc(c("#' @param a First.", "#' @param b Second.", "donor <- function(a, b) b",
                paste("#' @inheritParams donor", selector), "read <- function(b) b"))
        expect_match(topic_rename_call(f, 3L, "donor <- function(a, b", "other")$error$message, "@inheritParams")
    }
})
