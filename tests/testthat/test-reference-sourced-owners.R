source_owner_workspace <- function(scripts) {
    root <- index_normalize_path(withr::local_tempdir(.local_envir = parent.frame()))
    workspace <- Workspace$new(root)
    paths <- file.path(root, names(scripts))
    for (i in seq_along(paths)) writeLines(scripts[[i]], paths[[i]])
    documents <- lapply(seq_along(paths), function(i) {
        uri <- path_to_uri(paths[[i]])
        doc <- Document$new(uri, content = scripts[[i]], version = 1L)
        parsed <- parse_document(uri, doc$content)
        parsed$version <- doc$version
        workspace$documents$set(uri, doc)
        workspace$update_parse_data(uri, parsed)
        workspace$index$update_content(uri, doc$content, parse_data = parsed)
        doc
    })
    list(workspace = workspace, docs = stats::setNames(documents, names(scripts)))
}

source_owner_query <- function(f, file, row, col, rename = TRUE) {
    doc <- f$docs[[file]]
    point <- list(row = row, col = col)
    if (rename) rename_reply(1L, doc$uri, f$workspace, doc, point, "Builder") else
        references_reply(1L, doc$uri, f$workspace, doc, point)
}

source_owner_rows <- function(reply, doc, rename = TRUE) {
    expect_null(reply$error)
    sites <- if (rename) reply$result$changes[[doc$uri]] else Filter(function(ref) identical(ref$uri, doc$uri), reply$result)
    sort(vapply(sites, function(site) site$range$start$line, integer(1L)))
}

test_that("ordinary references select the active sourced global owner", {
    f <- source_owner_workspace(list(
        "a.R" = 'Maker <- R6::R6Class("A", public = list(a = TRUE))',
        "b.R" = 'Maker <- R6::R6Class("B", public = list(b = TRUE))',
        "main.R" = c('source("a.R")', "Alias <- Maker", 'source("b.R")',
            "second_value <- Maker$new()", "Maker <- 5", "third_value <- Maker")))
    for (rename in c(TRUE, FALSE)) {
        first <- source_owner_query(f, "a.R", 0L, 1L, rename)
        second <- source_owner_query(f, "b.R", 0L, 1L, rename)
        last <- source_owner_query(f, "main.R", 4L, 1L, rename)
        expect_identical(source_owner_rows(first, f$docs[["main.R"]], rename), 1L)
        expect_identical(source_owner_rows(second, f$docs[["main.R"]], rename), 3L)
        expect_identical(source_owner_rows(last, f$docs[["main.R"]], rename), 4:5)
        read <- source_owner_query(f, "main.R", 3L, 18L, rename)
        expect_identical(source_owner_rows(read, f$docs[["b.R"]], rename), 0L)
        expect_length(source_owner_rows(read, f$docs[["a.R"]], rename), 0L)
    }
})

test_that("nested sources replay ordinary reads and reject conflicting invocation times", {
    scripts <- list("a.R" = "Maker <- 1", "b.R" = "Maker <- 2",
        "middle.R" = c("before <- Maker", 'source("b.R")', "after <- Maker"),
        "main.R" = c('source("a.R")', 'source("middle.R")'))
    f <- source_owner_workspace(scripts)
    for (rename in c(TRUE, FALSE)) {
        expect_identical(source_owner_rows(source_owner_query(f, "a.R", 0L, 1L, rename), f$docs[["middle.R"]], rename), 0L)
        expect_identical(source_owner_rows(source_owner_query(f, "b.R", 0L, 1L, rename), f$docs[["middle.R"]], rename), 2L)
    }
    scripts[["main.R"]] <- c(scripts[["main.R"]], 'source("middle.R")')
    f <- source_owner_workspace(scripts)
    for (file in c("a.R", "b.R")) {
        reply <- source_owner_query(f, file, 0L, 1L)
        expect_match(reply$error$message, "Different sourced declarations")
        expect_null(reply$result)
    }
})

test_that("late-bound globals block a split rename only when multiple owners are sourced", {
    scripts <- list("a.R" = c("Maker <- 1", "later <- function() Maker"), "b.R" = "Maker <- 2",
        "main.R" = c('source("a.R")', "before <- Maker", 'source("b.R")', "after <- Maker"))
    f <- source_owner_workspace(scripts)
    for (file in c("a.R", "b.R")) {
        reply <- source_owner_query(f, file, 0L, 1L)
        expect_match(reply$error$message, "Different sourced declarations")
        expect_null(reply$result)
        refs <- source_owner_query(f, file, 0L, 1L, FALSE)
        expect_false(1L %in% source_owner_rows(refs, f$docs[["a.R"]], FALSE))
    }
    scripts[["main.R"]] <- scripts[["main.R"]][1:2]
    f <- source_owner_workspace(scripts)
    reply <- source_owner_query(f, "a.R", 0L, 1L)
    expect_identical(source_owner_rows(reply, f$docs[["a.R"]]), 0:1)
    expect_identical(source_owner_rows(reply, f$docs[["main.R"]]), 1L)
})

test_that("a global selected inside glue follows the matching source invocation", {
    f <- source_owner_workspace(list("a.R" = "Maker <- 1", "b.R" = "Maker <- 2",
            "main.R" = c('source("a.R")', 'glue::glue("{Maker}")', 'source("b.R")', 'glue::glue("{Maker}")')))
    for (rename in c(FALSE, TRUE)) {
        for (i in 1:2) {
            row <- 2L * i - 1L
            owner <- c("a.R", "b.R")[[i]]
            result <- source_owner_query(f, "main.R", row, 14L, rename)
            expect_identical(source_owner_rows(result, f$docs[["main.R"]], rename), row)
            expect_identical(source_owner_rows(result, f$docs[[owner]], rename), 0L)
        }
    }
})

test_that("a reference request includes closed siblings when a caller sources competing declarations", {
    f <- source_owner_workspace(list("a.R" = "Maker <- 1", "b.R" = "Maker <- 2",
        "reader.R" = "observed <- Maker", "unrelated.R" = "Maker <- 3",
        "main.R" = c('source("a.R")', 'source("reader.R")', 'source("b.R")')))
    for (name in c("b.R", "reader.R")) f$workspace$documents$remove(f$docs[[name]]$uri)
    required <- reference_request_documents(f$workspace, f$docs[["a.R"]], list(row = 0L, col = 1L))
    expected <- vapply(f$docs[c("a.R", "b.R", "reader.R", "main.R")], `[[`, "", "uri")
    expect_setequal(required, unname(expected))
})

test_that("shared reads agree across independently sourced entry scripts", {
    scripts <- list("a.R" = "Maker <- 1", "b.R" = "Maker <- 2", "reader.R" = "result <- Maker",
        "one.R" = c('source("a.R")', 'source("reader.R")', 'source("b.R")'),
        "two.R" = c('source("b.R")', 'source("reader.R")'))
    f <- source_owner_workspace(scripts)
    for (file in c("a.R", "b.R")) {
        rename <- source_owner_query(f, file, 0L, 1L)
        expect_match(rename$error$message, "Different sourced declarations")
        refs <- source_owner_query(f, file, 0L, 1L, FALSE)
        expect_length(source_owner_rows(refs, f$docs[["reader.R"]], FALSE), 0L)
    }
    needed <- reference_request_documents(f$workspace, f$docs[["a.R"]], list(row = 0L, col = 1L))
    expect_true(f$docs[["two.R"]]$uri %in% needed)
    scripts[["two.R"]][1L] <- 'source("a.R")'
    f <- source_owner_workspace(scripts)
    res <- source_owner_query(f, "a.R", 0L, 1L)
    expect_identical(source_owner_rows(res, f$docs[["reader.R"]]), 0L)
})

test_that("field writes retain the binding owner's certainty until the next declaration", {
    exprs <- parse(text = c("x <- list()", "x$a <- 1", "if (cond) x <- other", "x$b <- 2", "x <- list()", "x$c <- 3"))
    items <- lapply(seq_along(exprs), function(i) list(expr = exprs[[i]], start = c(i, 0L), end = c(i, 20L)))
    history <- member_index_bindings(items)$x
    owner <- function(i) member_history_owner(history, c(i, 21L), "source.R")
    expect_identical(owner(2L), "source.R")
    expect_null(owner(3L))
    expect_null(owner(4L))
    expect_identical(owner(6L), "source.R")
})

test_that("independent entries must agree on a shared reader before a declaration is renamed", {
    scripts <- list("a.R" = "Maker <- 1", "b.R" = "Maker <- 2", "reader.R" = "observed <- Maker",
        "shared.R" = 'source("reader.R")', "one.R" = c('source("a.R")', 'source("shared.R")', "value <- Maker"),
        "two.R" = c('source("b.R")', 'source("shared.R")', "value <- Maker"),
        "unused.R" = "Maker <- 3", "unrelated.R" = 'source("unused.R")')
    f <- source_owner_workspace(scripts)
    uri <- function(file) f$docs[[file]]$uri
    for (pair in list(c("a.R", "one.R"), c("b.R", "two.R"))) {
        result <- source_owner_query(f, pair[1L], 0L, 1L, FALSE)
        expect_identical(source_owner_rows(result, f$docs[[pair[2L]]], FALSE), 2L)
        expect_length(source_owner_rows(result, f$docs[["reader.R"]], FALSE), 0L)
        edit <- source_owner_query(f, pair[1L], 0L, 1L)
        expect_null(edit$result)
        expect_match(edit$error$message, "Different sourced declarations")
    }
    request <- function() reference_request_documents(f$workspace, f$docs[["a.R"]], list(row = 0L, col = 1L))
    expect_setequal(request(), vapply(f$docs[1:6], `[[`, "", "uri", USE.NAMES = FALSE))
    f$workspace$documents$remove(uri("reader.R"))
    expect_true(uri("reader.R") %in% request())
    expect_false(uri("two.R") %in% request())
    scripts[["reader.R"]] <- "observed <- 1"
    f <- source_owner_workspace(scripts)
    expect_setequal(request(), c(uri("a.R"), uri("one.R")))
    expect_null(source_owner_query(f, "a.R", 0L, 1L)$error)
    expect_null(f$docs[["two.R"]]$member_workspace_view)

    scripts[["reader.R"]] <- "observed <- Maker"
    scripts[["two.R"]][1L] <- 'source("a.R")'
    scripts[c("unused.R", "unrelated.R")] <- NULL
    f <- source_owner_workspace(scripts)
    for (rename in c(FALSE, TRUE)) {
        result <- source_owner_query(f, "a.R", 0L, 1L, rename)
        expect_identical(source_owner_rows(result, f$docs[["reader.R"]], rename), 0L)
        expect_identical(source_owner_rows(result, f$docs[["two.R"]], rename), 2L)
    }
    expect_true(f$workspace$index$shared_source_count > 0L)
    f$workspace$index$remove(uri("two.R"))
    expect_identical(f$workspace$index$shared_source_count, 0L)
    expect_null(reference_shared_readers(f$workspace, stop("Unneeded caller scan"), "Maker"))
})
