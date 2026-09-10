test_that("source discovery visits nested syntax without evaluating it", {
    expr <- parse(text = c(
        "source('first.R')",
        "f <- function(x) { base::source(file = 'second.R'); sys.source('third.R', ) }",
        "other::source('ignored.R')",
        "ordinary_call(, source(dynamic), base::sys.source('fourth.R', environment()))"
    ))
    specs <- index_source_specs(expr)
    expect_equal(
        vapply(specs, `[[`, character(1L), "path"),
        c("first.R", "second.R", "third.R", "fourth.R")
    )
    expect_length(index_source_specs(quote(x + y)), 0L)
    expect_length(index_source_specs(NULL), 0L)
})

test_that("accepted parse results supply index metadata without reparsing", {
    root <- withr::local_tempdir()
    path <- file.path(root, "main.R")
    writeLines("helper <- TRUE", file.path(root, "helper.R"))
    content <- c("source('helper.R')", "value <- function(x) x")
    parsed <- parse_document(path_to_uri(path), content)
    summarize <- index_shallow_summary
    stub(summarize, "parse", function(...) stop("must reuse the completed parse"))
    summary <- summarize(path, content, root, parse_data = parsed)
    expect_equal(summary$definitions, as.list(parsed$definitions))
    expect_equal(summary$sources, list(path_to_uri(file.path(root, "helper.R"))))
    expect_false(summary$parse_error)
})

test_that("workspace scope caches follow source edges and document membership", {
    root <- withr::local_tempdir()
    workspace <- Workspace$new(root)
    paths <- file.path(root, c("main.R", "first.R", "second.R"))
    uris <- vapply(paths, path_to_uri, character(1L))
    for (i in seq_along(paths)) {
        writeLines("value <- 1", paths[[i]])
        workspace$documents$set(uris[[i]], Document$new(uris[[i]]))
    }
    workspace$index$update_content(uris[[1L]], "source('first.R')")
    expect_setequal(workspace$document_uris_for_context(uris[[1L]]), uris[1:2])
    first_namespace <- workspace$get_namespace(WORKSPACE, uri = uris[[1L]])
    expect_identical(
        workspace$get_namespace(WORKSPACE, uri = uris[[1L]]),
        first_namespace
    )
    expect_setequal(workspace$document_uris_for_references(uris[[2L]]), uris[1:2])

    workspace$index$update_content(uris[[1L]], "source('second.R')")
    expect_setequal(workspace$document_uris_for_context(uris[[1L]]), uris[c(1, 3)])
    expect_setequal(workspace$document_uris_for_references(uris[[2L]]), uris[[2L]])
    expect_false(identical(
        workspace$get_namespace(WORKSPACE, uri = uris[[1L]]),
        first_namespace
    ))
    workspace$documents$remove(uris[[3L]])
    remaining <- unlist(workspace$document_uris_for_context(uris[[1L]]),
        use.names = FALSE)
    expect_equal(remaining, uris[[1L]])
})

test_that("didChange leaves index work to the accepted background parse", {
    workspace <- new.env(parent = baseenv())
    workspace$root <- tempdir()
    workspace$documents <- collections::dict()
    self <- new.env(parent = baseenv())
    self$pending_replies <- collections::dict()
    self$get_workspace <- function(...) workspace
    self$syncs <- list()
    self$text_sync <- function(...) {
        self$syncs[[length(self$syncs) + 1L]] <- list(...)
    }
    fixture <- list(self = self, workspace = workspace)
    fixture$workspace$index <- new.env(parent = emptyenv())
    fixture$workspace$index$enabled <- TRUE
    fixture$workspace$index$update_content <- function(...) {
        stop("didChange must not parse synchronously")
    }
    uri <- path_to_uri(file.path(fixture$workspace$root, "change.R"))
    fixture$workspace$documents$set(
        uri,
        Document$new(uri, version = 1L, content = "value <- 1")
    )
    text_document_did_change(fixture$self, list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(text = "value <- 2"))
    ))
    expect_equal(fixture$workspace$documents$get(uri)$content, "value <- 2")
    expect_true(fixture$self$syncs[[1L]]$parse)
})

test_that("parse cache hits retain document identity", {
    uri <- "file:///cache-first.R"
    other_uri <- "file:///cache-second.R"
    content <- "value <- function(input) input"
    workspace <- Workspace$new(NULL)
    self <- new.env(parent = baseenv())
    self$get_workspace <- function(...) workspace
    self$pending_replies <- collections::dict()
    first <- Document$new(uri, version = 1L, content = content)
    first$requested_packages <- character()
    workspace$documents$set(uri, first)
    parse_callback(self, uri, 1L, parse_document(uri, content))
    expect_null(parse_task(self, uri, first))

    second <- Document$new(other_uri, version = 1L, content = content)
    second$requested_packages <- character()
    workspace$documents$set(other_uri, second)
    expect_s3_class(parse_task(self, other_uri, second), "Task")
    expect_null(second$parse_data)
})

test_that("equivalent workspace scopes share their aggregate symbol maps", {
    workspace <- Workspace$new(NULL)
    uris <- c("file:///scope-one.R", "file:///scope-two.R")
    for (uri in uris) workspace$documents$set(uri, Document$new(uri))
    expect_identical(
        workspace$get_namespace(WORKSPACE, uri = uris[[1L]]),
        workspace$get_namespace(WORKSPACE, uri = uris[[2L]])
    )
})
