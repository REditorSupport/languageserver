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
    first$requested_packages <- list(packages = character(), namespace_packages = character())
    workspace$documents$set(uri, first)
    parse_callback(self, uri, 1L, parse_document(uri, content))
    expect_null(parse_task(self, uri, first))

    second <- Document$new(other_uri, version = 1L, content = content)
    second$requested_packages <- list(packages = character(), namespace_packages = character())
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

test_that("parse cache hits do not rebuild or resize the cached snapshot", {
    uri <- "file:///cached-snapshot.R"
    workspace <- Workspace$new(NULL)
    content <- "value <- function(x) x"
    document <- Document$new(uri, version = 1L, content = content)
    document$requested_packages <- list(packages = character(), namespace_packages = character())
    workspace$documents$set(uri, document)
    self <- list(get_workspace = function(...) workspace, pending_replies = collections::dict())
    parse_callback(self, uri, 1L, parse_document(uri, content))
    bytes <- workspace$parse_cache$bytes()
    mockery::stub(parse_callback, "workspace$parse_cache$set", function(...) stop("snapshot already cached"))
    cached_task <- parse_task
    mockery::stub(cached_task, "parse_callback", parse_callback)
    document$set_content(2L, content)
    expect_null(cached_task(self, uri, document))
    expect_identical(document$parse_data$version, 2L)
    expect_equal(workspace$parse_cache$bytes(), bytes)
    expect_length(document$parse_data$definitions, 1L)
})

test_that("unchanged buffers reuse their current parse after cache eviction", {
    uri <- "file:///evicted-buffer.R"
    workspace <- Workspace$new(NULL)
    content <- "value <- function(x) x"
    document <- Document$new(uri, version = 1L, content = content)
    document$requested_packages <- list(packages = character(), namespace_packages = character())
    workspace$documents$set(uri, document)
    self <- list(get_workspace = function(...) workspace, pending_replies = collections::dict())
    parse_callback(self, uri, 1L, parse_document(uri, content))
    parsed <- document$parse_data
    workspace$parse_cache$clear()
    expect_null(parse_task(self, uri, document))
    expect_identical(document$parse_data, parsed)
    expect_equal(workspace$parse_cache$size(), 0L)

    # didSave can change content without changing the document version.
    document$set_content(1L, "changed <- 2")
    expect_s3_class(parse_task(self, uri, document), "Task")
})
test_that("source discovery includes computed callees without executing them", {
    marker <- new.env(parent = emptyenv())
    marker$ran <- FALSE
    expression <- quote((function() {
        marker$ran <- TRUE
        base::source("callee.R")
        sys.source("callee.R", envir = environment())
    })(source("argument.R")))
    calls <- .Call("source_calls_c", expression, PACKAGE = "languageserver")
    expect_identical(calls, list(quote(base::source("callee.R")),
            quote(sys.source("callee.R", envir = environment())), quote(source("argument.R"))))
    expect_false(marker$ran)
    expect_identical(vapply(index_source_specs(expression), function(spec) spec$path, character(1L)),
        c("callee.R", "callee.R", "argument.R"))
    local <- quote((local(source("local.R")))())
    expect_identical(.Call("source_calls_c", local, PACKAGE = "languageserver"),
        list(quote(source("local.R"))))
    root <- withr::local_tempdir()
    for (name in c("callee.R", "argument.R")) writeLines("helper <- 1", file.path(root, name))
    path <- file.path(root, "main.R")
    content <- deparse(expression)
    writeLines(content, path)
    summary <- index_shallow_summary(path, content, root)
    expect_identical(summary$sources,
        lapply(file.path(root, c("callee.R", "argument.R")), path_to_uri))
    expect_false(marker$ran)
})

test_that("old persistent summaries refresh omitted computed-callee dependencies", {
    settings <- list(index_mode = "full", index_persistent_cache = TRUE)
    old <- stats::setNames(lapply(names(settings), lsp_settings$get), names(settings))
    withr::defer(for (name in names(old)) lsp_settings$set(name, old[[name]]))
    for (name in names(settings)) lsp_settings$set(name, settings[[name]])
    root <- index_normalize_path(withr::local_tempdir())
    helper <- file.path(root, "helper.R")
    writeLines("helper <- function(value) value", helper)
    path <- file.path(root, "main.R")
    content <- '(function() source("helper.R"))()'
    writeLines(content, path)
    stale <- index_shallow_summary(path, content, root)
    stale$sources <- list()
    stale$source_candidates <- character()
    stale$source_candidate_exists <- logical()
    expect_true(index_source_candidates_current(stale))
    cache_file <- file.path(root, "index.rds")
    saveRDS(list(version = 2L, root = root, summaries = list(stale)), cache_file)

    index <- WorkspaceIndex$new(root, cache_path = cache_file)
    index$load_cache()
    expect_identical(index$summaries$size(), 0L)
    index$discover()
    uri <- path_to_uri(path)
    expect_true(uri %in% index$pending)
    for (i in seq_len(5L)) {
        index$process_batch()
        if (index$complete()) break
    }
    expect_true(index$complete())
    expect_identical(index$source_edges$get(uri), path_to_uri(helper))
    expect_identical(index$summaries$get(uri)$sources, list(path_to_uri(helper)))
    refreshed <- readRDS(cache_file)
    expect_identical(refreshed$version, index_cache_version)
    restored <- WorkspaceIndex$new(root, cache_path = cache_file)
    restored$load_cache()
    expect_identical(restored$source_edges$get(uri), path_to_uri(helper))
})
