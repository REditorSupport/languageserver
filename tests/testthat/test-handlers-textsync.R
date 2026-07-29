textsync_fixture <- function(root = tempdir()) {
    workspace <- new.env(parent = baseenv())
    workspace$root <- root
    workspace$documents <- collections::dict()
    workspace$diagnostics_globals_cache <- "cached"
    workspace$type_hierarchy_cache <- collections::dict()
    workspace$type_hierarchy_cache$set("cached", TRUE)
    workspace$update_count <- 0L
    workspace$update_loaded_packages <- function() {
        workspace$update_count <- workspace$update_count + 1L
    }

    self <- new.env(parent = baseenv())
    self$pending_replies <- collections::dict()
    self$deliveries <- list()
    self$syncs <- list()
    self$get_workspace <- function(...) workspace
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }
    self$text_sync <- function(...) {
        self$syncs[[length(self$syncs) + 1L]] <- list(...)
    }

    list(self = self, workspace = workspace)
}

test_that("didOpen replaces stale documents and schedules immediate parsing", {
    fixture <- textsync_fixture()
    path <- file.path(fixture$workspace$root, "open.R")
    uri <- path_to_uri(path)
    stale <- Document$new(uri, version = 0L, content = "stale")
    fixture$workspace$documents$set(uri, stale)

    text_document_did_open(fixture$self, list(textDocument = list(
        uri = uri,
        languageId = "r",
        version = 1L,
        text = "first\nsecond"
    )))

    document <- fixture$workspace$documents$get(uri)
    expect_equal(document$content, c("first", "second"))
    expect_equal(document$version, 1L)
    expect_true(document$is_open)
    expect_length(fixture$self$syncs, 1L)
    expect_equal(fixture$self$syncs[[1L]]$delay, 0)
    expect_true(fixture$self$syncs[[1L]]$parse)
})

test_that("didOpen and didSave can read content from disk", {
    fixture <- textsync_fixture()
    path <- file.path(fixture$workspace$root, "saved.R")
    writeLines(c("before", "save"), path)
    uri <- path_to_uri(path)

    text_document_did_open(fixture$self, list(textDocument = list(
        uri = uri, languageId = "r", version = 1L, text = NULL
    )))
    expect_equal(
        fixture$workspace$documents$get(uri)$content,
        c("before", "save")
    )

    writeLines(c("after", "save"), path)
    text_document_did_save(fixture$self, list(
        textDocument = list(uri = uri), text = NULL
    ))
    expect_equal(
        fixture$workspace$documents$get(uri)$content,
        c("after", "save")
    )

    text_document_did_save(fixture$self, list(
        textDocument = list(uri = uri), text = "client\ncontent"
    ))
    expect_equal(
        fixture$workspace$documents$get(uri)$content,
        c("client", "content")
    )

    missing_uri <- path_to_uri(file.path(fixture$workspace$root, "missing.R"))
    expect_null(text_document_did_save(fixture$self, list(
        textDocument = list(uri = missing_uri), text = "ignored"
    )))
})

test_that("didChange cancels stale replies and applies incremental content", {
    fixture <- textsync_fixture()
    uri <- path_to_uri(file.path(fixture$workspace$root, "change.R"))
    document <- Document$new(uri, version = 1L, content = "abc")
    fixture$workspace$documents$set(uri, document)

    queue <- collections::queue()
    queue$push(list(id = 1L, version = 1L))
    queue$push(list(id = 3L, version = 3L))
    fixture$self$pending_replies$set(uri, list(completion = queue))

    text_document_did_change(fixture$self, list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(
            range = list(
                start = list(line = 0L, character = 1L),
                end = list(line = 0L, character = 2L)
            ),
            text = "X"
        ))
    ))

    expect_equal(document$content, "aXc")
    expect_equal(document$version, 2L)
    expect_length(fixture$self$deliveries, 1L)
    expect_false(is.null(fixture$self$deliveries[[1L]]$error))
    expect_equal(fixture$self$deliveries[[1L]]$id, 1L)
    expect_equal(queue$size(), 1L)
    expect_equal(queue$peek()$id, 3L)
    expect_equal(
        fixture$self$syncs[[1L]]$parse_delay,
        lsp_settings$get("parse_delay")
    )
})

test_that("didChange tolerates a full replacement before didOpen", {
    fixture <- textsync_fixture()
    uri <- path_to_uri(file.path(fixture$workspace$root, "late-open.R"))

    text_document_did_change(fixture$self, list(
        textDocument = list(uri = uri, version = 4L),
        contentChanges = list(
            list(range = list(
                start = list(line = 0L, character = 0L),
                end = list(line = 0L, character = 0L)
            ), text = "ignored"),
            list(text = "full\nreplacement")
        )
    ))

    document <- fixture$workspace$documents$get(uri)
    expect_equal(document$content, c("full", "replacement"))
    expect_equal(document$version, 4L)

    other <- textsync_fixture()
    other_uri <- path_to_uri(file.path(other$workspace$root, "incremental.R"))
    text_document_did_change(other$self, list(
        textDocument = list(uri = other_uri, version = 1L),
        contentChanges = list(list(
            range = list(
                start = list(line = 0L, character = 0L),
                end = list(line = 0L, character = 0L)
            ),
            text = "ignored"
        ))
    ))
    expect_equal(other$workspace$documents$get(other_uri)$content, "")
})

test_that("didClose removes non-package documents and clears caches", {
    fixture <- textsync_fixture()
    path <- file.path(fixture$workspace$root, "closed.R")
    uri <- path_to_uri(path)
    document <- Document$new(uri, version = 1L, content = "value <- 1")
    document$did_open()
    fixture$workspace$documents$set(uri, document)
    fixture$self$pending_replies$set(uri, list())

    text_document_did_close(fixture$self, list(
        textDocument = list(uri = uri)
    ))

    expect_false(fixture$workspace$documents$has(uri))
    expect_null(fixture$workspace$diagnostics_globals_cache)
    expect_equal(fixture$workspace$type_hierarchy_cache$size(), 0L)
    expect_equal(fixture$workspace$update_count, 1L)
    expect_false(fixture$self$pending_replies$has(uri))
    expect_true(length(fixture$self$deliveries) >= 1L)
})

test_that("didClose retains documents belonging to an open package", {
    package_root <- normalizePath(file.path(getwd(), "..", ".."))
    fixture <- textsync_fixture(package_root)
    uri <- path_to_uri(file.path(package_root, "R", "retained.R"))
    document <- Document$new(uri, version = 1L, content = "value <- 1")
    document$did_open()
    fixture$workspace$documents$set(uri, document)
    fixture$self$pending_replies$set(uri, list())

    text_document_did_close(fixture$self, list(
        textDocument = list(uri = uri)
    ))

    expect_true(fixture$workspace$documents$has(uri))
    expect_false(document$is_open)
    expect_equal(fixture$workspace$update_count, 0L)
})
