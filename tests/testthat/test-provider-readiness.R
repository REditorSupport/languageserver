readiness_catalogs <- local({
    cached <- NULL
    function() {
        if (is.null(cached)) cached <<- stats::setNames(lapply(startup_packages, s3_prepare_namespace), startup_packages)
        cached
    }
})

readiness_fixture <- function(content, .local_envir = parent.frame()) {
    root <- withr::local_tempdir(.local_envir = .local_envir)
    uri <- path_to_uri(file.path(root, "main.R"))
    workspace <- testthat::with_mocked_bindings(Workspace$new(root),
        workspace_startup_packages = function() startup_packages,
        workspace_startup_s3 = readiness_catalogs,
        .package = "languageserver")
    document <- Document$new(uri, "r", 1L, content)
    document$did_open()
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, content, include_xml = FALSE)
    parsed$version <- document$version
    workspace$update_parse_data(uri, parsed)
    document$requested_packages <- list(packages = character(), namespace_packages = c("base", "pending"))
    server <- new.env(parent = emptyenv())
    server$get_workspace <- function(...) workspace
    server$pending_replies <- collections::dict()
    methods <- c("textDocument/completion", "textDocument/hover", "textDocument/signatureHelp")
    server$pending_replies$set(uri, stats::setNames(lapply(methods, function(...) collections::queue()), methods))
    server$request_handlers <- stats::setNames(list(text_document_completion, text_document_hover,
            text_document_signature_help), methods)
    server$replies <- list()
    server$deliver <- function(response, ...) server$replies[[length(server$replies) + 1L]] <- response
    server$resolve_task_manager <- list(has = function(...) TRUE, promote = function(...) NULL,
        add_task = function(...) NULL)
    server$parse_task_manager <- list(promote = function(...) NULL, add_task = function(...) NULL)
    server$refresh_index_documents <- function(...) NULL
    server$ClientCapabilities <- list()
    list(server = server, workspace = workspace, document = document, uri = uri)
}

readiness_params <- function(fixture, row, col) {
    list(textDocument = list(uri = fixture$uri), position = position(row, col))
}

readiness_package_fixture <- function() {
    fixture <- readiness_fixture("findInterval(1, c(0, 2))", .local_envir = parent.frame())
    workspace <- fixture$workspace
    dir.create(file.path(workspace$root, "R"))
    writeLines("Package: readinessfixture", file.path(workspace$root, "DESCRIPTION"))
    workspace$documents$remove(fixture$uri)
    workspace$index$remove(fixture$uri)
    uri <- path_to_uri(file.path(workspace$root, "R", "main.R"))
    document <- Document$new(uri, "r", 1L, fixture$document$content)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, document$content, include_xml = FALSE)
    parsed$version <- document$version
    workspace$update_parse_data(uri, parsed)
    fixture$uri <- uri
    fixture$document <- document
    fixture
}

readiness_peer <- function(fixture, name, content, full = TRUE) {
    uri <- path_to_uri(file.path(fixture$workspace$root, "R", paste0(name, ".R")))
    if (full) {
        document <- Document$new(uri, "r", 1L, content)
        fixture$workspace$documents$set(uri, document)
        parsed <- parse_document(uri, content, include_xml = FALSE)
        parsed$version <- document$version
        fixture$workspace$update_parse_data(uri, parsed)
    } else {
        fixture$workspace$index$update_content(uri, content)
    }
    uri
}

ReadinessSyncServer <- R6::R6Class("ReadinessSyncServer",
    inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(initialize = function(workspace) {
        self$workspaces <- collections::dict()
        self$workspaces$set(DEFAULT_WORKSPACE, workspace)
        self$workspace_cache <- collections::dict()
        self$pending_replies <- collections::dict()
    })
)

readiness_masking_catalog <- function() {
    list(schema = 1L, package = "masking", generation = "ready",
        callables = "findInterval", objects = list(), dependencies = "base",
        functions = list(findInterval = list(name = "findInterval", owner = "masking",
                generic = list(name = "findInterval", formal = "x"), formals = as.pairlist(alist(x = , ... = )))),
        methods = list("masking::findInterval.data.frame" = list(name = "findInterval.data.frame",
                package = "masking", owner = "masking", generic = "findInterval", generic_package = "masking",
                class = "data.frame", formals = as.pairlist(alist(x = , columns = NULL, ... = )))))
}

readiness_task_manager <- function() {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::dict()
    manager$add_task <- function(uri, task) {
        if (is.null(task)) manager$tasks$pop(uri, NULL) else manager$tasks$set(uri, task)
    }
    manager$cancel <- function(uri) manager$tasks$pop(uri, NULL)
    manager$has <- function(uri) manager$tasks$has(uri)
    manager$promote <- function(...) NULL
    manager
}

test_that("generic providers do not wait for member-completion model preparation", {
    fixture <- readiness_fixture("print()")
    server <- fixture$server
    server$resolve_task_manager <- readiness_task_manager()
    server$member_task_manager <- readiness_task_manager()
    prepare_calls <- 0L
    testthat::local_mocked_bindings(member_prepare_package = function(...) {
        prepare_calls <<- prepare_calls + 1L
        list(schema = 1L, roots = list(), namespace_roots = list(), generation = "fixture")
    }, .package = "languageserver")
    task <- resolve_task(server, fixture$uri, fixture$document, character(), namespace_packages = c("base", "pending"))
    server$resolve_task_manager$add_task(fixture$uri, task)
    expect_true(fixture$workspace$index$indexing_status()$done)
    text_document_signature_help(server, 1L, readiness_params(fixture, 0L, 6L))
    text_document_hover(server, 2L, readiness_params(fixture, 0L, 2L))
    expect_length(server$replies, 0L)
    private <- task$.__enclos_env__$private
    expect_false(private$args$prepare)
    result <- do.call(private$target, private$args)
    expect_identical(prepare_calls, 0L)
    server$resolve_task_manager$cancel(fixture$uri)
    private$callback(result)
    expect_true(server$member_task_manager$has(fixture$uri))
    expect_length(server$replies, 2L)
    replies <- stats::setNames(server$replies, vapply(server$replies, function(reply) as.character(reply$id), ""))
    expect_match(replies$`1`$result$signatures[[1L]]$label, "print(x", fixed = TRUE)
    expect_match(paste(replies$`2`$result$contents, collapse = " "), "print", fixed = TRUE)
    model <- server$member_task_manager$tasks$get(fixture$uri)$.__enclos_env__$private
    expect_true(model$args$prepare)
    expect_false(model$args$prepare_s3)

    resolve_task(server, fixture$uri, fixture$document, character(), namespace_packages = "base")
    expect_false(server$member_task_manager$has(fixture$uri))
})

test_that("resolve completion wakes requests even if result application fails or is stale", {
    fixture <- readiness_fixture("print()")
    server <- fixture$server
    queued <- character()
    server$queue_pending_replies <- function(uri) queued <<- c(queued, uri)
    load <- fixture$workspace$load_packages
    unlockBinding("load_packages", fixture$workspace)
    fixture$workspace$load_packages <- function(...) stop("cannot apply package metadata")
    withr::defer(fixture$workspace$load_packages <- load)
    expect_error(resolve_callback(server, fixture$uri, 1L, startup_packages), "cannot apply package metadata")
    expect_identical(queued, fixture$uri)
    queued <- character()
    fixture$document$version <- 2L
    expect_null(resolve_callback(server, fixture$uri, 1L, startup_packages))
    expect_identical(queued, fixture$uri)
})

test_that("ordinary providers respond while unrelated package metadata is pending", {
    fixture <- readiness_fixture(c("base::findInterval(1, c(0, 2))", "findInterval(1, c(0, 2))",
            "local_fn <- function(value = 1) value", "local_fn(1)"))
    for (row in 0:1) {
        col <- if (row == 0L) 19L else 13L
        params <- readiness_params(fixture, row, col)
        text_document_signature_help(fixture$server, row + 1L, params)
        expect_match(tail(fixture$server$replies, 1L)[[1L]]$result$signatures[[1L]]$label, "findInterval(x, vec", fixed = TRUE)
        params$position$character <- if (row == 0L) 12L else 5L
        text_document_hover(fixture$server, row + 3L, params)
        expect_match(tail(fixture$server$replies, 1L)[[1L]]$result$contents[[1L]], "findInterval", fixed = TRUE)
        params$position$character <- col
        text_document_completion(fixture$server, row + 5L, params)
        items <- tail(fixture$server$replies, 1L)[[1L]]$result$items
        expect_true("vec" %in% vapply(items, `[[`, "", "label"))
    }
    text_document_signature_help(fixture$server, 7L, readiness_params(fixture, 3L, 9L))
    expect_match(tail(fixture$server$replies, 1L)[[1L]]$result$signatures[[1L]]$label, "local_fn(value", fixed = TRUE)
    expect_length(fixture$server$replies, 7L)
    expect_true(all(vapply(fixture$server$pending_replies$get(fixture$uri), function(queue) queue$size() == 0L, logical(1L))))
})

test_that("queued ordinary requests drain independently of S3 metadata waits", {
    fixture <- readiness_fixture(c("base::findInterval(1, c(0, 2))", "summary(data.frame(x = 1))"))
    server <- fixture$server
    queues <- server$pending_replies$get(fixture$uri)
    queues$`textDocument/signatureHelp`$push(list(id = 1L, version = 1L,
            params = readiness_params(fixture, 0L, 19L)))
    queues$`textDocument/hover`$push(list(id = 2L, version = 1L,
            params = readiness_params(fixture, 1L, 3L)))
    expect_true(s3_metadata_waiting(server, fixture$uri, fixture$document, list(row = 1L, col = 3L), hover = TRUE))
    result <- drain_pending_replies(server, fixture$uri, 1L)
    expect_identical(result$processed, 1L)
    expect_identical(queues$`textDocument/signatureHelp`$size(), 0L)
    expect_identical(queues$`textDocument/hover`$size(), 1L)
    expect_length(server$replies, 1L)
    server$resolve_task_manager$has <- function(...) FALSE
    result <- drain_pending_replies(server, fixture$uri, 1L)
    expect_identical(result$processed, 1L)
    expect_length(server$replies, 2L)
})

test_that("unknown package generics still wait for their first catalog", {
    fixture <- readiness_fixture(c("new_generic(1)", "pending::new_generic(1)",
            "f <- function() base::findInterval(1, c(0, 2))"))
    expect_true(s3_metadata_waiting(fixture$server, fixture$uri, fixture$document, list(row = 0L, col = 12L)))
    expect_true(s3_metadata_waiting(fixture$server, fixture$uri, fixture$document, list(row = 1L, col = 21L)))
    location <- s3_call_location(fixture$document, list(row = 2L, col = 35L))
    expect_false(s3_may_dispatch(fixture$workspace, fixture$document, location))
    fixture$workspace$s3_metadata$set("pending", list(schema = 1L, generation = "ready",
            functions = list(), objects = list(), methods = list(), dependencies = character()))
    expect_false(s3_metadata_waiting(fixture$server, fixture$uri, fixture$document, list(row = 0L, col = 12L)))
})

test_that("ordinary calls inside functions remain responsive without bypassing shadowed calls", {
    fixture <- readiness_fixture(c("f <- function() {", "  findInterval(1, c(0, 2))", "}"))
    params <- readiness_params(fixture, 1L, 15L)
    expect_false(s3_metadata_waiting(fixture$server, fixture$uri, fixture$document, list(row = 1L, col = 15L)))
    text_document_signature_help(fixture$server, 1L, params)
    expect_match(fixture$server$replies[[1L]]$result$signatures[[1L]]$label, "findInterval", fixed = TRUE)

    shadowed <- readiness_fixture("f <- function(findInterval) findInterval(1)")
    expect_true(s3_metadata_waiting(shadowed$server, shadowed$uri, shadowed$document, list(row = 0L, col = 41L)))
    aliased <- readiness_fixture(c("f <- function() {", "  findInterval <- base::summary", "  findInterval(1)", "}"))
    expect_true(s3_metadata_waiting(aliased$server, aliased$uri, aliased$document, list(row = 2L, col = 15L)))
    quoted <- readiness_fixture("f <- function(`findInterval`) findInterval(1)")
    expect_true(s3_metadata_waiting(quoted$server, quoted$uri, quoted$document, list(row = 0L, col = 43L)))
    quoted_alias <- readiness_fixture(c("f <- function() {", "  `findInterval` <- base::summary", "  findInterval(1)", "}"))
    expect_true(s3_metadata_waiting(quoted_alias$server, quoted_alias$uri, quoted_alias$document, list(row = 2L, col = 15L)))
})

test_that("pending attached packages can mask ordinary functions with S3 generics", {
    fixture <- readiness_fixture(c("library(masking)", "findInterval(data.frame(x = 1), )",
            "base::findInterval(1, c(0, 2))"))
    fixture$document$requested_packages <- list(packages = "masking", namespace_packages = "base")
    server <- fixture$server
    params <- readiness_params(fixture, 1L, nchar(fixture$document$line0(1L)) - 1L)
    text_document_signature_help(server, 1L, params)
    hover_params <- readiness_params(fixture, 1L, 5L)
    text_document_hover(server, 2L, hover_params)
    text_document_completion(server, 3L, params)
    text_document_signature_help(server, 4L, readiness_params(fixture, 2L, 19L))
    expect_length(server$replies, 1L)
    queues <- server$pending_replies$get(fixture$uri)
    expect_true(all(vapply(queues, function(queue) queue$size() == 1L, logical(1L))))
    fixture$workspace$s3_metadata$set("masking", list(schema = 1L, package = "masking", generation = "failed",
            functions = list(), callables = character(), methods = list(), objects = list(), error = "Previous resolution failed"))
    expect_identical(drain_pending_replies(server, fixture$uri, 1L)$processed, 0L)

    fixture$workspace$s3_metadata$set("masking", readiness_masking_catalog())
    expect_identical(drain_pending_replies(server, fixture$uri, 1L)$processed, 3L)
    replies <- stats::setNames(server$replies, vapply(server$replies, function(reply) as.character(reply$id), ""))
    expect_match(replies$`1`$result$signatures[[1L]]$label, "columns = NULL", fixed = TRUE)
    expect_match(paste(replies$`2`$result$contents, collapse = " "), "findInterval.data.frame", fixed = TRUE)
    expect_true("columns" %in% vapply(replies$`3`$result$items, `[[`, "", "label"))
    expect_match(replies$`4`$result$signatures[[1L]]$label, "vec", fixed = TRUE)
})

test_that("namespace-only and later package requests do not mask an earlier ordinary call", {
    fixture <- readiness_fixture("findInterval(1, c(0, 2))")
    fixture$document$requested_packages <- list(packages = character(), namespace_packages = c("base", "masking"))
    params <- readiness_params(fixture, 0L, 13L)
    text_document_signature_help(fixture$server, 1L, params)
    expect_match(fixture$server$replies[[1L]]$result$signatures[[1L]]$label, "vec", fixed = TRUE)
    fixture$workspace$s3_metadata$set("masking", readiness_masking_catalog())
    text_document_signature_help(fixture$server, 2L, params)
    expect_match(fixture$server$replies[[2L]]$result$signatures[[1L]]$label, "vec", fixed = TRUE)

    later <- readiness_fixture(c("findInterval(1, c(0, 2))", "library(masking)"))
    later$document$requested_packages <- list(packages = "masking", namespace_packages = "base")
    text_document_signature_help(later$server, 1L, readiness_params(later, 0L, 13L))
    expect_match(later$server$replies[[1L]]$result$signatures[[1L]]$label, "vec", fixed = TRUE)
})

test_that("repeated unqualified calls avoid individual document and summary scans", {
    fixture <- readiness_package_fixture()
    workspace <- fixture$workspace
    for (i in seq_len(1000L)) {
        readiness_peer(fixture, paste0("peer", i), paste0("fn_", i, " <- function(x) x"), full = i <= 500L)
    }
    expect_identical(workspace$index$files$size(), 1001L)
    counts <- new.env(parent = emptyenv())
    counts$documents <- counts$summaries <- counts$keys <- 0L
    get_document <- workspace$documents$get
    get_summary <- workspace$index$summaries$get
    get_keys <- workspace$documents$keys
    withr::defer({
        workspace$documents$get <- get_document
        workspace$index$summaries$get <- get_summary
        workspace$documents$keys <- get_keys
    })
    workspace$documents$get <- function(...) {
        counts$documents <- counts$documents + 1L
        get_document(...)
    }
    workspace$index$summaries$get <- function(...) {
        counts$summaries <- counts$summaries + 1L
        get_summary(...)
    }
    workspace$documents$keys <- function(...) {
        counts$keys <- counts$keys + 1L
        get_keys(...)
    }
    location <- s3_call_location(fixture$document, list(row = 0L, col = 13L))
    expect_false(s3_may_dispatch(workspace, fixture$document, location))
    expect_gt(counts$documents, 0L)
    expect_gt(counts$summaries, 0L)
    counts$documents <- counts$summaries <- counts$keys <- 0L
    for (i in seq_len(10L)) expect_false(s3_may_dispatch(workspace, fixture$document, location))
    expect_identical(counts$documents, 0L)
    expect_identical(counts$summaries, 0L)
    expect_identical(counts$keys, 0L)
})

test_that("cached peer imports follow edits, indexing and workspace membership", {
    fixture <- readiness_package_fixture()
    workspace <- fixture$workspace
    peer <- readiness_peer(fixture, "peer", "library(masking)")
    disk <- readiness_peer(fixture, "disk", "library(disk_package)", full = FALSE)
    packages <- function() s3_call_packages(workspace, fixture$document, c(0L, 13L))
    expect_true(all(c("masking", "disk_package") %in% packages()))

    document <- workspace$documents$get(peer)
    document$set_content(2L, "helper <- 1")
    ReadinessSyncServer$new(workspace)$text_sync(peer, document)
    expect_false("masking" %in% packages())
    expect_true("disk_package" %in% packages())
    document$set_content(3L, "library(changed_package)")
    parsed <- parse_document(peer, document$content, include_xml = FALSE)
    parsed$version <- document$version
    workspace$update_parse_data(peer, parsed)
    expect_true("changed_package" %in% packages())

    workspace$index$update_content(disk, "library(updated_disk_package)")
    expect_true("updated_disk_package" %in% packages())
    expect_false("disk_package" %in% packages())

    replacement <- Document$new(path_to_uri(file.path(workspace$root, "R", "replacement.R")), "r", 1L,
        "library(replacement_package)")
    parsed <- parse_document(replacement$uri, replacement$content, include_xml = FALSE)
    parsed$version <- replacement$version
    replacement$update_parse_data(parsed)
    workspace$documents$remove(peer)
    workspace$documents$set(replacement$uri, replacement)
    workspace$update_loaded_packages()
    expect_true("replacement_package" %in% packages())
    # Unloaded package files remain visible through their disk summaries.
    expect_true("changed_package" %in% packages())
    workspace$index$remove(peer)
    expect_false("changed_package" %in% packages())

    workspace$imported_packages <- "imported_package"
    expect_true("imported_package" %in% packages())
    workspace$index <- WorkspaceIndex$new(workspace$root)
    expect_false("updated_disk_package" %in% packages())
    workspace$documents$remove(replacement$uri)
    expect_false("replacement_package" %in% packages())
})

test_that("cached peer imports preserve cursor-specific masking in the current document", {
    fixture <- readiness_package_fixture()
    document <- fixture$document
    document$set_content(2L, c("findInterval(1, c(0, 2))", "library(masking)", "findInterval(1, c(0, 2))"))
    parsed <- parse_document(fixture$uri, document$content, include_xml = FALSE)
    parsed$version <- document$version
    fixture$workspace$update_parse_data(fixture$uri, parsed)
    expect_false("masking" %in% s3_call_packages(fixture$workspace, document, c(0L, 13L)))
    expect_true("masking" %in% s3_call_packages(fixture$workspace, document, c(2L, 13L)))
    expect_false("masking" %in% s3_call_packages(fixture$workspace, document, c(0L, 13L)))
})

test_that("ordinary requests do not rescan the workspace definition catalog", {
    fixture <- readiness_fixture("findInterval(1, c(0, 2))")
    index <- fixture$workspace$index
    values <- index$summaries$values
    index$summaries$values <- function(...) stop("request scanned all workspace summaries")
    withr::defer(index$summaries$values <- values)
    for (i in seq_len(100L)) {
        uri <- path_to_uri(file.path(fixture$workspace$root, paste0(i, ".R")))
        index$set_summary(list(uri = uri, sources = character(),
                definitions = list(shared = list(name = "shared"))))
        text_document_signature_help(fixture$server, i, readiness_params(fixture, 0L, 13L))
    }
    expect_length(fixture$server$replies, 100L)
    expect_true(index$has_definition("shared"))
    index$remove(uri)
    expect_true(index$has_definition("shared"))
    for (i in seq_len(99L)) index$remove(path_to_uri(file.path(fixture$workspace$root, paste0(i, ".R"))))
    expect_false(index$has_definition("shared"))
})

test_that("failed parse workers settle queued requests and allow a later parse", {
    fixture <- readiness_fixture("base::findInterval(1, c(0, 2))")
    server <- fixture$server
    document <- fixture$document
    document$set_content(2L, c(document$content, "new_value <- 1"))
    queues <- server$pending_replies$get(fixture$uri)
    queues$`textDocument/signatureHelp`$push(list(id = 1L, version = 2L,
            params = readiness_params(fixture, 0L, 19L)))
    queues$`textDocument/hover`$push(list(id = 2L, version = 1L, params = list()))
    queues$`textDocument/completion`$push(list(id = 3L, version = 3L, params = list()))
    task <- parse_task(server, fixture$uri, document, retry = FALSE)
    task$.__enclos_env__$private$error(simpleError("worker exited"))
    expect_length(server$replies, 2L)
    expect_identical(server$replies[[1L]]$error$code, -32800)
    expect_identical(server$replies[[2L]]$error$code, -32603)
    expect_match(server$replies[[2L]]$error$message, "worker exited", fixed = TRUE)
    expect_identical(queues$`textDocument/completion`$size(), 1L)
    text_document_signature_help(server, 4L, readiness_params(fixture, 0L, 19L))
    expect_identical(tail(server$replies, 1L)[[1L]]$error$code, -32603)

    queues$`textDocument/completion`$pop()
    document$set_content(3L, "base::findInterval(1, c(0, 2))")
    task <- parse_task(server, fixture$uri, document, retry = FALSE)
    parsed <- parse_document(fixture$uri, document$content, include_xml = FALSE)
    task$.__enclos_env__$private$callback(parsed)
    expect_null(document$parse_failure)
    text_document_signature_help(server, 5L, readiness_params(fixture, 0L, 19L))
    expect_match(tail(server$replies, 1L)[[1L]]$result$signatures[[1L]]$label, "findInterval", fixed = TRUE)
})
