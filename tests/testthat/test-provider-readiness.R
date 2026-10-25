readiness_catalogs <- local({
    cached <- NULL
    function() {
        if (is.null(cached)) cached <<- stats::setNames(lapply(startup_packages, s3_prepare_namespace), startup_packages)
        cached
    }
})

readiness_fixture <- function(content) {
    root <- withr::local_tempdir(.local_envir = parent.frame())
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
    document$requested_packages <- list(packages = "pending", namespace_packages = "base")
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
