symbol_edit_fixture <- function() {
    uri <- "file:///symbol-edits.R"
    document <- Document$new(uri, version = 1L,
        content = c("# Before ----", "f <- function(x) x + 1"))
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, document$content, include_xml = FALSE)
    parsed$version <- document$version
    workspace$update_parse_data(uri, parsed)
    server <- new.env(parent = baseenv())
    server$get_workspace <- function(...) workspace
    server$ClientCapabilities <- list(textDocument = list(
        documentSymbol = list(hierarchicalDocumentSymbolSupport = TRUE)))
    server$pending_replies <- collections::dict()
    server$pending_replies$set(uri, list(`textDocument/documentSymbol` = collections::queue()))
    server$request_handlers <- list(`textDocument/documentSymbol` = text_document_document_symbol)
    server$resolve_task_manager <- list(add_task = function(...) NULL)
    server$refresh_index_documents <- function(...) NULL
    server$promoted <- character()
    server$parse_task_manager <- list(promote = function(uri) server$promoted <- c(server$promoted, uri))
    server$replies <- list()
    server$deliver <- function(reply) server$replies[[length(server$replies) + 1L]] <- reply
    list(server = server, document = document, workspace = workspace, uri = uri,
        params = list(textDocument = list(uri = uri)))
}

test_that("outline requests promote an edited document's parse and settle once", {
    fixture <- symbol_edit_fixture()
    document <- fixture$document
    server <- fixture$server
    document$apply_content_changes(2L, list(list(
        range = range(position(1L, 22L), position(1L, 22L)),
        text = "\n## Testing ----\ng <- function(x) x - 1"
    )))
    text_document_document_symbol(server, 1L, fixture$params)
    text_document_document_symbol(server, 2L, fixture$params)
    expect_equal(server$promoted, rep(fixture$uri, 2L))
    expect_length(server$replies, 0L)

    parse_callback(server, fixture$uri, 2L,
        parse_document(fixture$uri, document$content, include_xml = FALSE), document$content)
    expect_length(server$replies, 2L)
    expect_equal(server$replies[[1L]]$id, 1L)
    expect_equal(server$replies[[2L]]$id, 2L)
    expect_identical(server$replies[[1L]]$result, server$replies[[2L]]$result)
    expect_setequal(map_chr(flatten_document_symbols(server$replies[[2L]]$result), "name"),
        c("Before", "Testing", "f", "g"))
    expect_equal(server$pending_replies$get(fixture$uri)$`textDocument/documentSymbol`$size(), 0L)
})

test_that("outline requests after a failed worker return an error and recover on the next edit", {
    fixture <- symbol_edit_fixture()
    server <- fixture$server
    document <- fixture$document
    document$set_content(2L, c(document$content, "# Testing ----"))
    text_document_document_symbol(server, 1L, fixture$params)
    task <- parse_task(server, fixture$uri, document, retry = FALSE)
    task$.__enclos_env__$private$error(simpleError("worker exited"))
    expect_length(server$replies, 1L)
    expect_equal(server$replies[[1L]]$error$code, ErrorCodes$InternalError)
    text_document_document_symbol(server, 2L, fixture$params)
    expect_length(server$replies, 2L)
    expect_match(server$replies[[2L]]$error$message, "worker exited", fixed = TRUE)
    expect_equal(server$pending_replies$get(fixture$uri)$`textDocument/documentSymbol`$size(), 0L)

    document$set_content(3L, c(document$content, "g <- function(x) x - 1"))
    text_document_document_symbol(server, 3L, fixture$params)
    parse_callback(server, fixture$uri, 3L,
        parse_document(fixture$uri, document$content, include_xml = FALSE), document$content)
    expect_length(server$replies, 3L)
    expect_null(server$replies[[3L]]$error)
    expect_setequal(map_chr(flatten_document_symbols(server$replies[[3L]]$result), "name"),
        c("Before", "Testing", "f", "g"))
})

test_that("the outline updates after incremental section edits without reopening", {
    skip_on_cran()
    root <- withr::local_tempdir()
    path <- file.path(root, "outline.R")
    client <- language_client(working_dir = root, capabilities = list(textDocument = list(
        documentSymbol = list(hierarchicalDocumentSymbolSupport = TRUE))))
    client %>% did_open(path, text = c("# Before ----", "f <- function(x) x + 1"))
    initial <- client %>% respond_document_symbol(path, retry = FALSE)
    expect_setequal(map_chr(flatten_document_symbols(initial), "name"), c("Before", "f"))

    change <- function(version, edits) {
        notify(client, "textDocument/didChange", list(
            textDocument = list(uri = path_to_uri(path), version = version),
            contentChanges = edits))
        respond_document_symbol(client, path, retry = FALSE)
    }
    added <- change(2L, list(list(
        range = range(position(1L, 22L), position(1L, 22L)),
        text = "\n## Testing ----")))
    expect_setequal(map_chr(flatten_document_symbols(added), "name"), c("Before", "f", "Testing"))
    renamed <- change(3L, list(list(
        range = range(position(2L, 3L), position(2L, 10L)), text = "Renamed")))
    expect_setequal(map_chr(flatten_document_symbols(renamed), "name"), c("Before", "f", "Renamed"))
    invalid <- change(4L, list(list(
        range = range(position(2L, 15L), position(2L, 15L)), text = "\ng <- function(")))
    expect_setequal(map_chr(flatten_document_symbols(invalid), "name"), c("Before", "Renamed"))
    repaired <- change(5L, list(list(
        range = range(position(3L, 14L), position(3L, 14L)), text = "x) x - 1")))
    expect_setequal(map_chr(flatten_document_symbols(repaired), "name"), c("Before", "f", "Renamed", "g"))
    expect_equal(map_chr(repaired, "name"), "Before")
})
