text_content_server <- function(capabilities = list()) {
    server <- new.env(parent = emptyenv())
    server$ClientCapabilities <- capabilities
    server$requests <- list()
    server$replies <- list()
    server$request <- function(method, params) list(method = method, params = params)
    server$deliver <- function(value, callback = NULL) {
        if (inherits(value, "Response")) {
            server$replies[[length(server$replies) + 1L]] <- value
        } else {
            server$requests[[length(server$requests) + 1L]] <- value
        }
    }
    server
}

text_content_capabilities <- function() {
    list(workspace = list(textDocumentContent = list(dynamicRegistration = TRUE)))
}

text_content_definition_fixture <- function(content = "base::print") {
    fixture <- provider_fixture(content)
    namespace <- PackageNamespace$new("base")
    fixture$workspace$get_definition <- function(symbol, pkgname = NULL,
        exported_only = TRUE, uri = NULL, content_scheme = NULL) {
        namespace$get_definition(symbol, exported_only, content_scheme)
    }
    fixture$namespace <- namespace
    fixture
}

test_that("generated content is negotiated with a distinct scheme per server", {
    server <- text_content_server()
    caps <- configure_text_document_content(server, list(workspace = list()))
    expect_null(caps$workspace$textDocumentContent)
    expect_null(server$text_document_content)

    server$ClientCapabilities <- text_content_capabilities()
    caps <- configure_text_document_content(server, caps)
    scheme <- server$text_document_content$scheme
    expect_match(scheme, "^r-lsp-[0-9]+-[a-z0-9]+$")
    expect_identical(caps$workspace$textDocumentContent$schemes, list(scheme))
    second <- text_content_server(text_content_capabilities())
    configure_text_document_content(second, list(workspace = list()))
    expect_false(identical(scheme, second$text_document_content$scheme))

    disabled <- configure_text_document_content(server,
        list(workspace = list(textDocumentContent = FALSE)))
    expect_null(disabled$workspace$textDocumentContent)
    expect_null(server$text_document_content)
})

test_that("package definitions use cached virtual content without writing files", {
    fixture <- text_content_definition_fixture()
    testthat::local_mocked_bindings(
        stri_write_lines = function(...) stop("A capable client must not generate a file"),
        .package = "stringi")
    refreshed <- character()
    state <- TextDocumentContent$new(refresh = function(uri) refreshed <<- c(refreshed, uri))
    reply <- definition_reply(1L, fixture$uri, fixture$workspace,
        fixture$document, list(row = 0L, col = 8L), text_document_content = state)
    expect_identical(reply$result$uri, paste0(state$scheme, ":/base/print.R"))
    expect_null(attr(reply$result, "text_document_content"))
    text <- state$get(reply$result$uri)
    expect_match(text, "Generated from function body", fixed = TRUE)
    expect_match(text, "function", fixed = TRUE)
    expect_identical(reply$result$range$end,
        position(length(strsplit(text, "\n", fixed = TRUE)[[1L]]), 0L))
    again <- definition_reply(2L, fixture$uri, fixture$workspace,
        fixture$document, list(row = 0L, col = 8L), text_document_content = state)
    expect_identical(again$result$uri, reply$result$uri)
    expect_identical(state$size(), 1L)
    expect_identical(refreshed, reply$result$uri)
    # Replace only this fixture's body resolver, rather than mutate a loaded
    # package. A repeated definition must replace the snapshot at the same URI.
    unlockBinding("get_body", fixture$namespace)
    fixture$namespace$get_body <- function(...) "function(changed) changed"
    lockBinding("get_body", fixture$namespace)
    changed <- definition_reply(3L, fixture$uri, fixture$workspace,
        fixture$document, list(row = 0L, col = 8L), text_document_content = state)
    expect_identical(changed$result$uri, reply$result$uri)
    expect_match(state$get(changed$result$uri), "function(changed) changed", fixed = TRUE)
    expect_identical(refreshed, rep(reply$result$uri, 2L))
})

test_that("virtual definitions retain package and special-symbol identity", {
    state <- TextDocumentContent$new()
    base <- PackageNamespace$new("base")
    graphics <- PackageNamespace$new("graphics")
    first <- base$get_definition("plot", content_scheme = state$scheme)
    second <- graphics$get_definition("plot", content_scheme = state$scheme)
    expect_false(identical(first$uri, second$uri))
    expect_match(first$uri, ":/base/plot.R", fixed = TRUE)
    expect_match(second$uri, ":/graphics/plot.R", fixed = TRUE)
    operator <- base$get_definition("[<-", content_scheme = state$scheme)
    expect_match(operator$uri, ":/base/~5b3c2d.R", fixed = TRUE)
    expect_identical(text_document_content_segment("a/b"), "~612f62")
    expect_false(identical(text_document_content_segment("~612f62"), "~612f62"))
})

test_that("oversized virtual bodies retain the legacy file definition fallback", {
    fixture <- text_content_definition_fixture()
    state <- TextDocumentContent$new(max_bytes = 1)
    reply <- definition_reply(1L, fixture$uri, fixture$workspace,
        fixture$document, list(row = 0L, col = 8L), text_document_content = state)
    expect_identical(reply$result$uri, path_to_uri(file.path(tempdir(), "print.R")))
    expect_true(file.exists(path_from_uri(reply$result$uri)))
    expect_identical(state$size(), 0L)
    expect_null(attr(reply$result, "text_document_content"))
})

test_that("source and workspace definitions retain their real document URIs", {
    fixture <- provider_fixture(c("mine <- function() 1", "mine"))
    state <- TextDocumentContent$new()
    reply <- definition_reply(1L, fixture$uri, fixture$workspace,
        fixture$document, list(row = 1L, col = 2L), text_document_content = state)
    expect_identical(reply$result$uri, fixture$uri)
    expect_identical(state$size(), 0L)
})

test_that("content requests serve exact cached URIs and reject unknown documents", {
    server <- text_content_server(text_content_capabilities())
    configure_text_document_content(server, list(workspace = list()))
    state <- server$text_document_content
    uri <- paste0(state$scheme, ":/base/print.R")
    expect_true(state$set(uri, "function(x) x\n"))
    workspace_text_document_content(server, 1L, list(uri = uri))
    expect_identical(server$replies[[1L]]$result, list(text = "function(x) x\n"))
    wire <- jsonlite::fromJSON(server$replies[[1L]]$to_json(), simplifyVector = FALSE)
    expect_identical(wire$result$text, "function(x) x\n")
    for (invalid in list(list(), list(uri = 1L), 1L, list(uri = NA_character_))) {
        workspace_text_document_content(server, 2L, invalid)
        expect_identical(tail(server$replies, 1L)[[1L]]$error$code, ErrorCodes$InvalidParams)
    }
    for (foreign in c("file:///etc/passwd", paste0(uri, "?other"),
            paste0(state$scheme, ":/base/not-issued.R"), "r-lsp-other:/base/print.R")) {
        workspace_text_document_content(server, 3L, list(uri = foreign))
        expect_identical(tail(server$replies, 1L)[[1L]]$error$code, ErrorCodes$RequestFailed)
    }
})

test_that("cached content updates refresh one URI and eviction stays bounded", {
    changed <- character()
    state <- TextDocumentContent$new(max_bytes = 1024, max_entries = 2L,
        refresh = function(uri) changed <<- c(changed, uri))
    uris <- paste0(state$scheme, ":/base/", c("one", "two", "three"), ".R")
    expect_true(state$set(uris[[1L]], "old"))
    expect_true(state$set(uris[[1L]], "old"))
    expect_length(changed, 1L)
    expect_true(state$set(uris[[1L]], "new"))
    expect_identical(changed, rep(uris[[1L]], 2L))
    state$set(uris[[2L]], "two")
    state$set(uris[[3L]], "three")
    expect_identical(state$size(), 2L)
    expect_lte(state$bytes(), 1024)
    expect_null(state$get(uris[[1L]]))
    expect_identical(state$get(uris[[2L]]), "two")
    state$clear()
    expect_identical(state$size(), 0L)
    expect_identical(state$bytes(), 0)
})

test_that("LSP package definitions can be fetched as read-only document content", {
    skip_on_cran()
    client <- language_client(capabilities = text_content_capabilities())
    scheme <- client$ServerCapabilities$workspace$textDocumentContent$schemes[[1L]]
    expect_match(scheme, "^r-lsp-")
    client$request_handlers[["workspace/textDocumentContent/refresh"]] <- function(self, id, params) {
        self$deliver(Response$new(id, NULL))
    }
    file <- withr::local_tempfile(fileext = ".R")
    writeLines("base::print", file)
    client %>% did_open(file)
    definition <- client %>% respond_definition(file, c(0L, 8L))
    expect_identical(definition$uri, paste0(scheme, ":/base/print.R"))
    result <- respond(client, "workspace/textDocumentContent", list(uri = definition$uri))
    expect_match(result$text, "Generated from function body", fixed = TRUE)
    expect_match(result$text, "function", fixed = TRUE)
    error <- respond(client, "workspace/textDocumentContent",
        list(uri = "file:///not-generated.R"), allow_error = TRUE, retry = FALSE)
    expect_equal(error$code, ErrorCodes$RequestFailed)
})
