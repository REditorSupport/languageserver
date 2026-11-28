embedded_client <- function(folders = NULL) {
    receive <- run_embedded()
    send <- function(method, params = list(), id = NULL) {
        packet <- list(jsonrpc = "2.0", method = method, params = params)
        if (!is.null(id)) packet$id <- id
        result <- receive(as.character(jsonlite::toJSON(packet, auto_unbox = TRUE, null = "null")))
        if (length(result)) jsonlite::fromJSON(result[[1L]], simplifyVector = FALSE)
    }
    init <- send("initialize", list(workspaceFolders = folders, capabilities = list(
        textDocument = list(documentSymbol = list(hierarchicalDocumentSymbolSupport = TRUE)))), 1L)
    list(send = send, capabilities = init$result$capabilities,
        open = function(uri, text, version = 1L, language = "r") {
            send("textDocument/didOpen", list(textDocument = list(
                uri = uri, text = text, languageId = language, version = version)))
        },
        ask = function(method, uri, line = 0L, col = 0L, ...) {
            send(paste0("textDocument/", method), c(list(textDocument = list(uri = uri),
                position = list(line = line, character = col)), list(...)), "rq")
        },
        change = function(uri, version, changes) {
            send("textDocument/didChange", list(textDocument = list(uri = uri, version = version),
                contentChanges = changes))
        },
        close = function(uri) send("textDocument/didClose", list(textDocument = list(uri = uri))))
}

test_that("embedded requests use current open editor text and retain original URIs", {
    local_mocked_bindings(workspace_startup_packages = function() stop("native package probe"),
        workspace_startup_members = function() stop("native member probe"),
        workspace_startup_s3 = function() stop("native S3 probe"))
    e <- embedded_client()
    a <- "vscode-vfs://github/owner/project/helpers.R"
    b <- "untitled:Untitled-2"
    e$open(a, "double_it <- function(x) x * 2\nstop('do not execute my script')")
    e$open(b, "double_it(9)\ndoub")
    def <- e$ask("definition", b, 0, 3)
    expect_identical(def$id, "rq")
    expect_identical(def$result$uri, a)
    expect_identical(def$result$range$start$line, 0L)
    expect_match(e$ask("hover", b, 0, 3)$result$contents, "double_it")
    items <- e$ask("completion", b, 1, 4)$result$items
    expect_true("double_it" %in% vapply(items, "[[", character(1), "label"))
    expect_match(e$ask("signatureHelp", b, 0, 10)$result$signatures[[1L]]$label, "double_it")
    expect_identical(e$ask("documentSymbol", a)$result[[1L]]$name, "double_it")
    expect_true(length(e$ask("semanticTokens/full", b)$result$data) > 0L)
    refs <- e$ask("references", b, 0, 2, context = list(includeDeclaration = TRUE))$result
    expect_setequal(vapply(refs, "[[", character(1), "uri"), c(a, b))
    edits <- e$ask("rename", b, 0, 2, newName = "twice")$result
    expect_setequal(names(edits$changes), c(a, b))
    e$close(a)
    expect_null(e$ask("definition", b, 0, 3)$result)
})

test_that("changes, including UTF-16 ranges, become visible across buffers", {
    e <- embedded_client()
    a <- "untitled:declaration"
    b <- "untitled:caller"
    e$open(a, paste0('"', "\U0001f600", '"; twice <- function(x) 2 * x'))
    e$open(b, "twice(2)")
    expect_identical(e$ask("definition", b, 0, 1)$result$range$start$character, 6L)
    edit <- list(range = list(start = list(line = 0L, character = 6L),
        end = list(line = 0L, character = 11L)), text = "thrice")
    e$change(a, 2L, list(edit))
    e$change(b, 3L, list(list(text = "thrice(9)")))
    e$change(b, 2L, list(list(text = "stale(9)")))
    expect_identical(e$ask("definition", b, 0, 2)$result$uri, a)
    e$send("textDocument/didSave", list(textDocument = list(uri = a), text = "saved <- 1"))
    expect_identical(e$ask("documentSymbol", a)$result[[1L]]$name, "saved")
    e$close(a)
    e$open(a, "reopened <- 2")
    expect_identical(e$ask("documentSymbol", a)$result[[1L]]$name, "reopened")
})

test_that("embedded instances and workspace folders have independent open documents", {
    folder <- function(uri) list(uri = uri, name = uri)
    outer <- "vscode-vfs://example/repo"
    inner <- paste0(outer, "/sub")
    e <- embedded_client(list(folder(outer), folder(inner)))
    d <- paste0(outer, "/a.R")
    e$open(d, "outer_func <- function(x) x")
    b <- paste0(inner, "/b.R")
    e$open(b, "outer_func(3)")
    expect_null(e$ask("definition", b, 0, 2)$result)
    e$open(paste0(inner, "/d.R"), "outer_func <- function(y) y")
    expect_identical(e$ask("definition", b, 0, 2)$result$uri, paste0(inner, "/d.R"))
    independent <- embedded_client()
    expect_null(independent$ask("documentSymbol", d)$result)
    expect_null(e$send("shutdown", id = 9L)$result)
    expect_identical(e$ask("documentSymbol", d)$error$code, -32600L)
    e$send("exit")
    expect_null(e$ask("documentSymbol", d))
})

test_that("embedded capabilities cover installed packages and optional formatting", {
    withr::local_options(styler.cache_name = NULL)
    e <- embedded_client()
    expect_true(e$capabilities$definitionProvider)
    expect_null(e$capabilities$workspaceSymbolProvider)
    expect_identical(e$capabilities$documentFormattingProvider, requireNamespace("styler", quietly = TRUE))
    d <- "untitled:packages"
    e$open(d, "stats::lm(mpg ~ wt, data = mtcars)\nstats::l")
    items <- e$ask("completion", d, 1, 8)$result$items
    expect_true("lm" %in% vapply(items, "[[", character(1), "label"))
    sig <- e$ask("signatureHelp", d, 0, 15)$result
    expect_match(sig$signatures[[1L]]$label, "lm\\(formula")
    if (isTRUE(e$capabilities$documentFormattingProvider)) {
        e$open(d, "x<-1+2")
        edits <- e$ask("formatting", d, options = list(tabSize = 2L, insertSpaces = TRUE))$result
        expect_match(edits[[1L]]$newText, "x <- 1 \\+ 2")
    }
    expect_identical(e$ask("unsupported", d)$error$code, -32601L)
})

test_that("embedded requests only return package locations the client can open", {
    uri <- "untitled:pkg"
    plain <- embedded_client()
    plain$open(uri, "stats::lm(1)")
    expect_null(plain$ask("definition", uri, 0, 8)$result)

    receive <- run_embedded()
    send <- function(method, params = list(), id = 1L) {
        msg <- list(jsonrpc = "2.0", method = method, params = params)
        if (!is.null(id)) msg$id <- id
        reply <- receive(as.character(jsonlite::toJSON(msg, auto_unbox = TRUE)))
        if (length(reply)) jsonlite::fromJSON(reply, simplifyVector = FALSE)
    }
    cap <- send("initialize", list(capabilities = list(workspace = list(textDocumentContent = list()))))
    scheme <- cap$result$capabilities$workspace$textDocumentContent$schemes[[1L]]
    send("textDocument/didOpen", list(textDocument = list(uri = uri, languageId = "r",
        version = 1L, text = "stats::lm(1)")), id = NULL)
    loc <- send("textDocument/definition", list(textDocument = list(uri = uri),
        position = list(line = 0L, character = 8L)))$result
    expect_true(startsWith(loc$uri, paste0(scheme, ":/")))
    expect_match(send("workspace/textDocumentContent", list(uri = loc$uri))$result$text, "function")
    expect_identical(send("workspace/textDocumentContent", list(uri = "file:///x"))$error$code, -32803L)
})

test_that("embedded transport rejects malformed requests and coalesces edits", {
    receive <- run_embedded()
    result <- function(msg) jsonlite::fromJSON(receive(msg), simplifyVector = FALSE)
    expect_identical(result("{")$error$code, -32700L)
    expect_identical(result("1")$error$code, -32600L)
    expect_identical(result('{"id":"a","jsonrpc":"2.0","method":"shutdown"}')$error$code, -32002L)
    expect_identical(receive('{"jsonrpc":"2.0","id":1,"result":null}'), character())
    count <- 0L
    parse <- parse_document
    local_mocked_bindings(parse_document = function(...) { count <<- count + 1L; parse(...) })
    e <- embedded_client()
    u <- "untitled:coalesce"
    e$open(u, "first <- 0")
    e$change(u, 2L, list(list(text = "second <- 0")))
    e$change(u, 3L, list(list(text = "final <- 0")))
    expect_identical(count, 0L)
    expect_identical(e$ask("documentSymbol", u)$result[[1L]]$name, "final")
    expect_identical(count, 1L)
    e$ask("documentSymbol", u)
    expect_identical(count, 1L)
})

test_that("pull diagnostics use open text, UTF-16 positions and current settings", {
    e <- embedded_client()
    u <- "untitled:errors"
    e$open(u, paste0('"', "\U0001f600", '";\t)'))
    result <- e$ask("diagnostic", u)$result
    expect_identical(result$kind, "full")
    fault <- result$items[[1L]]
    expect_identical(fault$severity, 1L)
    expect_identical(fault$range$start$character, 6L)
    expect_match(fault$message, "unexpected")
    expect_identical(e$ask("diagnostic", u, previousResultId = result$resultId)$result$kind, "unchanged")
    e$change(u, 2L, list(list(text = "y <- 1")))
    clean <- e$ask("diagnostic", u, previousResultId = result$resultId)$result
    expect_identical(clean$kind, "full")
    expect_length(clean$items, 0L)
    e$change(u, 3L, list(list(text = "x <-")))
    eof <- e$ask("diagnostic", u)$result$items[[1L]]
    expect_identical(eof$range$start, list(line = 0L, character = 4L))
    e$send("workspace/didChangeConfiguration", list(settings = list(r = list(lsp = list(diagnostics = FALSE)))))
    expect_length(e$ask("diagnostic", u)$result$items, 0L)
})

test_that("embedded lintr uses only the selected non-evaluating rule", {
    skip_if_not_installed("lintr")
    dir <- withr::local_tempdir()
    withr::local_dir(dir)
    writeLines('linters: stop("must not load project config")', ".lintr")
    e <- embedded_client()
    u <- "untitled:style"
    e$open(u, 'stop("must not run the document")\nx<-1+2')
    lints <- e$ask("diagnostic", u)$result$items
    expect_length(lints, 2L)
    expect_true(all(vapply(lints, function(x) x$source == "lintr" && x$range$start$line == 1L, logical(1))))
})
