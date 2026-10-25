# Separate from normal package tests: run with the pinned Air/Jarl CI binaries.
check_cli_backends <- function() {
    namespace <- loadNamespace("languageserver")
    namespace$lsp_settings$set("diagnostics_backend", "jarl")
    namespace$lsp_settings$set("formatting_backend", "air")
    stopifnot(nzchar(Sys.which("jarl")), nzchar(Sys.which("air")))
    root <- tempfile("backend-contract-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    nested <- file.path(root, "nested")
    dir.create(nested)
    writeLines(c("[lint]", 'select = ["any_is_na"]'), file.path(root, "jarl.toml"))
    writeLines(c("[format]", "indent-width = 4"), file.path(root, "air.toml"))
    uri <- namespace$path_to_uri(file.path(nested, "unsaved.R"))
    workspace <- list(root = root)
    document <- namespace$Document$new(uri, "r", 1L, c("any(is.na(x))", ""))
    invocation <- namespace$backend_request("diagnostics", uri, document, workspace)
    diagnostics <- namespace$run_backend(invocation)
    stopifnot(length(diagnostics) == 1L, diagnostics[[1L]]$code == "any_is_na",
        !file.exists(namespace$path_from_uri(uri)))
    writeLines(c("[lint]", 'ignore = ["any_is_na"]'), file.path(nested, "jarl.toml"))
    stopifnot(length(namespace$run_backend(invocation)) == 0L)
    document$set_content(2L, "x <- (")
    syntax <- namespace$run_backend(namespace$backend_request("diagnostics", uri, document, workspace))
    stopifnot(length(syntax) > 0, any(vapply(syntax, function(item) item$severity == 1L, logical(1L))))
    document$set_content(3L, c("f <- function() {", "\"\U0001f600\"", "}", ""))
    edits <- namespace$run_backend(namespace$backend_request("formatting", uri, document, workspace))
    stopifnot(length(edits) == 1L, grepl('    "\U0001f600"', edits[[1L]]$newText, fixed = TRUE))
    document$set_content(4L, strsplit(edits[[1L]]$newText, "\n", fixed = TRUE)[[1L]])
    # Preserve the output's terminal newline in the line-vector representation.
    document$set_content(4L, c(document$content, ""))
    stopifnot(length(namespace$run_backend(namespace$backend_request("formatting", uri, document, workspace))) == 0L)
    writeLines(c("[lint]", 'select = ["any_is_na"]'), file.path(nested, "jarl.toml"))
    client <- namespace$LanguageClient$new(file.path(R.home("bin"), "R"),
        c("--no-echo", "--vanilla", "-e", paste0(
            "options(languageserver.diagnostics_backend = 'jarl', ",
            "languageserver.formatting_backend = 'air'); languageserver::run()")))
    on.exit({
        if (client$process$is_alive()) client$process$kill_tree()
    }, add = TRUE)
    state <- new.env(parent = emptyenv())
    state$diagnostics <- NULL
    state$publications <- 0L
    client$notification_handlers[["textDocument/publishDiagnostics"]] <- function(self, params) {
        if (identical(params$uri, uri)) {
            state$diagnostics <- params$diagnostics
            state$publications <- state$publications + 1L
        }
    }
    wait <- function(done) {
        deadline <- Sys.time() + 20
        while (!done() && Sys.time() < deadline) {
            data <- client$fetch(blocking = TRUE, timeout = 1)
            if (!is.null(data)) client$handle_raw(data)
        }
        stopifnot(done())
    }
    request <- function(method, params) {
        state$response <- NULL
        client$deliver(client$request(method, params), callback = function(self, result, error = NULL) {
            state$response <- list(result = result, error = error)
        })
        wait(function() !is.null(state$response))
        stopifnot(is.null(state$response$error))
        state$response$result
    }
    client$start(working_dir = root)
    wait(function() !is.null(client$ServerCapabilities))
    client$deliver(namespace$Notification$new("initialized"))
    client$deliver(namespace$Notification$new("textDocument/didOpen", list(textDocument = list(
        uri = uri, languageId = "r", version = 1L, text = "f <- function() {\n1\n}\nany(is.na(x))\n"))))
    wait(function() length(state$diagnostics) == 1L)
    stopifnot(state$diagnostics[[1L]]$source == "jarl")
    edits <- request("textDocument/formatting", list(textDocument = list(uri = uri),
            options = list(tabSize = 2L, insertSpaces = TRUE)))
    stopifnot(length(edits) == 1L, grepl("    1", edits[[1L]]$newText, fixed = TRUE))
    writeLines(c("[lint]", 'ignore = ["any_is_na"]'), file.path(nested, "jarl.toml"))
    wait(function() state$publications >= 3L && length(state$diagnostics) == 0L)
    request("shutdown", NULL)
    client$process$wait(5000)
    cat("Pinned CLI contracts: stdin, configuration, Unicode, formatting and idle LSP refresh passed.\n")
}

check_cli_backends()
