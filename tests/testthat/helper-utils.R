suppressPackageStartupMessages({
    library(magrittr)
    library(mockery)
    library(purrr)
    library(fs)
})

# a hack to make withr::defer_parent to work, see https://github.com/r-lib/withr/issues/123
defer <- withr::defer

expect_equivalent <- function(x, y) {
    expect_equal(x, y, ignore_attr = TRUE)
}

language_client <- function(working_dir = getwd(), diagnostics = FALSE, capabilities = NULL) {

    if (nzchar(Sys.getenv("R_LANGSVR_LOG"))) {
        script <- sprintf(
            "languageserver::run(debug = '%s')",
            normalizePath(Sys.getenv("R_LANGSVR_LOG"), "/", mustWork = FALSE))
    } else {
        script <- "languageserver::run()"
    }

    client <- LanguageClient$new(
        file.path(R.home("bin"), "R"), c("--no-echo", "-e", script))

    client$notification_handlers <- list(
        `textDocument/publishDiagnostics` = function(self, params) {
            uri <- params$uri
            diagnostics <- params$diagnostics
            self$diagnostics$set(uri, diagnostics)
        }
    )

    client$start(working_dir = working_dir, capabilities = capabilities)
    client$catch_callback_error <- FALSE
    # initialize request
    data <- client$fetch(blocking = TRUE)
    client$handle_raw(data)
    client %>% notify("initialized")
    client %>% notify(
        "workspace/didChangeConfiguration", list(settings = list(diagnostics = diagnostics)))
    withr::defer_parent({
        # it is sometimes necessary to shutdown the server probably
        # we skip this for other times for speed
        if (Sys.getenv("R_LANGSVR_TEST_FAST", "YES") == "NO") {
            client %>% respond("shutdown", NULL, retry = FALSE)
            client$process$wait(10 * 1000)  # 10 sec
            if (client$process$is_alive()) {
                cat("server did not shutdown peacefully\n")
                client$process$kill_tree()
            }
        } else {
            client$process$kill_tree()
        }
    })
    client
}


notify <- function(client, method, params = NULL) {
    client$deliver(Notification$new(method, params))
    invisible(client)
}


did_open <- function(client, path, uri = path_to_uri(path), text = NULL, languageId = NULL) {
    if (is.null(text)) {
        text <- stringi::stri_read_lines(path)
    }
    text <- paste0(text, collapse = "\n")

    if (is.null(languageId)) {
        languageId <- if (is_rmarkdown(uri)) "rmd" else "r"
    }

    notify(
        client,
        "textDocument/didOpen",
        list(
            textDocument = list(
                uri = uri,
                languageId = languageId,
                version = 1,
                text = text
            )
        )
    )
    invisible(client)
}


did_save <- function(client, path, uri = path_to_uri(path), text = NULL) {
    includeText <- tryCatch(
        client$ServerCapabilities$textDocumentSync$save$includeText,
        error = function(e) FALSE
    )
    if (includeText) {
        if (is.null(text)) {
            text <- stringi::stri_read_lines(path)
        }
        text <- paste0(text, collapse = "\n")
        params <- list(textDocument = list(uri = uri), text = text)
    } else {
        params <- list(textDocument = list(uri = uri))
    }
    notify(
        client,
        "textDocument/didSave",
        params)
    Sys.sleep(0.5)
    invisible(client)
}


respond <- function(client, method, params, timeout, allow_error = FALSE,
    retry = TRUE, retry_when = function(result) length(result) == 0) {
    if (missing(timeout)) {
        if (Sys.getenv("R_COVR", "") == "true") {
            # we give more time to covr
            timeout <- 30
        } else {
            timeout <- 10
        }
    }
    storage <- new.env(parent = .GlobalEnv)
    cb <- function(self, result, error = NULL) {
        if (is.null(error)) {
            storage$done <- TRUE
            storage$result <- result
        } else if (allow_error) {
            storage$done <- TRUE
            storage$result <- error
        }
    }

    start_time <- Sys.time()
    remaining <- timeout
    client$deliver(client$request(method, params), callback = cb)
    if (method == "shutdown") {
        # do not expect the server returns anything
        return(NULL)
    }
    while (!isTRUE(storage$done)) {
        if (remaining < 0) {
            fail("timeout when obtaining response")
            return(NULL)
        }
        data <- client$fetch(blocking = TRUE, timeout = remaining)
        if (!is.null(data)) client$handle_raw(data)
        remaining <- (start_time + timeout) - Sys.time()
    }
    result <- storage$result
    if (retry && retry_when(result)) {
        remaining <- (start_time + timeout) - Sys.time()
        if (remaining < 0) {
            fail("timeout when obtaining desired response")
            return(NULL)
        }
        Sys.sleep(0.2)
        return(Recall(client, method, params, remaining, allow_error, retry, retry_when))
    }
    return(result)
}


respond_completion <- function(client, path, pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/completion",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2])
        ),
        ...
    )
}

respond_completion_item_resolve <- function(client, params, ...) {
    respond(
        client,
        "completionItem/resolve",
        params,
        ...
    )
}

respond_signature <- function(client, path, pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/signatureHelp",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2])
        ),
        ...
    )
}

respond_hover <- function(client, path, pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/hover",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2])
        ),
        ...
    )
}

respond_definition <- function(client, path, pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/definition",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2])
        ),
        ...
    )
}

respond_references <- function(client, path, pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/references",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2])
        ),
        ...
    )
}

respond_rename <- function(client, path, pos, newName, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/rename",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2]),
            newName = newName
        ),
        ...
    )
}

respond_prepare_rename <- function(client, path, pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/prepareRename",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2])
        ),
        ...
    )
}


respond_formatting <- function(client, path, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/formatting",
        list(
            textDocument = list(uri = uri),
            options = list(tabSize = 4, insertSpaces = TRUE)
        ),
        ...
    )
}

respond_range_formatting <- function(client, path, start_pos, end_pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/rangeFormatting",
        list(
            textDocument = list(uri = uri),
            range = range(
                start = position(start_pos[1], start_pos[2]),
                end = position(end_pos[1], end_pos[2])
            ),
            options = list(tabSize = 4, insertSpaces = TRUE)
        ),
        ...
    )
}

respond_folding_range <- function(client, path, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/foldingRange",
        list(
            textDocument = list(uri = uri)),
        ...
    )
}

respond_selection_range <- function(client, path, positions, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/selectionRange",
        list(
            textDocument = list(uri = uri),
            positions = positions),
        ...
    )
}

respond_on_type_formatting <- function(client, path, pos, ch, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/onTypeFormatting",
        list(
            textDocument = list(uri = uri),
            position = position(pos[1], pos[2]),
            ch = ch,
            options = list(tabSize = 4, insertSpaces = TRUE)
        ),
        ...
    )
}


respond_document_highlight <- function(client, path, pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/documentHighlight",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2])
        ),
        ...
    )
}

respond_document_symbol <- function(client, path, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/documentSymbol",
        list(
            textDocument = list(uri = uri)
        ),
        ...
    )
}

respond_workspace_symbol <- function(client, query, ...) {
    respond(
        client,
        "workspace/symbol",
        list(
            query = query
        ),
        ...
    )
}

respond_document_link <- function(client, path, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/documentLink",
        list(
            textDocument = list(uri = uri)
        ),
        ...
    )
}

respond_document_link_resolve <- function(client, params, ...) {
    respond(
        client,
        "documentLink/resolve",
        params,
        ...
    )
}

respond_document_color <- function(client, path, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/documentColor",
        list(
            textDocument = list(uri = uri)
        ),
        ...
    )
}

respond_document_folding_range <- function(client, path, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/foldingRange",
        list(
            textDocument = list(uri = uri)
        ),
        ...
    )
}

respond_prepare_call_hierarchy <- function(client, path, pos, ..., uri = path_to_uri(path)) {
    respond(
        client,
        "textDocument/prepareCallHierarchy",
        list(
            textDocument = list(uri = uri),
            position = list(line = pos[1], character = pos[2])
        ),
        ...
    )
}

respond_call_hierarchy_incoming_calls <- function(client, item, ...) {
    respond(
        client,
        "callHierarchy/incomingCalls",
        list(
            item = item
        ),
        ...
    )
}

respond_call_hierarchy_outgoing_calls <- function(client, item, ...) {
    respond(
        client,
        "callHierarchy/outgoingCalls",
        list(
            item = item
        ),
        ...
    )
}

respond_code_action <- function(client, path, start_pos, end_pos, ..., uri = path_to_uri(path)) {
    diagnostics <- client$diagnostics$get(uri)
    range <- range(
        start = position(start_pos[1], start_pos[2]),
        end = position(end_pos[1], end_pos[2])
    )
    respond(
        client,
        "textDocument/codeAction",
        list(
            textDocument = list(uri = uri),
            range = range,
            context = list(
                diagnostics = Filter(function(item) {
                    range_overlap(item$range, range)
                }, diagnostics)
            )
        ),
        ...
    )
}

wait_for <- function(client, method, timeout = 30) {
    storage <- new.env(parent = .GlobalEnv)
    start_time <- Sys.time()
    remaining <- timeout

    original_handler <- client$notification_handlers[[method]]
    on.exit({
        client$notification_handlers[[method]] <- original_handler
    })
    client$notification_handlers[[method]] <- function(self, params) {
        storage$params <- params
        original_handler(self, params)
    }

    while (remaining > 0) {
        data <- client$fetch(blocking = TRUE, timeout = remaining)
        if (!is.null(data)) {
            client$handle_raw(data)
            if (hasName(storage, "params")) {
                return(storage$params)
            }
        }
        remaining <- (start_time + timeout) - Sys.time()
    }
    NULL
}
