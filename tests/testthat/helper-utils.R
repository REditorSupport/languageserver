suppressPackageStartupMessages({
    library(magrittr)
    library(purrr)
    library(fs)
})

# a hack to make withr::defer_parent to work, see https://github.com/r-lib/withr/issues/123
defer <- withr::defer

language_client <- function(working_dir = getwd(), debug = FALSE) {
    client <- LanguageClient$new(
        file.path(R.home("bin"), "R"), c("--slave", "-e", "languageserver::run(debug=TRUE)"))

    client$notification_handlers <- list(
        `textDocument/publishDiagnostics` = function(...) {}
    )

    client$start(working_dir = working_dir)
    client$catch_callback_error <- FALSE
    # initialize request
    data <- client$fetch(blocking = TRUE)
    client$handle_raw(data)
    client %>% notify("initialized")
    withr::defer_parent(client$stop())
    client
}


notify <- function(client, method, params = NULL) {
    client$deliver(Notification$new(method, params))
    invisible(client)
}


did_save <- function(client, path) {
    notify(
        client,
        "textDocument/didSave",
        list(textDocument = list(uri = path_to_uri(path))))
    invisible(client)
}


respond <- function(client, method, params, timeout=5, retry=TRUE,
                            retry_when = function(result) length(result) == 0) {
    storage <- new.env()
    cb <- function(self, result) {
        storage$result <- result
    }

    start_time <- Sys.time()
    remaining <- timeout
    client$deliver(client$request(method, params), callback = cb)
    while (is.null(storage$result)) {
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
        return(Recall(client, method, params, remaining, retry, retry_when))
    }
    return(result)
}


respond_completion <- function(client, path, pos, ...) {
    respond(
        client,
        "textDocument/completion",
        list(
            textDocument = list(uri = path_to_uri(path)),
            position = list(line = pos[1], character = pos[2])),
        ...
    )
}

respond_signature <- function(client, path, pos, ...) {
    respond(
        client,
        "textDocument/signatureHelp",
        list(
            textDocument = list(uri = path_to_uri(path)),
            position = list(line = pos[1], character = pos[2])),
        ...
    )
}

respond_hover <- function(client, path, pos, ...) {
    respond(
        client,
        "textDocument/hover",
        list(
            textDocument = list(uri = path_to_uri(path)),
            position = list(line = pos[1], character = pos[2])),
        ...
    )
}

respond_definition <- function(client, path, pos, ...) {
    respond(
        client,
        "textDocument/definition",
        list(
            textDocument = list(uri = path_to_uri(path)),
            position = list(line = pos[1], character = pos[2])),
        ...
    )
}


respond_formatting <- function(client, path, ...) {
    respond(
        client,
        "textDocument/formatting",
        list(
            textDocument = list(uri = path_to_uri(path)),
            options = list(tabSize = 4, insertSpaces = TRUE)),
        ...
    )
}

respond_range_formatting <- function(client, path, start_pos, end_pos, ...) {
    respond(
        client,
        "textDocument/rangeFormatting",
        list(
            textDocument = list(uri = path_to_uri(path)),
            range = range(position(start_pos[1], start_pos[2]), position(end_pos[1], end_pos[2])),
            options = list(tabSize = 4, insertSpaces = TRUE)),
        ...
    )
}
