suppressPackageStartupMessages({
    library(magrittr)
    library(purrr)
    library(fs)
})

# a hack to make withr::defer_parent to work, see https://github.com/r-lib/withr/issues/123
defer <- withr::defer

language_client <- function(working_dir = getwd(), debug = FALSE) {
    client <- LanguageClient$new(
        file.path(R.home("bin"), "R"), c("--slave", "-e", "languageserver::run()"))

    client$notification_handlers <- list(
        `textDocument/publishDiagnostics` = function(self, params) {
            uri <- params$uri
            diagnostics <- params$diagnostics
            self$diagnostics$set(uri, diagnostics)
        }
    )

    client$start(working_dir = working_dir)
    client$catch_callback_error <- FALSE
    # initialize request
    data <- client$fetch(blocking = TRUE)
    client$handle_raw(data)
    client %>% notify("initialized")
    withr::defer_parent({
        if (Sys.getenv("R_COVR", "") == "true") {
            # it is necessary to shutdown the server in covr
            # we skip this for other times for speed
            client %>% respond("shutdown", NULL, retry = FALSE)
            client$process$wait()
        } else {
            client$stop()
        }
    })
    client
}


notify <- function(client, method, params = NULL) {
    client$deliver(Notification$new(method, params))
    invisible(client)
}


did_open <- function(client, path) {
    text <- paste0(readr::read_lines(path), collapse = "\n")
    notify(
        client,
        "textDocument/didOpen",
        list(
            textDocument = list(
                uri = path_to_uri(path),
                languageId = "R",
                version = 1,
                text = text
            )
        )
    )
    invisible(client)
}


did_save <- function(client, path) {
    notify(
        client,
        "textDocument/didSave",
        list(textDocument = list(uri = path_to_uri(path))))
    invisible(client)
}


respond <- function(client, method, params, timeout, retry=TRUE,
                            retry_when = function(result) length(result) == 0) {
    if (missing(timeout)) {
        if (Sys.getenv("R_COVR", "") == "true") {
            # we give more time to covr
            timeout <- 30
        } else {
            timeout <- 10
        }
    }
    storage <- new.env()
    cb <- function(self, result) {
        storage$done <- TRUE
        storage$result <- result
    }

    start_time <- Sys.time()
    remaining <- timeout
    client$deliver(client$request(method, params), callback = cb)
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


wait_for <- function(client, method, timeout = 5) {
    storage <- new.env()
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
