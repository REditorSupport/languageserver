# Run once per fresh server, using the ggplot2 4.0.4 source archive.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1L]], mustWork = TRUE)
manifest <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Package", "Version"))
stopifnot(manifest[1L, "Package"] == "ggplot2", manifest[1L, "Version"] == "4.0.4")

first_references <- function(root) {
    ns <- asNamespace("languageserver")
    path <- file.path(root, "R", "properties.R")
    uri <- ns$path_to_uri(path)
    peer <- ns$path_to_uri(file.path(root, "R", "theme-elements.R"))
    text <- paste(c(readLines(path, encoding = "UTF-8"), ""), collapse = "\n")
    cache <- tempfile("references-cache-")
    Sys.setenv(R_USER_CACHE_DIR = cache, XDG_CACHE_HOME = cache)
    setwd(root)
    client <- ns$LanguageClient$new(file.path(R.home("bin"), "Rscript"), c("--vanilla", "-e",
            "options(languageserver.index_persistent_cache = FALSE); languageserver::run()"))
    on.exit(if (client$process$is_alive()) client$process$kill_tree())
    client$catch_callback_error <- FALSE
    wait <- function(done) {
        deadline <- proc.time()[["elapsed"]] + 120
        while (!done()) {
            remaining <- deadline - proc.time()[["elapsed"]]
            if (remaining <= 0) stop("Timed out waiting for a language server reply")
            data <- client$fetch(blocking = TRUE, timeout = remaining)
            if (!is.null(data)) client$handle_raw(data)
        }
    }
    request <- function(method, params = NULL) {
        reply <- NULL
        client$deliver(client$request(method, params), callback = function(self, result, error = NULL) {
            reply <<- list(result = result, error = error)
        })
        wait(function() !is.null(reply))
        if (!is.null(reply$error)) stop(method, " failed: ", reply$error$message)
        reply$result
    }
    client$start(working_dir = root)
    wait(function() !is.null(client$ServerCapabilities))
    client$deliver(ns$Notification$new("initialized"))
    item <- list(uri = uri, languageId = "r", version = 1L, text = text)
    client$deliver(ns$Notification$new("textDocument/didOpen", list(textDocument = item)))

    params <- list(textDocument = list(uri = uri), position = list(line = 2L, character = 16L),
        context = list(includeDeclaration = TRUE))
    started <- proc.time()[["elapsed"]]
    result <- request("textDocument/references", params)
    elapsed <- proc.time()[["elapsed"]] - started
    ranges <- vapply(result, function(x) {
        from <- x$range$start
        to <- x$range$end
        sprintf("%s %d:%d-%d:%d", x$uri, from$line, from$character, to$line, to$character)
    }, "")
    expected <- paste(c(uri, uri, uri, peer), c("0:29-0:39", "2:15-2:25", "4:9-4:19", "123:32-123:42"))
    if (!identical(sort(ranges), sort(expected))) stop("Unexpected reference locations:\n", paste(ranges, collapse = "\n"))
    request("shutdown")
    client$deliver(ns$Notification$new("exit"))
    client$process$wait(10000L)
    stopifnot(identical(client$process$get_exit_status(), 0L))
    data.frame(locations = length(result), elapsed_seconds = elapsed)
}

print(first_references(root))
