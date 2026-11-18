# Run after a regular installation to locate dependencies. Install the source
# into a fresh library with mandatory dependencies only, then start an LSP client.
minimal_install_dependencies <- function(installed, package = "languageserver") {
    runtime <- tools::package_dependencies(package, db = installed,
        which = c("Depends", "Imports"), recursive = TRUE)[[1L]]
    # Header packages are required to build this source package, but not to load
    # already-installed dependencies. Their own runtime dependencies still apply.
    headers <- tools::package_dependencies(package, db = installed,
        which = "LinkingTo", recursive = FALSE)[[1L]]
    header_runtime <- if (length(headers)) unlist(tools::package_dependencies(headers, db = installed,
            which = c("Depends", "Imports"), recursive = TRUE), use.names = FALSE) else character()
    unique(c(runtime, headers, header_runtime))
}

check_minimal_install <- function(source_path = ".") {
    stopifnot(!"languageserver" %in% loadedNamespaces())
    source_path <- normalizePath(source_path, mustWork = TRUE)
    installed <- installed.packages()
    # Use this checkout's requirements even when the installed server predates
    # a dependency change. Dependencies themselves are copied from that library.
    fields <- c("Depends", "Imports", "LinkingTo")
    description <- read.dcf(file.path(source_path, "DESCRIPTION"), fields = fields)
    for (field in intersect(fields, colnames(installed))) {
        installed["languageserver", field] <- description[[1L, field]]
    }
    dependencies <- minimal_install_dependencies(installed)
    optional <- c("lintr", "styler", "roxygen2", "pkgload")
    stopifnot(!any(optional %in% dependencies))
    library <- tempfile("languageserver-minimal-")
    dir.create(library)
    on.exit(unlink(library, recursive = TRUE), add = TRUE)
    for (package in unique(dependencies)) {
        if (package %in% c("R", "languageserver")) next
        source <- find.package(package)
        target <- file.path(library, package)
        if (.Platform$OS.type == "windows") {
            stopifnot(file.copy(source, library, recursive = TRUE))
        } else {
            stopifnot(file.symlink(source, target))
        }
    }
    old_library <- .libPaths()
    on.exit(.libPaths(old_library), add = TRUE)
    .libPaths(c(library, .Library), include.site = FALSE)
    stopifnot(all(vapply(optional, function(package) {
        !length(find.package(package, quiet = TRUE)) && !requireNamespace(package, quietly = TRUE)
    }, logical(1L))))
    variables <- c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE", "R_PROFILE", "R_PROFILE_USER",
        "R_ENVIRON", "R_ENVIRON_USER")
    previous_environment <- Sys.getenv(variables, unset = NA_character_)
    on.exit({
        for (variable in variables) {
            value <- previous_environment[[variable]]
            if (is.na(value)) Sys.unsetenv(variable)
            else do.call(Sys.setenv, stats::setNames(list(value), variable))
        }
    }, add = TRUE)
    profile <- nullfile()
    # Installers, servers and workers must all inherit the same isolated library.
    Sys.setenv(R_LIBS = library, R_LIBS_USER = library, R_LIBS_SITE = "",
        R_PROFILE = profile, R_PROFILE_USER = profile, R_ENVIRON = profile, R_ENVIRON_USER = profile)
    processx::run(file.path(R.home("bin"), "R"),
        c("CMD", "INSTALL", "--no-multiarch", "-l", library, source_path),
        stdout = "|", stderr = "|", error_on_status = TRUE)
    namespace <- loadNamespace("languageserver")
    stopifnot(identical(normalizePath(dirname(getNamespaceInfo(namespace, "path"))), normalizePath(library)))
    stopifnot(is.null(namespace$roxygen2_namespace()))
    comments <- c("#' Local documentation marker", "#' @param x The input marker")
    fallback <- "Local documentation marker  \n@param x The input marker"
    owner <- new.env(parent = emptyenv())
    stopifnot(identical(namespace$analysis_documentation(owner, comments), fallback))
    cache <- owner$analysis_caches[["source-documentation"]]$values
    cached <- namespace$analysis_cache_get(cache, digest::digest(comments, algo = "xxhash64"))
    stopifnot(cached$hit, identical(cached$value, fallback),
        identical(namespace$analysis_documentation(owner, comments), fallback),
        is.null(namespace$analysis_documentation(owner, character())))
    server <- paste0("stopifnot(!requireNamespace('roxygen2', quietly = TRUE), ",
        "!requireNamespace('pkgload', quietly = TRUE)); languageserver::run()")
    client <- namespace$LanguageClient$new(file.path(R.home("bin"), "R"),
        c("--no-echo", "--vanilla", "-e", server))
    on.exit({
        if (client$process$is_alive()) client$process$kill_tree()
    }, add = TRUE)
    wait <- function(done) {
        deadline <- Sys.time() + 20
        while (!done() && Sys.time() < deadline) {
            data <- client$fetch(blocking = TRUE, timeout = 1)
            if (!is.null(data)) client$handle_raw(data)
        }
        stopifnot(done())
    }
    request <- function(method, params) {
        response <- NULL
        client$deliver(client$request(method, params), callback = function(self, result, error = NULL) {
            response <<- list(result = result, error = error)
        })
        wait(function() !is.null(response))
        response
    }
    client$start(working_dir = tempdir())
    wait(function() !is.null(client$ServerCapabilities))
    client$deliver(namespace$Notification$new("initialized"))
    client$deliver(namespace$Notification$new("workspace/didChangeConfiguration",
            list(settings = list(diagnostics = FALSE))))
    uri <- namespace$path_to_uri(file.path(tempdir(), "minimal-buffer.R"))
    content <- c(comments, "hello <- function(x, y = 2) x + y",
        "plain <- function(value = 1) value", "hello(x = 1)", "plain()",
        "fs::path_abs('.')", "hel", "pla")
    client$deliver(namespace$Notification$new("textDocument/didOpen", list(textDocument = list(
        uri = uri, languageId = "r", version = 1L,
        text = paste(content, collapse = "\n")))))
    symbols <- request("textDocument/documentSymbol", list(textDocument = list(uri = uri)))
    stopifnot(is.null(symbols$error), any(vapply(symbols$result,
                function(item) identical(item$name, "hello"), logical(1L))))
    text_request <- function(method, line, character) {
        result <- request(method, list(textDocument = list(uri = uri),
                position = list(line = line, character = character)))
        stopifnot(is.null(result$error))
        result$result
    }
    for (case in list(list(name = "hello", line = 7L, call = 4L, character = 6L,
                signature = "hello(x, y = 2)"),
            list(name = "plain", line = 8L, call = 5L, character = 6L,
                signature = "plain(value = 1)"))) {
        completion <- text_request("textDocument/completion", case$line, 3L)
        items <- Filter(function(item) identical(item$label, case$name), completion$items)
        stopifnot(length(items) == 1L)
        resolved <- request("completionItem/resolve", items[[1L]])
        stopifnot(is.null(resolved$error))
        hover <- text_request("textDocument/hover", case$call, 2L)
        stopifnot(grepl(case$signature, paste(unlist(hover$contents), collapse = "\n"), fixed = TRUE))
        signature <- text_request("textDocument/signatureHelp", case$call, case$character)
        stopifnot(length(signature$signatures) == 1L,
            identical(signature$signatures[[1L]]$label, case$signature))
        documentation <- list(resolved$result$documentation, hover$contents,
            signature$signatures[[1L]]$documentation)
        if (case$name == "hello") {
            # The existing plain-comment fallback retains source text and tags.
            stopifnot(all(vapply(documentation, function(value) {
                grepl(fallback, paste(unlist(value), collapse = "\n"), fixed = TRUE)
            }, logical(1L))))
        } else {
            stopifnot(!grepl("documentation marker|input marker",
                    paste(unlist(documentation), collapse = "\n")))
        }
    }
    package_signature <- text_request("textDocument/signatureHelp", 6L, 13L)
    stopifnot(length(package_signature$signatures) == 1L,
        grepl("path_abs(", package_signature$signatures[[1L]]$label, fixed = TRUE),
        identical(package_signature$signatures[[1L]]$documentation$kind, "markdown"),
        nzchar(package_signature$signatures[[1L]]$documentation$value))
    package_hover <- text_request("textDocument/hover", 6L, 7L)
    stopifnot(length(package_hover$contents) > 0L,
        any(grepl("path_abs", unlist(package_hover$contents), fixed = TRUE)))
    formatting <- request("textDocument/formatting", list(textDocument = list(uri = uri),
            options = list(tabSize = 2L, insertSpaces = TRUE)))
    stopifnot(!is.null(formatting$error), grepl("styler", formatting$error$message))
    state <- new.env(parent = emptyenv())
    state$messages <- list()
    client$notification_handlers[["window/showMessage"]] <- function(self, params) {
        state$messages[[length(state$messages) + 1L]] <- params$message
    }
    client$deliver(namespace$Notification$new("workspace/didChangeConfiguration",
            list(settings = list(diagnostics = TRUE))))
    wait(function() length(state$messages) > 0)
    stopifnot(grepl("lintr", state$messages[[1L]]))
    stopifnot(is.null(request("shutdown", NULL)$error))
    client$process$wait(5000)
    cat(paste0("Minimal installation: fresh install without roxygen2/pkgload, documentation cache fallback, ",
            "local completion/hover/signatures, installed-package help and missing-backend errors passed.\n"))
}

check_minimal_install()
