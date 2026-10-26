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
    source_path <- normalizePath(source_path, mustWork = TRUE)
    installed <- installed.packages()
    dependencies <- minimal_install_dependencies(installed)
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
    stopifnot(!any(c("lintr", "styler") %in% dependencies))
    old_library <- .libPaths()
    on.exit(.libPaths(old_library), add = TRUE)
    .libPaths(c(library, .Library), include.site = FALSE)
    stopifnot(!requireNamespace("lintr", quietly = TRUE), !requireNamespace("styler", quietly = TRUE))
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
    profile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
    # Installers, servers and workers must all inherit the same isolated library.
    Sys.setenv(R_LIBS = library, R_LIBS_USER = library, R_LIBS_SITE = "",
        R_PROFILE = profile, R_PROFILE_USER = profile, R_ENVIRON = profile, R_ENVIRON_USER = profile)
    processx::run(file.path(R.home("bin"), "R"),
        c("CMD", "INSTALL", "--no-multiarch", "-l", library, source_path),
        stdout = "|", stderr = "|", error_on_status = TRUE)
    namespace <- loadNamespace("languageserver")
    stopifnot(identical(normalizePath(dirname(getNamespaceInfo(namespace, "path"))), normalizePath(library)))
    client <- namespace$LanguageClient$new(file.path(R.home("bin"), "R"),
        c("--no-echo", "--vanilla", "-e", "languageserver::run()"))
    on.exit({
        if (client$process$is_alive()) client$process$kill_tree()
    }, add = TRUE)
    wait <- function(done, deadline = Sys.time() + 20) {
        while (!done() && Sys.time() < deadline) {
            data <- client$fetch(blocking = TRUE, timeout = 1)
            if (!is.null(data)) client$handle_raw(data)
        }
        stopifnot(done())
    }
    request <- function(method, params, deadline = Sys.time() + 20) {
        response <- NULL
        client$deliver(client$request(method, params), callback = function(self, result, error = NULL) {
            response <<- list(result = result, error = error)
        })
        wait(function() !is.null(response), deadline)
        response
    }
    client$start(working_dir = tempdir())
    wait(function() !is.null(client$ServerCapabilities))
    client$deliver(namespace$Notification$new("initialized"))
    client$deliver(namespace$Notification$new("workspace/didChangeConfiguration",
            list(settings = list(diagnostics = FALSE))))
    uri <- namespace$path_to_uri(file.path(tempdir(), "minimal-buffer.R"))
    client$deliver(namespace$Notification$new("textDocument/didOpen", list(textDocument = list(
        uri = uri, languageId = "r", version = 1L,
        text = "hello <- function(x) x\nhel"))))
    completion_params <- list(textDocument = list(uri = uri), position = list(line = 1L, character = 3L))
    has_hello <- function(response) {
        any(vapply(response$result$items, function(item) identical(item$label, "hello"), logical(1L)))
    }
    deadline <- Sys.time() + 20
    completion <- request("textDocument/completion", completion_params, deadline)
    # Startup-package suggestions are valid partial results before the first
    # parse. Retrigger incomplete lists until the local declaration is ready.
    while (is.null(completion$error) && isTRUE(completion$result$isIncomplete) && !has_hello(completion)) {
        stopifnot(Sys.time() < deadline)
        Sys.sleep(0.05)
        completion <- request("textDocument/completion", completion_params, deadline)
    }
    stopifnot(is.null(completion$error), has_hello(completion))
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
    cat("Minimal installation: fresh install, namespace, LSP startup, completion and missing-backend errors passed.\n")
}

check_minimal_install()
