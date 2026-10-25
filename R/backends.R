#' Resolve and execute diagnostics and formatting adapters
#' @noRd
backend_error <- function(message, unavailable = FALSE, unsupported = FALSE) {
    classes <- c(if (unavailable) "backend_unavailable", if (unsupported) "backend_unsupported",
        "backend_error", "error", "condition")
    stop(structure(list(message = message, call = NULL), class = classes))
}

backend_descriptor <- function(kind, selection = lsp_settings$get(paste0(kind, "_backend"))) {
    if (is.null(selection)) selection <- if (kind == "diagnostics") "lintr" else "styler"
    if (is.character(selection) && length(selection) == 1L && !is.na(selection)) {
        descriptor <- switch(paste(kind, selection, sep = ":"),
            "diagnostics:lintr" = list(id = "lintr", run = lintr_diagnostics,
                document_types = c("r", "literate"), needs_workspace_globals = TRUE,
                package = "lintr", version = "3.3.0-1", config_files = ".lintr"),
            "diagnostics:jarl" = list(id = "jarl", run = jarl_diagnostics,
                document_types = "r", executable = "jarl",
                config_files = c("jarl.toml", "DESCRIPTION", "NAMESPACE")),
            "formatting:styler" = list(id = "styler", run = styler_formatting,
                document_types = c("r", "literate"),
                modes = c("document", "range", "ranges", "on_type"),
                package = "styler", version = "1.5.1"),
            "formatting:air" = list(id = "air", run = air_formatting,
                document_types = "r", modes = "document", executable = "air",
                config_files = c("air.toml", ".air.toml")))
        if (is.null(descriptor)) backend_error(sprintf("Unknown %s backend '%s'.", kind, selection))
    } else {
        descriptor <- selection
    }
    if (!is.list(descriptor) || !backend_string(descriptor$id) || !is.function(descriptor$run) ||
            !is.character(descriptor$document_types) || !length(descriptor$document_types) ||
            anyNA(descriptor$document_types) ||
            !all(descriptor$document_types %in% c("r", "literate")) ||
            (kind == "formatting" && (!is.character(descriptor$modes) ||
                        !length(descriptor$modes) || anyNA(descriptor$modes) ||
                        !all(descriptor$modes %in% c("document", "range", "ranges", "on_type")))) ||
            (!is.null(descriptor$needs_workspace_globals) &&
                    !identical(descriptor$needs_workspace_globals, TRUE) &&
                    !identical(descriptor$needs_workspace_globals, FALSE)) ||
            (!is.null(descriptor$package) && (!backend_string(descriptor$package) ||
                        !backend_string(descriptor$version) || !backend_version_valid(descriptor$version))) ||
            (!is.null(descriptor$executable) && !backend_string(descriptor$executable)) ||
            (!is.null(descriptor$config_files) &&
                    (!is.character(descriptor$config_files) || anyNA(descriptor$config_files) ||
                            any(!nzchar(descriptor$config_files))))) {
        backend_error(paste("Invalid", kind, "adapter descriptor."))
    }
    descriptor
}

backend_string <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

backend_version_valid <- function(value) {
    tryCatch({
        package_version(value)
        TRUE
    }, error = function(e) FALSE)
}

backend_named_list <- function(value) {
    is.list(value) && (length(value) == 0L || (length(names(value)) == length(value) &&
                !anyNA(names(value)) && all(nzchar(names(value))) && !anyDuplicated(names(value))))
}

backend_execution_options <- function(id) {
    all_options <- lsp_settings$get("backend_options")
    if (is.null(all_options)) return(list())
    if (!backend_named_list(all_options)) backend_error("backend_options must be a named list of per-backend options.")
    options <- all_options[[id]]
    if (is.null(options)) return(list())
    if (!backend_named_list(options) || any(!names(options) %in% c("executable", "timeout", "env"))) {
        backend_error("Backend options must be a named list of executable, timeout and env overrides.")
    }
    options
}

backend_generation <- function(self, kind) {
    generation <- self$backend_generations[[kind]]
    if (is.null(generation)) 0L else generation
}

backend_r_options <- function(descriptor, load = TRUE) {
    # Styler's package-defined defaults are needed only for an actual formatting
    # request. Lintr's filename default can be resolved without loading lintr.
    if (load && identical(descriptor$id, "styler") && !is.null(descriptor$package)) {
        requireNamespace(descriptor$package, quietly = TRUE)
    }
    prefix <- switch(descriptor$id, lintr = "^lintr\\.", styler = "^styler\\.", NULL)
    result <- if (is.null(prefix)) list() else base::options()[
        grepl(prefix, names(base::options()))]
    if (identical(descriptor$id, "styler")) {
        result["languageserver.formatting_style"] <- list(getOption("languageserver.formatting_style"))
    } else if (identical(descriptor$id, "lintr") && is.null(result[["lintr.linter_file"]])) {
        result["lintr.linter_file"] <- list(Sys.getenv("R_LINTR_LINTER_FILE", ".lintr"))
    }
    result
}

backend_existing_directory <- function(path) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    while (!dir.exists(path) && !identical(path, dirname(path))) path <- dirname(path)
    if (dir.exists(path)) path else getwd()
}

backend_ancestors <- function(path) {
    result <- character()
    repeat {
        result <- c(result, path)
        parent <- dirname(path)
        if (identical(parent, path)) break
        path <- parent
    }
    result
}

backend_configuration_paths <- function(descriptor, path, working_directory, r_options, env = NULL) {
    if (identical(descriptor$id, "lintr")) return(lintr_configuration_paths(path, working_directory, r_options))
    directories <- backend_ancestors(if (nzchar(path)) dirname(path) else working_directory)
    files <- descriptor$config_files
    paths <- unlist(lapply(files, function(file) {
        if (fs::is_absolute_path(file)) file else file.path(directories, file)
    }), use.names = FALSE)
    if (identical(descriptor$id, "air")) {
        environment_value <- function(name, default = "") {
            if (name %in% names(env)) unname(env[[name]]) else Sys.getenv(name, unset = default)
        }
        user_config <- c(environment_value("XDG_CONFIG_HOME", path.expand("~/.config")),
            path.expand("~/Library/Application Support"), environment_value("APPDATA"))
        paths <- c(paths, file.path(user_config[nzchar(user_config)], "air", "air.toml"))
    }
    unique(paths)
}

backend_file_fingerprints <- function(paths, contents = TRUE) {
    if (!length(paths)) return(list(paths = character(), size = numeric(), mode = character(),
            mtime = numeric(), hash = if (contents) character() else NULL))
    stats <- file.info(paths)
    hashes <- if (contents) vapply(paths, function(path) {
        if (!file.exists(path) || dir.exists(path)) return(NA_character_)
        tryCatch(digest::digest(file = path, algo = "xxhash64"), error = function(e) NA_character_)
    }, character(1L)) else NULL
    list(paths = paths, size = unname(stats$size), mode = as.character(stats$mode),
        mtime = as.numeric(stats$mtime), hash = hashes)
}

backend_request <- function(kind, uri, document, workspace = NULL,
    mode = "document", options = list(), range = NULL, ranges = NULL, point = NULL, ch = NULL) {
    descriptor <- backend_descriptor(kind)
    execution_options <- backend_execution_options(descriptor$id)
    timeout <- execution_options$timeout
    if (is.null(timeout)) timeout <- 10
    if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0) {
        backend_error("Backend timeout must be a positive number of seconds.")
    }
    env <- execution_options$env
    if (is.list(env)) {
        valid <- vapply(env, function(value) is.character(value) && length(value) == 1L && !is.na(value), logical(1L))
        if (any(!valid)) backend_error("Backend env must contain named strings.")
        env <- unlist(env, use.names = TRUE)
    }
    if (!is.null(env) && (!is.character(env) || length(names(env)) != length(env) ||
                anyNA(env) || anyNA(names(env)) || any(!nzchar(names(env))) || anyDuplicated(names(env)))) {
        backend_error("Backend env must contain named strings.")
    }
    original_path <- path_from_uri(uri)
    root <- workspace$root
    working_directory <- getwd()
    if (!is.null(descriptor$executable)) {
        working_directory <- backend_existing_directory(if (nzchar(original_path)) dirname(original_path)
            else if (length(root) && nzchar(root)) root else getwd())
    }
    path <- if (nzchar(original_path)) normalizePath(original_path, winslash = "/", mustWork = FALSE)
    else file.path(working_directory, "__languageserver_untitled__.R")
    executable <- execution_options$executable
    if (is.null(executable)) executable <- descriptor$executable
    if (!is.null(executable) && !backend_string(executable)) backend_error("Backend executable must be a path or command name.")
    resolved <- if (is.null(executable)) NULL else if (fs::is_absolute_path(executable)) executable
    else unname(Sys.which(executable))
    package_file <- if (is.null(descriptor$package)) character()
    else normalizePath(system.file("DESCRIPTION", package = descriptor$package), winslash = "/", mustWork = FALSE)
    r_options <- backend_r_options(descriptor)
    configuration_files <- backend_configuration_paths(descriptor, original_path, working_directory, r_options, env)
    signature <- list(adapter = descriptor, options = execution_options, r_options = r_options,
        working_directory = working_directory, root = root,
        configuration = backend_file_fingerprints(configuration_files),
        dependency = backend_file_fingerprints(c(package_file, resolved), contents = FALSE),
        lint_cache = if (kind == "diagnostics") isTRUE(lsp_settings$get("lint_cache")) else NULL)
    list(kind = kind, backend = descriptor, signature = signature, uri = uri,
        original_path = original_path, path = path, workspace_root = root,
        working_directory = working_directory, executable = resolved, timeout = timeout, env = env,
        r_options = r_options, language = document$language,
        document_type = if (isTRUE(document$is_rmarkdown)) "literate" else "r",
        version = document$version, content = document$content, is_open = document$is_open,
        lint_cache = isTRUE(lsp_settings$get("lint_cache")), mode = mode, options = options,
        range = range, ranges = ranges, point = point, ch = ch)
}

backend_supported <- function(request) {
    request$document_type %in% request$backend$document_types &&
        (request$kind != "formatting" || request$mode %in% request$backend$modes)
}

backend_unsupported_message <- function(request) {
    sprintf("Backend '%s' does not support %s formatting for %s documents.",
        request$backend$id, request$mode, request$document_type)
}

backend_check_available <- function(request) {
    descriptor <- request$backend
    if (!is.null(descriptor$package) && (!requireNamespace(descriptor$package, quietly = TRUE) ||
                utils::packageVersion(descriptor$package) < package_version(descriptor$version))) {
        backend_error(sprintf("Backend '%s' requires %s >= %s. Install it or select another backend.",
                descriptor$id, descriptor$package, descriptor$version), unavailable = TRUE)
    }
    if (!is.null(descriptor$executable) &&
            (!backend_string(request$executable) || !file.exists(request$executable))) {
        backend_error(sprintf("Backend '%s' executable was not found. Install it or set backend_options.%s.executable.",
                descriptor$id, descriptor$id), unavailable = TRUE)
    }
}

run_backend <- function(request) {
    if (!backend_supported(request)) {
        if (request$kind == "formatting") backend_error(backend_unsupported_message(request), unsupported = TRUE)
        return(list())
    }
    backend_check_available(request)
    prefix <- switch(request$backend$id, lintr = "^lintr\\.", styler = "^styler\\.", NULL)
    keys <- if (is.null(prefix)) character() else names(base::options())[
        grepl(prefix, names(base::options()))]
    changes <- stats::setNames(rep(list(NULL), length(keys)), keys)
    for (key in names(request$r_options)) changes[key] <- request$r_options[key]
    previous_options <- base::options(changes)
    on.exit(base::options(previous_options), add = TRUE)
    previous_directory <- setwd(request$working_directory)
    on.exit(setwd(previous_directory), add = TRUE)
    result <- request$backend$run(request)
    if (!is.null(result) && !is.list(result)) backend_error("An adapter must return a list of LSP diagnostics or edits.")
    if (request$kind == "diagnostics") {
        result <- lapply(result, function(diagnostic) {
            if (!is.list(diagnostic) || !is.list(diagnostic$range) || !backend_string(diagnostic$message)) {
                backend_error("An adapter returned an invalid diagnostic.")
            }
            diagnostic$source <- request$backend$id
            diagnostic
        })
    } else {
        for (edit in result) {
            if (!is.list(edit) || !is.list(edit$range) || !is.character(edit$newText) ||
                    length(edit$newText) != 1L || is.na(edit$newText)) {
                backend_error("An adapter returned an invalid text edit.")
            }
        }
    }
    result
}

lintr_diagnostics <- function(request) {
    diagnose_file(request$uri, request$content, request$document_type == "literate",
        globals = request$globals, cache = request$lint_cache)
}

styler_formatting <- function(request) {
    document <- Document$new(request$uri, request$language, request$version, request$content)
    switch(request$mode,
        document = styler_document_edits(request$uri, document, request$options),
        range = styler_range_edits(request$uri, document, request$range, request$options),
        ranges = styler_ranges_edits(request$uri, document, request$ranges, request$options),
        on_type = styler_on_type_edits(request$uri, document, request$point, request$ch, request$options))
}

backend_report <- function(self, request, error) {
    if (is.null(self$backend_messages)) self$backend_messages <- collections::dict()
    key <- request$kind
    signature <- if (backend_unavailable(error) && !is.null(request$backend)) list(
        id = request$backend$id, options = request$signature$options,
        dependency = request$signature$dependency, generation = backend_generation(self, request$kind))
    else request$signature
    message <- backend_error_message(error)
    identity <- list(signature = signature, message = message)
    if (!identical(self$backend_messages$get(key, NULL), identity)) {
        self$backend_messages$set(key, identity)
        self$deliver(Notification$new("window/showMessage", list(type = 1L, message = message)))
    }
    invisible(NULL)
}

backend_unavailable <- function(error) {
    # callr preserves a remotely raised condition as the parent of its wrapper.
    for (i in seq_len(10L)) {
        if (inherits(error, "backend_unavailable")) return(TRUE)
        error <- error$parent
        if (is.null(error)) break
    }
    FALSE
}

backend_error_message <- function(error) {
    current <- error
    for (i in seq_len(10L)) {
        if (inherits(current, "backend_error")) return(conditionMessage(current))
        current <- current$parent
        if (is.null(current)) break
    }
    conditionMessage(error)
}

backend_track <- function(self, request, document, workspace) {
    if (is.null(self$backend_documents)) self$backend_documents <- collections::dict()
    self$backend_documents$set(paste(request$kind, request$uri),
        list(request = request, document = document, workspace = workspace))
}

backend_request_current <- function(self, request, document, workspace, generation) {
    current_workspace <- self$get_workspace(request$uri)
    current <- current_workspace$documents$get(request$uri, NULL)
    if (!identical(current_workspace, workspace) || !identical(current, document) ||
            !identical(current$version, request$version) ||
            !identical(current$content, request$content) || !identical(current$is_open, request$is_open) ||
            !identical(backend_generation(self, request$kind), generation)) return(FALSE)
    context <- tryCatch(backend_request(request$kind, request$uri, current, workspace), error = function(e) NULL)
    !is.null(context) && identical(context$signature, request$signature)
}

backend_error_request <- function(kind, uri) {
    list(kind = kind, uri = uri, signature = list(
        selection = lsp_settings$get(paste0(kind, "_backend")),
        options = lsp_settings$get("backend_options"),
        r_options = tryCatch(backend_r_options(backend_descriptor(kind), load = FALSE), error = function(e) NULL)))
}

backend_refresh_document <- function(self, workspace, document) {
    for (kind in c("diagnostics", "formatting")) {
        key <- paste(kind, document$uri)
        old <- self$backend_documents$get(key, NULL)
        if ((kind == "diagnostics" && !isTRUE(lsp_settings$get("diagnostics"))) ||
                (kind == "formatting" && is.null(old))) next
        context <- tryCatch(backend_request(kind, document$uri, document, workspace), error = identity)
        failed <- inherits(context, "error")
        request <- if (failed) backend_error_request(kind, document$uri) else context
        backend_track(self, request, document, workspace)
        if ((!failed && is.null(old)) || (!is.null(old) && identical(old$request$signature, request$signature) &&
                    identical(old$document, document) && identical(old$workspace, workspace))) next
        if (kind == "formatting") {
            cancel_formatting_requests(self, uri = document$uri, message = "Formatting backend configuration changed")
        } else if (isTRUE(lsp_settings$get("diagnostics"))) {
            self$diagnostics_task_manager$cancel(document$uri)
            if (!is.null(workspace$diagnostics_cache)) workspace$diagnostics_cache$clear()
            diagnostics_callback(self, document$uri, NULL, list(), clear = TRUE)
            if (!failed) backend_schedule_current(self, document)
        }
        if (failed) backend_report(self, request, context)
    }
}

backend_schedule_current <- function(self, document) {
    if (!identical(document$content, document$parsed_content) ||
            !identical(document$version, document$parse_data$version)) {
        document$pending_diagnostics <- TRUE
        document$diagnostics_delay <- 0
    } else {
        schedule_diagnostics(self, document$uri, document, delay = 0)
    }
}

# Poll a bounded batch, including idle buffers on clients without file watches.
poll_backend_configuration <- function(self, force = FALSE, now = proc.time()[[3L]]) {
    if (is.null(self$backend_documents)) self$backend_documents <- collections::dict()
    if (!length(self$backend_poll_queue)) {
        if (!force && now - self$backend_poll_time < 1) return(invisible(NULL))
        self$backend_poll_time <- now
        for (key in self$backend_documents$keys()) {
            entry <- self$backend_documents$get(key)
            current <- self$get_workspace(entry$request$uri)$documents$get(entry$request$uri, NULL)
            if (is.null(current) || !isTRUE(current$is_open)) self$backend_documents$remove(key)
        }
        self$backend_poll_queue <- unlist(lapply(self$workspaces$values(), function(workspace) {
            lapply(Filter(function(doc) isTRUE(doc$is_open), workspace$documents$values()), function(document) {
                list(workspace = workspace, document = document)
            })
        }), recursive = FALSE)
    }
    limit <- if (force) length(self$backend_poll_queue) else min(20L, length(self$backend_poll_queue))
    started <- proc.time()[[3L]]
    processed <- 0L
    for (entry in utils::head(self$backend_poll_queue, limit)) {
        processed <- processed + 1L
        if (identical(self$get_workspace(entry$document$uri), entry$workspace) &&
                identical(entry$workspace$documents$get(entry$document$uri, NULL), entry$document) &&
                isTRUE(entry$document$is_open)) backend_refresh_document(self, entry$workspace, entry$document)
        if (!force && (proc.time()[[3L]] - started) * 1000 >= 5) break
    }
    if (processed) self$backend_poll_queue <- self$backend_poll_queue[-seq_len(processed)]
    invisible(NULL)
}

backend_settings_identity <- function() {
    lapply(c("diagnostics", "formatting"), function(kind) {
        descriptor <- tryCatch(backend_descriptor(kind), error = function(e) NULL)
        list(
            enabled = if (kind == "diagnostics") lsp_settings$get("diagnostics") else TRUE,
            selection = lsp_settings$get(paste0(kind, "_backend")),
            options = if (is.null(descriptor)) lsp_settings$get("backend_options")
            else tryCatch(backend_execution_options(descriptor$id), error = function(e) lsp_settings$get("backend_options")),
            lint_cache = if (kind == "diagnostics") lsp_settings$get("lint_cache") else NULL,
            r_options = if (is.null(descriptor)) NULL else backend_r_options(descriptor, load = FALSE))
    })
}

backend_settings_changed <- function(self, previous) {
    current <- backend_settings_identity()
    for (i in seq_along(current)) {
        if (identical(current[[i]], previous[[i]])) next
        kind <- c("diagnostics", "formatting")[[i]]
        if (is.null(self$backend_generations)) self$backend_generations <- list()
        self$backend_generations[[kind]] <- backend_generation(self, kind) + 1L
        if (kind == "formatting") {
            cancel_formatting_requests(self, message = "Formatting backend configuration changed")
        } else {
            for (workspace in self$workspaces$values()) {
                if (!is.null(workspace$diagnostics_cache)) workspace$diagnostics_cache$clear()
                for (document in workspace$documents$values()) {
                    if (!is.null(self$diagnostics_task_manager)) self$diagnostics_task_manager$cancel(document$uri)
                    diagnostics_callback(self, document$uri, NULL, list(), clear = TRUE)
                    if (isTRUE(document$is_open) && isTRUE(lsp_settings$get("diagnostics"))) {
                        backend_schedule_current(self, document)
                    }
                }
            }
        }
    }
}

backend_watched_configuration <- function(self, path) {
    configured <- basename(getOption("lintr.linter_file", Sys.getenv("R_LINTR_LINTER_FILE", ".lintr")))
    names <- c(".lintr", ".lintr.R", "jarl.toml", "air.toml", ".air.toml", "DESCRIPTION", "NAMESPACE",
        configured, paste0(configured, ".R"))
    tracked <- if (is.null(self$backend_documents)) list() else self$backend_documents$values()
    if (basename(path) %in% names || any(vapply(tracked, function(entry) {
        path %in% entry$request$signature$configuration$paths
    }, logical(1L)))) {
        self$backend_poll_queue <- NULL
        poll_backend_configuration(self, force = TRUE)
    }
    invisible(NULL)
}

register_backend_watches <- function(self) {
    if (!isTRUE(self$ClientCapabilities$workspace$didChangeWatchedFiles$dynamicRegistration)) {
        return(invisible(NULL))
    }
    registration <- list(
        id = "languageserver-backend-configuration", method = "workspace/didChangeWatchedFiles",
        registerOptions = list(watchers = list(list(
            globPattern = "**/{.lintr,.lintr.R,jarl.toml,air.toml,.air.toml,DESCRIPTION,NAMESPACE}", kind = 7L))))
    self$deliver(self$request("client/registerCapability", list(registrations = list(registration))),
        callback = function(self, result, error = NULL) {
            if (!is.null(error)) logger$info("Backend configuration watches unavailable: ", error)
        })
    invisible(NULL)
}
