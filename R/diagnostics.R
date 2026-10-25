#' Identify the workspace symbols used by a diagnostic result
#' @noRd
diagnostics_workspace_generation <- function(workspace, uri) {
    index <- workspace$index
    if (is.null(index) || !isTRUE(index$enabled)) return(NULL)
    index$diagnostics_generation(uri)
}

DiagnosticSeverity <- list(
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4
)

diagnostics_callback <- function(self, uri, version, diagnostics, clear = FALSE) {
    workspace <- self$get_workspace(uri)
    if (is.null(diagnostics) || !workspace$documents$has(uri)) return(NULL)
    if (!lsp_settings$get("diagnostics") && !isTRUE(clear)) return(NULL)
    if (isTRUE(clear)) diagnostics <- list()
    document <- workspace$documents$get(uri)
    if (!is.null(version) && !identical(document$version, version)) {
        logger$info("diagnostics_callback: discarded stale result", list(
            uri = uri,
            result_version = version,
            document_version = document$version
        ))
        return(NULL)
    }

    logger$info("diagnostics_callback called:", list(
        uri = uri,
        version = version,
        diagnostics = diagnostics
    ))
    self$deliver(
        Notification$new(
            method = "textDocument/publishDiagnostics",
            params = list(
                uri = uri,
                version = version,
                diagnostics = diagnostics
            )
        )
    )
}

#' Queue diagnostics after the current parse has completed
#' @noRd
schedule_diagnostics <- function(self, uri, document, delay = 0) {
    if (!lsp_settings$get("diagnostics")) return(NULL)
    temp_root <- dirname(tempdir())
    # Only suppress temporary files when there is a non-temporary workspace.
    if (!length(self$rootPath) || path_has_parent(self$rootPath, temp_root) ||
            !path_has_parent(path_from_uri(uri), temp_root)) {
        self$diagnostics_task_manager$add_task(
            uri,
            diagnostics_task(self, uri, document, delay = delay)
        )
    }
}


diagnostics_cache_key <- function(uri, content, signature, generation = 0L) {
    paste(uri, get_content_hash(content), digest::digest(list(backend_cache_identity(signature), generation),
            algo = "xxhash64"), sep = "::")
}

# Function environments can contain instrumentation counters unrelated to
# backend configuration. Keep code and environment identity without walking
# those mutable environments when hashing an invocation.
backend_cache_identity <- function(value) {
    if (is.function(value)) return(list(
        class = class(value), formals = deparse(formals(value)), body = deparse(body(value)),
        environment = format(environment(value))))
    if (is.environment(value)) return(format(value))
    if (is.list(value)) return(list(class = class(value), values = lapply(value, backend_cache_identity)))
    value
}

diagnostics_needs_workspace_globals <- function() {
    tryCatch(isTRUE(backend_descriptor("diagnostics")$needs_workspace_globals), error = function(e) FALSE)
}

diagnostics_task <- function(self, uri, document, delay = 0) {
    workspace <- self$get_workspace(uri)
    request <- tryCatch(backend_request("diagnostics", uri, document, workspace), error = function(error) {
        failed <- backend_error_request("diagnostics", uri)
        backend_track(self, failed, document, workspace)
        diagnostics_callback(self, uri, document$version, list(), clear = TRUE)
        backend_report(self, failed, error)
        NULL
    })
    if (is.null(request)) return(NULL)
    backend_track(self, request, document, workspace)
    generation <- if (isTRUE(request$backend$needs_workspace_globals))
        diagnostics_workspace_generation(workspace, uri) else NULL
    document$diagnostics_generation <- generation
    backend_revision <- backend_generation(self, "diagnostics")
    cache_ttl <- lsp_settings$get("diagnostics_cache_ttl")
    if (is.null(cache_ttl)) cache_ttl <- 0
    cache_key <- diagnostics_cache_key(uri, request$content, request$signature, backend_revision)
    if (cache_ttl > 0 && workspace$diagnostics_cache$has(cache_key)) {
        cached_entry <- workspace$diagnostics_cache$get(cache_key)
        age <- as.numeric(difftime(Sys.time(), cached_entry$time, units = "secs"))
        if (!is.na(age) && age <= cache_ttl && identical(cached_entry$generation, generation)) {
            diagnostics_callback(self, uri, request$version, cached_entry$diagnostics)
            return(NULL)
        }
    }
    request$globals <- if (!isTRUE(request$backend$needs_workspace_globals)) NULL
    else if (!is.null(workspace$index) && isTRUE(workspace$index$enabled)) workspace$get_diagnostics_globals(uri)
    else if (is_package(workspace$root)) workspace$get_diagnostics_globals() else NULL
    generation <- if (isTRUE(request$backend$needs_workspace_globals))
        diagnostics_workspace_generation(workspace, uri) else NULL
    document$diagnostics_generation <- generation
    current <- function() {
        backend_request_current(self, request, document, workspace, backend_revision) &&
            (!isTRUE(request$backend$needs_workspace_globals) ||
                    identical(generation, diagnostics_workspace_generation(workspace, uri)))
    }
    create_task(target = package_call(run_backend), args = list(request = request),
        callback = function(result) {
            if (!current()) return(NULL)
            if (cache_ttl > 0) workspace$diagnostics_cache$set(cache_key,
                list(time = Sys.time(), generation = generation, diagnostics = result))
            diagnostics_callback(self, uri, request$version, result)
        }, error = function(error) {
            if (!current()) return(NULL)
            logger$info("diagnostics_task:", error)
            if (backend_unavailable(error)) {
                diagnostics_callback(self, uri, request$version, list(), clear = TRUE)
                backend_report(self, request, error)
            } else {
                diagnostics_callback(self, uri, request$version, list(list(
                    range = range(position(0L, 0L), position(0L, 0L)),
                    severity = DiagnosticSeverity$Error, source = request$backend$id,
                    message = paste0("Failed to run diagnostics: ", conditionMessage(error)))))
            }
        }, delay = delay, track_descendants = !is.null(request$backend$executable))
}
