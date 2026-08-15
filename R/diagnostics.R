#' diagnostics
#'
#' Diagnose problems in files after linting.
#'
#' @name diagnostics
NULL

DiagnosticSeverity <- list(
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4
)

#' @rdname diagnostics
#' @noRd
diagnostic_range <- function(result, content) {
    line <- result$line_number - 1
    column <- result$column_number - 1
    if (is.null(column) || is.na(column)) {
        column <- 0
    }
    text <- if (line + 1 <= length(content)) content[line + 1] else ""
    if (is.null(result$ranges)) {
        cols <- code_point_to_unit(text, c(column, column + 1))
        range(
            start = position(line = line, character = cols[1]),
            end = position(line = line, character = cols[2])
        )
    } else {
        cols <- code_point_to_unit(text, c(result$ranges[[1]][1] - 1, result$ranges[[1]][2]))
        range(
            start = position(line = line, character = cols[1]),
            end = position(line = line, character = cols[2])
        )
    }
}

#' @rdname diagnostics
#' @noRd
diagnostic_severity <- function(result) {
    switch(result$type,
        error = DiagnosticSeverity$Error,
        warning = DiagnosticSeverity$Warning,
        style = DiagnosticSeverity$Information,
        DiagnosticSeverity$Information)
}

#' @rdname diagnostics
#' @noRd
diagnostic_from_lint <- function(result, content) {
    list(
        range = diagnostic_range(result, content),
        severity = diagnostic_severity(result),
        source = "lintr",
        message = result$message,
        code = result$linter,
        codeDescription = list(
            href = sprintf("https://lintr.r-lib.org/reference/%s.html", result$linter)
        )
    )
}

#' Find the lintr config file
#' @noRd
find_config <- function(filename) {
    # instead of calling `lintr:::find_config` directly
    # since CRAN doesn't like :::.
    asNamespace("lintr")$find_config(filename)
}

#' Run diagnostic on a file
#'
#' Lint and diagnose problems in a file.
#' @noRd
diagnose_file <- function(uri, content, is_rmarkdown = FALSE, globals = NULL, cache = FALSE) {
    if (length(content) == 0) {
        return(list())
    }

    if (is_rmarkdown) {
        content <- purl(content, parseable_only = TRUE)
        if (!any(nzchar(trimws(content)))) {
            return(list())
        }
    }

    path <- path_from_uri(uri)

    if (length(content) == 1) {
        content <- c(content, "")
    }

    if (length(globals)) {
        env_name <- "languageserver:globals"
        do.call("attach", list(globals, name = env_name, warn.conflicts = FALSE))
        on.exit(do.call("detach", list(env_name, character.only = TRUE)))
    }

    linters <- NULL
    if (nzchar(path)) {
        config_path <- tryCatch(find_config(path), error = function(e) NULL)
        if (is.null(config_path) || !nzchar(config_path) || !file.exists(config_path)) {
            linters <- lintr::linters_with_defaults()
        }
    } else {
        linters <- lintr::linters_with_defaults()
    }

    if (file.exists(path) && !is_rmarkdown) {
        lints <- lintr::lint(path,
            cache = cache,
            text = content,
            parse_settings = TRUE,
            linters = linters
        )
    } else {
        lints <- lintr::lint(
            text = content,
            cache = cache,
            parse_settings = TRUE,
            linters = linters
        )
    }

    diagnostics <- lapply(lints, diagnostic_from_lint, content = content)
    names(diagnostics) <- NULL
    diagnostics
}

diagnostics_callback <- function(self, uri, version, diagnostics) {
    workspace <- self$get_workspace(uri)
    if (is.null(diagnostics) || !workspace$documents$has(uri) || !lsp_settings$get("diagnostics")) return(NULL)
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
    if (path_has_parent(self$rootPath, temp_root) ||
            !path_has_parent(path_from_uri(uri), temp_root)) {
        self$diagnostics_task_manager$add_task(
            uri,
            diagnostics_task(self, uri, document, delay = delay)
        )
    }
}


diagnostics_task <- function(self, uri, document, delay = 0) {
    version <- document$version
    content <- document$content

    cache_ttl <- lsp_settings$get("diagnostics_cache_ttl")
    if (is.null(cache_ttl)) {
        cache_ttl <- 0
    }
    content_hash <- get_content_hash(content)
    cache_key <- paste(uri, content_hash, sep = "::")

    workspace <- self$get_workspace(uri)

    if (cache_ttl > 0 && workspace$diagnostics_cache$has(cache_key)) {
        cached_entry <- workspace$diagnostics_cache$get(cache_key)
        age <- as.numeric(difftime(Sys.time(), cached_entry$time, units = "secs"))
        if (!is.na(age) && age <= cache_ttl) {
            logger$info("diagnostics_task: cache hit for", uri)
            diagnostics_callback(self, uri, version, cached_entry$diagnostics)
            return(NULL)
        }
    }

    globals <- if (!is.null(workspace$index) &&
            isTRUE(workspace$index$enabled)) {
        workspace$get_diagnostics_globals(uri)
    } else if (is_package(workspace$root)) {
        workspace$get_diagnostics_globals()
    } else {
        NULL
    }

    create_task(
        target = package_call(diagnose_file),
        args = list(
            uri = uri,
            content = content,
            is_rmarkdown = document$is_rmarkdown,
            globals = globals,
            cache = lsp_settings$get("lint_cache")
        ),
        callback = function(result) {
            if (cache_ttl > 0) {
                workspace$diagnostics_cache$set(cache_key, list(
                    time = Sys.time(),
                    diagnostics = result
                ))
            }
            diagnostics_callback(self, uri, version, result)
        },
        error = function(e) {
            logger$info("diagnostics_task:", e)
            diagnostics_callback(self, uri, version, list(list(
                range = range(
                    start = position(0, 0),
                    end = position(0, 0)
                ),
                severity = DiagnosticSeverity$Error,
                source = "lintr",
                message = paste0("Failed to run diagnostics: ", conditionMessage(e))
            )))
        },
        delay = delay
    )
}
