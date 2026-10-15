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

lintr_supports_text_terminal_newline <- function() {
    lines <- asNamespace("lintr")$get_lines(
        filename = "x.R",
        text = "x"
    )
    isFALSE(attr(lines, "terminal_newline", exact = TRUE))
}

lint_without_terminal_newline <- function(path, content, linters = NULL) {
    lintr_namespace <- asNamespace("lintr")
    on.exit(lintr_namespace$reset_settings(), add = TRUE)
    if (nzchar(path)) {
        lintr_namespace$read_settings(path)
    } else {
        lintr_namespace$read_settings()
    }
    effective_linters <- lintr_namespace$define_linters(linters)
    linter <- effective_linters[["trailing_blank_lines_linter"]]
    if (is.null(linter)) {
        return(list())
    }
    filename <- if (nzchar(path)) {
        normalizePath(path, winslash = "/", mustWork = FALSE)
    } else {
        "<text>"
    }
    source_exclusions <- lintr_namespace$parse_exclusions(
        filename,
        lines = content,
        linter_names = names(effective_linters)
    )
    if (lintr_namespace$is_excluded(length(content), "trailing_blank_lines_linter", source_exclusions)) {
        return(list())
    }
    lints <- Filter(function(lint) {
        identical(lint$message, "Add a terminal newline.")
    }, linter(list(
        filename = filename,
        file_lines = enc2utf8(content),
        terminal_newline = FALSE
    )))
    for (i in seq_along(lints)) {
        lints[[i]]$linter <- "trailing_blank_lines_linter"
    }
    class(lints) <- c("lints", "list")
    if (!nzchar(path)) {
        return(lints)
    }
    lintr_namespace$exclude(lints, lines = character())
}

#' Run diagnostic on a file
#'
#' Lint and diagnose problems in a file.
#' @noRd
diagnose_file <- function(uri, content, is_rmarkdown = FALSE, globals = NULL, cache = FALSE) {
    if (length(content) == 0 || identical(content, "")) {
        return(list())
    }

    if (is_rmarkdown) {
        content <- purl(content, parseable_only = TRUE)
        if (!any(nzchar(trimws(content)))) {
            return(list())
        }
    }

    path <- path_from_uri(uri)

    terminal_newline <- is_rmarkdown || !nzchar(content[[length(content)]])

    if (length(globals)) {
        env_name <- "languageserver:globals"
        do.call("attach", list(globals, name = env_name, warn.conflicts = FALSE))
        on.exit(do.call("detach", list(env_name, character.only = TRUE)), add = TRUE)
    }

    if (nzchar(path)) {
        lints <- lintr::lint(path,
            cache = cache,
            text = content,
            parse_settings = TRUE
        )
    } else {
        # There is no stable filename to cache for a pathless document.
        lints <- lintr::lint(
            text = content,
            parse_settings = TRUE
        )
    }
    if (!terminal_newline && (!nzchar(path) || !lintr_supports_text_terminal_newline())) {
        lints <- c(lints, lint_without_terminal_newline(path, content))
    }

    diagnostics <- lapply(lints, diagnostic_from_lint, content = content)
    names(diagnostics) <- NULL
    diagnostics
}

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
