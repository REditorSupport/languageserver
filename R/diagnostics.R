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
        source = result$linter,
        message = result$message
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
        # make sure Rmarkdown file has at least one block
        if (!any(stringi::stri_detect_regex(content, "```\\{r[ ,\\}]"))) {
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

    lints <- lintr::lint(path, cache = cache, text = content)
    diagnostics <- lapply(lints, diagnostic_from_lint, content = content)
    names(diagnostics) <- NULL
    diagnostics
}

diagnostics_callback <- function(self, uri, version, diagnostics) {
    if (is.null(diagnostics) || !self$workspace$documents$has(uri) || !lsp_settings$get("diagnostics")) return(NULL)

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


diagnostics_task <- function(self, uri, document, delay = 0) {
    version <- document$version
    content <- document$content

    is_package <- is_package(self$rootPath)
    globals <- NULL

    if (is_package) {
        globals <- new.env(parent = emptyenv())
        for (doc in self$workspace$documents$values()) {
            if (dirname(path_from_uri(doc$uri)) != file.path(self$rootPath, "R")) next
            parse_data <- doc$parse_data
            if (is.null(parse_data)) next
            for (symbol in parse_data$nonfuncts) {
                globals[[symbol]] <- NULL
            }
            list2env(parse_data$functions, globals)
        }
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
        callback = function(result) diagnostics_callback(self, uri, version, result),
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
