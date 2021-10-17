#' diagnostics
#'
#' Diagnose problems in files after linting.
#'
#' @name diagnostics
NULL


#' Check if lintr is 2.0.1.9000 above. A number of features relies on it.
#' @noRd
lintr_is_new_enough <- function() {
    return(
        utils::packageVersion("lintr") >= "2.0.1.9000" &&
            "text" %in% names(formals(lintr::lint))
    )
}


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
diagnose_file <- function(uri, content, cache = FALSE) {
    if (length(content) == 0) {
        return(list())
    }

    if (is_rmarkdown(uri)) {
        # make sure Rmarkdown file has at least one block
        if (!any(stringi::stri_detect_regex(content, "```\\{r[ ,\\}]"))) {
            return(list())
        }
    }

    path <- path_from_uri(uri)

    if (length(content) == 1) {
        content <- c(content, "")
    }

    if (lintr_is_new_enough()) {
        lints <- lintr::lint(path, cache = cache, text = content)
    } else {
        # TODO: remove it once new version of lintr is released
        linter_file <- find_config(path)
        if (!is.null(linter_file)) {
            op <- options(lintr.linter_file = linter_file)
            on.exit(options(op))
        }
        text <- paste0(content, collapse = "\n")
        lints <- lintr::lint(text, cache = cache)
    }

    diagnostics <- lapply(lints, diagnostic_from_lint, content = content)
    names(diagnostics) <- NULL
    diagnostics
}

diagnostics_callback <- function(self, uri, version, diagnostics) {
    if (is.null(diagnostics) || !self$workspace$documents$has(uri)) return(NULL)
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
    create_task(
        target = package_call(diagnose_file),
        args = list(uri = uri, content = content, cache = lsp_settings$get("lint_cache")),
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
