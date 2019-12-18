#' diagnostics
#'
#' Diagnose problems in files after linting.
#'
#' @name diagnostics
NULL

#' @rdname diagnostics
#' @keywords internal
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
#' @keywords internal
diagnostic_severity <- function(result) {
    if (result$type == "error") {
        severity <- 1
    } else if (result$type == "warning") {
        severity <- 2
    } else if (result$type == "style") {
        severity <- 3
    } else {
        severity <- 3
    }
    severity
}

#' @rdname diagnostics
#' @keywords internal
diagnostic_from_lint <- function(result, content) {
    list(
        range = diagnostic_range(result, content),
        severity = diagnostic_severity(result),
        source = "lintr",
        message = result$message
    )
}

#' Find the lintr config file
#' @keywords internal
find_config <- function(filename) {
    # instead of calling `lintr:::find_config` directly
    # since CRAN doesn't like :::.
    asNamespace("lintr")$find_config(filename)
}

#' Run diagnostic on a file
#'
#' Lint and diagnose problems in a file.
#' @keywords internal
diagnose_file <- function(path, content = NULL) {
    if (is.null(find_config(path))) {
        linters <- getOption("languageserver.default_linters", NULL)
    } else {
        linters <- NULL
    }
    if (is.null(content)) {
        content <- readr::read_lines(path)
        diagnostics <- lapply(
            lintr::lint(path, linters = linters), diagnostic_from_lint, content = content)
    } else {
        # use inline data
        text <- paste0(content, collapse = "\n")
        diagnostics <- lapply(
            lintr::lint(text, linters = linters), diagnostic_from_lint, content = content)
    }
    names(diagnostics) <- NULL
    diagnostics
}


diagnostics_callback <- function(self, uri, diagnostics) {
    if (is.null(diagnostics)) return(NULL)
    logger$info("diagnostics_callback called")
    self$deliver(
        Notification$new(
            method = "textDocument/publishDiagnostics",
            params = list(
                uri = uri,
                diagnostics = diagnostics
            )
        )
    )
}


diagnostics_task <- function(self, uri, document) {
    if (is.null(document)) {
        content <- NULL
    } else {
        content <- document$content
    }
    create_task(
        diagnose_file,
        list(path = path_from_uri(uri), content = content),
        callback = function(result) diagnostics_callback(self, uri, result))
}
