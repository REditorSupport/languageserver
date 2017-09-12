result_range <- function(result) {
    line <- result$line_number - 1
    column <- result$column_number - 1
    if (is.null(result$ranges)) {
        list(
            start = list(line = line, character = column),
            end = list(line = line, character = column + 1)
        )
    } else {
        list(
            start = list(line = line, character = result$ranges[[1]][1] - 1),
            end = list(line = line, character = result$ranges[[1]][2])
        )
    }
}

result_severity <- function(result) {
    if (result$type == "error") {
        severity <- 1
    } else {
        severity <- 2
    }
    severity
}


lint_to_diagnostic <- function(result) {
    list(
        range = result_range(result),
        severity = result_severity(result),
        source = result$type,
        message = result$message
    )
}

diagnose_file <- function(path) {
    diagnostics <- lapply(lintr::lint(path), lint_to_diagnostic)
    names(diagnostics) <- NULL
    diagnostics
}

publish_diagnostics <- function(self, uri) {
    path <- parse_uri(uri)
    diagnostics <- diagnose_file(path)
    self$deliver(Notification$new(
        method = "textDocument/publish_diagnostics",
        params = list(
            uri = uri,
            diagnostics = diagnostics
        )
    ))
}
