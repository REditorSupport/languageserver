diagnostic_range <- function(result) {
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

diagnostic_severity <- function(result) {
    if (result$type == "error") {
        severity <- 1
    } else {
        severity <- 2
    }
    severity
}


dianostic_from_lint <- function(result) {
    list(
        range = diagnostic_range(result),
        severity = diagnostic_severity(result),
        source = result$type,
        message = result$message
    )
}

diagnose_file <- function(path) {
    diagnostics <- lapply(lintr::lint(path), dianostic_from_lint)
    names(diagnostics) <- NULL
    diagnostics
}

diagnose_text <- function(text) {
    temp_file <- tempfile(fileext = ".R")
    write(text, file = temp_file)
    diagnostics <- diagnose_file(temp_file)
    file.remove(temp_file)
    diagnostics
}

diagnostic_reply <- function(uri, text=NULL) {
    if (is.null(text)) {
        path <- path_from_uri(uri)
        diagnostics <- diagnose_file(path)
    } else {
        diagnostics <- diagnose_text(text)
    }
    Notification$new(
        method = "textDocument/publishDiagnostics",
        params = list(
            uri = uri,
            diagnostics = diagnostics
        )
    )
}

diagnostic_add_queue <- function(self, uri, text=NULL) {
    self$deliver(diagnostic_reply(uri, text))
}
