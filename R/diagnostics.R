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
    } else if (result$type == "warning") {
        severity <- 2
    } else if (result$type == "style") {
        severity <- 3
    }
    severity
}

dianostic_from_lint <- function(result) {
    list(
        range = diagnostic_range(result),
        severity = diagnostic_severity(result),
        source = "lintr",
        message = result$message
    )
}

diagnose_file <- function(path) {
    diagnostics <- lapply(lintr::lint(path), dianostic_from_lint)
    names(diagnostics) <- NULL
    diagnostics
}
