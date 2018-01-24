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

diagnostic_queue <- new.env()
diagnostic_processes <- new.env()

diagnostic_queue_add <- function(self, uri, text=NULL) {
    diagnostic_queue[[uri]] <- text
}

process_diagnostic_queue <- function(){
    for (uri in names(diagnostic_queue)) {
        text <- diagnostic_queue[[uri]]
        rm(list = uri, envir = diagnostic_queue)
        if (is.null(text)) {
            lintfile <- path_from_uri(uri)
        } else {
            lintfile <- tempfile(fileext = ".R")
            write(text, file = lintfile)
        }
        dprocess <- callr::r_bg(
            function(f, remove) {
                d <- tryCatch({
                    languageserver:::diagnose_file(f)
                }, error = function(e) list())
                if (remove) file.remove(f)
                d
            },
            list(f = lintfile, remove = !is.null(text)))
        diagnostic_processes[[uri]] <- dprocess
    }
}

process_diagnostic_notifications <- function(self) {
    for (uri in names(diagnostic_processes)) {
        dprocess <- diagnostic_processes[[uri]]
        if (dprocess$is_alive()) next
        rm(list = uri, envir = diagnostic_processes)
        logger$info("process_diagnostic: ", uri)
        diagnostics <- dprocess$get_result()
        self$deliver(Notification$new(
            method = "textDocument/publishDiagnostics",
            params = list(
                uri = uri,
                diagnostics = diagnostics
            )
        ))
    }
}

process_diagnostics <- function(self)  {
    process_diagnostic_queue()
    process_diagnostic_notifications(self)
}
