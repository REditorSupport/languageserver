#' Run a CLI with a UTF-8 buffer and independent output streams
#' @noRd
backend_cli_run <- function(request, arguments) {
    # Spool stdin to a private file so writing a large buffer cannot deadlock
    # against a child writing to stdout/stderr. Its original path is never written.
    input <- tempfile("backend-stdin-")
    on.exit(unlink(input), add = TRUE)
    writeBin(charToRaw(enc2utf8(paste(request$content, collapse = "\n"))), input)
    environment <- request$env
    if (length(environment)) environment <- c("current", environment) else environment <- NULL
    result <- tryCatch(processx::run(request$executable, arguments, stdin = input,
            wd = request$working_directory, env = environment, timeout = request$timeout,
            stdout = "|", stderr = "|", encoding = "UTF-8", error_on_status = FALSE,
            cleanup_tree = TRUE, supervise = TRUE, windows_hide_window = TRUE),
        error = function(error) {
            if (inherits(error, "system_command_timeout_error")) {
                backend_error(sprintf("Backend '%s' timed out after %s seconds.",
                        request$backend$id, request$timeout))
            }
            backend_error(sprintf("Could not execute backend '%s': %s", request$backend$id, conditionMessage(error)))
        })
    if (isTRUE(result$timeout)) backend_error(sprintf("Backend '%s' timed out after %s seconds.",
            request$backend$id, request$timeout))
    result
}

backend_cli_failure <- function(request, result) {
    detail <- trimws(result$stderr)
    backend_error(sprintf("Backend '%s' failed (exit status %s)%s", request$backend$id,
            result$status, if (nzchar(detail)) paste0(": ", detail) else "."))
}

#' Convert half-open UTF-8 byte offsets against the submitted buffer
#' @noRd
backend_byte_range <- function(content, offsets) {
    bytes <- charToRaw(enc2utf8(paste(content, collapse = "\n")))
    offsets <- unlist(offsets, use.names = FALSE)
    if (!is.numeric(offsets) || length(offsets) != 2L || anyNA(offsets) ||
            any(!is.finite(offsets)) || any(offsets != floor(offsets)) ||
            offsets[[1L]] < 0 || offsets[[2L]] < offsets[[1L]] || offsets[[2L]] > length(bytes)) {
        backend_error("CLI diagnostic contains an invalid byte range.")
    }
    starts <- c(0L, which(bytes == as.raw(10L)))
    positions <- lapply(offsets, function(offset) {
        line <- findInterval(offset, starts) - 1L
        start <- starts[[line + 1L]]
        prefix <- if (offset == start) "" else rawToChar(bytes[seq.int(start + 1L, offset)])
        if (!validUTF8(prefix)) backend_error("CLI diagnostic splits a UTF-8 character.")
        position(line, sum(ifelse(utf8ToInt(prefix) > 65535L, 2L, 1L)))
    })
    range(positions[[1L]], positions[[2L]])
}

backend_text_edits <- function(request, text) {
    old <- paste(request$content, collapse = "\n")
    # Documents store logical lines; retain the server's LF edit convention.
    text <- gsub("\r\n", "\n", text, fixed = TRUE)
    if (identical(text, old)) return(list())
    last <- length(request$content)
    ending <- if (last) position(last - 1L, code_point_to_unit(request$content[[last]],
            nchar(request$content[[last]]))) else position(0L, 0L)
    list(text_edit(range(position(0L, 0L), ending), text))
}

air_formatting <- function(request) {
    result <- backend_cli_run(request, c("format", "--stdin-file-path", request$path))
    if (!identical(result$status, 0L)) backend_cli_failure(request, result)
    if (nzchar(trimws(paste(request$content, collapse = "\n"))) && !nzchar(result$stdout)) {
        backend_error("Air returned an empty buffer for nonempty input.")
    }
    backend_text_edits(request, result$stdout)
}

jarl_diagnostics <- function(request) {
    result <- backend_cli_run(request, c("check", "--stdin-file-path", request$path,
            "--output-format", "json"))
    # Jarl exits before emitting JSON when a virtual path is excluded.
    if (identical(result$status, 0L) && !nzchar(trimws(result$stdout))) return(list())
    if (!result$status %in% c(0L, 1L, 255L)) backend_cli_failure(request, result)
    output <- tryCatch(jsonlite::fromJSON(result$stdout, simplifyVector = FALSE),
        error = function(e) backend_error("Jarl returned malformed JSON diagnostics."))
    if (!is.list(output) || !all(c("diagnostics", "errors") %in% names(output)) ||
            !is.list(output$diagnostics) || !is.list(output$errors)) {
        backend_error("Jarl JSON must contain diagnostics and errors arrays.")
    }
    diagnostics <- lapply(output$diagnostics, function(item) {
        if (!is.list(item) || !backend_string(item$message$body) || !backend_string(item$message$name)) {
            backend_error("Jarl returned an invalid diagnostic.")
        }
        list(range = backend_byte_range(request$content, item$range),
            severity = DiagnosticSeverity$Warning, source = "jarl",
            code = item$message$name, message = item$message$body,
            codeDescription = list(href = paste0("https://jarl.etiennebacher.com/rules/", item$message$name)))
    })
    for (error in output$errors) {
        if (!is.list(error) || !length(error$syntax_errors) || !is.list(error$syntax_errors)) {
            if (is.list(error) && backend_string(error$error) && !nzchar(trimws(result$stderr))) {
                result$stderr <- error$error
            }
            backend_cli_failure(request, result)
        }
        for (syntax in error$syntax_errors) {
            if (!backend_string(syntax$message)) backend_error("Jarl returned an invalid syntax error.")
            diagnostics[[length(diagnostics) + 1L]] <- list(
                range = backend_byte_range(request$content, syntax$range),
                severity = DiagnosticSeverity$Error, source = "jarl", message = syntax$message)
        }
    }
    if (result$status == 255L && !length(output$errors)) backend_cli_failure(request, result)
    if (result$status == 1L && !length(diagnostics)) backend_error("Jarl reported findings without diagnostics.")
    diagnostics
}
