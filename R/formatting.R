#' Format an immutable document snapshot in a background worker
#' @noRd
format_document <- function(snapshot, options, range = NULL, ranges = NULL,
    formatting_options = list(), working_directory = getwd(), invocation = NULL) {
    if (is.null(invocation)) {
        descriptor <- backend_descriptor("formatting", "styler")
        inherited_options <- backend_r_options(descriptor)
        for (key in names(formatting_options)) inherited_options[key] <- formatting_options[key]
        invocation <- list(kind = "formatting", backend = descriptor,
            uri = snapshot$uri, language = snapshot$language, version = snapshot$version,
            content = snapshot$content, document_type = if (is_rmarkdown(snapshot$uri, snapshot$language))
                "literate" else "r", mode = if (!is.null(ranges)) "ranges" else if (!is.null(range)) "range" else "document",
            options = options, range = range, ranges = ranges, r_options = inherited_options,
            working_directory = working_directory)
    }
    run_backend(invocation)
}

#' Complete formatting only while its document snapshot is still current
#' @noRd
formatting_callback <- function(self, request, result = NULL, error = NULL) {
    key <- as.character(request$id)
    if (!identical(self$formatting_requests$get(key, NULL), request)) {
        return(invisible(NULL))
    }
    self$formatting_requests$remove(key)
    workspace <- self$get_workspace(request$uri)
    current <- workspace$documents$get(request$uri, NULL)
    if (is.null(current) || !identical(current, request$document) ||
            !identical(current$version, request$version) ||
            !identical(current$content, request$content) ||
            !identical(current$is_open, request$is_open) ||
            !backend_request_current(self, request$invocation, request$document,
                request$workspace, request$backend_generation)) {
        reply <- ResponseErrorMessage$new(request$id, "RequestCancelled",
            "Request superseded by updated document content")
    } else if (!is.null(error)) {
        logger$info("formatting task error: ", error)
        reply <- ResponseErrorMessage$new(request$id, "InternalError",
            paste0("Background formatting failed: ", backend_error_message(error)))
    } else {
        reply <- Response$new(request$id, result)
    }
    self$deliver(reply)
    invisible(NULL)
}

#' Queue full-document and explicit-selection formatting without blocking input
#' @noRd
enqueue_formatting <- function(self, id, uri, document, options,
    range = NULL, ranges = NULL) {
    workspace <- self$get_workspace(uri)
    invocation <- tryCatch(backend_request("formatting", uri, document, workspace,
            mode = if (!is.null(ranges)) "ranges" else if (!is.null(range)) "range" else "document",
            options = options, range = range, ranges = ranges), error = function(error) {
            self$deliver(ResponseErrorMessage$new(id, "InternalError", conditionMessage(error)))
            NULL
        })
    if (is.null(invocation)) return(invisible(NULL))
    backend_track(self, invocation, document, workspace)
    if (!backend_supported(invocation)) return(self$deliver(Response$new(id, list())))
    request <- list(id = id, uri = uri, version = document$version,
        content = document$content, is_open = document$is_open, document = document,
        token = new.env(parent = emptyenv()), invocation = invocation, workspace = workspace,
        backend_generation = backend_generation(self, "formatting"))
    snapshot <- list(uri = uri, language = document$language,
        version = document$version, content = document$content)
    task <- create_task(target = package_call(format_document),
        args = list(snapshot = snapshot, options = options, range = range, ranges = ranges,
            formatting_options = invocation$r_options, working_directory = invocation$working_directory,
            invocation = invocation),
        callback = function(result) formatting_callback(self, request, result),
        error = function(error) formatting_callback(self, request, error = error))
    key <- as.character(id)
    self$formatting_requests$set(key, request)
    self$formatting_task_manager$add_task(key, task)
    invisible(NULL)
}

#' Cancel pending or running formatting and send exactly one response
#' @noRd
cancel_formatting_requests <- function(self, uri = NULL, id = NULL,
    message = "Request cancelled by client") {
    if (is.null(self$formatting_requests)) return(invisible(NULL))
    for (key in self$formatting_requests$keys()) {
        request <- self$formatting_requests$get(key)
        if ((!is.null(uri) && !identical(request$uri, uri)) ||
                (!is.null(id) && !identical(key, as.character(id)))) next
        self$formatting_requests$remove(key)
        self$formatting_task_manager$cancel(key)
        self$deliver(ResponseErrorMessage$new(
            request$id, "RequestCancelled", message))
    }
    invisible(NULL)
}

#' Return a conservative indentation-only edit
#' @noRd
indentation_only_edits <- function(document, point, options,
    start_line = 1L) {
    row <- point$row
    if (row < 0L || row >= document$nline) return(NULL)

    line <- document$line0(row)
    if (!grepl("^\\s*$", line)) return(NULL)

    tab_size <- options$tabSize
    if (is.null(tab_size) || length(tab_size) != 1L || is.na(tab_size) ||
            tab_size < 1L) {
        tab_size <- 2L
    }
    indent_unit <- if (isFALSE(options$insertSpaces)) {
        "\t"
    } else {
        strrep(" ", tab_size)
    }

    scoped_content <- document$content[seq.int(start_line, row + 1L)]
    scoped_row <- row - start_line + 1L
    result <- find_unbalanced_bracket(
        scoped_content,
        scoped_row,
        nchar(line) - 1L
    )
    location <- result[[1L]]

    if (all(location >= 0L)) {
        context_row <- location[[1L]] + start_line - 1L
        context_line <- document$line0(context_row)
        base <- stringi::stri_extract_first_regex(context_line, "^\\s*")
        indentation <- paste0(base, indent_unit)
    } else {
        previous <- row - 1L
        minimum_row <- start_line - 1L
        while (previous >= minimum_row && !grepl("\\S", document$line0(previous))) {
            previous <- previous - 1L
        }
        if (previous < minimum_row) return(NULL)

        previous_line <- document$line0(previous)
        indentation <- stringi::stri_extract_first_regex(previous_line, "^\\s*")
        if (is_incomplete_line(previous_line)) {
            indentation <- paste0(indentation, indent_unit)
        }
    }

    if (identical(line, indentation)) return(NULL)

    edit_range <- range(
        start = document$to_lsp_position(row = row, col = 0L),
        end = document$to_lsp_position(row = row, col = nchar(line))
    )
    list(text_edit(range = edit_range, new_text = indentation))
}


formatting_reply <- function(id, uri, document, options) {
    Response$new(id, run_backend(backend_request("formatting", uri, document, options = options)))
}

range_formatting_reply <- function(id, uri, document, range, options) {
    Response$new(id, run_backend(backend_request("formatting", uri, document,
                mode = "range", options = options, range = range)))
}

ranges_formatting_reply <- function(id, uri, document, ranges, options) {
    Response$new(id, run_backend(backend_request("formatting", uri, document,
                mode = "ranges", options = options, ranges = ranges)))
}

indentation_only_reply <- function(id, document, point, options, start_line = 1L) {
    Response$new(id, indentation_only_edits(document, point, options, start_line))
}

on_type_formatting_reply <- function(id, uri, document, point, ch, options) {
    tryCatch({
        request <- backend_request("formatting", uri, document, mode = "on_type",
            options = options, point = point, ch = ch)
        if (backend_supported(request)) return(Response$new(id, run_backend(request)))
        if (!check_scope(uri, document, point) || ch != "\n") return(Response$new(id))
        run <- if (document$is_rmarkdown) literate_r_run_at(document$regions, point$row)
        else list(start_line = 1L)
        if (is.null(run)) return(Response$new(id))
        indentation_only_reply(id, document, point, options, run$start_line)
    }, error = function(error) ResponseErrorMessage$new(id, "InternalError", conditionMessage(error)))
}
