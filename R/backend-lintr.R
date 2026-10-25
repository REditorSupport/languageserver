#' diagnostics
#'
#' Diagnose problems in files after linting.
#'
#' @name diagnostics
NULL

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
lintr_configuration_paths <- function(path, working_directory, r_options) {
    filename <- r_options[["lintr.linter_file"]]
    if (is.null(filename)) filename <- Sys.getenv("R_LINTR_LINTER_FILE", ".lintr")
    if (!backend_string(filename)) backend_error("lintr.linter_file must be one configuration filename.")
    directories <- backend_ancestors(if (nzchar(path)) dirname(path) else working_directory)
    if (fs::is_absolute_path(filename)) {
        candidates <- filename
    } else {
        variants <- c(paste0(filename, ".R"), filename)
        candidates <- unlist(lapply(directories, function(directory) {
            c(file.path(directory, variants), file.path(directory, ".github", "linters", variants))
        }),
        use.names = FALSE)
        candidates <- c(candidates, file.path(path.expand("~"), filename),
            file.path(tools::R_user_dir("lintr", "config"), "config"))
    }
    # Let lintr resolve the active file and its precedence; candidate paths also
    # detect creation of a nearer config that did not exist at dispatch time.
    if (requireNamespace("lintr", quietly = TRUE)) {
        previous <- base::options(lintr.linter_file = filename)
        on.exit(base::options(previous), add = TRUE)
        resolved <- asNamespace("lintr")$find_config(if (nzchar(path)) path else working_directory)
        candidates <- c(resolved, candidates)
    }
    unique(candidates)
}
