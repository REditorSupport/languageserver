#' diagnostics
#'
#' Diagnose problems in files after linting.
#'
#' @name diagnostics
#' @param result the result from [lintr::lint()]
NULL

#' @rdname diagnostics
diagnostic_range <- function(result) {
    line <- result$line_number - 1
    column <- result$column_number - 1
    if (is.null(result$ranges)) {
        range(
            start = list(line = line, character = column),
            end = list(line = line, character = column + 1)
        )
    } else {
        range(
            start = list(line = line, character = result$ranges[[1]][1] - 1),
            end = list(line = line, character = result$ranges[[1]][2])
        )
    }
}

#' @rdname diagnostics
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
diagnostic_from_lint <- function(result) {
    list(
        range = diagnostic_range(result),
        severity = diagnostic_severity(result),
        source = "lintr",
        message = result$message
    )
}

#' find the lintr config file
#'
#' This is a copy of `lintr:::find_config` since CRAN doesn't like :::
#'
#' @keywords internal
find_config <- function(filename) {
    if (is.null(filename)) {
        return(NULL)
    }
    linter_file <- getOption("lintr.linter_file")
    path <- if (is_directory(filename)) {
        filename
    } else {
        dirname(filename)
    }
    linter_config <- file.path(path, linter_file)
    if (isTRUE(file.exists(linter_config))) {
        return(linter_config)
    }
    project <- find_package(path)
    linter_config <- file.path(project, linter_file)
    if (isTRUE(file.exists(linter_config))) {
        return(linter_config)
    }
    # home_dir <- Sys.getenv("HOME", unset = "~")
    # linter_config <- file.path(home_dir, linter_file)
    # if (isTRUE(file.exists(linter_config))) {
    #     return(linter_config)
    # }
    NULL
}

#' run diagnostic on a file
#'
#' Lint and diagnose problems in a file.
#'
#' @param path a character, the path to a file
diagnose_file <- function(path) {
    if (is.null(find_config(path))) {
        linters <- getOption("languageserver.default_linters", NULL)
    } else {
        linters <- NULL
    }
    diagnostics <- lapply(lintr::lint(path, linters = linters), diagnostic_from_lint)
    names(diagnostics) <- NULL
    diagnostics
}
