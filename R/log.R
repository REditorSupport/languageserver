#' Transform any object to a string
#'
#' @param ... anything
#'
#' @noRd
to_string <- function(...) {
    dots <- list(...)
    if (length(dots) > 0) {
        str <- vapply(
            dots, function(x) {
                if (inherits(x, "error")) {
                    capture_print(x)
                } else if (is.atomic(x) || is.list(x)) {
                    if (is.list(x) || length(x) > 1) {
                        jsonlite::toJSON(
                            x, auto_unbox = TRUE, force = TRUE, null = "null", pretty = TRUE)
                    } else if (length(x) == 1) {
                        as.character(x)
                    } else {
                        ""
                    }
                } else {
                    capture_print(x)
                }
            }, character(1L), USE.NAMES = FALSE
        )
    } else {
        str <- ""
    }
    paste0(paste(str, collapse = " "), "\n")
}

#' Write to log
#'
#' @param ... anything
#'
#' @noRd
log_write <- function(..., log_file = NULL) {
    if (is.null(log_file)) {
        log_file <- stderr()
    }
    txt <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), "] ", to_string(...))
    # writeLines doesn't support `append`
    # writeLines(txt, log_file, sep = "", useBytes = TRUE)
    cat(txt, file = log_file, append = TRUE)
}

#' A basic logger class
#'
#' @noRd
Logger <- R6::R6Class("Logger",
    public = list(
        error = function(...) {
            log_write(..., log_file = lsp_settings$get("log_file"))
        },
        info = function(...) {
            if (lsp_settings$get("debug")) log_write(..., log_file = lsp_settings$get("log_file"))
        }
    )
)


# create the logger
logger <- Logger$new()
