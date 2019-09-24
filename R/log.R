

#' transform any object to a string
#'
#' @param ... anything
#'
#' @keywords internal
to_string <- function(...) {
    dots <- list(...)
    if (length(dots) > 0) {
        str <- vapply(
            dots, function(x) {
                if (is.atomic(x) || is.list(x)) {
                    if (is.list(x) || length(x) > 1) {
                        tryCatch(jsonlite::toJSON(x, auto_unbox = TRUE),
                            error = function(e) {
                                paste0(e$message, utils::capture.output(print(x)), collapse = "\n")
                            }
                        )
                    } else if (length(x) == 1) {
                        as.character(x)
                    } else {
                        ""
                    }
                } else {
                    utils::capture.output(print(x))
                }
            }, character(1L)
        )
    } else {
        str <- ""
    }
    paste0(paste(str, collapse = " "), "\n")
}

#' write to log
#'
#' @param ... anything
#'
#' @keywords internal
log_write <- function(..., file = stderr()){
    # cat(to_string(...), file = "/tmp/rls")
    cat(to_string(...), file = file)
}

#' a basic logger class
#'
#' @keywords internal
Logger <- R6::R6Class("Logger",
    private = list(
        debug = FALSE,
        file = stderr()
    ),
    public = list(
        debug_mode = function(debug) {
            if (isTRUE(debug) || is.character(debug)) {
                private$debug <- TRUE
                if (is.character(debug)) {
                    private$file <- debug
                }
            }
        },
        error = function(...) {
            log_write(..., file = private$file)
        },
        info = function(...) {
            if (private$debug) log_write(..., file = private$file)
        }
    )
)


# create the logger
logger <- Logger$new()
