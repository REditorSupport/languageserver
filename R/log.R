

#' transform any object to a string
#'
#' @param ... anything
#'
#' @keywords internal
to_string <- function(...) {
    dots <- list(...)
    if (length(dots) > 0) {
        str <- sapply(
            dots, function(x) {
                tryCatch({
                if (length(x) > 1)
                    jsonlite::toJSON(x, auto_unbox = TRUE)
                else
                    x
                },
                error = function(e) x)
            })
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
