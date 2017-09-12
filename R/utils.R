parse_uri <- function(uri) {
    stringr::str_sub(uri, start = 8)
}

str_empty <- function(s) {
    stringr::str_length(stringr::str_trim(s)) == 0
}

log_write <- function(...){
    dots <- list(...)
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
    cat(paste0(paste(str, collapse = ""), "\n"), file = stderr())
}

Logger <- R6::R6Class("Logger",
    public = list(
        debug = NULL,
        initialize = function(debug) {
            self$debug <- debug
        },
        error = function(...){
            log_write(...)
        },
        info = function(...){
            if (self$debug) {
                log_write(...)
            }
        }
    )
)

Logging <- R6::R6Class("Logging",
    public = list(
        logger = NULL,
        set_logger = function(debug) {
            self$logger <- Logger$new(debug)
            self$logger
        },
        get_logger = function() {
            self$logger
        }
    )
)

logging <- Logging$new()
