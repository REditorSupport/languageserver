parse_uri <- function(uri) {
    substr(uri, 8, nchar(uri))
}

str_empty <- function(s) {
    trimws(s) == ""
}

document_line <- function(document, lineno) {
    if (lineno <= length(document)) {
        line <- document[[lineno]]
    } else {
        line <- ""
    }
    line
}

log_write <- function(..., file = stderr()){
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
    cat(paste0(paste(str, collapse = ""), "\n"), file = file)
}

Logger <- R6::R6Class("Logger",
    public = list(
        debug = NULL,
        set_mode = function(debug) {
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

logger <- Logger$new()
