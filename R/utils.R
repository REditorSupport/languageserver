str_empty <- function(s) {
    trimws(s) == ""
}

path_from_uri <- function(uri) {
    substr(uri, 8, nchar(uri))
}

document_line <- function(document, lineno) {
    if (lineno <= length(document)) {
        line <- document[lineno]
    } else {
        line <- ""
    }
    line
}

to_string <- function(...) {
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
    paste0(paste(str, collapse = ""), "\n")
}

log_write <- function(..., file = stderr()){
    cat(to_string(...), file = file)
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
