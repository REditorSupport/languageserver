read_char <- function(con, n) {
    if (.Platform$OS.type == "windows" && "file" %in% class(con)) {
        .Call("stdin_read_char", PACKAGE = "languageserver", n)
    } else {
        readChar(con, n, useBytes = TRUE)
    }
}


read_line <- function(con) {
    if (.Platform$OS.type == "windows" && "file" %in% class(con)) {
        .Call("stdin_read_line", PACKAGE = "languageserver")
    } else {
        readLines(con, n = 1)
    }
}

getppid <- function() {
    .Call("do_getppid", PACKAGE = "languageserver")
}

document_backward_search <- function(document, line, character, char, skip_empty_line = TRUE) {
    .Call("document_backward_search", PACKAGE = "languageserver",
        document, line, character - 1, char, skip_empty_line)
}

leisurize <- function(fun, t = 1) {
    last_execuation_time <- 0
    function(...) {
        if (Sys.time() - last_execuation_time > t) {
            last_execuation_time <<- Sys.time()
            fun(...)
        }
    }
}

sanitize_names <- function(objects) {
    objects[stringr::str_detect(objects, "^(?:[a-zA-Z.][a-zA-Z0-9_.]*)?$")]
}


if (.Platform$OS.type == "windows") {
    path_from_uri <- function(uri) {
        utils::URLdecode(substr(uri, 9, nchar(uri)))
    }
} else {
    path_from_uri <- function(uri) {
        utils::URLdecode(substr(uri, 8, nchar(uri)))
    }
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
    if (length(str) > 0) {
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
    paste0(paste(str, collapse = ""), "\n")
}

log_write <- function(..., file = stderr()){
    # cat(to_string(...), file = "/tmp/rls")
    cat(to_string(...), file = file)
}

Logger <- R6::R6Class("Logger",
    public = list(
        debug_mode = function() {
            self$info <- function(...) {
                        log_write(...)
                    }
        },
        error = function(...) {
            log_write(...)
        },
        info = function(...) {
        }
    )
)

logger <- Logger$new()
