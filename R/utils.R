#' check if a filename is a directory
#'
#' @param filename a character
#'
#' @keywords internal
is_directory <- function(filename) {
    is_dir <- file.info(filename)$isdir
    !is.na(is_dir) && is_dir
}

#' find the root package folder
#'
#' This function searches backwards in the folder structure until it finds
#' a DESCRIPTION file or it reaches the top-level directory.
#'
#' @param path a character
#'
#' @keywords internal
find_package <- function(path = getwd()) {
    start_path <- getwd()
    on.exit(setwd(start_path))
    if (!file.exists(path)) {
        return(NULL)
    }
    setwd(path)
    prev_path <- ""
    while (!file.exists(file.path(prev_path, "DESCRIPTION"))) {
        if (identical(prev_path, getwd())) {
            return(NULL)
        }
        prev_path <- getwd()
        setwd("..")
    }
    normalizePath(prev_path)
}

#' check if an URI is a package folder
#'
#' @param rootUri a character representing a URI
#'
#' @keywords internal
is_package <- function(rootUri) {
    file.exists(file.path(path_from_uri(rootUri), "DESCRIPTION"))
}

#' read a character from stdin
#'
#' @param n an integer
#'
#' @keywords internal
stdin_read_char <- function(n) {
    .Call("stdin_read_char", PACKAGE = "languageserver", n)
}

#' read a line from stdin
#'
#' @keywords internal
stdin_read_line <- function() {
    .Call("stdin_read_line", PACKAGE = "languageserver")
}

#' get the parent process pid
#'
#' @keywords internal
getppid <- function() {
    .Call("do_getppid", PACKAGE = "languageserver")
}

#' throttle a function execution
#'
#' Execute a function if the last execution time is older than a specified
#' value.
#'
#' @param fun the function to execute
#' @param t an integer, the threshold in seconds
#'
#' @keywords internal
throttle <- function(fun, t = 1) {
    last_execution_time <- 0
    function(...) {
        if (Sys.time() - last_execution_time > t) {
            last_execution_time <<- Sys.time()
            fun(...)
        }
    }
}

#' sanitize package objects names
#'
#' Remove unwanted objects, _e.g._ `names<-`, `%>%`, etc.
#'
#' @param objects a character vector
#'
#' @keywords internal
sanitize_names <- function(objects) {
    objects[stringr::str_detect(objects, "^(?:[a-zA-Z.][a-zA-Z0-9_.]*)?$")]
}

#' replace elements of a list
#'
#' @param x,y named lists
#'
#' @keywords internal
merge_list <- function(x, y) {
  x[names(y)] <- y
  x
}

#' transform any object to a string
#'
#' @param ... anything
#'
#' @keywords internal
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

#' check if a character vector looks like a function
#' 
#' @param text a character vector
#' 
#' @keywords internal
detect_function <- function(text) {
    matches <- stringr::str_match(text, "(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?([a-zA-Z0-9_.]*)$")
    list(
        package  = matches[2],
        accessor = matches[3],
        funct    = matches[4]
    )
}

#' create the logger
#'
#' @keywords internal
logger <- Logger$new()
