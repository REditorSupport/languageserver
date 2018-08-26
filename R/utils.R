is_directory <- function(filename) {
    is_dir <- file.info(filename)$isdir
    !is.na(is_dir) && is_dir
}

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

stdin_read_char <- function(n) {
    .Call("stdin_read_char", PACKAGE = "languageserver", n)
}

stdin_read_line <- function() {
    .Call("stdin_read_line", PACKAGE = "languageserver")
}

getppid <- function() {
    .Call("do_getppid", PACKAGE = "languageserver")
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

merge_list <- function(x, y) {
  x[names(y)] <- y
  x
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
    private = list(
        debug = FALSE
    ),
    public = list(
        debug_mode = function() {
            private$debug <- TRUE
        },
        error = function(...) {
            log_write(...)
        },
        info = function(...) {
            if (private$debug) log_write(...)
        }
    )
)

logger <- Logger$new()
