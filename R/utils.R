is_rmarkdown <- function(uri) {
    filename <- path_from_uri(uri)
    endsWith(tolower(filename), "rmd") || endsWith(tolower(filename), "rmarkdown")
}

check_scope <- function(uri, document, line) {
    if (is_rmarkdown(uri)) {
        !identical(sum(sapply(document[1:(line + 1)], function(x) startsWith(x, "```"))) %% 2, 0)
    } else {
        TRUE
    }
}

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
