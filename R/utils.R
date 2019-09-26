#' replace elements of a list
#'
#' @param x,y named lists
#'
#' @keywords internal
merge_list <- function(x, y) {
  x[names(y)] <- y
  x
}


#' paths and uris
#'
#' @param uri a character, the path to a file in URI format
path_from_uri <- function(uri) {
    if (is.null(uri)) {
        return(NULL)
    }
    start_char <- ifelse(.Platform$OS.type == "windows", 9, 8)
    utils::URLdecode(substr(uri, start_char, nchar(uri)))
}


#' @param path a character, the path to a file
#' @rdname path_from_uri
path_to_uri <- function(path) {
    if (is.null(path)) {
        return(NULL)
    }
    prefix <- ifelse(.Platform$OS.type == "windows", "file:///", "file://")
    paste0(prefix, utils::URLencode(path))
}

#' check if a file is an RMarkdown file
#'
#' @template uri
#'
#' @return a logical
is_rmarkdown <- function(uri) {
    filename <- path_from_uri(uri)
    endsWith(tolower(filename), ".rmd") || endsWith(tolower(filename), ".rmarkdown")
}

#' check if a token is in a R code block in an Rmarkdown file
#'
#' In an RMarkdown document, tokens can be either inside an R code block or
#' in the text. This function will return `FALSE` if the token is in the text
#' and `TRUE` if it is in a code block. For R scripts, it always returns `TRUE`.
#'
#' @template uri
#' @template document
#' @template position
#'
#' @return a logical
check_scope <- function(uri, document, position) {
    if (is_rmarkdown(uri)) {
        line <- position$line
        !identical(sum(vapply(
            document$content[1:(line + 1)], startsWith, integer(1), "```")) %% 2, 0)
    } else {
        TRUE
    }
}


#' calculate character offset based on the protocol
#'
#' @param s a character / character vector
#'
#' @keywords internal
ncodeunit <- function(s) {
    lengths(iconv(s, to = "UTF-16BE", toRaw = TRUE)) / 2
}


#' determinal code units given code points
#'
#' @param line a character of text
#' @param pts 0-indexed code points
#'
#' @keywords internal
code_point_to_unit <- function(line, pts) {
    if (!nzchar(line)) return(pts)
    offsets <- cumsum(ncodeunit(strsplit(line, "")[[1]]))
    loc_map <- match(seq_len(utils::tail(offsets, 1)), offsets)
    result <- c(0, loc_map)[pts + 1]
    result[is.infinite(pts)] <- nchar(line)
    result
}


#' determinal utf16 code points given utf8 code points
#'
#' @param line a character of text
#' @param pts 0-indexed code points
#'
#' @keywords internal
utf8_to_utf16_code_point <- function(line, pts) {
    utf16cp <- c(0, cumsum(ncodeunit(strsplit(line, "")[[1]])))
    utf8cp <- c(0, cumsum(nchar(strsplit(line, "")[[1]], type = "bytes")))
    utf16cp[match(pts, utf8cp)]
}


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
    objects[stringr::str_detect(objects, "^(?:[^\\W_]|\\.)(?:[^\\W]|\\.)*$")]
}

na_to_empty_string <- function(x) if (is.na(x)) "" else x
empty_string_to_null <- function(x) if (nzchar(x)) x else NULL

look_forward <- function(text) {
    matches <- stringr::str_match(text, "^(?:[^\\W]|\\.)*\\b")[1]
    list(
        token = na_to_empty_string(matches[1])
    )
}

look_backward <- function(text) {
    matches <- stringr::str_match(
        text, "\\b(?<!\\$)(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?((?:[^\\W_]|\\.)(?:[^\\W]|\\.)*)?$")
    list(
        full_token = na_to_empty_string(matches[1]),
        package  = na_to_empty_string(matches[2]),
        accessor = na_to_empty_string(matches[3]),
        token = na_to_empty_string(matches[4])
    )
}
