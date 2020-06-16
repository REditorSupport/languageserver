#' Merge two lists
#'
#' @keywords internal
merge_list <- function(x, y) {
    x[names(y)] <- y
    x
}

#' tryCatch with stack captured
#'
#' @keywords internal
tryCatchStack <- function(expr, ...) {
    expr <- substitute(expr)
    env <- parent.frame()
    capture_calls <- function(e) {
        calls <- sys.calls()
        ncalls <- length(calls)
        e$calls <- calls[-c(seq_len(frame + 7), ncalls - 1, ncalls)]
        class(e) <- c("errorWithStack", class(e))
        signalCondition(e)
    }
    frame <- sys.nframe()
    tryCatch(withCallingHandlers(eval(expr, env), error = capture_calls), ...)
}

print.errorWithStack <- function(x, ...) {
    cat("Error: ", conditionMessage(x), "\n", sep = "")

    call <- conditionCall(x)
    if (!is.null(call)) {
        cat("Call: ")
        print(call)
    }

    if (length(x$calls)) {
        cat("Stack trace:\n")
        rev_calls <- rev(x$calls)
        for (i in seq_along(rev_calls)) {
            cat(i, ": ", sep = "")
            print(rev_calls[[i]])
        }
    }
    invisible(x)
}

tryCatchTimeout <- function(expr, timeout = Inf, ...) {
    expr <- substitute(expr)
    envir <- parent.frame()
    setTimeLimit(timeout, transient = TRUE)
    on.exit(setTimeLimit())
    tryCatch(eval(expr, envir), ...)
}

capture_print <- function(x) {
    paste0(utils::capture.output(print(x)), collapse = "\n")
}

#' Paths and uris
#' @keywords internal
path_from_uri <- function(uri) {
    if (length(uri) == 0) {
        return(character())
    }
    if (!startsWith(uri, "file:///")) {
        return("")
    }
    start_char <- if (.Platform$OS.type == "windows") 9 else 8
    uri <- utils::URLencode(uri)
    path <- utils::URLdecode(substr(uri, start_char, nchar(uri)))
    Encoding(path) <- "UTF-8"
    path
}


#' @keywords internal
#' @rdname path_from_uri
path_to_uri <- function(path) {
    if (length(path) == 0) {
        return(character())
    }
    path <- path.expand(path)
    if (.Platform$OS.type == "windows") {
        prefix <- "file:///"
        path <- gsub("\\", "/", path, fixed = TRUE)
    } else {
        prefix <- "file://"
    }
    paste0(prefix, utils::URLencode(path))
}

#' Check if a file is an RMarkdown file
#' @keywords internal
is_rmarkdown <- function(uri) {
    filename <- path_from_uri(uri)
    endsWith(tolower(filename), ".rmd") || endsWith(tolower(filename), ".rmarkdown")
}

#' Check if a token is in a R code block in an Rmarkdown file
#'
#' In an RMarkdown document, tokens can be either inside an R code block or
#' in the text. This function will return `FALSE` if the token is in the text
#' and `TRUE` if it is in a code block. For any other files, it always returns `TRUE`.
#'
#' @keywords internal
check_scope <- function(uri, document, point) {
    if (is_rmarkdown(uri)) {
        row <- point$row
        flags <- vapply(
            document$content[1:(row + 1)], startsWith, logical(1), "```", USE.NAMES = FALSE)
        if (any(flags)) {
            last_match <- document$content[max(which(flags))]
            stringi::stri_detect_regex(last_match, "`{3,}\\s*\\{r[ ,\\}]") &&
                !identical(sum(flags) %% 2, 0) &&
                !enclosed_by_quotes(document, point)
        } else {
            FALSE
        }
    } else {
        !enclosed_by_quotes(document, point)
    }
}


fuzzy_find <- function(x, pattern) {
    subsequence_regex <- paste0(strsplit(pattern, "")[[1]], collapse = ".*")
    grepl(subsequence_regex, x, ignore.case = TRUE)
}


#' Safer version of `seq` which returns empty vector if b < a
#' @keywords internal
seq_safe <- function(a, b) {
    seq(a, b, length = max(0, b - a + 1))
}


#' Extract the R code blocks of a Rmarkdown file
#' @keywords internal
extract_blocks <- function(content) {
    begins_or_ends <- which(stringi::stri_detect_fixed(content, "```"))
    begins <- which(stringi::stri_detect_regex(content, "`{3,}\\s*\\{r[ ,\\}]"))
    ends <- setdiff(begins_or_ends, begins)
    blocks <- list()
    for (begin in begins) {
        z <- which(ends > begin)
        if (length(z) == 0) break
        end <- ends[min(z)]
        lines <- seq_safe(begin + 1, end - 1)
        if (length(lines) > 0) {
            blocks[[length(blocks) + 1]] <- list(lines = lines, text = content[lines])
        }
    }
    blocks
}

#' Strip out all the non R blocks in a R markdown file
#' @param content a character vector
#' @keywords internal
purl <- function(content) {
    blocks <- extract_blocks(content)
    rmd_content <- rep("", length(content))
    for (block in blocks) {
        rmd_content[block$lines] <- content[block$lines]
    }
    rmd_content
}


#' Calculate character offset based on the protocol
#' @keywords internal
ncodeunit <- function(s) {
    lengths(iconv(s, from = "UTF-8", to = "UTF-16BE", toRaw = TRUE)) / 2
}


#' Determine code points given code units
#'
#' @param line a character of text
#' @param units 0-indexed code points
#'
#' @keywords internal
code_point_from_unit <- function(line, units) {
    if (!nzchar(line)) return(units)
    offsets <- cumsum(ncodeunit(strsplit(line, "")[[1]]))
    loc_map <- match(seq_len(utils::tail(offsets, 1)), offsets)
    result <- c(0, loc_map)[units + 1]
    n <- nchar(line)
    result[units > length(loc_map)] <- n
    result[is.infinite(units)] <- n
    result
}

#' Determine code units given code points
#'
#' @param line a character of text
#' @param units 0-indexed code units
#'
#' @keywords internal
code_point_to_unit <- function(line, pts) {
    if (!nzchar(line)) return(pts)
    offsets <- c(0, cumsum(ncodeunit(strsplit(line, "")[[1]])))
    result <- offsets[pts + 1]
    n <- length(offsets)
    m <- offsets[n]
    result[pts >= n] <- m
    result[is.infinite(pts)] <- m
    result
}


#' Check if a path is a directory
#' @keywords internal
is_directory <- function(path) {
    is_dir <- file.info(path)$isdir
    !is.na(is_dir) && is_dir
}

#' Find the root package folder
#'
#' This function searches backwards in the folder structure until it finds
#' a DESCRIPTION file or it reaches the top-level directory.
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

#' check if a path is a package folder
#'
#' @param rootPath a character representing a path
#'
#' @keywords internal
is_package <- function(rootPath) {
    file <- file.path(rootPath, "DESCRIPTION")
    file.exists(file) && !dir.exists(file)
}

#' read a character from stdin
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

#' check if the current process becomes an orphan
#'
#' @keywords internal
process_is_detached <- function() {
    .Call("process_is_detached", PACKAGE = "languageserver")
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
#' @keywords internal
sanitize_names <- function(objects) {
    objects[stringi::stri_detect_regex(objects, "^(?:[^\\W_]|\\.)(?:[^\\W]|\\.)*$")]
}

na_to_empty_string <- function(x) if (is.na(x)) "" else x
empty_string_to_null <- function(x) if (nzchar(x)) x else NULL

look_forward <- function(text) {
    matches <- stringi::stri_match_first_regex(text, "^(?:[^\\W]|\\.)*\\b")[1]
    list(
        token = na_to_empty_string(matches[1])
    )
}

look_backward <- function(text) {
    matches <- stringi::stri_match_first_regex(
        text, "(?<!\\$)(?:\\b|(?=\\.))(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?((?:[^\\W_]|\\.)(?:[^\\W]|\\.)*)?$")
    list(
        full_token = na_to_empty_string(matches[1]),
        package  = na_to_empty_string(matches[2]),
        accessor = na_to_empty_string(matches[3]),
        token = na_to_empty_string(matches[4])
    )
}

str_trunc <- function(string, width, ellipsis = "...") {
    trunc <- !is.na(string) && nchar(string) > width
    if (trunc) {
        width2 <- width - nchar(ellipsis)
        paste0(substr(string, 1, width2), ellipsis)
    } else {
        string
    }
}

uncomment <- function(x) gsub("^\\s*#+'?\\s*", "", x)

find_doc_item <- function(doc, tag) {
    for (item in doc) {
        if (attr(item, "Rd_tag") == tag) {
            return(item)
        }
    }
}

convert_doc_to_markdown <- function(doc) {
    unlist(lapply(doc, function(item) {
        tag <- attr(item, "Rd_tag")
        if (is.null(tag)) {
            if (length(item)) {
                convert_doc_to_markdown(item)
            }
        } else if (tag == "\\R") {
            "**R**"
        } else if (tag == "\\dots") {
            "..."
        } else if (tag %in% c("\\code", "\\env", "\\eqn")) {
            sprintf("`%s`", paste0(convert_doc_to_markdown(item), collapse = ""))
        } else if (tag %in% c("\\ifelse", "USERMACRO")) {
            ""
        } else if (is.character(item)) {
            trimws(item)
        } else if (length(item)) {
            convert_doc_to_markdown(item)
        }
    }))
}

convert_doc_string <- function(doc) {
    paste0(convert_doc_to_markdown(doc), collapse = " ")
}

glue <- function(.x, ...) {
    param <- list(...)
    for (key in names(param)) {
        .x <- gsub(paste0("{", key, "}"), param[[key]], .x, fixed = TRUE)
    }
    .x
}

xdoc_find_enclosing_scopes <- function(x, line, col, top = FALSE) {
    if (top) {
        xpath <- "/exprlist | //expr[(@line1 < {line} or (@line1 = {line} and @col1 <= {col})) and
                (@line2 > {line} or (@line2 = {line} and @col2 >= {col}))]"
    } else {
        xpath <- "//expr[(@line1 < {line} or (@line1 = {line} and @col1 <= {col})) and
                (@line2 > {line} or (@line2 = {line} and @col2 >= {col}))]"
    }
    xpath <- glue(xpath, line = line, col = col)
    xml_find_all(x, xpath)
}

xdoc_find_token <- function(x, line, col) {
    xpath <- glue("//*[not(*)][(@line1 < {line} or (@line1 = {line} and @col1 <= {col})) and (@line2 > {line} or (@line2 = {line} and @col2 >= {col}))]",
        line = line, col = col)
    xml_find_first(x, xpath)
}

xml_single_quote <- function(x) {
    x <- gsub("'", "&apos;", x, fixed = TRUE)
    x
}
