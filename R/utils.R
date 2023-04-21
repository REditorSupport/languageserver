#' Merge two lists
#'
#' @noRd
merge_list <- function(x, y) {
    x[names(y)] <- y
    x
}

#' tryCatch with stack captured
#'
#' @noRd
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

get_expr_type <- function(expr) {
    if (is.call(expr)) {
        func <- deparse(expr[[1]], nlines = 1)
        if (func == "function") {
            "function"
        } else if (func %in% c("c", "matrix", "array")) {
            "array"
        } else if (func == "list") {
            "list"
        } else if (grepl("(R6:::?)?R6Class", func)) {
            "class"
        } else {
            "variable"
        }
    } else {
        typeof(expr)
    }
}

uri_escape_unicode <- function(uri) {
    if (length(uri) == 0) {
        return(character())
    }
    if (.Platform$OS.type == "windows") {
        uri <- utils::URLdecode(uri)
        Encoding(uri) <- "UTF-8"
        utils::URLencode(uri)
    } else {
        utils::URLencode(uri)
    }
}

#' Paths and uris
#' @noRd
path_from_uri <- function(uri) {
    if (length(uri) == 0) {
        return(character())
    }
    if (!startsWith(uri, "file:///")) {
        return("")
    }
    # URLdecode gives unknown encoding, we need to mark them as UTF-8
    start_char <- if (.Platform$OS.type == "windows") 9 else 8
    path <- utils::URLdecode(substr(uri, start_char, nchar(uri)))
    Encoding(path) <- "UTF-8"
    path
}

#' @noRd
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


path_has_parent <- function(x, y) {
    if (.Platform$OS.type == "windows") {
        tryCatch(
            # https://github.com/REditorSupport/languageserver/issues/279
            fs::path_has_parent(x, y),
            error = function(e) {
                # try encode to native encoding
                fs::path_has_parent(enc2native(x), enc2native(y))
            }
        )
    } else {
        fs::path_has_parent(x, y)
    }
}

with_wd <- function(wd, expr) {
    if (is.null(wd)) {
        wd <- getwd()
    }
    oldwd <- setwd(wd)
    on.exit(setwd(oldwd))
    expr
}

equal_position <- function(x, y) {
    x$line == y$line && x$character == y$character
}

equal_range <- function(x, y) {
    equal_position(x$start, y$start) && equal_position(x$end, y$end)
}

equal_definition <- function(x, y) {
    x$uri == y$uri && equal_range(x$range, y$range)
}

#' Check if a file is an RMarkdown file
#' @noRd
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
#' @noRd
check_scope <- function(uri, document, point) {
    if (document$is_rmarkdown) {
        row <- point$row
        flags <- startsWith(document$content[1:(row + 1)], "```")
        if (any(flags)) {
            last_match <- document$content[max(which(flags))]
            stringi::stri_detect_regex(last_match, "```+\\s*\\{[rR][ ,\\}]") &&
                !identical(sum(flags) %% 2, 0) &&
                !enclosed_by_quotes(document, point)
        } else {
            FALSE
        }
    } else {
        !enclosed_by_quotes(document, point)
    }
}

match_with <- function(x, token) {
    pattern <- gsub(".", "\\.", token, fixed = TRUE)
    grepl(pattern, x, ignore.case = TRUE)
}

fuzzy_find <- function(x, pattern) {
    subsequence_regex <- gsub("(.)", "\\1.*", pattern)
    grepl(subsequence_regex, x, ignore.case = TRUE)
}


#' Safer version of `seq` which returns empty vector if b < a
#' @noRd
seq_safe <- function(a, b) {
    seq(a, b, length = max(0, b - a + 1))
}


#' Extract the R code blocks of a Rmarkdown file
#' @noRd
extract_blocks <- function(content) {
    begins_or_ends <- which(stringi::stri_detect_fixed(content, "```"))
    begins <- which(stringi::stri_detect_regex(content, "```+\\s*\\{[rR][ ,\\}]"))
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

get_signature <- function(symbol, expr) {
    signature <- format(expr[1:2])
    signature <- paste0(trimws(signature, which = "left"), collapse = "")
    signature <- gsub("^function\\s*", symbol, signature)
    signature <- gsub("\\s*NULL$", "", signature)
    signature
}

#' Strip out all the non R blocks in a R markdown file
#' @param content a character vector
#' @noRd
purl <- function(content) {
    blocks <- extract_blocks(content)
    rmd_content <- rep("", length(content))
    for (block in blocks) {
        rmd_content[block$lines] <- content[block$lines]
    }
    rmd_content
}


#' Calculate character offset based on the protocol
#' @noRd
ncodeunit <- function(s) {
    lengths(iconv(s, from = "UTF-8", to = "UTF-16BE", toRaw = TRUE)) / 2
}


#' Determine code points given code units
#'
#' @param line a character of text
#' @param units 0-indexed code points
#'
#' @noRd
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
#' @noRd
code_point_to_unit <- function(line, pts) {
    if (!nzchar(line)) return(pts)
    pts[pts < 0] <- 0
    offsets <- c(0, cumsum(ncodeunit(strsplit(line, "")[[1]])))
    result <- offsets[pts + 1]
    n <- length(offsets)
    m <- offsets[n]
    result[pts >= n] <- m
    result[!is.finite(pts)] <- m
    result
}


#' Check if a path is a directory
#' @noRd
is_directory <- function(path) {
    is_dir <- file.info(path)$isdir
    !is.na(is_dir) && is_dir
}

#' Find the root package folder
#'
#' This function searches backwards in the folder structure until it finds
#' a DESCRIPTION file or it reaches the top-level directory.
#'
#' @noRd
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
#' @noRd
is_package <- function(rootPath) {
    file <- file.path(rootPath, "DESCRIPTION")
    length(file) && file.exists(file) && !dir.exists(file)
}

get_root_path_for_uri <- function(uri, rootPath) {
    if (length(rootPath)) {
        # valid workspace folder
        rootPath
    } else if (nzchar(path <- path_from_uri(uri))) {
        # null workspace folder
        dirname(path)
    } else {
        # untitled document
        getwd()
    }
}

#' read a character from stdin
#'
#' @noRd
stdin_read_char <- function(n) {
    .Call("stdin_read_char", PACKAGE = "languageserver", n)
}

#' read a line from stdin
#'
#' @noRd
stdin_read_line <- function() {
    .Call("stdin_read_line", PACKAGE = "languageserver")
}

#' check if the current process becomes an orphan
#'
#' @noRd
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
#' @noRd
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
#' Remove unwanted objects, _e.g._ `names<-`, `%>%`, `.__C_` etc.
#'
#' @noRd
sanitize_names <- function(objects) {
    objects[stringi::stri_detect_regex(objects, "^([^\\W_]|\\.(?!_))(\\w|\\.)*$")]
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

convert_comment_to_documentation <- function(comment) {
    result <- NULL
    roxy <- tryCatch(roxygen2::parse_text(c(
        comment,
        "NULL"
    ), env = NULL), error = function(e) NULL)
    if (length(roxy)) {
        result <- list(
            title = NULL,
            description = NULL,
            arguments = list(),
            markdown = NULL
        )
        items <- lapply(roxy[[1]]$tags, function(item) {
            if (item$tag == "title") {
                result$title <<- item$val
            } else if (item$tag == "description") {
                result$description <<- item$val
            } else if (item$tag == "param") {
                result$arguments[[item$val$name]] <<- item$val$description
            }

            format_roxy_tag(item)
        })
        if (is.null(result$description)) {
            result$description <- result$title
        }
        result$markdown <- paste0(items, collapse = "\n")
    }
    if (is.null(result)) {
        result <- paste0(uncomment(comment), collapse = "  \n")
    }
    result
}

format_roxy_tag <- function(item) {
    if (item$tag %in% c("title", "description")) {
        tag <- ""
        content <- format_roxy_text(item$val)
    } else {
        tag <- sprintf("`@%s` ", item$tag)
        content <- if (item$tag %in% c("usage", "example", "examples", "eval", "evalRd")) {
            sprintf("\n```r\n%s\n```", trimws(item$raw))
        } else if (is.character(item$val) && length(item$val) > 1) {
            paste0(sprintf("`%s`", item$val), collapse = " ")
        } else if (is.list(item$val)) {
            sprintf("`%s` %s", item$val$name, format_roxy_text(item$val$description))
        } else {
            format_roxy_text(item$raw)
        }
    }
    paste0(tag, content, "  \n")
}

format_roxy_text <- function(text) {
    gsub("\n", "  \n", text, fixed = TRUE)
}

find_doc_item <- function(doc, tag) {
    for (item in doc) {
        if (attr(item, "Rd_tag") == tag) {
            return(item)
        }
    }
}

get_help_rd <- function(hfile) {
    getNamespace("utils")$.getHelpFile(hfile)
}

get_help <- function(hfile, format = c("html", "text")) {
    format <- match.arg(format)

    rd <- get_help_rd(hfile)
    paths <- as.character(hfile)

    if (length(paths) == 0) {
        return(NULL)
    }

    pkgname <- basename(dirname(dirname(paths[[1]])))

    if (format == "html") {
        result <- paste0(utils::capture.output(
            tools::Rd2HTML(rd, package = pkgname, outputEncoding = "UTF-8")
        ), collapse = "\n")
        result <- gsub(".*<body>\n*(.*)\n*</body>.*", "\\1", result)
    } else if (format == "text") {
        result <- paste0(utils::capture.output(
            tools::Rd2txt(rd, package = pkgname, outputEncoding = "UTF-8")
        ), collapse = "\n")
    } else {
        stop("Invalid format")
    }

    enc2utf8(result)
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
                (@line2 > {line} or (@line2 = {line} and @col2 >= {col}-1))]"
    } else {
        xpath <- "//expr[(@line1 < {line} or (@line1 = {line} and @col1 <= {col})) and
                (@line2 > {line} or (@line2 = {line} and @col2 >= {col}-1))]"
    }
    xpath <- glue(xpath, line = line, col = col)
    xml_find_all(x, xpath)
}

xdoc_find_token <- function(x, line, col) {
    xpath <- glue("//*[not(*)][(@line1 < {line} or (@line1 = {line} and @col1 <= {col})) and (@line2 > {line} or (@line2 = {line} and @col2 >= {col}-1))]",
        line = line, col = col)
    xml_find_first(x, xpath)
}

xml_single_quote <- function(x) {
    x <- gsub("'", "&apos;", x, fixed = TRUE)
    x
}

html_to_markdown <- function(html) {
    html_file <- file.path(tempdir(), "temp.html")
    md_file <- file.path(tempdir(), "temp.md")
    logger$info("Converting html to markdown using", html_file, md_file)
    stringi::stri_write_lines(html, html_file)
    result <- tryCatch({
        pandoc_version <- rmarkdown::pandoc_version()
        format <- if (pandoc_version >= "2.0") "gfm" else "markdown_github"
        options <- if (pandoc_version >= "2.11") c("--lua-filter", system.file(package = "languageserver", "lua/html-to-markdown.lua"))
        rmarkdown::pandoc_convert(html_file, to = format, output = md_file, options = options)
        paste0(stringi::stri_read_lines(md_file, encoding = "utf-8"), collapse = "\n")
    }, error = function(e) {
        logger$info("html_to_markdown failed: ", conditionMessage(e))
        NULL
    })
    result
}

format_file_size <- function(bytes) {
    obj_size <- structure(bytes, class = "object_size")
    format(obj_size, units = "auto")
}

is_text_file <- function(path, n = 1000) {
    bin <- readBin(path, "raw", n = n)
    is_utf8 <- stringi::stri_enc_isutf8(bin)
    if (is_utf8) {
        return(TRUE)
    } else {
        result <- stringi::stri_enc_detect(bin)[[1]]
        conf <- result$Confidence[1]
        if (identical(conf, 1)) {
            return(TRUE)
        }
    }
    return(FALSE)
}

compare_position <- function(position1, position2) {
    if (position1$line < position2$line ||
        (position1$line == position2$line &&
            position1$character < position2$character)) {
        -1L
    } else if (position1$line > position2$line ||
        (position1$line == position2$line &&
            position1$character > position2$character)) {
        1L
    } else {
        0L
    }
}

range_overlap <- function(range1, range2) {
    !(compare_position(range1$end, range2$start) < 0 ||
        compare_position(range1$start, range2$end) > 0 ||
        compare_position(range2$end, range1$start) < 0 ||
        compare_position(range2$start, range1$end) > 0)
}
