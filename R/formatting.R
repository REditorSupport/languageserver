#' style a file
#'
#' This functions formats a document using [styler::style_file()] with the
#' [styler::tidyverse_style()] style.
#'
#' @param path file path
#' @param options a named list of options, with a `tabSize` parameter
style_file <- function(path, options) {
    document <- readLines(path, warn = FALSE)
    if (is_rmarkdown(path)) {
        temp_file <- tempfile(fileext = ".Rmd")
    } else {
        temp_file <- tempfile(fileext = ".R")
    }
    writeLines(document, temp_file)
    styler::style_file(temp_file,
        transformers = styler::tidyverse_style(indent_by = options$tabSize)
    )
    contents <- readLines(temp_file, warn = FALSE)
    file.remove(temp_file)
    paste(contents, collapse = "\n")
}


#' edit code style
#'
#' This functions formats a list of text using [styler::style_text()] with the
#' [styler::tidyverse_style()] style.
#'
#' @param text a vector of text
#' @param options a named list of options, with a `tabSize` parameter
#' @param indentation amount of whitespaces put at the begining of each line
style_text <- function(text, options, indentation = "") {
    new_text <- tryCatch(
        styler::style_text(
            text,
            transformers = styler::tidyverse_style(indent_by = options$tabSize)
        ),
        error = function(e) e)
    if (inherits(new_text, "error")) {
        logger$info("formatting error:", e$message)
        return(NULL)
    }
    paste(indentation, new_text, sep = "", collapse = "\n")
}


#' format a document
#'
#' @template id
#' @template uri
#' @template document
#' @param options a named list of options, with a `tabSize` parameter
formatting_reply <- function(id, uri, document, options) {
    # do not use `style_file` because the changes are not necessarily saved on disk.
    new_text <- style_text(document$content, options)
    if (is.null(new_text)) {
        return(Response$new(id, list()))
    }
    nline <- document$nline
    range <- range(
        start = position(line = 0, character = 0),
        end = if (nline) {
            position(line = nline - 1, character = ncodeunit(document$line(nline)))
        } else position(line = 0, character = 0)
    )
    TextEdit <- text_edit(range = range, new_text = new_text)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}


#' format a part of a document
#'
#' @template id
#' @template uri
#' @template document
#' @param range a [range], the part of the document to format
#' @param options a named list of options, with a `tabSize` parameter
range_formatting_reply <- function(id, uri, document, range, options) {
    line1 <- range$start$line
    line2 <- range$end$line
    lastline <- document$content[line2 + 1]
    # check if the selection contains complete lines
    if (range$start$character != 0 || range$end$character < ncodeunit(lastline)) {
        return(Response$new(id, list()))
    }

    selection <- document$content[(line1:line2) + 1]
    indentation <- stringr::str_extract(selection[1], "^\\s*")
    new_text <- style_text(selection, options, indentation)
    if (is.null(new_text)) {
        return(Response$new(id, list()))
    }
    range <- range(
        start = position(line = line1, character = 0),
        end = position(line = line2, character = ncodeunit(document$line(line2 + 1)))
    )
    TextEdit <- text_edit(range = range, new_text = new_text)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}
