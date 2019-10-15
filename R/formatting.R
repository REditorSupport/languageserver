get_style <- function(options) {
    style <- getOption("languageserver.formatting_style")
    if (is.null(style)) {
        style <- styler::tidyverse_style(indent_by = options$tabSize)
    } else {
        style <- style(options)
    }
    style
}

#' Style a file
#'
#' This functions formats a document using [styler::style_file()] with the
#' specified style.
#'
#' @keywords internal
style_file <- function(path, style) {
    document <- readr::read_lines(path)
    if (is_rmarkdown(path)) {
        temp_file <- tempfile(fileext = ".Rmd")
    } else {
        temp_file <- tempfile(fileext = ".R")
    }
    readr::write_lines(document, temp_file)
    styler::style_file(temp_file,
        transformers = style
    )
    contents <- readr::read_lines(temp_file)
    file.remove(temp_file)
    paste(contents, collapse = "\n")
}


#' Edit code style
#'
#' This functions formats a list of text using [styler::style_text()] with the
#' specified style.
#'
#' @keywords internal
style_text <- function(text, style, indentation = "") {
    new_text <- tryCatch(
        styler::style_text(
            text,
            transformers = style
        ),
        error = function(e) e
    )
    if (inherits(new_text, "error")) {
        logger$info("formatting error:", new_text$message)
        return(NULL)
    }
    paste(indentation, new_text, sep = "", collapse = "\n")
}


#' Format a document
#' @keywords internal
formatting_reply <- function(id, uri, document, options) {
    # do not use `style_file` because the changes are not necessarily saved on disk.
    style <- get_style(options)
    new_text <- style_text(document$content, style)
    if (is.null(new_text)) {
        return(Response$new(id, list()))
    }
    nline <- document$nline
    range <- range(
        start = position(line = 0, character = 0),
        end = if (nline) {
            position(line = nline - 1, character = ncodeunit(document$line(nline)))
        } else {
              position(line = 0, character = 0)
          }
    )
    TextEdit <- text_edit(range = range, new_text = new_text)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}


#' Format a part of a document
#' @keywords internal
range_formatting_reply <- function(id, uri, document, range, options) {
    line1 <- range$start$line
    character1 <- range$start$character
    if (range$end$character == 0 && line1 < range$end$line) {
        # if the cursor is at the beginning of a line, move up one line
        line2 <- range$end$line - 1
        lastline <- document$content[line2 + 1]
        character2 <- ncodeunit(lastline)
    } else {
        line2 <- range$end$line
        lastline <- document$content[line2 + 1]
        character2 <- range$end$character
    }

    # check if the selection is empty
    if (line1 == line2 && character1 == character2) {
        return(Response$new(id, list()))
    }

    style <- get_style(options)
    # check if the selection contains complete lines
    if (character1 != 0 || character2 < ncodeunit(lastline)) {
        # disable assignment operator fix for partial selection
        style$token$force_assignment_op <- NULL
    }

    selection <- document$content[(line1:line2) + 1]
    indentation <- stringr::str_extract(selection[1], "^\\s*")
    new_text <- style_text(selection, style, indentation = indentation)
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
