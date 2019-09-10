#' style a file
#'
#' This functions formats a document using [styler::style_text()] with the
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

#' format a document
#'
#' @template id
#' @template uri
#' @template document
#' @param options a named list of options, with a `tabSize` parameter
formatting_reply <- function(id, uri, document, options) {
    newText <- style_text(document, options)
    ndoc <- length(document)
    range <- range(
        start = position(line = 0, character = 0),
        end = position(line = ndoc - 1, character = nchar(document[[ndoc]]))
    )
    TextEdit <- text_edit(range = range, new_text = newText)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}

#' edit code style
#'
#' This functions formats a list of text using [styler::style_text()] with the
#' [styler::tidyverse_style()] style.
#'
#' @param text a list of text
#' @param options a named list of options, with a `tabSize` parameter
style_text <- function(text, options) {
    newTextList <- styler::style_text(
        text,
        transformers = styler::tidyverse_style(indent_by = options$tabSize)
    )
    paste(newTextList, collapse = "\n")
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
    line2 <- if (range$end$character == 0) range$end$line - 1 else range$end$line
    selection <- document[(line1:line2) + 1]
    newText <- style_text(selection, options)
    range <- range(
        start = position(line = line1, character = 0),
        end = position(line = line2, character = nchar(document[[line2 + 1]]))
    )
    TextEdit <- text_edit(range = range, new_text = newText)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}
