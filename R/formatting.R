#' edit code style
#'
#' This functions formats a document using [styler::style_text()] with the
#' [styler::tidyverse_style()] style.
#'
#' @template document
#' @param options a named list of options, with a `tabSize` parameter
stylize <- function(document, options) {
    newTextList <- styler::style_text(
        document, transformers = styler::tidyverse_style(indent_by = options$tabSize))
    paste(newTextList, collapse = "\n")
}

#' format a document
#'
#' @template id
#' @template uri
#' @template document
#' @param options a named list of options, with a `tabSize` parameter
formatting_reply <- function(id, uri, document, options) {
    newText <- stylize(document, options)
    ndoc <- length(document)
    range <- range(
        start = position(line = 0, character = 0),
        end = position(line = ndoc - 1, character = nchar(document[[ndoc]]))
    )
    TextEdit <- text_edit(range = range, new_text = newText)
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
    line2 <- if (range$end$character == 0) range$end$line - 1 else range$end$line
    selection <- document[(line1:line2) + 1]
    newText <- stylize(selection, options)
    range <- range(
        start = position(line = line1, character = 0),
        end = position(line = line2, character = nchar(document[[line2 + 1]]))
    )
    TextEdit <- text_edit(range = range, new_text = newText)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}
