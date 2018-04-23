stylize <- function(document, options) {
    newTextList <- styler::style_text(
        document, transformers = styler::tidyverse_style(indent_by = options$tabSize))
    paste(newTextList, collapse = "\n")
}

formatting_reply <- function(id, document, options) {
    newText <- stylize(document, options)
    ndoc <- length(document)
    range <- list(
        start = list(line = 0, character = 0),
        end = list(line = ndoc - 1, character = nchar(document[[ndoc]]))
    )
    TextEdit <- list(range = range, newText = newText)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}

range_formatting_reply <- function(id, document, range, options) {
    line1 <- range$start$line
    line2 <- range$end$line
    selection <- document[(line1:line2) + 1]
    newText <- stylize(selection, options)
    range <- list(
        start = list(line = line1, character = 0),
        end = list(line = line2, character = nchar(document[[line2 + 1]]))
    )
    TextEdit <- list(range = range, newText = newText)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}
