get_style <- function(options) {
    style <- getOption("languageserver.formatting_style")
    if (is.null(style)) {
        style <- styler::tidyverse_style(indent_by = options$tabSize)
    } else {
        style <- style(options)
    }
    style
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
    style <- get_style(options)
    nline <- document$nline
    if (document$is_rmarkdown) {
        logger$info("formatting R markdown file")
        blocks <- extract_blocks(document$content)
        if (length(blocks) == 0) {
            return(Response$new(id, list()))
        }
        TextEditList <- list()
        for (block in blocks) {
            new_text <- style_text(block$text, style)
            if (is.null(new_text)) {
                new_text <- block$text
            }
            a <- min(block$lines)
            b <- max(block$lines)
            range <- range(
                start = document$to_lsp_position(row = a - 1, col = 0),
                end = document$to_lsp_position(row = b - 1, col = nchar(document$line(b)))
            )
            TextEdit <- text_edit(range = range, new_text = new_text)
            TextEditList[[length(TextEditList) + 1]] <- TextEdit
        }
    } else {
        logger$info("formatting R file")
        new_text <- style_text(document$content, style)
        if (is.null(new_text)) {
            return(Response$new(id, list()))
        }
        range <- range(
            start = document$to_lsp_position(row = 0, col = 0),
            end = if (nline) {
                document$to_lsp_position(row = nline - 1, col = nchar(document$line(nline)))
            } else {
                document$to_lsp_position(row = 0, col = 0)
            }
        )
        TextEdit <- text_edit(range = range, new_text = new_text)
        TextEditList <- list(TextEdit)
    }
    Response$new(id, TextEditList)
}


#' Format a part of a document
#' @keywords internal
range_formatting_reply <- function(id, uri, document, range, options) {
    row1 <- range$start$row
    col1 <- range$start$col
    if (range$end$col == 0 && row1 < range$end$row) {
        # if the cursor is at the beginning of a line, move up one line
        row2 <- range$end$row - 1
        lastline <- document$content[row2 + 1]
        col2 <- nchar(lastline)
    } else {
        row2 <- range$end$row
        lastline <- document$content[row2 + 1]
        col2 <- range$end$col
    }

    # check if the selection is empty
    if (row1 == row2 && col1 == col2) {
        return(Response$new(id, list()))
    }

    style <- get_style(options)
    # check if the selection contains complete lines
    if (col1 != 0 || col2 < nchar(lastline)) {
        # disable assignment operator fix for partial selection
        style$token$force_assignment_op <- NULL
    }

    selection <- document$content[(row1:row2) + 1]
    indentation <- stringr::str_extract(selection[1], "^\\s*")
    new_text <- style_text(selection, style, indentation = indentation)
    if (is.null(new_text)) {
        return(Response$new(id, list()))
    }
    range <- range(
        start = document$to_lsp_position(row = row1, col = 0),
        end = document$to_lsp_position(row = row2, col = nchar(document$line0(row2)))
    )
    TextEdit <- text_edit(range = range, new_text = new_text)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}

#' Format on type
#' @keywords internal
on_type_formatting_reply <- function(id, uri, document, point, ch, options) {
    if (ch == "\n") {
        row <- point$row - 1
    } else {
        row <- point$row
    }
    style <- get_style(options)
    selection <- document$line0(row)
    indentation <- stringr::str_extract(selection, "^\\s*")
    new_text <- style_text(selection, style, indentation = indentation)
    if (is.null(new_text)) {
        return(Response$new(id, list()))
    }
    range <- range(
        start = document$to_lsp_position(row = row, col = 0),
        end = document$to_lsp_position(row = row, col = nchar(document$line0(row)))
    )
    TextEdit <- text_edit(range = range, new_text = new_text)
    TextEditList <- list(TextEdit)
    Response$new(id, TextEditList)
}
