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
                start = position(line = a - 1, character = 0),
                end = position(line = b - 1, character = ncodeunit(document$line(b)))
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
            start = position(line = 0, character = 0),
            end = if (nline) {
                position(line = nline - 1, character = ncodeunit(document$line(nline)))
            } else {
                position(line = 0, character = 0)
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
