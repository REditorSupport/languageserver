#' Style a file
#'
#' This functions formats a document using [styler::style_file()] with the
#' [styler::tidyverse_style()] style.
#'
#' @keywords internal
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


#' Edit code style
#'
#' This functions formats a list of text using [styler::style_text()] with the
#' [styler::tidyverse_style()] style.
#'
#' @keywords internal
style_text <- function(text, options, scope = "tokens", indentation = "") {
    new_text <- tryCatch(
        styler::style_text(
            text,
            transformers = styler::tidyverse_style(
                scope = scope,
                indent_by = options$tabSize
            )
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
    new_text <- style_text(document$content, options)
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

    # check if the selection contains complete lines
    if (character1 != 0 || character2 < ncodeunit(lastline)) {
        scope <- "line_breaks"
    } else {
        scope <- "tokens"
    }

    selection <- document$content[(line1:line2) + 1]
    indentation <- stringr::str_extract(selection[1], "^\\s*")
    new_text <- style_text(selection, options, scope = scope, indentation = indentation)
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
