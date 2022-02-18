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
#' @noRd
style_text <- function(text, style, indention = 0L, trailing_empty_line = FALSE) {
    new_text <- tryCatch(
        styler::style_text(
            text,
            transformers = style,
            base_indention = indention
        ),
        error = function(e) e
    )
    if (inherits(new_text, "error")) {
        logger$info("formatting error:", new_text$message)
        return(NULL)
    }
    if (isTRUE(trailing_empty_line)) {
        if (new_text[[length(new_text)]] != "") {
            new_text <- c(new_text, "")
        }
    }
    paste0(new_text, collapse = "\n")
}


#' Format a document
#' @noRd
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
        new_text <- style_text(document$content, style, trailing_empty_line = TRUE)
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
#' @noRd
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
    indention <- nchar(stringi::stri_extract_first_regex(selection[1], "^\\s*"))
    new_text <- style_text(selection, style, indention = indention)
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
#' @noRd
on_type_formatting_reply <- function(id, uri, document, point, ch, options) {
    if (!check_scope(uri, document, point)) {
        return(Response$new(id))
    }

    content <- document$content
    end_line <- point$row + 1
    use_completer <- FALSE
    if (ch == "\n") {
        start_line <- end_line - 1
        if (grepl("^\\s*(#.*)?$", content[[start_line]])) {
            return(Response$new(id))
        }
        if (grepl("^\\s*(#.*)?$", content[[end_line]])) {
            # use completer to complete the potentially incomplete expression
            last_line <- content[end_line]
            content[end_line] <- "f()"
            use_completer <- TRUE
        }
    } else {
        start_line <- end_line
    }

    # backward parse until more expressions or non-parseable
    expr <- NULL
    nexpr <- 0
    res <- tryCatchTimeout({
        while (start_line >= 1) {
            expr <- tryCatch(parse(
                text = content[start_line:end_line],
                keep.source = FALSE),
            error = function(e) NULL)
            nexpr1 <- length(expr)
            # stop backward parsing when
            # 1. we have at least one expression parsed; and
            # 2. we are entering the previous expression:
            #    * if it is one-line expression, then we got 1 more expression.
            #    * if it is multi-line expression, then we got no expression
            if (nexpr > 0 && (nexpr1 > nexpr || nexpr1 == 0)) {
                start_line <- start_line + 1
                break
            }
            nexpr <- nexpr1
            start_line <- start_line - 1
        }
        TRUE
    }, timeout = 0.1, error = function(e) logger$info("on_type_formatting_reply:parser:", e))

    if (is.null(res)) {
        # timeout
        return(Response$new(id))
    }

    if (nexpr >= 1) {
        if (start_line == 0) {
            start_line <- 1
        }

        # find first non-empty line for the detection of indention
        while (start_line < end_line) {
            if (grepl("\\S", content[[start_line]])) {
                break
            }
            start_line <- start_line + 1
        }

        # logger$info("on_type_formatting_reply:", list(
        #     start_line = start_line,
        #     end_line = end_line,
        #     chunk = content[start_line:end_line]
        # ))

        style <- get_style(options)

        # disable assignment operator fix since end_line could be function parameter
        style$token$force_assignment_op <- NULL

        indention <- nchar(stringi::stri_extract_first_regex(content[start_line], "^\\s*"))
        new_text <- tryCatchTimeout(
            style_text(content[start_line:end_line], style, indention = indention),
            timeout = 1,
            error = function(e) logger$info("on_type_formatting_reply:styler:", e)
        )
        if (!is.null(new_text)) {
            if (use_completer) {
                # remove completer from formatted text
                new_text <- substr(new_text, 1, nchar(new_text) - 3)
                new_text <- paste0(new_text, trimws(last_line))
            }
            range <- range(
                start = document$to_lsp_position(row = start_line - 1, col = 0),
                end = document$to_lsp_position(row = end_line - 1, col = nchar(document$line(end_line)))
            )
            TextEdit <- text_edit(range = range, new_text = new_text)
            TextEditList <- list(TextEdit)
            return(Response$new(id, TextEditList))
        }
    }

    Response$new(id)
}
