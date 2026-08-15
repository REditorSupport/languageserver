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


#' Find the closing delimiters needed to make an incomplete expression parseable
#' @noRd
missing_closing_delimiters <- function(text) {
    if (!length(text)) return("")

    row <- length(text) - 1L
    col <- nchar(text[[length(text)]]) - 1L
    openers <- character()

    while (row >= 0L) {
        result <- find_unbalanced_bracket(text, row, col)
        location <- result[[1L]]
        if (any(location < 0L)) break

        opener <- result[[2L]]
        if (!opener %in% c("(", "[", "{")) break
        openers <- c(openers, opener)

        row <- location[[1L]]
        col <- location[[2L]] - 1L
        if (col < 0L) {
            row <- row - 1L
            if (row >= 0L) col <- nchar(text[[row + 1L]]) - 1L
        }
    }

    closing <- c("(" = ")", "[" = "]", "{" = "}")
    paste0(unname(closing[openers]), collapse = "")
}


#' Complete an incomplete expression with a unique sentinel
#'
#' The R parser and styler both require complete syntax. This function only
#' accepts a completion after the R parser validates it. The sentinel lets the
#' caller discard all synthesized text after styling without relying on its
#' formatted length or position.
#' @noRd
complete_incomplete_expression <- function(text) {
    source <- paste0(text, collapse = "\n")
    if (!nzchar(trimws(source))) return(NULL)

    sentinel <- ".__languageserver_formatting_sentinel__"
    while (grepl(sentinel, source, fixed = TRUE)) {
        sentinel <- paste0(sentinel, "_")
    }

    last_line <- text[[length(text)]]
    separator <- if (grepl("^\\s*#", last_line)) {
        "\n"
    } else if (grepl("\\s$", source)) {
        ""
    } else {
        " "
    }
    closers <- missing_closing_delimiters(text)
    holes <- c(
        sentinel,
        paste0(sentinel, "()"),
        paste0(sentinel, " in NULL")
    )

    for (trailing_body in c("", "\nNULL")) {
        for (hole in holes) {
            completed <- paste0(
                source, separator, hole, closers, trailing_body
            )
            parsed <- tryCatch(
                parse(text = completed, keep.source = FALSE),
                error = function(e) NULL
            )
            if (!is.null(parsed) && length(parsed)) {
                return(list(
                    text = completed,
                    sentinel = sentinel,
                    nexpr = length(parsed)
                ))
            }
        }
    }

    NULL
}


#' Remove synthesized completion text from styled output
#' @noRd
remove_formatting_sentinel <- function(text, sentinel) {
    locations <- gregexpr(sentinel, text, fixed = TRUE)[[1L]]
    if (length(locations) != 1L || locations[[1L]] < 1L) return(NULL)
    substr(text, 1L, locations[[1L]] - 1L)
}


#' Format a document
#' @noRd
formatting_reply <- function(id, uri, document, options) {
    style <- get_style(options)
    nline <- document$nline
    if (document$is_rmarkdown) {
        logger$info("formatting R markdown file")
        blocks <- literate_r_blocks(document$content, document$regions)
        if (length(blocks) == 0) {
            return(Response$new(id, list()))
        }
        TextEditList <- vector("list", length(blocks))
        idx <- 0L
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
            idx <- idx + 1L
            TextEditList[[idx]] <- TextEdit
        }
        if (idx < length(TextEditList)) {
            TextEditList <- TextEditList[seq_len(idx)]
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

    if (document$is_rmarkdown) {
        start_cell <- literate_r_cell_at(document$regions, row1)
        end_cell <- literate_r_cell_at(document$regions, row2)
        if (is.null(start_cell) || is.null(end_cell) ||
                start_cell$start_line != end_cell$start_line) {
            return(Response$new(id, list()))
        }
        selected_lines <- seq.int(row1 + 1L, row2 + 1L)
        if (any(document$regions$line_type[selected_lines] != "r")) {
            return(Response$new(id, list()))
        }
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

#' Format several non-overlapping document ranges (LSP 3.18)
#' @noRd
ranges_formatting_reply <- function(id, uri, document, ranges, options) {
    if (!length(ranges)) return(Response$new(id, result = list()))

    # range_formatting_reply expands edits to whole lines. Merge requested
    # ranges that would therefore produce overlapping edits.
    normalized <- lapply(ranges, function(item) {
        end_row <- item$end$row
        if (item$end$col == 0L && item$start$row < end_row) {
            end_row <- end_row - 1L
        }
        list(start = item$start, end = list(row = end_row, col = item$end$col))
    })
    order_index <- order(
        vapply(normalized, function(item) item$start$row, numeric(1L)),
        vapply(normalized, function(item) item$start$col, numeric(1L))
    )
    normalized <- normalized[order_index]

    merged <- list()
    for (item in normalized) {
        if (!length(merged)) {
            merged[[1L]] <- item
            next
        }
        last <- merged[[length(merged)]]
        if (item$start$row <= last$end$row) {
            if (item$end$row > last$end$row ||
                (item$end$row == last$end$row && item$end$col > last$end$col)) {
                last$end <- item$end
            }
            merged[[length(merged)]] <- last
        } else {
            merged[[length(merged) + 1L]] <- item
        }
    }

    edits <- list()
    for (item in merged) {
        reply <- range_formatting_reply(NULL, uri, document, item, options)
        if (length(reply$result)) edits <- c(edits, reply$result)
    }
    Response$new(id, result = edits)
}


#' Find the expression affected by on-type formatting
#' @noRd
find_on_type_formatting_chunk <- function(content, end_line, complete_at_end = FALSE) {
    start_line <- end_line
    nexpr <- 0L
    best <- NULL

    while (start_line >= 1L) {
        text <- content[start_line:end_line]
        parsed <- tryCatch(
            parse(text = text, keep.source = FALSE),
            error = function(e) NULL
        )

        completion <- NULL
        if ((isTRUE(complete_at_end) && any(nzchar(trimws(text)))) ||
                is.null(parsed)) {
            completion <- complete_incomplete_expression(text)
        }

        if (!is.null(completion)) {
            nexpr1 <- completion$nexpr
        } else if (!is.null(parsed)) {
            nexpr1 <- length(parsed)
        } else {
            nexpr1 <- 0L
        }

        # Stop after crossing into the preceding expression. An incomplete
        # line ending in an operator or comma remains part of the current one.
        if (nexpr > 0L && (nexpr1 > nexpr || nexpr1 == 0L) &&
                !is_incomplete_line(content[[start_line]])) {
            break
        }

        if (nexpr1 > 0L) {
            best <- list(
                start_line = start_line,
                text = if (is.null(completion)) text else completion$text,
                sentinel = if (is.null(completion)) NULL else completion$sentinel
            )
        }
        nexpr <- nexpr1
        start_line <- start_line - 1L
    }

    best
}


#' Return a conservative indentation-only edit
#' @noRd
indentation_only_reply <- function(id, document, point, options,
    start_line = 1L) {
    row <- point$row
    if (row < 0L || row >= document$nline) return(Response$new(id))

    line <- document$line0(row)
    if (!grepl("^\\s*$", line)) return(Response$new(id))

    tab_size <- options$tabSize
    if (is.null(tab_size) || length(tab_size) != 1L || is.na(tab_size) ||
            tab_size < 1L) {
        tab_size <- 2L
    }
    indent_unit <- if (isFALSE(options$insertSpaces)) {
        "\t"
    } else {
        strrep(" ", tab_size)
    }

    scoped_content <- document$content[seq.int(start_line, row + 1L)]
    scoped_row <- row - start_line + 1L
    result <- find_unbalanced_bracket(
        scoped_content,
        scoped_row,
        nchar(line) - 1L
    )
    location <- result[[1L]]

    if (all(location >= 0L)) {
        context_row <- location[[1L]] + start_line - 1L
        context_line <- document$line0(context_row)
        base <- stringi::stri_extract_first_regex(context_line, "^\\s*")
        indentation <- paste0(base, indent_unit)
    } else {
        previous <- row - 1L
        minimum_row <- start_line - 1L
        while (previous >= minimum_row && !grepl("\\S", document$line0(previous))) {
            previous <- previous - 1L
        }
        if (previous < minimum_row) return(Response$new(id))

        previous_line <- document$line0(previous)
        indentation <- stringi::stri_extract_first_regex(previous_line, "^\\s*")
        if (is_incomplete_line(previous_line)) {
            indentation <- paste0(indentation, indent_unit)
        }
    }

    if (identical(line, indentation)) return(Response$new(id))

    edit_range <- range(
        start = document$to_lsp_position(row = row, col = 0L),
        end = document$to_lsp_position(row = row, col = nchar(line))
    )
    Response$new(id, list(text_edit(range = edit_range, new_text = indentation)))
}

#' Format on type
#' @noRd
on_type_formatting_reply <- function(id, uri, document, point, ch, options) {
    if (!check_scope(uri, document, point)) {
        return(Response$new(id))
    }

    run <- if (document$is_rmarkdown) {
        literate_r_run_at(document$regions, point$row)
    } else {
        list(start_line = 1L, end_line = document$nline)
    }
    if (is.null(run)) return(Response$new(id))

    content <- document$content
    end_line <- point$row + 1L
    complete_at_end <- FALSE
    if (ch == "\n") {
        start_line <- end_line - 1L
        if (start_line < 1L) return(Response$new(id))
        if (grepl("^\\s*(#.*)?$", content[[start_line]])) {
            return(Response$new(id))
        }
        if (grepl("^\\s*(#.*)?$", content[[end_line]])) {
            complete_at_end <- TRUE
        }
    }

    scoped_content <- content[seq.int(run$start_line, end_line)]
    chunk <- tryCatchTimeout(
        find_on_type_formatting_chunk(
            scoped_content, length(scoped_content), complete_at_end),
        timeout = 0.1,
        error = function(e) {
            logger$info("on_type_formatting_reply:parser:", e)
            NULL
        }
    )

    if (!is.null(chunk)) {
        start_line <- chunk$start_line + run$start_line - 1L

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
            style_text(chunk$text, style, indention = indention),
            timeout = 1,
            error = function(e) logger$info("on_type_formatting_reply:styler:", e)
        )
        if (!is.null(new_text)) {
            if (!is.null(chunk$sentinel)) {
                new_text <- remove_formatting_sentinel(
                    new_text, chunk$sentinel
                )
            }
            if (!is.null(new_text)) {
                edit_range <- range(
                    start = document$to_lsp_position(row = start_line - 1L, col = 0L),
                    end = document$to_lsp_position(
                        row = end_line - 1L,
                        col = nchar(document$line(end_line))
                    )
                )
                TextEdit <- text_edit(range = edit_range, new_text = new_text)
                return(Response$new(id, list(TextEdit)))
            }
        }
    }

    if (ch == "\n") {
        return(indentation_only_reply(
            id, document, point, options, start_line = run$start_line))
    }
    Response$new(id)
}
