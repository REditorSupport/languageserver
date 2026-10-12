FoldingRangeKind <- list(
    Comment = "comment",
    Imports = "imports",
    Region = "region"
)

get_comment_folding_ranges <- function(xdoc, outline = NULL) {
    if (is.null(outline)) {
        comments <- xml_find_all(xdoc, "//COMMENT")
        text <- xml_text(comments, trim = FALSE)
        lines <- as.integer(xml_attr(comments, "line1"))
        cols <- as.integer(xml_attr(comments, "col1"))
    } else {
        text <- outline$comments$text
        lines <- outline$comments$line
        cols <- outline$comments$col
    }
    if (!length(text)) return(NULL)
    keep <- !grepl(paste0("(", section_range_regex, ")\\s*$"), text, perl = TRUE) &
        !grepl("#\\s*%%\\s*", text, perl = TRUE)
    lines <- lines[keep]
    cols <- cols[keep]
    if (!length(lines)) return(NULL)
    # A fold is one contiguous run with the same indentation. Identify all
    # runs together instead of rescanning every comment for every group.
    starts <- which(c(TRUE, diff(cols) != 0L | diff(lines) != 1L))
    ends <- c(starts[-1L] - 1L, length(lines))
    keep <- ends > starts
    starts <- starts[keep]
    ends <- ends[keep]
    comm_folding_ranges <- lapply(seq_along(starts), function(i) {
        list(
            startLine = lines[[starts[[i]]]] - 1L,
            endLine = lines[[ends[[i]]]] - 1L,
            kind = FoldingRangeKind$Comment
        )
    })
    comm_folding_ranges
}

get_section_and_block_folding_ranges <- function(document, xdoc, outline = NULL) {

    sections <- get_document_sections_and_blocks(
        document = document, xdoc = xdoc, outline = outline
    )
    if (!length(sections)) {
        return(NULL)
    }
    section_folding_ranges <- lapply(sections, function(section) {
        list(
            startLine = section$start_line - 1,
            endLine = section$end_line - 1,
            kind = FoldingRangeKind$Region
        )
    })
    section_folding_ranges
}

#' Main util function to get folding range (sections and blocks).
#' sections ranges are indicated by `section_mark_suffix`
#' blocks ranges are codes between pairs like ("[" and "]"), ("(" and ")"), and ("{" and "}").
#' @noRd
get_document_sections_and_blocks <- function(document, xdoc, outline = NULL) {
    if (document$is_rmarkdown) {
        get_rmd_document_sections_and_blocks(document$content, xdoc = xdoc, outline = outline)
    } else {
        get_r_document_sections_and_blocks(
            content = document$content, xdoc = xdoc, symbol = FALSE, outline = outline
        )
    }
}

#' @noRd
get_document_blocks <- function(xdoc, outline = NULL) {
    if (!is.null(outline)) {
        return(.mapply(function(start_line, end_line) {
            list(type = "block", start_line = start_line, end_line = end_line)
        }, outline$blocks, NULL))
    }
    if (is.null(xdoc)) {
        return(NULL)
    }
    blocks <- xml_find_all(xdoc, "//expr[@line1 < @line2 and
        (OP-LEFT-PAREN | OP-LEFT-BRACKET | OP-LEFT-BRACE)/@line1 <
        (OP-RIGHT-PAREN | OP-RIGHT-BRACKET | OP-RIGHT-BRACE)/@line1]")
    if (!length(blocks)) { # prevent floating point comparision
        return(NULL)
    }

    block_start <- xml_find_first(blocks, "OP-LEFT-PAREN | OP-LEFT-BRACKET | OP-LEFT-BRACE")
    block_end <- xml_find_first(blocks, "OP-RIGHT-PAREN | OP-RIGHT-BRACKET | OP-RIGHT-BRACE")

    block_start_line <- as.integer(xml_attr(block_start, "line1"))
    block_end_line <- as.integer(xml_attr(block_end, "line1"))

    block_folding_ranges <- .mapply(function(start_line, end_line) {
        list(
            type = "block",
            start_line = start_line,
            end_line = end_line - 1L
        )
    }, list(block_start_line, block_end_line), NULL)
    block_folding_ranges
}

#---------------------------rmd document utils functions----------------------#
#' rmd document util function to get folding range - sections and blocks.
#' @noRd
get_rmd_document_sections_and_blocks <- function(content, xdoc, outline = NULL) {
    blocks <- get_document_blocks(xdoc, outline)
    sections <- get_literate_document_sections(
        content, c("section", "chunk", "yaml", "div", "code")
    )
    c(blocks, sections)
}

#' Get all the folding ranges in the document
#' @noRd
document_folding_range_reply <- function(id, uri, workspace, document) {
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    succession <- lsp_settings$get("nline_to_break_succession")
    cached <- parse_data$folding_ranges_cache
    if (!is.null(cached) && identical(cached$content, document$content) &&
            identical(cached$is_rmarkdown, document$is_rmarkdown) &&
            identical(cached$succession, succession)) {
        return(Response$new(id, result = cached$result))
    }

    outline <- parse_data$outline_data
    xdoc <- if (is.null(outline)) parse_data_xml(parse_data) else NULL
    comment_ranges <- NULL
    if (!is.null(xdoc) || !is.null(outline)) {
        comment_ranges <- get_comment_folding_ranges(xdoc, outline)
    }
    section_ranges <- get_section_and_block_folding_ranges(
        document, xdoc, outline
    )

    result <- c(comment_ranges, section_ranges)

    result <- unique(result)
    # Markdown structure belongs to the original document, while the parse
    # cache is keyed by extracted R code. Retain the original content as the
    # cache key so documents with identical chunks cannot share stale folds.
    if (is.environment(parse_data)) {
        parse_data$folding_ranges_cache <- list(
            content = document$content,
            is_rmarkdown = document$is_rmarkdown,
            succession = succession,
            result = result
        )
    }
    if (!length(result)) { # prevent floating point comparision
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
