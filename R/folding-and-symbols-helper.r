# define suffix marks for section and prefix marks for section levels
# ** use `section_mark_suffix` to indicate section beginning
# ** numbers over 4 is indicative of section range
# ** see `get_r_document_sections`
section_mark_suffix <- c("#", "+", "-", "=", "*")
section_range_regex <- paste0(
    "\\", section_mark_suffix, "{4,}",
    collapse = "|"
)

# ** use `section_level_prefix` to indicate section levels
section_level_prefix <- "#" # c("#", "*", "-", "+", "=")
section_level_regex <- paste0(
    "[", paste0("\\", section_level_prefix, collapse = ""), "]*+"
)

#' Main util function to get folding range - sections and blocks.
#' sections are indicated by `section_mark_suffix`
#' blocks are codes between something like [], (), and {}.
#' @noRd
get_document_sections_and_blocks <- function(uri, document, xdoc) {
    if (document$is_rmarkdown) {
        get_rmd_document_sections_and_blocks(uri, document, xdoc = xdoc)
    } else {
        get_r_document_sections_and_blocks(
            document = document, xdoc = xdoc, symbol = FALSE
        )
    }
}

#' Main util function to get document symbols
#' @noRd
get_document_symbols <- function(uri, document, xdoc) {
    if (document$is_rmarkdown) {
        get_rmd_document_symbols(uri, document)
    } else {
        get_r_document_sections_and_blocks(
            document = document, xdoc = xdoc, symbol = TRUE
        )
    }
}

#---------------------------r document utils functions------------------------#
get_r_document_sections_and_blocks <- function(document, xdoc, symbol = FALSE) {
    doc_content <- document$content
    block_lines <- extract_document_block_lines(xdoc)
    section_ranges <- get_r_document_sections_rec(
        start_line = 1L,
        end_line = length(doc_content),
        doc_content = doc_content,
        block_lines = block_lines
    )
    if (symbol) {
        return(section_ranges)
    }
    block_ranges <- .mapply(function(start_line, end_line) {
        list(
            type = "block",
            start_line = start_line,
            end_line = end_line - 1L
        )
    }, block_lines, NULL)
    c(section_ranges, block_ranges)
}

get_r_document_sections_rec <- function(start_line, end_line, doc_content, block_lines) {
    if (start_line > end_line) {
        return(NULL)
    }
    line_seq <- start_line:end_line
    block_lines <- lapply(
        block_lines, "[",
        block_lines[[1L]] >= start_line &
            block_lines[[2L]] - 1L <= end_line & !(
            block_lines[[1L]] == start_line &
                block_lines[[2L]] - 1L == end_line
        )
    )

    lines_out_blocks <- extract_lines_out_blocks(
        line_seq, block_lines
    )
    sections_in_blocks <- NULL
    if (length(block_lines[[1L]])) {
        highest_level_block_lines <- lapply(block_lines, "[", unlist(
            .mapply(function(start_line, end_line) {
                !any(start_line >= block_lines[[1L]] &
                    end_line <= block_lines[[2L]] &
                    !(start_line == block_lines[[1L]] &
                        end_line == block_lines[[2L]]))
            }, block_lines, NULL),
            recursive = FALSE, use.names = FALSE
        ))


        sections_in_blocks <- .mapply(function(start_line, end_line) {
            get_r_document_sections_rec( # recursive function
                start_line, end_line - 1L,
                doc_content = doc_content,
                block_lines = block_lines
            )
        }, highest_level_block_lines, NULL)
        sections_in_blocks <- unlist(
            sections_in_blocks,
            recursive = FALSE,
            use.names = FALSE
        )
    }
    sections_out_blocks <- NULL
    if (length(lines_out_blocks)) {
        sections_out_blocks <- get_r_document_sections_base(
            lines_out_blocks,
            doc_content = doc_content
        )
    }
    c(sections_in_blocks, sections_out_blocks)
}

get_r_document_sections_base <- function(line_seq, doc_content) {
    line_content <- doc_content[line_seq]
    section_range_groups <- extract_section_groups(
        line_seq, line_content
    )
    res <- lapply(section_range_groups, function(x) {
        determine_section_ranges(
            section_lines = x$section_lines,
            section_names = x$section_names,
            section_levels = x$section_levels,
            breakpoint = x$breakpoint
        )
    })
    unlist(res, recursive = FALSE, use.names = FALSE)
}

#' @noRd
extract_document_block_lines <- function(xdoc) {
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

    block_lines <- unique(cbind(
        as.integer(xml_attr(block_start, "line1")),
        as.integer(xml_attr(block_end, "line1"))
    ))
    lapply(1:2, function(i) block_lines[, i, drop = TRUE])
}

#' `block_lines` should in `line_seq`
extract_lines_out_blocks <- function(line_seq, block_lines) {
    block_span_lines <- unlist(
        .mapply(":", block_lines, NULL),
        recursive = FALSE, use.names = FALSE
    )
    lines_out_blocks <- setdiff(line_seq, block_span_lines)
    # should keep the last line
    unique(c(lines_out_blocks, max(line_seq)))
}

#' `line_seq` is the line numbers of line_content, both have equal numbers
#' @noRd
extract_document_section_lines <- function(line_seq, line_content) {

    # extract comment line with at least 4 of one of c("#", "+", "-", "=", "*")
    is_section_lines <- grepl(
        paste0("^\\s*\\#.+", "(", section_range_regex, ")\\s*$"),
        line_content,
        perl = TRUE
    )
    section_lines <- line_seq[is_section_lines]
    if (length(section_lines)) {
        # extract section marks of section levels and its name
        # this should be a matrix
        # ** section levels - the second column
        # ** section names - the fourth column
        section_levels_and_names <- stringi::stri_match_first(
            line_content[is_section_lines],
            regex = paste0(
                "\\s*\\#\\s*+",
                "(", section_level_regex, ")\\s*+", # section levels group
                "(%%)?\\s*+",
                "(.+?)\\s*+", # section names group
                "(", section_range_regex, ")\\s*$"
            )
        )
        section_names <- section_levels_and_names[, 4L, drop = TRUE]

        # define section levels based on the number of one of
        # `section_level_prefix`
        section_levels <- nchar(
            section_levels_and_names[, 2L, drop = TRUE]
        )

        return(
            list(
                lines = section_lines,
                names = section_names,
                levels = section_levels
            )
        )
    }

    NULL
}

#' two or more blank lines out of block ranges should break sections succession
#' @noRd
extract_document_section_breaks <- function(line_seq, line_content) {
    blank_lines <- line_seq[
        grepl("^\\s*$", line_content, perl = TRUE)
    ]
    if (!length(blank_lines)) {
        return(NULL)
    }
    # group continuous blank lines
    group <- split(blank_lines, factor(cumsum(diff(c(0L, blank_lines)) != 1L)))
    break_lines <- vapply(group, function(x) {
        # how many lines should break off section succession ? ( 2L )
        if (length(x) >= 2L) {
            return(min(x))
        } else {
            return(NA_integer_)
        }
    }, integer(1L))
    break_lines <- break_lines[!is.na(break_lines)]
    if (length(blank_lines)) break_lines else NULL
}

extract_section_groups <- function(line_seq, line_content) {
    sections_lines <- extract_document_section_lines(line_seq, line_content)
    if (!length(sections_lines)) {
        return(NULL)
    }

    break_lines <- extract_document_section_breaks(line_seq, line_content)
    if (length(break_lines)) {
        section_breaks <- sort(c(break_lines, max(line_seq) + 1L))
        # section_breaks_index in seq_along(section_breaks)
        section_breaks_index <- findInterval(
            sections_lines$lines,
            c(0L, section_breaks),
            rightmost.closed = TRUE
        )
        section_groups <- lapply(
            sections_lines, split,
            section_breaks_index
        )
        section_breakpoints <- section_breaks - 1L
        section_data_groups <- lapply(
            names(section_groups$lines), function(i) {
                breakpoint <- section_breakpoints[as.integer(i)]
                list(
                    section_lines = section_groups$lines[[i]],
                    section_names = section_groups$names[[i]],
                    section_levels = section_groups$levels[[i]],
                    breakpoint = breakpoint
                )
            }
        )
    } else {
        section_data_groups <- list(
            list(
                section_lines = sections_lines$lines,
                section_names = sections_lines$names,
                section_levels = sections_lines$levels,
                breakpoint = max(line_seq)
            )
        )
    }
    section_data_groups
}

determine_section_ranges <- function(section_lines, section_names, section_levels, breakpoint) {
    seq_sections <- seq_along(section_lines)
    # the section range end line should be the first occurence among
    # following document where the section level is equal or lower than
    # current section level, otherwise, the end line of this document
    section_end_lines <- vapply(seq_sections, function(i) {
        # extract sections after current section
        section_index_after_i <- setdiff(
            seq_sections, seq_len(i)
        )
        # when no higher-level section is after current section
        # the end line should be the end of current document
        if (!length(section_index_after_i)) {
            return(breakpoint)
        }
        # find the first section with higher-level than current section
        section_range_end_index <- which(
            section_levels[section_index_after_i] <= section_levels[[i]]
        )

        if (length(section_range_end_index)) {
            section_range_end_index <- section_range_end_index[[1L]]
            return(
                section_lines[
                    section_index_after_i[section_range_end_index]
                ] - 1L
            )
        } else {
            return(breakpoint)
        }
    }, integer(1L))

    .mapply(function(name, start_line, end_line) {
        list(
            name = name,
            type = "section",
            start_line = start_line,
            end_line = end_line
        )
    }, list(section_names, section_lines, section_end_lines), NULL)
}

#----------------------------rmd document utils functions----------------------#
#' rmd document util function to get folding range - sections and blocks.
#' @noRd
get_rmd_document_sections_and_blocks <- function(uri, document, xdoc) {
    blocks <- get_document_blocks_helper(xdoc)
    sections <- get_rmd_document_sections_helper(
        uri, document, c("section", "chunk")
    )
    c(blocks, sections)
}

get_rmd_document_symbols <- function(uri, document) {
    get_rmd_document_sections_helper(uri, document)
}

#' @noRd
get_document_blocks_helper <- function(xdoc) {
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
            end_line = end_line
        )
    }, list(block_start_line, block_end_line), NULL)
    block_folding_ranges
}

get_rmd_document_sections_helper <- function(uri, document, type = c("section", "chunk")) {
    content <- document$content
    if (length(content) == 0) {
        return(NULL)
    }

    block_lines <- grep("^\\s*```", content)
    if (length(block_lines) %% 2 != 0) {
        return(NULL)
    }

    sections <- NULL
    if ("section" %in% type) {
        section_lines <- grepl("^#+\\s+\\S+", content)
        if (grepl("^---\\s*$", content[[1]])) {
            front_start <- 1L
            front_end <- 2L
            while (front_end <= document$nline) {
                if (grepl("^---\\s*$", content[[front_end]])) {
                    break
                }
                front_end <- front_end + 1L
            }
            section_lines[seq.int(front_start, front_end)] <- FALSE
        }

        for (i in seq_len(length(block_lines) / 2)) {
            section_lines[seq.int(block_lines[[2 * i - 1]], block_lines[[2 * i]])] <- FALSE
        }

        section_lines <- which(section_lines)
        section_num <- length(section_lines)
        section_texts <- content[section_lines]
        section_hashes <- gsub("^(#+)\\s+.+$", "\\1", section_texts)
        section_levels <- nchar(section_hashes)
        section_names <- gsub("^#+\\s+(.+?)(\\s+#+)?\\s*$", "\\1", section_texts, perl = TRUE)

        sections <- lapply(seq_len(section_num), function(i) {
            start_line <- section_lines[[i]]
            end_line <- document$nline
            level <- section_levels[[i]]
            j <- i + 1
            while (j <= section_num) {
                if (section_levels[[j]] <= level) {
                    end_line <- section_lines[[j]] - 1
                    break
                }
                j <- j + 1
            }
            list(
                name = section_names[[i]],
                type = "section",
                start_line = start_line,
                end_line = end_line
            )
        })
    }

    chunks <- NULL
    if ("chunk" %in% type) {
        unnamed_chunks <- 0
        chunks <- lapply(seq_len(length(block_lines) / 2), function(i) {
            start_line <- block_lines[[2 * i - 1]]
            end_line <- block_lines[[2 * i]]
            label <- stringi::stri_match_first_regex(
                content[[start_line]],
                "^\\s*```+\\s*\\{[a-zA-Z0-9_]+\\s*(([^,='\"]+)|'(.+)'|\"(.+)\")\\s*(,.+)?\\}\\s*$"
            )[1, 3:5]
            name <- label[!is.na(label)]

            if (length(name) == 0) {
                unnamed_chunks <<- unnamed_chunks + 1
                name <- sprintf("unnamed-chunk-%d", unnamed_chunks)
            }

            list(
                name = name,
                type = "chunk",
                start_line = start_line,
                end_line = end_line
            )
        })
    }

    c(sections, chunks)
}
