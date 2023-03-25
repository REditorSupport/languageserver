# define suffix markers for section and prefix markers for section levels
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

#---------------------------r document utils functions------------------------#
get_r_document_sections_and_blocks <- function(content, xdoc, symbol = FALSE) {
    block_lines_list <- extract_document_block_lines(xdoc)
    section_ranges <- get_r_document_sections_rec(
        content = content,
        start_line = 1L,
        end_line = length(content),
        block_lines_list = block_lines_list
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
    }, block_lines_list, NULL)
    c(section_ranges, block_ranges)
}

# for a r document, we split the document into two element, one for document
# content out of blocks, another for document content in blocks.
# ** For document content out of blocks, we define a function
# `get_r_document_sections_base` to extract section ranges.
# ** For document content in blocks, we only need reuse this function to treat
# each blocks as a new r document.
get_r_document_sections_rec <- function(content, start_line, end_line, block_lines_list) {
    if (start_line > end_line) {
        return(NULL)
    }
    line_seq <- start_line:end_line
    # only keep blocks between start_line and end_line
    block_lines_list <- lapply(
        block_lines_list, "[",
        block_lines_list[[1L]] >= start_line &
            block_lines_list[[2L]] - 1L <= end_line & !(
            block_lines_list[[1L]] == start_line &
                block_lines_list[[2L]] - 1L == end_line
        )
    )

    lines_out_blocks <- extract_lines_out_blocks(
        line_seq, block_lines_list
    )
    sections_in_blocks <- NULL
    if (length(block_lines_list) && length(block_lines_list[[1L]])) {
        highest_level_block_lines <- lapply(block_lines_list, "[", unlist(
            .mapply(function(start_line, end_line) {
                !any(start_line >= block_lines_list[[1L]] &
                    end_line <= block_lines_list[[2L]] &
                    !(start_line == block_lines_list[[1L]] &
                        end_line == block_lines_list[[2L]]))
            }, block_lines_list, NULL),
            recursive = FALSE, use.names = FALSE
        ))

        sections_in_blocks <- .mapply(function(start_line, end_line) {
            get_r_document_sections_rec(# recursive function
                content = content,
                start_line = start_line,
                end_line = end_line - 1L,
                block_lines_list = block_lines_list
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
            content,
            lines_out_blocks
        )
    }
    c(sections_in_blocks, sections_out_blocks)
}

get_r_document_sections_base <- function(content, line_seq) {
    line_content <- content[line_seq]
    section_range_groups <- extract_section_groups(
        line_seq, line_content
    )
    res <- lapply(section_range_groups, function(x) {
        get_section_ranges(
            section_lines = x$section_lines,
            section_names = x$section_names,
            section_levels = x$section_levels,
            endline = x$group_endline
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

extract_lines_out_blocks <- function(line_seq, block_lines_list) {
    block_span_lines <- unlist(
        .mapply(`:`, block_lines_list, NULL),
        recursive = FALSE, use.names = FALSE
    )
    lines_out_blocks <- setdiff(line_seq, block_span_lines)
    # should keep the last line
    unique(c(lines_out_blocks, max(line_seq)))
}

#' `line_seq` is the line numbers of line_content, both have equal numbers
#' @noRd
extract_document_section_lines <- function(line_seq, line_content) {

    # extract comment line with at least 4 of one of `section_range_regex`
    is_section_lines <- grepl(
        paste0(
            "^\\s*\\#",
            "(", section_level_regex, ")\\s*+", # section levels group
            "(%%)?\\s*+",
            "(\\S.+?)\\s*+", # section names group
            "(", section_range_regex, ")\\s*$"),
        line_content,
        perl = TRUE
    )
    section_lines <- line_seq[is_section_lines]
    if (length(section_lines)) {
        # extract section markers of section levels and its name
        # this should be a matrix
        # ** section levels - the second column
        # ** section names - the fourth column
        section_levels_and_names <- stringi::stri_match_first(
            line_content[is_section_lines],
            regex = paste0(
                "^\\s*\\#",
                "(", section_level_regex, ")\\s*+", # section levels group
                "(%%)?\\s*+",
                "(\\S.+?)\\s*+", # section names group
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
        # we split sections_lines into different group based on `section_breaks`
        # section_group_index is the group number.
        section_group_index <- findInterval(
            sections_lines$lines,
            c(0L, section_breaks),
            rightmost.closed = TRUE
        )
        section_groups <- lapply(
            sections_lines, split,
            f = factor(section_group_index)
        )
        group_endlines <- section_breaks - 1L
        section_data_groups <- lapply(
            names(section_groups$lines), function(i) {
                group_endline <- group_endlines[as.integer(i)]
                list(
                    section_lines = section_groups$lines[[i]],
                    section_names = section_groups$names[[i]],
                    section_levels = section_groups$levels[[i]],
                    group_endline = group_endline
                )
            }
        )
    } else {
        section_data_groups <- list(
            list(
                section_lines = sections_lines$lines,
                section_names = sections_lines$names,
                section_levels = sections_lines$levels,
                group_endline = max(line_seq)
            )
        )
    }
    section_data_groups
}

get_section_ranges <- function(section_lines, section_names, section_levels, endline) {
    section_index <- seq_along(section_lines)
    # the section range end line should be the first occurence among
    # following document where the section level is equal or lower than
    # current section level, otherwise, the end line of this section group
    section_end_lines <- vapply(section_index, function(i) {
        # extract sections after current section
        section_index_after_i <- setdiff(
            section_index, seq_len(i)
        )
        # when no higher-level section is after current section
        # the end line should be the end of current document
        if (!length(section_index_after_i)) {
            return(endline)
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
            return(endline)
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

get_rmd_document_sections <- function(content, type = c("section", "chunk")) {
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
            while (front_end <= length(content)) {
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
            end_line <- length(content)
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
                "^\\s*```+\\s*\\{[a-zA-Z0-9_]+[\\s,]*(([^,='\"]+)|'(.+)'|\"(.+)\")\\s*(,.+)?\\}\\s*$"
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
