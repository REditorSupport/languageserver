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

# ?Syntax
# : can indicate :: :::
# - can indicate <-
# > can indicate |>
binary_opts <- c(
    ":", "\\$", "@", "\\^",
    "%[^#]*%", "\\+", "-", "\\*", "/",
    "<", ">", "=", "!", "&", "\\|", "~",
    "\\?"
)
binary_opts_regex <- paste0(binary_opts, collapse = "|")

# options, number of blank lines to break sections or binary ranges
# default 2L, the same with markdown
nline_to_break_succession <- 2L

#---------------------------r document utils functions------------------------#
get_r_document_sections_and_blocks <- function(content, xdoc, symbol = FALSE) {
    block_lines_list <- extract_document_block_lines(xdoc)
    sections_and_binary_ranges <- get_r_document_ranges_rec(
        content = content,
        start_line = 1L,
        end_line = length(content),
        block_lines_list = block_lines_list,
        symbol = symbol
    )
    if (symbol) {
        sections_and_binary_ranges$sections
    } else {
        block_ranges <- .mapply(function(start_line, end_line) {
            list(
                type = "block",
                start_line = start_line,
                end_line = end_line
            )
        }, block_lines_list, NULL)
        c(
            sections_and_binary_ranges$sections,
            sections_and_binary_ranges$binary_ranges,
            block_ranges
        )
    }
}

# for a r document, we split the document into two element, one for document
# content out of blocks, another for document content in blocks.
# ** For document content out of blocks, we define section ranges and binary
#   ranges.
# ** For document content in blocks, we only need reuse this function to treat
#   each blocks as a new r document.
get_r_document_ranges_rec <- function(content, start_line, end_line, block_lines_list, symbol) {
    if (start_line > end_line) {
        return(NULL)
    }
    line_seq <- start_line:end_line
    # only keep blocks between start_line and end_line
    block_lines_list <- lapply(
        block_lines_list, "[",
        block_lines_list[[1L]] >= start_line &
            block_lines_list[[2L]] <= end_line & !(
            block_lines_list[[1L]] == start_line &
                block_lines_list[[2L]] == end_line
        )
    )

    if (length(block_lines_list) && length(block_lines_list[[1L]])) {
        # block_lines_list define the blocks, the first element is the block
        # start lines, the second element is the block end lines, we first
        # extract blocks not nested in other blocks.
        highest_level_block_lines <- lapply(block_lines_list, `[`, unlist(
            .mapply(function(start_line, end_line) {
                !any(start_line >= block_lines_list[[1L]] &
                    end_line <= block_lines_list[[2L]] &
                    !(start_line == block_lines_list[[1L]] &
                        end_line == block_lines_list[[2L]]))
            }, block_lines_list, NULL),
            recursive = FALSE, use.names = FALSE
        ))
        # for each out-est blocks, we regard it as a new document and re-use
        # this function to get ranges
        ranges_in_blocks <- .mapply(function(start_line, end_line) {
            get_r_document_ranges_rec( # recursive function
                content = content,
                start_line = start_line,
                end_line = end_line,
                block_lines_list = block_lines_list,
                symbol = symbol
            )
        }, highest_level_block_lines, NULL)
        ranges_in_blocks <- list(
            sections = lapply(ranges_in_blocks, `[[`, "sections"),
            binary_ranges = lapply(ranges_in_blocks, `[[`, "binary_ranges")
        )
        ranges_in_blocks <- lapply(ranges_in_blocks, unlist,
            recursive = FALSE, use.names = FALSE
        )
        lines_out_blocks <- extract_lines_out_blocks(
            line_seq, highest_level_block_lines
        )
    } else {
        ranges_in_blocks <- NULL
        highest_level_block_lines <- NULL
        lines_out_blocks <- line_seq
    }
    sections_out_blocks <- NULL
    binary_out_blocks <- NULL
    if (length(lines_out_blocks)) {
        sections_out_blocks <- get_r_document_section_ranges(
            content, lines_out_blocks
        )
        if (!symbol) {
            binary_out_blocks <- get_r_document_binary_ranges(
                content, lines_out_blocks, highest_level_block_lines
            )
        }
    }
    list(
        sections = c(sections_out_blocks, ranges_in_blocks$sections),
        binary_ranges = c(binary_out_blocks, ranges_in_blocks$binary_ranges)
    )
}

# A section group should define all blocks and sub-sections in current
# section and the end line of current section. section groups are like following
# document:
# group 1 -----
# block1 <- function() {
#     ...
# }
# block2 <- function() {
#     ...
# }
#
#
# two blank lines (defined by function extract_document_section_breaks()) will
# break the document succession, so following document will be in next group.
# group 2 ----
# following blocks will be in the group2
# block3 <- function() {
#     ...
# }
# block4 <- function() {
#     ...
# }
get_r_document_section_ranges <- function(content, line_numbers) {
    line_content <- content[line_numbers]
    section <- extract_document_section(line_numbers, line_content)
    section_ranges <- NULL
    if (!is.null(section)) {
        break_lines <- extract_document_section_breaks(
            line_numbers, line_content
        )
        section_group_list <- define_section_groups(
            line_numbers, line_content, section, break_lines
        )
        section_ranges <- lapply(section_group_list, function(x) {
            get_section_group_ranges(
                section_lines = x$section_lines,
                section_names = x$section_names,
                section_levels = x$section_levels,
                group_endline = x$group_endline
            )
        })
        section_ranges <- unlist(section_ranges,
            recursive = FALSE, use.names = FALSE
        )
    }
    section_ranges
}

get_r_document_binary_ranges <- function(content, line_numbers, block_lines_list) {
    is_binary_line <- grepl(
        paste0("^[^#]*(", binary_opts_regex, ")\\s*(#.*)?$"),
        content[line_numbers],
        perl = TRUE
    )
    if (any(is_binary_line)) {
        start_lines <- line_numbers[is_binary_line]
        # the end line should be the first non-blank lines in the next
        # `nline_to_break_succession` lines, if all are blank lines, the start
        # lines will be used
        end_lines <- lapply(seq_len(nline_to_break_succession), function(x) {
            ends <- start_lines + x
            ends[grepl("^\\s*$", content[ends], perl = TRUE)] <- NA_integer_
            ends
        })
        end_lines <- c(end_lines, list(na.rm = TRUE))
        end_lines <- pmax(start_lines, do.call(pmin, end_lines), na.rm = TRUE)

        # then we merge the binary ranges with blocks, any overlapped ranges
        # will be merged into one interval.
        ranges <- merge_intervals(
            start = c(start_lines, block_lines_list[[1L]]),
            end = c(end_lines, block_lines_list[[2L]] + 1L)
        )
        .mapply(function(start_line, end_line) {
            list(
                type = "binary_ops",
                start_line = start_line,
                end_line = end_line
            )
        }, ranges, NULL)
    } else {
        NULL
    }
}

merge_intervals <- function(start, end) {
    order_idx <- order(start, end)
    start <- start[order_idx]
    end <- end[order_idx]
    len <- length(start)
    if (len >= 2L) {
        groups <- cumsum(c(0L, end[1:(len - 1L)] < start[-1L]))
        if (anyDuplicated(groups)) {
            groups <- factor(groups)
            start <- vapply(split(start, groups), min,
                integer(1L),
                USE.NAMES = FALSE
            )
            end <- vapply(split(end, groups), max,
                integer(1L),
                USE.NAMES = FALSE
            )
            Recall(start, end)
        } else {
            list(start, end)
        }
    } else {
        list(start, end)
    }
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
        as.integer(xml_attr(block_end, "line1")) - 1L
    ))
    lapply(1:2, function(i) block_lines[, i, drop = TRUE])
}

extract_lines_out_blocks <- function(line_seq, block_lines_list) {
    block_span_lines <- unlist(
        .mapply(`:`, block_lines_list, NULL),
        recursive = FALSE, use.names = FALSE
    )
    lines_out_blocks <- setdiff(line_seq, block_span_lines)
    # should keep the last line, otherwise, the final block will not included in
    # the last section, like follows:
    # last section ---------------
    # fn1 <- function() {
    #     ...
    # }
    unique(c(lines_out_blocks, max(line_seq)))
}

#' @return A list of start line number, section names and levels if there are
#' some sections. if no sections defined, NULL will be returned.
#' @noRd
extract_document_section <- function(line_numbers, line_content) {
    # extract comment line with at least 4 of one of `section_range_regex`
    is_section_lines <- grepl(
        paste0(
            "^\\s*\\#",
            "(", section_level_regex, ")\\s*+", # section levels group
            "(%%)?\\s*+",
            "(\\S.+?)\\s*+", # section names group
            "(", section_range_regex, ")\\s*$"
        ),
        line_content,
        perl = TRUE
    )
    if (any(is_section_lines)) {
        section_lines <- line_numbers[is_section_lines]
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
        return(list(
            lines = section_lines,
            names = section_names,
            levels = section_levels
        ))
    }
    NULL
}

#' two or more blank lines out of block ranges should break sections succession
#' @noRd
extract_document_section_breaks <- function(line_numbers, line_content) {
    blank_lines <- line_numbers[
        grepl("^\\s*$", line_content, perl = TRUE)
    ]
    if (!length(blank_lines)) {
        return(NULL)
    }
    # make continuous blank lines as groups
    groups <- split(blank_lines, factor(cumsum(diff(c(0L, blank_lines)) > 1L)))
    # how many lines should break off section succession ? ( 2L )
    groups <- groups[lengths(groups) >= nline_to_break_succession]
    if (length(groups)) {
        vapply(groups, min, integer(1L))
    } else {
        NULL
    }
}

define_section_groups <- function(line_numbers, line_content, section, break_lines) {
    if (length(break_lines)) {
        # break_lines is exclusive, so we should add 1 to the end line.
        section_breaks <- sort(c(break_lines, max(line_numbers) + 1L))
        # section_group_index is the group number.
        section_group_index <- findInterval(
            section$lines,
            c(0L, section_breaks),
            rightmost.closed = TRUE
        )
        section_groups <- lapply(
            section, split,
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
                section_lines = section$lines,
                section_names = section$names,
                section_levels = section$levels,
                group_endline = max(line_numbers)
            )
        )
    }
    section_data_groups
}

get_section_group_ranges <- function(section_lines, section_names, section_levels, group_endline) {
    section_index <- seq_along(section_lines)
    # the section end line should be the first occurence among
    # following document where the section level is equal or lower than
    # current section level, otherwise, the end line of this section group
    section_end_lines <- vapply(section_index, function(i) {
        # extract sections after current section
        section_index_after_i <- setdiff(section_index, seq_len(i))
        # when no higher-level section is after current section
        # the end line should be the end of current document
        if (!length(section_index_after_i)) {
            return(group_endline)
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
            return(group_endline)
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
