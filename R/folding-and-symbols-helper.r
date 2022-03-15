# define suffix marks for section and prefix marks for section levels
# ** use `section_mark_suffix` to indicate section beginning
section_mark_suffix <- c("#", "+", "-", "=", "*")
# *** numbers over 2 is indicative of section name (section range with one line)
# *** see `get_r_document_one_line_symbols`
section_symbol_regex <- paste0(
    "\\", section_mark_suffix, "{2,}",
    collapse = "|"
)
# *** numbers over 4 is indicative of section range
# *** see `get_r_document_sections`
section_range_regex <- paste0(
    "\\", section_mark_suffix, "{4,}",
    collapse = "|"
)

# ** use `section_level_prefix` to indicate section levels
section_level_prefix <- c("*", "-", "+", "=")
section_level_regex <- paste0(
    "[", paste0("\\", section_level_prefix, collapse = ""), "]*+"
)

get_document_sections <- function(uri, document, xdoc, type = c("section", "chunk")) {
    if (document$is_rmarkdown) {
        get_rmd_document_sections(uri, document, type)
    } else {
        get_r_document_sections(uri, document, xdoc = xdoc)
    }
}

get_document_symbols <- function(uri, document, xdoc, type = c("section", "chunk")) {
    if (document$is_rmarkdown) {
        get_rmd_document_sections(uri, document, type)
    } else {
        get_r_document_symbols(uri, document, xdoc = xdoc)
    }
}

get_r_document_sections <- function(uri, document, xdoc) {
    
    # derive all line number and document content in a vector
    line_seq <- seq_len(document$nline)
    doc_content <- document$content

    break_r_document_sections(
        line_seq, doc_content, xdoc = xdoc, symbol = FALSE
    )

}

break_r_document_sections <- function(line_seq, doc_content, xdoc, symbol = FALSE) {
    blocks <- get_block_helper(xdoc)

    sections <- get_r_document_sections_helper(line_seq, doc_content)
    section_breaks <- get_r_document_section_breaks(line_seq, doc_content)
    section_breaks <- section_breaks
    if (length(section_breaks) && length(sections)) {
        sections <- lapply(sections, function(section) {
            break_in_section <- section_breaks > section$start_line &
                section_breaks < section$end_line

            # omit breaks in blocks
            if (length(blocks)) {
                block_list <- lapply(blocks, function(block) {
                    matrix(c(block$start_line, block$end_line), nrow = 1)
                })
                blocks <- do.call("rbind", block_list)
                break_in_block <- vapply(
                    section_breaks, function(section_break) {
                        any(section_break > blocks[, 1, drop = TRUE] &
                            section_break < blocks[, 2, drop = TRUE])
                    }, logical(1L)
                )
                break_in_section <- break_in_section & !break_in_block
            }
            if (any(break_in_section)) {
                break_line <- min(section_breaks[break_in_section])
                section$end_line <- break_line - 1
            }
            section
        })
    }
    if (!symbol) c(blocks, sections) else sections
}

get_r_document_sections_helper <- function(line_seq, doc_content) {

    # extract comment line with at least 4 of one of c("#", "+", "-", "=", "*")
    section_lines <- line_seq[
        grepl(
            paste0("^\\#.+", "(", section_range_regex, ")\\s*$"),
            doc_content,
            perl = TRUE
        )
    ]

    if (length(section_lines)) {
        # extract section marks of section levels and its name
        # ** section levels - the third column
        # ** section names - the fourth column
        section_levels_and_names <- stringi::stri_match_first(
            doc_content[section_lines],
            regex = paste0(
                "^\\#+\\s*(%%)?\\s*+",
                "(", section_level_regex, ")\\s*+", # section levels group
                "(.+?)\\s*+", # section names group
                "(", section_range_regex, ")\\s*$"
            )
        )

        # define section levels based on the number of one of
        # `section_level_prefix`
        section_levels <- nchar(
            section_levels_and_names[, 3, drop = TRUE]
        )

        # the section range end line should be the first occurence among
        # following document where the section level is equal or lower than
        # current section level, otherwise, the end line of this document
        section_end_lines <- vapply(seq_along(section_lines), function(i) {
            # extract sections after current section
            section_index_after_i <- setdiff(
                seq_along(section_lines), seq_len(i)
            )
            # when no section is after current section
            # the end line should be the end of current document
            if (!length(section_index_after_i)) {
                return(length(doc_content))
            }
            section_range_end_index <- which(
                section_levels[section_index_after_i] <= section_levels[[i]]
            )
            if (length(section_range_end_index)) {
                section_range_end_index <- section_range_end_index[[1]]
                return(
                    section_lines[
                        section_index_after_i[section_range_end_index]
                    ] - 1L
                )
            } else {
                return(length(doc_content))
            }
        }, integer(1L))

        section_names <- section_levels_and_names[, 4, drop = TRUE]
        sections <- .mapply(function(name, start_line, end_line) {
            list(
                name = name,
                type = "section",
                start_line = start_line,
                end_line = end_line
            )
        }, list(section_names, section_lines, section_end_lines), NULL)

        return(sections)
    }

    NULL
}

get_block_helper <- function(xdoc) {
    
    if (is.null(xdoc)) return(NULL)
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

# two or more blank lines out of block ranges should break sections succession
get_r_document_section_breaks <- function(line_seq, doc_content) {
    blank_lines <- line_seq[
        grepl("^\\s*$", doc_content, perl = TRUE)
    ]
    if (!length(blank_lines)) {
        return(NULL)
    }
    group <- split(blank_lines, cumsum(diff(c(0L, blank_lines)) != 1L))
    break_lines <- vapply(group, function(x) {
        if (length(x) >= 2) {
            return(min(x))
        } else {
            return(NA_integer_)
        }
    }, integer(1L))
    break_lines <- break_lines[!is.na(break_lines)]
    if (length(blank_lines)) break_lines else NULL
}

# get_document_sections <- function(uri, document, type = c("section", "chunk")) {
#     if (document$is_rmarkdown) {
#         get_rmd_document_sections(uri, document, type)
#     } else {
#         get_r_document_sections(seq_len(document$nline), document$content)
#     }
# }

# indent can be indicative of symbol object in vscode outline
get_r_document_one_line_symbols <- function(line_seq, doc_content) {
    label_lines <- line_seq[
        grepl(
            paste0(
                "^\\s+\\#.+",
                "(", section_symbol_regex, ")\\s*$"
            ),
            doc_content,
            perl = TRUE
        )
    ]

    if (length(label_lines)) {
        label_names <- sub(
            paste0(
                "^\\s+\\#+\\s*(%%)?\\s*(.+?)\\s*",
                "(", section_symbol_regex, ")\\s*$"
            ),
            "\\2", doc_content[label_lines],
            perl = TRUE
        )
        label_sections <- .mapply(function(name, line) {
            list(
                name = name,
                type = "symbol",
                start_line = line,
                end_line = line
            )
        }, list(label_names, label_lines), NULL)

        return(label_sections)
    }
    NULL
}

get_r_document_symbols <- function(uri, document, xdoc) {
    # derive all line number and document content in a vector
    line_seq <- seq_len(document$nline)
    doc_content <- document$content

    c(
        break_r_document_sections(
            line_seq, doc_content, xdoc = xdoc, symbol = TRUE
        ),
        get_r_document_one_line_symbols(line_seq, doc_content)
    )
}

get_rmd_document_sections <- function(uri, document, type = c("section", "chunk")) {
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
