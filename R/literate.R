#' Parse the structural regions of an R Markdown or Quarto document
#'
#' The returned model is line preserving. Fenced code blocks and fenced divs
#' are parsed independently, so fence-like text inside a code block cannot
#' change the interpretation of the surrounding document.
#' @noRd
parse_literate_regions <- function(content) {
    nline <- length(content)
    line_type <- rep("markdown", nline)
    cells <- list()
    divs <- list()
    yaml <- list()

    if (!nline) {
        return(list(
            line_type = character(), yaml = list(), markdown = list(),
            divs = list(), cells = list()
        ))
    }

    yaml_end <- 0L
    if (grepl("^\ufeff?---[ \\t]*$", content[[1L]], perl = TRUE)) {
        closing <- which(grepl("^(?:---|\\.\\.\\.)[ \\t]*$", content, perl = TRUE))
        closing <- closing[closing > 1L]
        yaml_end <- if (length(closing)) closing[[1L]] else nline
        line_type[seq_len(yaml_end)] <- "yaml"
        yaml[[1L]] <- list(
            kind = "front-matter", start_line = 1L, end_line = yaml_end,
            closed = length(closing) > 0L
        )
    }

    fence_open <- function(line) {
        match <- regexec(
            "^ {0,3}((?:`{3,})|(?:~{3,}))[ \\t]*(.*)$",
            line, perl = TRUE
        )[[1L]]
        if (match[[1L]] == -1L) return(NULL)
        lengths <- attr(match, "match.length")
        marker <- substr(
            line, match[[2L]], match[[2L]] + lengths[[2L]] - 1L)
        info <- substr(
            line, match[[3L]], match[[3L]] + lengths[[3L]] - 1L)
        list(
            marker = marker,
            character = substr(marker, 1L, 1L),
            length = nchar(marker),
            info = trimws(info)
        )
    }

    fence_close <- function(line, fence) {
        pattern <- sprintf(
            "^ {0,3}%s{%d,}[ \\t]*$",
            if (fence$character == "~") "~" else "`",
            fence$length
        )
        grepl(pattern, line, perl = TRUE)
    }

    cell_engine <- function(info) {
        match <- regexec(
            "^\\{[ \\t]*([[:alnum:]][[:alnum:]_.+-]*)(?:[ \\t,}]|$)",
            info, perl = TRUE
        )[[1L]]
        if (match[[1L]] == -1L) return(NULL)
        lengths <- attr(match, "match.length")
        substr(info, match[[2L]], match[[2L]] + lengths[[2L]] - 1L)
    }

    header_label <- function(info, engine) {
        if (is.null(engine)) return(NULL)
        rest <- sub(
            "^\\{[ \\t]*[[:alnum:]][[:alnum:]_.+-]*", "", info,
            perl = TRUE)
        rest <- sub("}[ \\t]*$", "", rest, perl = TRUE)
        rest <- trimws(rest)
        rest <- sub("^,[ \\t]*", "", rest, perl = TRUE)
        if (!nzchar(rest)) return(NULL)

        first <- substr(rest, 1L, 1L)
        if (first %in% c("'", "\"")) {
            tail <- substring(rest, 2L)
            closing <- regexpr(first, tail, fixed = TRUE)[[1L]]
            label <- if (closing > 0L) {
                substr(tail, 1L, closing - 1L)
            } else {
                tail
            }
        } else {
            label <- strsplit(rest, ",", fixed = TRUE)[[1L]][[1L]]
            label <- trimws(label)
        }
        if (!nzchar(label) || grepl("=", label, fixed = TRUE)) NULL else label
    }

    option_label <- function(lines) {
        for (line in lines) {
            match <- regexec(
                "^[ \\t]*#\\|[ \\t]*label[ \\t]*:[ \\t]*(.*?)[ \\t]*$",
                line, perl = TRUE
            )[[1L]]
            if (match[[1L]] == -1L) next
            lengths <- attr(match, "match.length")
            label <- substr(
                line, match[[2L]], match[[2L]] + lengths[[2L]] - 1L)
            label <- trimws(label)
            if (nchar(label) >= 2L &&
                    substr(label, 1L, 1L) == substr(label, nchar(label), nchar(label)) &&
                    substr(label, 1L, 1L) %in% c("'", "\"")) {
                label <- substr(label, 2L, nchar(label) - 1L)
            }
            if (nzchar(label)) return(label)
        }
        NULL
    }

    div_stack <- list()
    cell <- NULL
    i <- yaml_end + 1L
    while (i <= nline) {
        line <- content[[i]]

        if (!is.null(cell)) {
            if (fence_close(line, cell$fence)) {
                cell$end_line <- i
                cell$body_end <- i - 1L
                cell$closed <- TRUE
                cells[[length(cells) + 1L]] <- cell
                cell <- NULL
            }
            i <- i + 1L
            next
        }

        opening <- fence_open(line)
        if (!is.null(opening)) {
            engine <- cell_engine(opening$info)
            cell <- list(
                start_line = i,
                body_start = i + 1L,
                body_end = nline,
                end_line = nline,
                closed = FALSE,
                fence = opening,
                engine = engine,
                executable = !is.null(engine),
                label = header_label(opening$info, engine),
                option_lines = integer()
            )
            i <- i + 1L
            next
        }

        div_match <- regexec(
            "^ {0,3}(:{3,})[ \\t]*(.*)$", line, perl = TRUE
        )[[1L]]
        if (div_match[[1L]] != -1L) {
            lengths <- attr(div_match, "match.length")
            marker <- substr(
                line,
                div_match[[2L]],
                div_match[[2L]] + lengths[[2L]] - 1L
            )
            attributes <- substr(
                line,
                div_match[[3L]],
                div_match[[3L]] + lengths[[3L]] - 1L
            )
            attributes <- trimws(attributes)
            if (nzchar(attributes)) {
                div_stack[[length(div_stack) + 1L]] <- list(
                    start_line = i,
                    end_line = nline,
                    marker_length = nchar(marker),
                    attributes = attributes,
                    depth = length(div_stack) + 1L,
                    closed = FALSE
                )
                line_type[[i]] <- "div"
            } else if (length(div_stack) &&
                    nchar(marker) >= div_stack[[length(div_stack)]]$marker_length) {
                current <- div_stack[[length(div_stack)]]
                current$end_line <- i
                current$closed <- TRUE
                divs[[length(divs) + 1L]] <- current
                div_stack[[length(div_stack)]] <- NULL
                line_type[[i]] <- "div"
            }
        }
        i <- i + 1L
    }

    if (!is.null(cell)) {
        cells[[length(cells) + 1L]] <- cell
    }
    if (length(div_stack)) {
        divs <- c(divs, rev(div_stack))
    }

    for (index in seq_along(cells)) {
        current <- cells[[index]]
        cell_lines <- seq.int(current$start_line, current$end_line)
        line_type[cell_lines] <- if (current$executable) "cell" else "code"

        body_lines <- seq_safe(current$body_start, current$body_end)
        if (length(body_lines)) {
            option_lines <- body_lines[grepl(
                "^[ \\t]*#\\|(?:[ \\t]|$)", content[body_lines], perl = TRUE
            )]
            current$option_lines <- option_lines
            if (length(option_lines)) {
                line_type[option_lines] <- "yaml"
                yaml[[length(yaml) + 1L]] <- list(
                    kind = "cell-options",
                    start_line = min(option_lines),
                    end_line = max(option_lines),
                    lines = option_lines,
                    cell_start_line = current$start_line,
                    closed = TRUE
                )
            }
            if (current$executable && identical(tolower(current$engine), "r")) {
                r_lines <- setdiff(body_lines, option_lines)
                line_type[r_lines] <- "r"
            }
            if (is.null(current$label)) {
                current$label <- option_label(content[option_lines])
            }
        }
        cells[[index]] <- current
    }

    # Div boundary lines are structural even when nested. Cell boundaries take
    # precedence because div-like text inside a code cell is literal code.
    for (current in divs) {
        boundaries <- c(current$start_line, if (current$closed) current$end_line)
        boundaries <- boundaries[line_type[boundaries] == "markdown"]
        line_type[boundaries] <- "div"
    }

    contiguous_regions <- function(lines, kind) {
        if (!length(lines)) return(list())
        groups <- cumsum(c(TRUE, diff(lines) != 1L))
        lapply(split(lines, groups), function(group) {
            list(
                kind = kind,
                start_line = group[[1L]],
                end_line = group[[length(group)]]
            )
        })
    }

    markdown <- contiguous_regions(which(line_type == "markdown"), "markdown")
    divs <- divs[order(vapply(divs, `[[`, integer(1L), "start_line"))]
    cells <- cells[order(vapply(cells, `[[`, integer(1L), "start_line"))]

    list(
        line_type = line_type,
        yaml = yaml,
        markdown = markdown,
        divs = divs,
        cells = cells
    )
}

#' Return the R code runs in a literate document
#' @noRd
literate_r_blocks <- function(content, regions = NULL) {
    if (is.null(regions)) regions <- parse_literate_regions(content)
    lines <- which(regions$line_type == "r")
    if (!length(lines)) return(list())
    groups <- cumsum(c(TRUE, diff(lines) != 1L))
    unname(lapply(split(lines, groups), function(group) {
        list(
            lines = as.integer(group),
            text = content[group]
        )
    }))
}

#' Return the executable R cell containing a zero-based row
#' @noRd
literate_r_cell_at <- function(regions, row) {
    line <- row + 1L
    for (cell in regions$cells) {
        if (!cell$executable || !identical(tolower(cell$engine), "r")) next
        if (line >= cell$body_start && line <= cell$body_end &&
                regions$line_type[[line]] == "r") {
            return(cell)
        }
    }
    NULL
}

#' Return the contiguous R run containing a zero-based row
#' @noRd
literate_r_run_at <- function(regions, row) {
    line <- row + 1L
    if (line < 1L || line > length(regions$line_type) ||
            regions$line_type[[line]] != "r") {
        return(NULL)
    }
    start <- line
    while (start > 1L && regions$line_type[[start - 1L]] == "r") {
        start <- start - 1L
    }
    end <- line
    while (end < length(regions$line_type) &&
            regions$line_type[[end + 1L]] == "r") {
        end <- end + 1L
    }
    list(start_line = start, end_line = end)
}

#' Build line-preserving R source from independently valid cells
#' @noRd
literate_r_content <- function(content, regions = NULL, parseable_only = FALSE) {
    if (is.null(regions)) regions <- parse_literate_regions(content)
    result <- rep("", length(content))

    for (cell in regions$cells) {
        if (!cell$executable || !identical(tolower(cell$engine), "r")) next
        body_lines <- seq_safe(cell$body_start, cell$body_end)
        r_lines <- body_lines[regions$line_type[body_lines] == "r"]
        if (!length(r_lines)) next

        if (isTRUE(parseable_only)) {
            cell_content <- rep("", length(body_lines))
            cell_content[match(r_lines, body_lines)] <- content[r_lines]
            parsed <- tryCatch(
                parse(text = cell_content, keep.source = TRUE),
                error = function(e) NULL
            )
            if (is.null(parsed)) next
        }
        result[r_lines] <- content[r_lines]
    }
    result
}

#' Get Markdown headings and executable cells from a literate document
#' @noRd
get_literate_document_sections <- function(content,
    type = c("section", "chunk"), regions = NULL) {
    if (!length(content)) return(NULL)
    if (is.null(regions)) regions <- parse_literate_regions(content)

    sections <- list()
    if ("section" %in% type) {
        heading_lines <- integer()
        heading_levels <- integer()
        heading_names <- character()

        for (i in seq_along(content)) {
            if (regions$line_type[[i]] != "markdown") next
            match <- regexec(
                "^ {0,3}(#{1,6})[ \\t]+(.+?)(?:[ \\t]+#+)?[ \\t]*$",
                content[[i]], perl = TRUE
            )[[1L]]
            if (match[[1L]] != -1L) {
                lengths <- attr(match, "match.length")
                hashes <- substr(
                    content[[i]], match[[2L]], match[[2L]] + lengths[[2L]] - 1L)
                name <- substr(
                    content[[i]], match[[3L]], match[[3L]] + lengths[[3L]] - 1L)
                heading_lines <- c(heading_lines, i)
                heading_levels <- c(heading_levels, nchar(hashes))
                heading_names <- c(heading_names, name)
                next
            }

            if (i > 1L && regions$line_type[[i - 1L]] == "markdown" &&
                    nzchar(trimws(content[[i - 1L]])) &&
                    grepl("^ {0,3}(?:=+|-+)[ \\t]*$", content[[i]], perl = TRUE)) {
                heading_lines <- c(heading_lines, i - 1L)
                heading_levels <- c(
                    heading_levels,
                    if (grepl("=", content[[i]], fixed = TRUE)) 1L else 2L
                )
                heading_names <- c(heading_names, trimws(content[[i - 1L]]))
            }
        }

        if (length(heading_lines)) {
            keep <- !duplicated(heading_lines)
            heading_lines <- heading_lines[keep]
            heading_levels <- heading_levels[keep]
            heading_names <- heading_names[keep]
            ordering <- order(heading_lines)
            heading_lines <- heading_lines[ordering]
            heading_levels <- heading_levels[ordering]
            heading_names <- heading_names[ordering]

            sections <- lapply(seq_along(heading_lines), function(i) {
                later <- which(
                    seq_along(heading_lines) > i &
                        heading_levels <= heading_levels[[i]]
                )
                end_line <- if (length(later)) {
                    heading_lines[[later[[1L]]]] - 1L
                } else {
                    length(content)
                }
                list(
                    name = heading_names[[i]], type = "section",
                    start_line = heading_lines[[i]], end_line = end_line
                )
            })
        }
    }

    chunks <- list()
    if ("chunk" %in% type) {
        executable <- Filter(function(cell) cell$executable, regions$cells)
        unnamed <- 0L
        chunks <- lapply(executable, function(cell) {
            name <- cell$label
            if (is.null(name) || !nzchar(name)) {
                unnamed <<- unnamed + 1L
                name <- sprintf("unnamed-chunk-%d", unnamed)
            }
            list(
                name = name,
                type = "chunk",
                engine = cell$engine,
                start_line = cell$start_line,
                end_line = cell$end_line
            )
        })
    }

    structural <- list()
    if ("yaml" %in% type) {
        structural <- c(structural, Filter(
            function(region) identical(region$kind, "front-matter"),
            regions$yaml
        ))
        structural <- lapply(structural, function(region) {
            c(list(name = "YAML", type = "yaml"), region)
        })
    }
    if ("div" %in% type) {
        div_regions <- lapply(regions$divs, function(region) {
            name <- region$attributes
            id <- regmatches(name, regexpr("#[[:alnum:]_-]+", name, perl = TRUE))
            class <- regmatches(name, regexpr("\\.[[:alnum:]_-]+", name, perl = TRUE))
            if (length(id) && nzchar(id)) name <- substring(id, 2L)
            else if (length(class) && nzchar(class)) name <- substring(class, 2L)
            list(
                name = name, type = "div",
                start_line = region$start_line, end_line = region$end_line
            )
        })
        structural <- c(structural, div_regions)
    }
    if ("code" %in% type) {
        code_regions <- lapply(
            Filter(function(cell) !cell$executable, regions$cells),
            function(cell) list(
                name = "code block", type = "code",
                start_line = cell$start_line, end_line = cell$end_line
            )
        )
        structural <- c(structural, code_regions)
    }

    c(sections, chunks, structural)
}
