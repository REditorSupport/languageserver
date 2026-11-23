glue_template_packages <- c(glue = "glue", glue_data = "glue", str_glue = "stringr", str_glue_data = "stringr")

# Identify directly quoted, unnamed template arguments of the supported glue functions.
glue_template_calls <- function(syntax, content, functions = which(syntax$token == "SYMBOL_FUNCTION_CALL")) {
    functions <- functions[syntax$text[functions] %in% names(glue_template_packages)]
    if (!length(functions)) return(list())
    calls <- lapply(functions, function(symbol) {
        head <- syntax$parent_row[[symbol]]
        if (!head || !syntax$parent_row[[head]]) return(NULL)
        row <- syntax$parent_row[[head]]
        call <- syntax_call_arguments(syntax, row)
        if (!identical(call$name, syntax$text[[symbol]])) return(NULL)
        if (!is.null(call$package) && (call$package != glue_template_packages[[call$name]] ||
                    !"NS_GET" %in% syntax$token[syntax_children(syntax, head)])) return(NULL)
        keys <- names(call$values)
        # Any explicit syntax override can change how braces are interpreted.
        if (any(keys %in% c(".open", ".close", ".comment", ".literal", ".transformer"))) return(NULL)
        values <- unname(call$values[!nzchar(keys)])
        received <- syntax_pipe_arguments(syntax, row, call)
        piped_text <- !identical(received$values, call$values) && !endsWith(call$name, "_data")
        if (endsWith(call$name, "_data")) {
            matched <- class_matched_arguments(received, c(".x", "..."))
            values <- setdiff(values, matched$.x)
        }
        rows <- vapply(values, function(value) class_literal_row(syntax, value, "STR_CONST"), integer(1L))
        if (!any(rows)) return(NULL)
        direct <- rows[rows > 0L]
        quotes <- substr(content[syntax$line1[direct]], syntax$col1[direct], syntax$col1[direct])
        quoted <- quotes %in% c('"', "'")
        if (!any(quoted)) return(NULL)
        list(row = row, symbol = symbol, name = call$name, package = call$package,
            lexical_prefix = !endsWith(call$name, "_data") && !piped_text && !any(nzchar(keys)), literals = direct[quoted],
            all_direct = !piped_text && length(direct) == length(rows) && all(quoted),
            separator = any(keys == ".sep"))
    })
    Filter(Negate(is.null), calls)
}

glue_literals_have_open <- function(syntax, content, rows) {
    spelling <- syntax$text[rows]
    for (i in which(startsWith(spelling, "["))) {
        spelling[[i]] <- paste(glue_literal_lines(syntax, rows[[i]], content), collapse = "\n")
    }
    any(grepl("{", spelling, fixed = TRUE))
}

glue_literal_lines <- function(syntax, row, content) {
    lines <- content[seq.int(syntax$line1[[row]], syntax$line2[[row]])]
    lines[[length(lines)]] <- substr(lines[[length(lines)]], 1L, syntax$col2[[row]])
    lines[[1L]] <- substring(lines[[1L]], syntax$col1[[row]])
    lines
}

# Decode an isolated quoted literal without evaluating R. Store physical offsets
# only when an escape changes width; ordinary text maps directly by code point.
glue_decode_literal <- function(lines, row, col) {
    spelling <- paste(lines, collapse = "\n")
    quote <- substr(spelling, 1L, 1L)
    body <- substr(spelling, 2L, nchar(spelling) - 1L)
    physical <- utf8ToInt(body)
    codes <- physical
    boundaries <- NULL
    if (any(physical == 92L)) {
        pattern <- "\\\\(?:[0-7]{1,3}|x[[:xdigit:]]{1,2}|u(?:\\{[[:xdigit:]]{1,4}\\}|[[:xdigit:]]{1,4})|U(?:\\{[[:xdigit:]]{1,8}\\}|[[:xdigit:]]{1,8})|[\\s\\S])"
        escapes <- gregexpr(pattern, body, perl = TRUE)[[1L]]
        if (escapes[[1L]] < 0L) return(NULL)
        widths <- attr(escapes, "match.length")
        values <- substring(body, escapes, escapes + widths - 1L)
        # Let R's lexer interpret escape values. Individual byte escapes that
        # only form a character together do not have an exact intermediate map.
        decoded <- vapply(unique(values), function(escape) {
            value <- tryCatch(parse(text = paste0('"', escape, '"'), keep.source = FALSE)[[1L]],
                error = function(e) NA_character_)
            if (!is.character(value) || is.na(value)) return(NA_integer_)
            code <- utf8ToInt(value)
            if (length(code) == 1L) code else NA_integer_
        }, integer(1L), USE.NAMES = FALSE)
        if (anyNA(decoded)) return(NULL)
        codes[escapes] <- decoded[match(values, unique(values))]
        # Each decoded escape begins at its physical backslash.
        kept <- rep(TRUE, length(codes))
        for (i in seq_along(escapes)) kept[seq.int(escapes[[i]] + 1L, length.out = widths[[i]] - 1L)] <- FALSE
        codes <- codes[kept]
        boundaries <- c(which(kept) - 1L, length(physical))
    }
    list(codes = codes, boundaries = boundaries, physical_lines = c(0L, which(physical == 10L)),
        row = row, col = col, quote = quote)
}

glue_offset_point <- function(offset, starts, row = 0L, col = 0L) {
    i <- findInterval(offset, starts)
    list(row = row + i - 1L, col = offset - starts[[i]] + if (i == 1L) col else 0L)
}

# R ranges are half-open. Both boundaries of an expression are valid insertion
# points, including the one between an empty pair of interpolation braces.
glue_make_fragment <- function(literal, start, end, call_row, complete) {
    codes <- literal$codes[seq.int(start + 1L, length.out = end - start)]
    offsets <- literal$boundaries
    from <- if (is.null(offsets)) start else offsets[[start + 1L]]
    to <- if (is.null(offsets)) end else offsets[[end + 1L]]
    physical <- literal$physical_lines
    first <- glue_offset_point(from, physical, literal$row, literal$col)
    last <- glue_offset_point(to, physical, literal$row, literal$col)
    if (!is.null(offsets) && to - from != end - start) {
        offsets <- offsets[seq.int(start + 1L, end + 1L)] - from
    } else {
        offsets <- NULL
    }
    text <- intToUtf8(codes)
    glue_fragment_record(strsplit(paste0(text, "\n"), "\n", fixed = TRUE)[[1L]],
        first, last, call_row, complete, literal$quote, end - start, offsets,
        c(0L, which(codes == 10L)), c(0L, physical[physical > from & physical <= to] - from))
}

glue_fragment_record <- function(content, start, end, call_row, complete, quote, width,
    boundaries = NULL, inner_lines = 0L, source_lines = 0L) {
    fragment <- list(content = content, start = start, end = end, call_row = call_row, complete = complete, quote = quote)
    if (is.null(boundaries) && length(inner_lines) == 1L && length(source_lines) == 1L) return(fragment)
    c(fragment, list(boundaries = boundaries, length = width, inner_lines = inner_lines, source_lines = source_lines))
}

# A single unescaped expression on a single physical line needs only its
# absolute endpoints. Classify those templates in a batch before running the
# stateful scanner for quoting, comments, nesting, or stitched arguments.
glue_straight_fragments <- function(syntax, content, calls, offset) {
    literal <- vapply(calls, function(call) {
        if (call$all_direct && length(call$literals) == 1L) call$literals else 0L
    }, integer(1L))
    i <- which(literal > 0L)
    rows <- literal[i]
    i <- i[syntax$line1[rows] == syntax$line2[rows]]
    rows <- literal[i]
    lines <- syntax$line1[rows]
    spelling <- substr(content[lines], syntax$col1[rows], syntax$col2[rows])
    plain <- !grepl("[{}\\\\]", spelling)
    simple <- grepl("^[^{}\\\\]*\\{[^{}\\\\'\"#\u0060]*\\}[^{}\\\\]*$", spelling)
    if (!any(simple)) return(list(calls = i[plain], fragments = vector("list", sum(plain))))
    ids <- i[simple]
    text <- spelling[simple]
    from <- regexpr("{", text, fixed = TRUE) + 1L
    to <- regexpr("}", text, fixed = TRUE) - 1L
    inner <- substring(text, from, to)
    cols <- syntax$col1[literal[ids]]
    start <- cols + from - 2L
    end <- cols + to - 1L
    host <- lines[simple] + offset - 1L
    quotes <- substr(text, 1L, 1L)
    mapped <- lapply(seq_along(ids), function(j) {
        list(glue_fragment_record(inner[[j]], list(row = host[[j]], col = start[[j]]),
                list(row = host[[j]], col = end[[j]]), calls[[ids[[j]]]]$row, TRUE,
                quotes[[j]], end[[j]] - start[[j]]))
    })
    list(calls = c(i[plain], ids), fragments = c(vector("list", sum(plain)), mapped))
}

# Scan glue's decoded input across all literal arguments. An expression belongs
# to a provider only when its two delimiters are in the same source literal.
glue_scan_call <- function(literals, call_row, provisional = FALSE) {
    codes <- unlist(lapply(literals, "[[", "codes"), use.names = FALSE)
    if (!any(codes == 123L)) return(list())
    # glue's default trim joins backslash-ended lines before tokenizing.
    # Such a join may assemble an identifier or a delimiter across source rows.
    if (length(codes) > 1L && any(codes[-length(codes)] == 92L & codes[-1L] == 10L)) {
        return(structure(list(), unmapped = list(list(call_row = call_row, content = NULL, complete = FALSE))))
    }
    ends <- cumsum(vapply(literals, function(x) length(x$codes), integer(1L)))
    starts <- c(0L, ends)[seq_along(literals)]
    result <- list()
    unmapped <- list()
    opening <- 0L
    depth <- 0L
    quoted <- 0L
    comment <- FALSE
    multiline_quote <- FALSE
    n <- length(codes)
    # Skip ordinary prose and identifiers in a single vectorized pass.
    special <- which(codes %in% c(123L, 125L, 92L, 34L, 39L, 96L, 35L, 10L))
    j <- 1L
    while (j <= length(special)) {
        i <- special[[j]]
        j <- j + 1L
        ch <- codes[[i]]
        if (!depth) {
            if (ch != 123L) next
            if (i < n && codes[[i + 1L]] == ch) {
                j <- j + 1L
                next
            }
            opening <- i
            multiline_quote <- FALSE
            depth <- 1L
        } else if (comment) {
            if (ch == 10L) comment <- FALSE
        } else if (quoted) {
            if (ch == 10L) multiline_quote <- TRUE
            if (ch == quoted) {
                quoted <- 0L
            } else if (ch == 92L && j <= length(special) && special[[j]] == i + 1L) {
                j <- j + 1L
            }
        } else if (ch %in% c(34L, 39L, 96L)) {
            quoted <- ch
        } else if (ch == 35L) {
            comment <- TRUE
        } else if (ch == 123L) {
            depth <- depth + 1L
        } else if (ch == 125L) {
            depth <- depth - 1L
            if (!depth) {
                k <- findInterval(opening - 1L, starts)
                # Default indentation trimming is immaterial to R tokens except
                # inside a quoted multiline token, whose value may change.
                if (i <= ends[[k]] && !multiline_quote) {
                    result[[length(result) + 1L]] <- glue_make_fragment(literals[[k]], opening - starts[[k]],
                        i - starts[[k]] - 1L, call_row, !provisional)
                } else {
                    text <- intToUtf8(codes[seq.int(opening + 1L, length.out = i - opening - 1L)])
                    unmapped[[length(unmapped) + 1L]] <- list(call_row = call_row,
                        content = strsplit(paste0(text, "\n"), "\n", fixed = TRUE)[[1L]], complete = !provisional)
                }
            }
        }
    }
    if (depth) {
        k <- findInterval(opening - 1L, starts)
        if (k == length(literals) && !multiline_quote) {
            result[[length(result) + 1L]] <- glue_make_fragment(literals[[k]], opening - starts[[k]],
                n - starts[[k]], call_row, FALSE)
        } else {
            unmapped[[length(unmapped) + 1L]] <- list(call_row = call_row, content = NULL, complete = FALSE)
        }
    }
    attr(result, "unmapped") <- unmapped
    result
}

glue_template_index <- function(syntax, content, offset = 0L, provisional = FALSE,
    calls = glue_template_calls(syntax, content)) {
    # A generated report can contain thousands of calls. Accumulate per-call
    # results and flatten once to avoid repeatedly copying all previous spans.
    unmapped <- fragments <- vector("list", length(calls))
    remaining <- seq_along(calls)
    if (!provisional && length(calls) > 1L) {
        straight <- glue_straight_fragments(syntax, content, calls, offset)
        fragments[straight$calls] <- straight$fragments
        remaining <- setdiff(remaining, straight$calls)
    }
    for (i in remaining) {
        call <- calls[[i]]
        # The value of a computed unnamed argument or separator can affect
        # delimiters in the following literal.
        call_row <- if (provisional) 0L else call$row
        unknown <- list(call_row = call_row, content = NULL, complete = FALSE)
        if (!call$all_direct || (length(call$literals) > 1L && call$separator)) {
            if (glue_literals_have_open(syntax, content, call$literals)) unmapped[[i]] <- list(unknown)
            next
        }
        literals <- lapply(call$literals, function(row) {
            glue_decode_literal(glue_literal_lines(syntax, row, content),
                syntax$line1[[row]] - 1L + offset, syntax$col1[[row]])
        })
        if (!any(vapply(literals, is.null, logical(1L)))) {
            found <- glue_scan_call(literals, call_row, provisional)
            unmapped[i] <- list(attr(found, "unmapped"))
            if (provisional && length(found)) {
                # The temporary call row belongs to a different tree. Preserve
                # the physical callee and its argument shape for advisory
                # completion while the user is typing the outer string.
                callee <- list(row = syntax$line1[[call$symbol]] + offset - 1L, col = syntax$col1[[call$symbol]] - 1L)
                found <- lapply(found, function(fragment) {
                    c(fragment, list(lexical_prefix = call$lexical_prefix, call_start = callee,
                            call_name = call$name, package = call$package))
                })
            }
            fragments[[i]] <- found
        } else if (glue_literals_have_open(syntax, content, call$literals)) {
            unmapped[[i]] <- list(unknown)
        }
    }
    list(fragments = unlist(fragments, recursive = FALSE, use.names = FALSE),
        unmapped = unlist(unmapped, recursive = FALSE, use.names = FALSE))
}

# R detaches the unfinished call's lexer tokens until its closing parenthesis
# appears. Complete just the final expression, bounded by its physical lines.
partial_glue_template_index <- function(data, content, offset = 0L) {
    candidates <- which(data$text %in% names(glue_template_packages))
    if (!length(candidates)) return(NULL)
    if (any(data$token[candidates] == "SYMBOL") && any(data$parent == 0L & data$token == "'('")) {
        first <- min(data$line1)
        last <- min(length(content), max(data$line2))
        if (last >= first && last - first < 64L) {
            lines <- content[seq.int(first, last)]
            if (sum(nchar(lines, type = "bytes")) < 16384L) {
                # Appending exactly one quote to the last physical line permits
                # completion while typing the template, before the string closes.
                # Reconstruct unmatched delimiters from lexer terminals, never
                # from prose in the potentially unfinished string.
                brackets <- data$text[data$terminal & data$token %in%
                        c("'('", "')'", "'{'", "'}'", "'['", "']'", "LBB")]
                if (length(brackets) > 256L) return(NULL)
                closers <- character()
                pairs <- c("(" = ")", "{" = "}", "[" = "]", "[[" = "]]")
                for (bracket in brackets) {
                    if (bracket %in% names(pairs)) {
                        closers <- c(strsplit(pairs[[bracket]], "", fixed = TRUE)[[1L]], closers)
                    } else if (length(closers) && bracket == closers[[1L]]) {
                        closers <- closers[-1L]
                    }
                }
                if (length(closers) > 32L) return(NULL)
                for (suffix in paste0(c("", '"', "'"), paste(closers, collapse = ""))) {
                    repaired <- lines
                    repaired[[length(lines)]] <- paste0(lines[[length(lines)]], suffix)
                    expr <- parse_provider_projection(normalize_parse_content(repaired))
                    if (!is.null(expr)) {
                        return(glue_template_index(syntax_parse_data(utils::getParseData(expr)), repaired,
                                offset + first - 1L, provisional = TRUE))
                    }
                }
            }
        }
    }
    glue_template_index(syntax_parse_data(data), content, offset, provisional = TRUE)
}

glue_order_fragments <- function(fragments, unmapped) {
    if (!length(fragments)) return(if (length(unmapped)) structure(list(), unmapped = unmapped))
    rows <- vapply(fragments, function(x) x$start$row, integer(1L))
    cols <- vapply(fragments, function(x) x$start$col, integer(1L))
    # Structural results come first and take precedence over provisional rows.
    keep <- !duplicated(data.frame(row = rows, col = cols))
    i <- which(keep)[order(rows[keep], cols[keep])]
    structure(fragments[i], rows = rows[i], unmapped = if (length(unmapped)) unmapped)
}

glue_source_point <- function(fragment, inner_point) {
    row <- inner_point$row + 1L
    col <- inner_point$col
    if (is.null(fragment$inner_lines)) {
        if (row != 1L || col < 0L || col > fragment$end$col - fragment$start$col) return(NULL)
        return(list(row = fragment$start$row, col = fragment$start$col + col))
    }
    if (row < 1L || row > length(fragment$inner_lines) || col < 0L) return(NULL)
    at <- fragment$inner_lines[[row]] + col
    if (at > fragment$length || (row < length(fragment$inner_lines) && at >= fragment$inner_lines[[row + 1L]])) return(NULL)
    if (!is.null(fragment$boundaries)) at <- fragment$boundaries[[at + 1L]]
    glue_offset_point(at, fragment$source_lines, fragment$start$row, fragment$start$col)
}

glue_inner_point <- function(fragment, host_point) {
    row <- host_point$row - fragment$start$row + 1L
    col <- host_point$col - if (row == 1L) fragment$start$col else 0L
    lines <- fragment$source_lines
    if (is.null(lines)) {
        if (row != 1L || col < 0L || host_point$col > fragment$end$col) return(NULL)
        return(list(row = 0L, col = col))
    }
    if (row < 1L || row > length(lines) || col < 0L) return(NULL)
    at <- lines[[row]] + col
    if (row < length(lines) && at >= lines[[row + 1L]]) return(NULL)
    if (!is.null(fragment$boundaries)) {
        i <- findInterval(at, fragment$boundaries)
        if (!i || at != fragment$boundaries[[i]]) return(NULL)
        at <- i - 1L
    }
    if (at > fragment$length) return(NULL)
    glue_offset_point(at, fragment$inner_lines)
}

glue_fragment_at <- function(parsed, point) {
    fragments <- parsed$glue_templates
    if (!length(fragments)) return(NULL)
    i <- findInterval(point$row, attr(fragments, "rows"))
    while (i > 0L) {
        fragment <- fragments[[i]]
        if (fragment$end$row < point$row) break
        if (!is.null(glue_inner_point(fragment, point))) return(fragment)
        i <- i - 1L
    }
    NULL
}

# Re-encode provider replacements for the original quote style. In particular
# a backslash introduced by a rename or completion must survive the outer lexer.
glue_encode_text <- function(fragment, text) {
    text <- gsub("\\", "\\\\", text, fixed = TRUE)
    gsub(fragment$quote, paste0("\\", fragment$quote), text, fixed = TRUE)
}
