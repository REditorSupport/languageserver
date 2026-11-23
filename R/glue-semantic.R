# Classify inner R exactly as ordinary code. The language server normally
# leaves brackets and separators to the grammar; inside an R string they
# need explicit tokens to escape the editor's outer string style.
glue_semantic_classify <- function(source, data) {
    tokens <- semantic_parse_data(data, source, encode = FALSE)
    row <- tokens$lines
    from <- tokens$cols
    to <- from + tokens$lengths
    # Keep the reusable classification in decoded code points. Convert to
    # physical UTF-16 after projecting all occurrences back to the host.
    unicode <- which(nchar(source, type = "bytes") != nchar(source)) - 1L
    for (indices in split(which(row %in% unicode), row[row %in% unicode])) {
        n <- length(indices)
        pts <- code_point_from_unit(source[[row[[indices[[1L]]]] + 1L]], c(from[indices], to[indices]))
        from[indices] <- pts[seq_len(n)]
        to[indices] <- pts[n + seq_len(n)]
    }
    inner <- cbind(line = row, from = from, to = to, type = tokens$types, modifier = tokens$modifiers)
    punctuation <- which(data$terminal & data$token %in% c("'('", "')'", "'['", "']'", "LBB", "'{'", "'}'", "','", "';'"))
    if (length(punctuation)) {
        inner <- rbind(inner, cbind(data$line1[punctuation] - 1L, data$col1[punctuation] - 1L, data$col2[punctuation],
                SemanticTokenTypes$operator, 0L))
    }
    inner
}

# Parse each distinct expression once. Put simple one-line expressions in
# separate braced blocks of a bounded parse, so a continuation cannot consume
# the next fragment. Definitions and classes retain their own semantic scope.
glue_semantic_expressions <- function(sources) {
    one <- function(source) {
        source <- normalize_parse_content(source)
        expr <- parse_provider_projection(source)
        if (!is.null(expr)) glue_semantic_classify(source, utils::getParseData(expr))
    }
    result <- vector("list", length(sources))
    plain_reads <- logical(length(sources))
    single <- which(lengths(sources) == 1L)
    text <- vapply(sources[single], `[[`, "", 1L)
    batch <- single[!grepl("[{}#]", text) & nchar(text, type = "bytes") < 8192L]
    rest <- setdiff(seq_along(sources), batch)
    result[rest] <- lapply(sources[rest], one)
    if (!length(batch)) return(result)
    for (ids in split(batch, (seq_along(batch) - 1L) %/% 128L)) {
        source <- normalize_parse_content(as.vector(rbind("{", vapply(sources[ids], `[[`, "", 1L), "}")))
        expr <- parse_provider_projection(source)
        data <- if (!is.null(expr)) utils::getParseData(expr)
        separate <- is.null(data) || any(data$token %in% c("FUNCTION", "'\\\\'", "LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "forcond")) ||
            any(reference_symbol_names(data$text[data$token == "SYMBOL_FUNCTION_CALL"]) %in% names(class_call_helpers))
        if (separate) {
            result[ids] <- lapply(sources[ids], one)
            next
        }
        # Unqualified value tokens in these isolated fragments cannot create
        # bindings or refer to namespace members. Fast host-formal coloring
        # can therefore use their classified variable spans directly.
        plain_reads[ids] <- TRUE
        accesses <- which(data$token %in% c("NS_GET", "NS_GET_INT", "'$'", "'@'"))
        plain_reads[ids[(data$line1[accesses] - 2L) %/% 3L + 1L]] <- FALSE
        inner <- glue_semantic_classify(source, data)
        inner <- inner[inner[, "line"] %% 3L == 1L, , drop = FALSE]
        group <- factor(inner[, "line"] %/% 3L + 1L, levels = seq_along(ids))
        inner[, "line"] <- 0L
        result[ids] <- lapply(split(seq_len(nrow(inner)), group), function(rows) inner[rows, , drop = FALSE])
    }
    attr(result, "plain_reads") <- plain_reads
    result
}

# Restrict Unicode inspection to the affected lines, including on a range
# request into a large document.
glue_semantic_units <- function(content, lines, from, to) {
    used <- unique(lines)
    syntax_unit_columns(content[used + 1L], match(lines, used), from, to)
}

# A scanner boundary may denote an R escape (e.g. \x7b or \u{007d}). Its
# spelling still lies on one physical line. A synthetic recovery delimiter
# or an unfinished fragment has no corresponding physical brace.
glue_semantic_braces <- function(fragments, content) {
    first <- vapply(fragments, function(f) f$start$row, 0L)
    last <- vapply(fragments, function(f) f$end$row, 0L)
    from <- vapply(fragments, function(f) f$start$col, 0L)
    to <- vapply(fragments, function(f) f$end$col, 0L)
    opening <- substr(content[first + 1L], pmax.int(1L, from - 11L), from)
    closing <- substr(content[last + 1L], to + 1L, to + 12L)
    left <- as.integer(endsWith(opening, "{"))
    right <- as.integer(startsWith(closing, "}"))
    escaped <- which(!left)
    if (length(escaped)) {
        match <- regexpr("\\\\(?:173|x7[bB]|[uU](?:0*7[bB]|\\{0*7[bB]\\}))$", opening[escaped], perl = TRUE)
        left[escaped] <- pmax.int(0L, attr(match, "match.length"))
    }
    escaped <- which(!right)
    if (length(escaped)) {
        match <- regexpr("^\\\\(?:175|x7[dD]|[uU](?:0*7[dD]|\\{0*7[dD]\\}))", closing[escaped], perl = TRUE)
        right[escaped] <- pmax.int(0L, attr(match, "match.length"))
    }
    # An unclosed interpolation can end directly against a raw string's } delimiter.
    right[vapply(fragments, function(f) isTRUE(f$unclosed), logical(1L))] <- 0L
    list(first = first, last = last, from = from, to = to, left = left, right = right)
}

glue_semantic_project <- function(fragment, inner) {
    if (!nrow(inner)) return(inner)
    if (is.null(fragment$inner_lines)) {
        inner[, "line"] <- fragment$start$row
        inner[, c("from", "to")] <- inner[, c("from", "to")] + fragment$start$col
    } else {
        # Complex escapes and virtual newlines use the same source map as
        # completion and rename. Each semantic token occupies one source line.
        for (i in seq_len(nrow(inner))) {
            first <- glue_source_point(fragment, list(row = inner[i, "line"], col = inner[i, "from"]))
            last <- glue_source_point(fragment, list(row = inner[i, "line"], col = inner[i, "to"]))
            if (is.null(first) || is.null(last) || first$row != last$row) return(NULL)
            inner[i, c("line", "from", "to")] <- c(first$row, first$col, last$col)
        }
    }
    inner
}

# One argument, one expression, and an ordinary glue()/str_glue() call
# means there are no named, data-mask, or preceding interpolation bindings.
# Registered wrappers have no native fragment record. Recovered calls and
# shadowed names always go through the normal binding checks.
glue_semantic_native_calls <- function(workspace, document, row) {
    parsed <- document$parse_data
    native <- parsed$glue_templates
    all <- vapply(native, `[[`, 0L, "call_row")
    uncertain <- vapply(attr(native, "unmapped", exact = TRUE), `[[`, 0L, "call_row")
    single <- all[!duplicated(all) & !duplicated(all, fromLast = TRUE) & !all %in% uncertain]
    s <- parsed$syntax_data
    native <- row > 0L & row %in% single
    i <- which(native)
    i <- i[s$child_end[row[i]] - s$child_start[row[i]] == 3L]
    head <- s$children[s$child_start[row[i]]]
    name <- s$text[s$children[s$child_end[head]]]
    keep <- name %in% c("glue", "str_glue")
    bare <- keep & s$child_start[head] == s$child_end[head]
    for (nm in unique(name[bare])) {
        index <- parsed$reference_index
        owner <- if (is.function(workspace$guess_namespace)) workspace$guess_namespace(nm, isf = TRUE, uri = document$uri)
        if (any(index$is_definition[reference_indices(index, nm)]) ||
                (length(owner) && !identical(owner, glue_template_packages[[nm]]))) keep[bare & name == nm] <- FALSE
    }
    list(single = native, unmasked = seq_along(row) %in% i[keep])
}

# Restrict resolution to free value reads that could be host parameters. The
# reference index checks the glue data mask, named arguments, assignments and
# nested R scopes. Its locations are already in physical UTF-16 units.
glue_semantic_parameters <- function(mapped, workspace, document, fragments, key, sources, classified) {
    parsed <- document$parse_data
    formals <- parsed$completion_data$formals
    if (is.null(workspace) || !length(formals$name) || !length(parsed$glue_templates) || is.null(parsed$reference_index) ||
            !identical(document$version, parsed$version) || !identical(document$content, document$parsed_content)) return(mapped)
    row <- vapply(fragments, `[[`, integer(1L), "call_row")
    syntax <- parsed$syntax_data
    stride <- as.double(syntax$maxcol) + 1
    positive <- which(row > 0L)
    at <- rep.int(-Inf, length(row))
    at[positive] <- syntax$line1[row[positive]] * stride + syntax$col1[row[positive]]
    start <- formals$line1 * stride + formals$col1
    end <- formals$line2 * stride + formals$col2
    enclosed <- function(at, definitions) {
        ids <- definitions[order(start[definitions])]
        at <= c(-Inf, cummax(end[ids]))[findInterval(at, start[ids]) + 1L] & is.finite(at)
    }
    nearby <- which(enclosed(at, seq_along(start)))
    if (!length(nearby)) return(mapped)
    formal_names <- reference_symbol_names(formals$name)
    used <- unique(key[nearby])
    variables <- vector("list", length(classified))
    variables[used] <- lapply(used, function(k) {
        m <- classified[[k]]
        reads <- which(m[, "type"] == SemanticTokenTypes$variable & m[, "modifier"] == 0L)
        spelling <- substring(sources[[k]][m[reads, "line"] + 1L], m[reads, "from"] + 1L, m[reads, "to"])
        names(reads) <- reference_symbol_names(spelling)
        reads[names(reads) %in% formal_names]
    })
    candidates <- lapply(variables[key[nearby]], function(reads) unique(names(reads)))
    if (!any(lengths(candidates))) return(mapped)
    reads <- split(rep.int(nearby, lengths(candidates)), unlist(candidates, use.names = FALSE))
    declarations <- split(seq_along(formal_names), formal_names)
    targets <- list()
    plain <- attr(classified, "plain_reads", exact = TRUE)[key]
    native <- if (any(plain)) glue_semantic_native_calls(workspace, document, row)
    simple <- if (is.null(native)) rep(FALSE, length(row)) else plain & native$single
    masks <- vector("list", length(row))
    offset <- cumsum(c(0L, vapply(classified[key], nrow, 0L)))[seq_along(key)]
    for (nm in names(reads)) {
        ids <- reads[[nm]]
        ids <- ids[enclosed(at[ids], declarations[[nm]])]
        flat <- ids[simple[ids]]
        if (length(flat)) {
            # A single flat expression has no inner binding. Its value reads
            # need only the mask and the host lexical scope, once per name
            # and call; the decoded syntax is already classified above.
            inherit <- native$unmasked[flat]
            for (j in which(!inherit)) {
                f <- flat[[j]]
                if (is.null(masks[[f]])) masks[[f]] <- glue_call_bindings(workspace, document, fragments[[f]])
                mask <- masks[[f]]
                inherit[[j]] <- !isTRUE(mask$open) && !isTRUE(mask$shadowed) && is.null(mask$targets[[nm]])
            }
            proven <- flat[inherit]
            index <- parsed$reference_index
            site <- reference_indices(index, nm)
            if (length(proven) && any(index$is_definition[site] & index$definition_kind[site] != "formal")) {
                proven <- proven[vapply(row[proven], function(r) isTRUE(glue_lexical_target(document, nm, r)$formal), logical(1L))]
            }
            hits <- lapply(variables[key[proven]], function(reads) reads[names(reads) == nm])
            mapped[rep.int(offset[proven], lengths(hits)) + unlist(hits, use.names = FALSE), 4L] <- SemanticTokenTypes$parameter
            ids <- ids[!simple[ids]]
        }
        calls <- unique(row[ids])
        if (!length(calls)) next
        sites <- glue_reference_occurrences(workspace, document, nm, calls)
        selected <- Filter(function(site) {
            !is.null(site$fragment) && !isTRUE(site$definition) && !isTRUE(site$function_position) && !isTRUE(site$argument_tag) &&
                identical(site$target$kind, "lexical") && isTRUE(site$target$formal) && identical(site$target$uri, document$uri)
        }, sites)
        targets[[nm]] <- lapply(selected, function(site) c(site$range$start$line, site$range$start$character, site$range$end$character))
    }
    targets <- unlist(targets, recursive = FALSE, use.names = FALSE)
    if (!length(targets)) return(mapped)
    positions <- do.call(rbind, targets)
    stride <- as.double(max(mapped[, 3L], positions[, 3L])) + 1
    vars <- which(mapped[, 4L] == SemanticTokenTypes$variable)
    hits <- match(mapped[vars, 1L] * stride + mapped[vars, 2L], positions[, 1L] * stride + positions[, 2L])
    confirmed <- which(!is.na(hits) & mapped[vars, 3L] == positions[hits, 3L])
    mapped[vars[confirmed], 4L] <- SemanticTokenTypes$parameter
    mapped
}

semantic_glue_overlay <- function(tokens, fragments, content, workspace = NULL, document = NULL) {
    if (!length(fragments) || !length(tokens$lines)) return(tokens)
    strings <- which(tokens$types == SemanticTokenTypes$string)
    if (!length(strings)) return(tokens)
    if (!isTRUE(getOption("keep.parse.data"))) {
        previous <- options(keep.parse.data = TRUE)
        on.exit(options(previous), add = TRUE)
    }
    braces <- glue_semantic_braces(fragments, content)
    ids <- which(braces$left > 0L & braces$right > 0L)
    if (!length(ids)) return(tokens)
    source <- vapply(fragments[ids], function(f) paste(f$content, collapse = "\n"), "")
    unique_ids <- which(!duplicated(source))
    sources <- lapply(fragments[ids[unique_ids]], `[[`, "content")
    parsed <- glue_semantic_expressions(sources)
    key <- match(source, source[unique_ids])
    inner <- lapply(seq_along(ids), function(i) {
        if (!is.null(parsed[[key[[i]]]])) glue_semantic_project(fragments[[ids[[i]]]], parsed[[key[[i]]]])
    })
    ok <- !vapply(inner, is.null, logical(1L))
    ids <- ids[ok]
    if (!length(ids)) return(tokens)
    inner <- inner[ok]
    first <- braces$first[ids]
    last <- braces$last[ids]
    from <- braces$from[ids]
    to <- braces$to[ids]
    left <- from - braces$left[ids]
    right <- to + braces$right[ids]
    # Split multiline intervals into physical lines before touching the host
    # index. Empty lines do not contribute protocol tokens.
    widths <- last - first + 1L
    owner <- rep.int(seq_along(ids), widths)
    line <- sequence(widths, from = first)
    cut_start <- ifelse(line == first[owner], left[owner], 0L)
    cut_end <- ifelse(line == last[owner], right[owner], nchar(content[line + 1L]))
    visible <- which(cut_end > cut_start)
    owner <- owner[visible]
    line <- line[visible]
    cuts <- glue_semantic_units(content, line, cut_start[visible], cut_end[visible])
    stride <- as.double(max(tokens$cols + tokens$lengths, cuts$end)) + 1
    string_at <- tokens$lines[strings] * stride + tokens$cols[strings]
    index <- findInterval(line * stride + cuts$start, string_at)
    original <- strings[pmax.int(index, 1L)]
    covered <- index > 0L & line == tokens$lines[original] & cuts$end <= tokens$cols[original] + tokens$lengths[original]
    valid <- !seq_along(ids) %in% owner[!covered]
    if (!any(valid)) return(tokens)
    keep <- which(valid[owner])
    keep <- keep[order(original[keep], cuts$start[keep])]
    original <- original[keep]
    a <- cuts$start[keep]
    b <- cuts$end[keep]
    # Several interpolations can cut the same string. Preserve every gap and
    # the final suffix, including the original quotes, exactly once.
    same <- duplicated(original)
    gap <- tokens$cols[original]
    gap[same] <- b[which(same) - 1L]
    tail <- which(!duplicated(original, fromLast = TRUE))
    line <- tokens$lines[original]
    prose <- rbind(cbind(line, gap, a, SemanticTokenTypes$string, 0L),
        cbind(line[tail], b[tail], tokens$cols[original[tail]] + tokens$lengths[original[tail]], SemanticTokenTypes$string, 0L))
    prose <- prose[prose[, 2L] < prose[, 3L], , drop = FALSE]

    mapped <- rbind(do.call(rbind, inner[valid]), cbind(first[valid], left[valid], from[valid], SemanticTokenTypes$operator, 0L),
        cbind(last[valid], to[valid], right[valid], SemanticTokenTypes$operator, 0L))
    units <- glue_semantic_units(content, mapped[, 1L], mapped[, 2L], mapped[, 3L])
    mapped[, 2L] <- units$start
    mapped[, 3L] <- units$end
    mapped <- glue_semantic_parameters(mapped, workspace, document, fragments[ids[valid]], key[ok][valid], sources, parsed)
    omitted <- unique(original)
    merged <- rbind(cbind(tokens$lines, tokens$cols, tokens$cols + tokens$lengths, tokens$types, tokens$modifiers)[-omitted, , drop = FALSE],
        prose, mapped)
    merged <- merged[order(merged[, 1L], merged[, 2L]), , drop = FALSE]
    result <- list(lines = as.integer(merged[, 1L]), cols = as.integer(merged[, 2L]), lengths = as.integer(merged[, 3L] - merged[, 2L]),
        types = as.integer(merged[, 4L]), modifiers = as.integer(merged[, 5L]))
    result$encoded <- .Call("encode_semantic_tokens_c", result$lines, result$cols, result$lengths,
        result$types, result$modifiers, PACKAGE = "languageserver")
    result
}
