max_type_hint_alternatives <- 32L
roxygen_type_identifier <- "(?!\\.[0-9])[[:alpha:].][[:alnum:]_.]*"

# Scan only potential comment lines before requesting a syntax tree, including
# when a single string contains a recovered multiline source fragment.
roxygen_has_type_span <- function(lines) {
    matches <- grep("(`", lines, fixed = TRUE)
    length(matches) > 0L && any(grepl(r"((?m)^\h*#'[^\r\n]*\(`)", lines[matches], perl = TRUE))
}

# A type annotation is one parenthesized code span at the start of a roxygen
# description. Leave all other Markdown and ordinary documentation alone.
roxygen_type_block <- function(comments, formals = NULL, rows = NULL, columns = NULL) {
    prefix <- attr(regexpr("^#'\\s*", comments, perl = TRUE), "match.length")
    lines <- substring(comments, prefix + 1L)
    starts <- which(startsWith(lines, "@"))
    ends <- c(starts[-1L] - 1L, length(lines))
    params <- list()
    returns <- character()
    occurrences <- list()
    qualified <- paste0("(*UCP)^", roxygen_type_identifier, "(?:::", roxygen_type_identifier, ")?$")
    pattern <- r"(^@(param\s+(`(?:\\.|[^`\\])*`|\S+)|returns?)\s+\(`([^`]+)`\)(?!\s*[,;]?\s*(?:\||/|(?i:or|and)\b|\\?\[|\(`))(?=\s|[.,:]|$))"
    for (i in seq_along(starts)) {
        section <- seq.int(starts[[i]], ends[[i]])
        tag <- paste(lines[section], collapse = "\n")
        match <- regexec(pattern, tag, perl = TRUE)
        parts <- regmatches(tag, match)[[1L]]
        if (!length(parts)) next
        span <- parts[[4L]]
        if (!nzchar(trimws(span)) || endsWith(trimws(span), "|")) next
        terms <- strsplit(span, "|", fixed = TRUE)[[1L]]
        types <- trimws(terms)
        unique_types <- unique(types)
        if (length(unique_types) > max_type_hint_alternatives || !all(grepl(qualified, types, perl = TRUE))) next
        parameter <- NA_character_
        if (startsWith(parts[[2L]], "param")) {
            parameter <- parts[[3L]]
            names <- if (startsWith(parameter, "`")) reference_symbol_names(parameter) else strsplit(parameter, ",", fixed = TRUE)[[1L]]
            if (!is.null(formals)) names <- intersect(names, formals)
            if (!length(names)) next
            params[names] <- rep(list(unique_types), length(names))
        } else {
            returns <- unique_types
        }
        if (!is.null(rows)) {
            # Match offsets count code points in the joined description. Map
            # the terminal identifier back to its original comment and column.
            term_starts <- c(0L, cumsum(nchar(terms) + 1L))[-(length(terms) + 1L)]
            class_names <- sub("^.*::", "", types)
            offsets <- match[[1L]][[4L]] - 1L + term_starts + nchar(terms) - nchar(trimws(terms, "left")) +
                nchar(types) - nchar(class_names)
            line_starts <- c(0L, cumsum(nchar(lines[section]) + 1L))
            which_line <- findInterval(offsets, line_starts)
            source <- section[which_line]
            start <- columns[source] + prefix[source] + offsets - line_starts[which_line]
            parameter_row <- parameter_start <- NA_integer_
            if (!is.na(parameter)) {
                offset <- match[[1L]][[3L]] - 1L
                line <- findInterval(offset, line_starts)
                source_line <- section[[line]]
                parameter_row <- rows[[source_line]]
                parameter_start <- columns[[source_line]] + prefix[[source_line]] + offset - line_starts[[line]]
            }
            occurrences[[length(occurrences) + 1L]] <- data.frame(type = types, row = rows[source],
                start = start, end = start + nchar(class_names), parameter = parameter,
                parameter_row = parameter_row, parameter_start = parameter_start, stringsAsFactors = FALSE)
        }
    }
    if (!length(params) && !length(returns)) return(NULL)
    hints <- list(params = params, returns = returns)
    if (length(occurrences)) attr(hints, "occurrences") <- do.call(rbind, occurrences)
    hints
}

# Follow assignments and parentheses to their value. Named arguments include
# R6 methods and need the value following their argument tag.
roxygen_function_row <- function(syntax, row) {
    token <- syntax$token
    parent <- syntax$parent_row
    if (token[[row]] %in% c("SYMBOL_SUB", "SYMBOL_FORMALS")) {
        parts <- syntax_children(syntax, parent[[row]])
        parts <- parts[token[parts] != "COMMENT"]
        i <- match(row, parts)
        if (is.na(i) || i + 2L > length(parts) || !token[parts[[i + 1L]]] %in% c("EQ_SUB", "EQ_FORMALS")) return(0L)
        row <- parts[[i + 2L]]
    } else {
        while (parent[[row]] && syntax$line1[[parent[[row]]]] == syntax$line1[[row]] &&
                syntax$col1[[parent[[row]]]] == syntax$col1[[row]]) row <- parent[[row]]
    }
    repeat {
        parts <- syntax_children(syntax, row)
        parts <- parts[token[parts] != "COMMENT"]
        if (!length(parts)) return(0L)
        kinds <- token[parts]
        if (kinds[[1L]] %in% c("FUNCTION", r"('\\')")) return(parts[[1L]])
        if (length(parts) != 3L) return(0L)
        if (kinds[[2L]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) row <- parts[[3L]]
        else if (kinds[[2L]] == "RIGHT_ASSIGN") row <- parts[[1L]]
        else if (identical(kinds[c(1L, 3L)], c("'('", "')'"))) row <- parts[[2L]]
        else return(0L)
    }
}

# Whitespace and ordinary comments can separate roxygen from its declaration.
# The first real token stops the search. A fence or a discarded statement in a
# partial projection is also a boundary, even if the projection blanked it out.
member_roxygen_type_index <- function(content, data, row_offset = 0L, barriers = integer(), allowed_comments = NULL, value_source = NULL) {
    if (!roxygen_has_type_span(content) || !length(data$token)) return(NULL)
    if (is.null(data$terminals)) data <- syntax_parse_data(data)
    terminals <- data$terminals
    kind <- data$token[terminals]
    line <- data$line1[terminals]
    text <- data$text[terminals]
    comments <- which(kind == "COMMENT" & startsWith(text, "#'"))
    comments <- comments[trimws(content[line[comments]]) == trimws(text[comments])]
    if (!is.null(allowed_comments)) comments <- comments[line[comments] %in% allowed_comments]
    if (!length(comments)) return(NULL)
    code <- which(kind != "COMMENT")
    next_code <- code[findInterval(comments, code) + 1L]
    keep <- !is.na(next_code)
    if (length(barriers)) {
        barriers <- sort(unique(barriers))
        keep <- keep & findInterval(line[comments], barriers) == findInterval(line[next_code], barriers)
    }
    groups <- split(comments[keep], next_code[keep])
    result <- locations <- list()
    rows <- cols <- integer()
    for (key in names(groups)) {
        group <- groups[[key]]
        if (!any(grepl("(`", text[group], fixed = TRUE))) next
        fn <- roxygen_function_row(data, terminals[[as.integer(key)]])
        if (!fn) next
        parts <- syntax_children(data, data$parent_row[[fn]])
        formals <- reference_symbol_names(data$text[parts[data$token[parts] == "SYMBOL_FORMALS"]])
        comment_rows <- line[group]
        comment_cols <- regexpr("#'", content[comment_rows], fixed = TRUE) - 1L
        comments <- if (is.null(value_source)) text[group] else substring(value_source[comment_rows], comment_cols + 1L)
        hints <- roxygen_type_block(comments, formals, comment_rows + row_offset - 1L, as.integer(comment_cols))
        if (is.null(hints)) next
        fn_row <- data$line1[[fn]] + row_offset
        fn_col <- data$col1[[fn]]
        position <- paste(fn_row, fn_col, sep = ":")
        occurrences <- attr(hints, "occurrences", exact = TRUE)
        attr(hints, "occurrences") <- NULL
        occurrences$function_row <- fn_row - 1L
        column <- fn_col - 1L
        source <- content[[data$line1[[fn]]]]
        if (grepl("\t", source, fixed = TRUE)) column <- index_physical_columns(column, 1L, source, TRUE)
        occurrences$function_col <- column
        locations[[position]] <- occurrences
        result[[position]] <- hints
        rows[position] <- fn_row
        cols[position] <- fn_col
    }
    if (!length(result)) return(NULL)
    order <- order(rows, cols)
    structure(result[order], rows = as.integer(rows[order]), cols = as.integer(cols[order]),
        occurrences = do.call(rbind, unname(locations[order])))
}

member_merge_type_hints <- function(partial, current) {
    if (!length(partial)) return(current)
    if (!length(current)) return(partial)
    keep_hints <- !names(partial) %in% names(current)
    rows <- c(attr(partial, "rows")[keep_hints], attr(current, "rows"))
    cols <- c(attr(partial, "cols")[keep_hints], attr(current, "cols"))
    order <- order(rows, cols)
    a <- attr(partial, "occurrences")
    b <- attr(current, "occurrences")
    keep <- !paste(a$function_row, a$function_col) %in% paste(b$function_row, b$function_col)
    occurrences <- rbind(a[keep, , drop = FALSE], b)
    structure(c(partial[keep_hints], current)[order], rows = rows[order], cols = cols[order],
        occurrences = occurrences[order(occurrences$row, occurrences$start), , drop = FALSE])
}

# Failed parses can leave the function keyword and its formals as lexer tokens
# without a reduced function expression. Such a block cannot provide inferred
# types, but Rename must not silently leave a matching type spelling behind.
roxygen_damaged_type_blocks <- function(data, comments, row_offset) {
    if (!length(comments)) return(NULL)
    terminals <- which(data$terminal)
    terminals <- terminals[order(data$line1[terminals], data$col1[terminals])]
    kind <- data$token[terminals]
    code <- which(kind != "COMMENT")
    groups <- split(comments, findInterval(match(comments, terminals), code) + 1L)
    result <- list()
    for (key in names(groups)) {
        group <- groups[[key]]
        if (!any(grepl("(`", data$text[group], fixed = TRUE))) next
        j <- as.integer(key)
        while (j <= length(code)) {
            if (kind[code[[j]]] == "'('") {
                j <- j + 1L
            } else if (j < length(code) && kind[code[[j]]] %in% c("SYMBOL", "SYMBOL_SUB", "SYMBOL_FORMALS") &&
                    kind[code[[j + 1L]]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "EQ_SUB", "EQ_FORMALS")) {
                j <- j + 2L
            } else {
                break
            }
        }
        if (j > length(code) || !kind[code[[j]]] %in% c("FUNCTION", r"('\\')")) next
        fn <- terminals[code[[j]]]
        # Partial recovery already cleared parent IDs that were never reduced.
        if (data$parent[[fn]] > 0L) next
        # A closed header identifies the valid @param names even when the
        # function body is unfinished. Only count the outer parameter list.
        formals <- NULL
        if (j < length(code) && kind[code[[j + 1L]]] == "'('") {
            depth <- 0L
            names <- character()
            for (k in seq.int(j + 1L, length(code))) {
                token <- kind[[code[[k]]]]
                if (token == "'('") depth <- depth + 1L
                if (token == "')'") depth <- depth - 1L
                if (!depth) {
                    formals <- names
                    break
                }
                if (depth == 1L && token == "SYMBOL_FORMALS") {
                    names <- c(names, reference_symbol_names(data$text[terminals[code[[k]]]]))
                }
            }
        }
        hints <- roxygen_type_block(data$text[group], formals)
        if (!is.null(hints)) {
            position <- paste(data$line1[[fn]] + row_offset, data$col1[[fn]], sep = ":")
            result[[position]] <- unique(unlist(hints, use.names = FALSE))
        }
    }
    result
}

roxygen_unresolved_type_hint <- function(data, spellings) {
    damaged <- data$damaged_type_blocks
    if (!length(damaged)) return(FALSE)
    missing <- setdiff(names(damaged), names(data$type_hints))
    any(spellings %in% unlist(damaged[missing], use.names = FALSE))
}

member_type_dependencies <- function(hints) {
    types <- unique(unlist(hints, use.names = FALSE))
    qualified <- types[grepl("::", types, fixed = TRUE)]
    unique(c(types, sub("::.*", "", qualified)))
}

# Copy only the paths leading to annotated functions. The native traversal is
# iterative; wide calls are converted to an indexed list once, and deep paths
# are rebuilt from leaves to roots without recursing on the R stack.
member_attach_type_hints <- function(parsed, index, row = 0L, col = 0L) {
    if (!length(index) || !length(parsed)) return(parsed)
    plan <- .Call("member_hint_paths_c", parsed, attr(index, "rows"), attr(index, "cols"),
        as.integer(c(row, col)), PACKAGE = "languageserver")
    n <- nrow(plan)
    if (!n) return(parsed)
    children <- split(seq_len(n)[-1L], factor(plan[-1L, 1L], levels = seq_len(n)))
    nodes <- parts <- vector("list", n)
    nodes[[1L]] <- parsed
    for (i in seq_len(n)) {
        kids <- children[[i]]
        if (length(kids)) {
            parts[[i]] <- as.list(nodes[[i]])
            nodes[kids] <- parts[[i]][plan[kids, 2L]]
        }
    }
    for (i in rev(seq_len(n))) {
        node <- nodes[[i]]
        kids <- children[[i]]
        dependencies <- attr(node, "member_type_names", exact = TRUE)
        if (length(kids)) {
            values <- parts[[i]]
            values[plan[kids, 2L]] <- nodes[kids]
            rebuilt <- if (is.call(node)) as.call(values) else if (is.expression(node)) as.expression(values) else as.pairlist(values)
            attributes(rebuilt) <- attributes(node)
            node <- rebuilt
            dependencies <- c(dependencies, unlist(lapply(nodes[kids], attr, "member_type_names", exact = TRUE), use.names = FALSE))
        }
        hint <- plan[i, 3L]
        if (hint) {
            attr(node, "member_type_hints") <- index[[hint]]
            dependencies <- c(member_type_dependencies(index[[hint]]), dependencies)
        }
        attr(node, "member_type_names") <- unique(dependencies)
        nodes[[i]] <- node
    }
    nodes[[1L]]
}
