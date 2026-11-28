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
roxygen_type_block <- function(comments, formals = NULL) {
    lines <- sub("^#'\\s*", "", comments, perl = TRUE)
    starts <- which(startsWith(lines, "@"))
    ends <- c(starts[-1L] - 1L, length(lines))
    params <- list()
    returns <- character()
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
        if (startsWith(parts[[2L]], "param")) {
            parameter <- parts[[3L]]
            names <- if (startsWith(parameter, "`")) reference_symbol_names(parameter) else strsplit(parameter, ",", fixed = TRUE)[[1L]]
            if (!is.null(formals)) names <- intersect(names, formals)
            if (!length(names)) next
            params[names] <- rep(list(unique_types), length(names))
        } else {
            returns <- unique_types
        }
    }
    if (!length(params) && !length(returns)) return(NULL)
    list(params = params, returns = returns)
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
    result <- vector("list", length(groups))
    functions <- integer(length(groups))
    targets <- as.integer(names(groups))
    for (i in seq_along(groups)) {
        group <- groups[[i]]
        if (!any(grepl("(`", text[group], fixed = TRUE))) next
        fn <- roxygen_function_row(data, terminals[[targets[[i]]]])
        if (!fn) next
        parts <- syntax_children(data, data$parent_row[[fn]])
        formals <- reference_symbol_names(data$text[parts[data$token[parts] == "SYMBOL_FORMALS"]])
        comments <- text[group]
        if (!is.null(value_source)) {
            comment_rows <- line[group]
            starts <- regexpr("#'", content[comment_rows], fixed = TRUE)
            comments <- substring(value_source[comment_rows], starts)
        }
        hints <- roxygen_type_block(comments, formals)
        if (is.null(hints)) next
        result[[i]] <- hints
        functions[[i]] <- fn
    }
    # Several comment groups around a multiline assignment can identify the
    # same function. As with a repeated tag, keep its last accepted block.
    keep <- which(functions > 0L & !duplicated(functions, fromLast = TRUE))
    if (!length(keep)) return(NULL)
    functions <- functions[keep]
    rows <- data$line1[functions] + row_offset
    cols <- data$col1[functions]
    order <- order(rows, cols)
    result <- stats::setNames(result[keep], paste(rows, cols, sep = ":"))
    structure(result[order], rows = as.integer(rows[order]), cols = as.integer(cols[order]))
}

member_merge_type_hints <- function(partial, current) {
    if (!length(partial)) return(current)
    if (!length(current)) return(partial)
    keep_hints <- !names(partial) %in% names(current)
    rows <- c(attr(partial, "rows")[keep_hints], attr(current, "rows"))
    cols <- c(attr(partial, "cols")[keep_hints], attr(current, "cols"))
    order <- order(rows, cols)
    structure(c(partial[keep_hints], current)[order], rows = rows[order], cols = cols[order])
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
