max_type_hint_alternatives <- 32L

# Scan only potential comment lines before requesting a syntax tree, including
# when a single string contains a recovered multiline source fragment.
roxygen_has_type_span <- function(lines) {
    matches <- grep("(`", lines, fixed = TRUE)
    length(matches) > 0L && any(grepl(r"((?m)^\h*#'[^\r\n]*\(`)", lines[matches], perl = TRUE))
}

# A type annotation is one parenthesized code span at the start of a roxygen
# description. Leave all other Markdown and ordinary documentation alone.
roxygen_type_block <- function(comments) {
    lines <- trimws(sub("^#'", "", comments))
    starts <- which(startsWith(lines, "@"))
    ends <- c(starts[-1L] - 1L, length(lines))
    params <- list()
    returns <- character()
    identifier <- "(?!\\.[0-9])[[:alpha:].][[:alnum:]_.]*"
    qualified <- paste0("^", identifier, "(?:::", identifier, ")?$")
    for (i in seq_along(starts)) {
        tag <- paste(lines[seq.int(starts[[i]], ends[[i]])], collapse = " ")
        pattern <- r"(^@(param\s+(`(?:\\.|[^`\\])*`|\S+)|returns?)\s+\(`([^`]+)`\)(?:\s|[.,:]|$))"
        match <- regmatches(tag, regexec(pattern, tag, perl = TRUE))[[1L]]
        if (!length(match)) next
        span <- trimws(match[[4L]])
        if (!nzchar(span) || endsWith(span, "|")) next
        types <- unique(trimws(strsplit(span, "|", fixed = TRUE)[[1L]]))
        if (length(types) > max_type_hint_alternatives || !all(grepl(qualified, types, perl = TRUE))) next
        if (startsWith(match[[2L]], "param")) {
            spelling <- match[[3L]]
            names <- if (startsWith(spelling, "`")) reference_symbol_names(spelling) else strsplit(spelling, ",", fixed = TRUE)[[1L]]
            params[names] <- rep(list(types), length(names))
        } else {
            returns <- types
        }
    }
    if (length(params) || length(returns)) list(params = params, returns = returns)
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

# Key each contiguous block by its function keyword. An adjacent declaration
# may contain other functions (defaults, callbacks, methods or nested bodies).
member_roxygen_type_index <- function(content, data, row_offset = 0L) {
    if (!roxygen_has_type_span(content) || !length(data$token)) return(NULL)
    if (is.null(data$terminals)) data <- syntax_parse_data(data)
    terminals <- data$terminals
    kind <- data$token[terminals]
    line <- data$line1[terminals]
    text <- data$text[terminals]
    comments <- which(kind == "COMMENT" & startsWith(text, "#'"))
    comments <- comments[trimws(content[line[comments]]) == trimws(text[comments])]
    if (!length(comments)) return(NULL)
    groups <- split(comments, cumsum(c(TRUE, diff(line[comments]) != 1L)))
    result <- list()
    rows <- integer()
    for (group in groups) {
        if (!any(grepl("(`", text[group], fixed = TRUE))) next
        end <- group[[length(group)]]
        i <- end + 1L
        if (i > length(kind) || line[[i]] != line[[end]] + 1L) next
        fn <- roxygen_function_row(data, terminals[[i]])
        if (!fn) next
        hints <- roxygen_type_block(text[group])
        if (!is.null(hints)) {
            parts <- syntax_children(data, data$parent_row[[fn]])
            formals <- reference_symbol_names(data$text[parts[data$token[parts] == "SYMBOL_FORMALS"]])
            hints$params <- hints$params[intersect(names(hints$params), formals)]
            if (!length(hints$params) && !length(hints$returns)) next
            function_line <- data$line1[[fn]] + row_offset
            result[[paste(function_line, data$col1[[fn]], sep = ":")]] <- hints
            rows <- c(rows, function_line)
        }
    }
    if (length(result)) structure(result, rows = rows)
}

member_merge_type_hints <- function(partial, current) {
    if (!length(partial)) return(current)
    if (!length(current)) return(partial)
    keep <- !names(partial) %in% names(current)
    rows <- c(attr(partial, "rows")[keep], attr(current, "rows"))
    order <- order(rows)
    structure(c(partial[keep], current)[order], rows = rows[order])
}

member_type_dependencies <- function(hints) {
    types <- unique(unlist(hints, use.names = FALSE))
    qualified <- types[grepl("::", types, fixed = TRUE)]
    unique(c(types, sub("::.*", "", qualified)))
}

# The AST retains srcrefs for function literals even inside named arguments.
# Decorate only statements whose ranges contain an annotated declaration, and
# cache its type dependencies on ancestors for the scope/capture indexes.
member_attach_type_hints <- function(parsed, index, row = 0L, col = 0L) {
    if (!length(index) || !length(parsed)) return(parsed)
    rows <- attr(index, "rows", exact = TRUE)
    walk <- function(node) {
        dependencies <- character()
        if (member_head(node, "function") && length(node) >= 4L) {
            ref <- node[[4L]]
            if (!is.null(ref)) {
                column <- ref[[5L]] + if (ref[[7L]] == 1L) col else 0L
                hints <- index[[paste(row + ref[[7L]], column, sep = ":")]]
                if (!is.null(hints)) {
                    attr(node, "member_type_hints") <- hints
                    dependencies <- member_type_dependencies(hints)
                }
            }
        }
        for (i in seq_along(node)) {
            if (identical(node[[i]], quote(expr = ))) next
            if (!is.call(node[[i]]) && !is.pairlist(node[[i]])) next
            if (is.null(node[[i]])) next
            child <- walk(node[[i]])
            refs <- attr(child, "member_type_names", exact = TRUE)
            if (length(refs)) {
                node[[i]] <- child
                dependencies <- c(dependencies, refs)
            }
        }
        if (length(dependencies)) attr(node, "member_type_names") <- unique(dependencies)
        node
    }
    refs <- attr(parsed, "srcref", exact = TRUE)
    statements <- if (length(refs)) {
        start <- vapply(refs, `[[`, integer(1L), 7L) + row
        end <- vapply(refs, `[[`, integer(1L), 8L) + row
        which(findInterval(start - 1L, rows) < findInterval(end, rows))
    } else {
        seq_along(parsed)
    }
    dependencies <- character()
    for (i in statements) {
        if (!is.call(parsed[[i]])) next
        parsed[[i]] <- walk(parsed[[i]])
        dependencies <- c(dependencies, attr(parsed[[i]], "member_type_names", exact = TRUE))
    }
    if (length(dependencies)) attr(parsed, "member_type_names") <- unique(dependencies)
    parsed
}
