# Some lexical and pipe errors discard the entire parse table. On R versions
# that supply a physical error location, retain tokens strictly before its line.
parse_prefix_data <- function(content, error) {
    line <- error$lineno
    if (!identical(error$filename, "<text>") || length(line) != 1L ||
            is.na(line) || line < 2L || line > length(content) ||
            any(startsWith(content, "#line"))) return(NULL)
    prefix <- content[seq_len(line - 1L)]
    srcfile <- srcfilecopy("<text>", prefix)
    tryCatch(parse(text = prefix, srcfile = srcfile, keep.source = TRUE), error = function(e) NULL)
    utils::getParseData(srcfile)
}

# Identify the damaged token once for the structural and highlighting paths.
# Never assume the last R token spans the complete physical line: the lexer
# may have stopped at an unterminated string, or on a synthetic EOF position.
partial_failure_token <- function(data, content, terminals) {
    n <- length(terminals)
    if (!n) return(NULL)
    last <- terminals[[n]]
    closer <- data$text[[last]] %in% c(")", "]", "}")
    row <- if (closer && n > 1L) terminals[[n - 1L]] else last
    operator <- data$parent[[row]] == 0L && (data$token[[row]] == "SPECIAL" ||
            grepl("^[+*/^:?!$@~&|=<>-]+$", data$text[[row]]))
    if (!operator) row <- last
    line <- data$line1[[row]]
    start <- data$col1[[row]]
    end <- data$col2[[row]]
    if (line < 1L || line > length(content) || data$line2[[row]] != line ||
            data$token[[row]] == "ERROR" || start < 1L || end < start ||
            substr(content[[line]], start, end) != data$text[[row]]) return(NULL)
    rest <- substring(content[[line]], end + 1L)
    if (!grepl("^[[:space:]]*([)}\\]]|#|$)", rest, perl = TRUE)) return(NULL)
    list(line = line, start = start, end = end, closer = closer, operator = operator, row = row)
}

# A dangling operator can prevent the enclosing function's parameter and
# declaration roles resolving. Blank only that operator for token classification.
partial_semantic_data <- function(data, content, token, terminals) {
    if (is.null(token) || !token$operator) return(data)
    line <- token$line
    if (!token$closer && (line == length(content) ||
                !any(grepl("[^[:space:]]", content[seq.int(line + 1L, length(content))])))) {
        detached <- terminals[data$parent[terminals] == 0L]
        if (all(detached == token$row | data$token[detached] %in% c("SYMBOL", "STR_CONST", "';'"))) {
            return(data)
        }
    }
    substr(content[[line]], token$start, token$end) <- strrep(" ", token$end - token$start + 1L)
    repaired <- tryCatch(parse(text = content, keep.source = TRUE), error = function(e) NULL)
    if (is.null(repaired)) data else utils::getParseData(repaired)
}

# getParseData reports physical lines, but srcrefs follow #line. Only a real
# directive should be neutralized; a multiline string may contain the same text.
physical_parse_content <- function(content, expr) {
    if (!any(startsWith(content, "#line"))) return(content)
    data <- utils::getParseData(expr)
    lines <- data$line1[data$token == "LINE_DIRECTIVE"]
    if (length(lines)) substr(content[lines], 2L, 2L) <- " "
    content
}

# Parse a line-preserving projection of the current buffer for structural
# providers, keeping source references on physical lines.
parse_provider_projection <- function(content) {
    parse_source <- function(text) {
        tryCatch(parse(text = text, keep.source = TRUE), error = function(e) NULL)
    }
    expr <- parse_source(content)
    if (!is.null(expr)) {
        physical <- physical_parse_content(content, expr)
        if (!identical(physical, content)) expr <- parse_source(physical)
    }
    expr
}

# A gap at top level or between statements in a brace block can be removed.
# Reject gaps that would join an operand, call argument or control-flow body
# to code following the error (e.g. x <- / pkg:: / unrelated()).
projection_statement_gap <- function(expr, line, col) {
    data <- utils::getParseData(expr)
    covering <- which(data$token %in% c("expr", "expr_or_assign_or_help") &
            (data$line1 < line | data$line1 == line & data$col1 < col) &
            (data$line2 > line | data$line2 == line & data$col2 >= col))
    if (!length(covering)) return(TRUE)
    # getParseData orders enclosing expressions before their children.
    inner <- data$id[covering[[length(covering)]]]
    any(data$parent == inner & data$token == "'{'")
}

# Retain the completed top-level prefix, including comments and earlier
# semicolon-separated statements on the broken line. The first detached
# operator/delimiter belongs to the unfinished expression, as does an
# adjacent root-level operand without a statement terminator.
partial_prefix_content <- function(content, data) {
    projected <- rep("", length(content))
    if (!nrow(data)) return(projected)
    roots <- which(data$parent == 0L &
            !data$token %in% c("COMMENT", "LINE_DIRECTIVE", "';'"))
    broken <- match(TRUE, !data$token[roots] %in% c("expr", "expr_or_assign_or_help"), nomatch = 0L)
    if (broken) {
        row <- roots[[broken]]
        if (broken > 1L) {
            previous <- roots[[broken - 1L]]
            if (data$line2[[previous]] == data$line1[[row]] && data$line1[[row]] <= length(content)) {
                between <- substr(content[[data$line1[[row]]]], data$col2[[previous]] + 1L,
                    data$col1[[row]] - 1L)
                if (!grepl(";", between, fixed = TRUE)) row <- previous
            }
        }
        line <- data$line1[[row]]
        col <- data$col1[[row]] - 1L
    } else {
        # A lexer error can leave only completed expressions and comments.
        row <- order(data$line2, data$col2, decreasing = TRUE)[[1L]]
        line <- data$line2[[row]]
        col <- data$col2[[row]]
    }
    if (line < 1L || line > length(content)) return(projected)
    projected[seq_len(line - 1L)] <- content[seq_len(line - 1L)]
    projected[[line]] <- substr(content[[line]], 1L, col)
    projected
}

recover_partial_content <- function(content, srcfile, error, include_tokens = TRUE) {
    data <- utils::getParseData(srcfile)
    prefix_only <- is.null(data)
    if (prefix_only) data <- parse_prefix_data(content, error)
    if (length(data$id)) {
        # An error can leave positive parent IDs whose rows were never reduced.
        missing_parent <- data$parent > 0L & !(data$parent %in% data$id)
        data$parent[missing_parent] <- 0L
    }
    terminals <- which(data$terminal & !data$token %in% c("COMMENT", "LINE_DIRECTIVE"))
    token <- if (!prefix_only) partial_failure_token(data, content, terminals)
    tokens <- if (include_tokens) partial_semantic_data(data, content, token, terminals)
    projected <- rep("", length(content))
    expr <- NULL
    # A body can begin on the next physical line even when every delimiter
    # in the header is balanced. Keep its function/control keyword attached
    # to the body, or discard the uncertain scope together with its header.
    headers <- which(data$parent == 0L & data$token %in%
            c("FUNCTION", "IF", "ELSE", "FOR", "WHILE", "REPEAT", "'\\\\'"))
    if (!is.null(token)) {
        line <- token$line
        # A semicolon or opening brace in an already-reduced subtree cannot
        # start a sibling statement outside that subtree.
        boundaries <- which(data$parent == 0L & data$terminal & data$line1 == line &
                data$col2 < token$start & data$token %in% c("';'", "'{'"))
        start <- if (length(boundaries)) max(data$col2[boundaries]) + 1L else 1L
        drops_header <- any(data$line1[headers] == line & data$col1[headers] >= start &
                data$col1[headers] <= token$end)
        # For function(x)) and analogous if/for/while headers, removing
        # just the extra closer lets R determine the real enclosing scope.
        previous <- if (length(terminals) >= 2L) terminals[[length(terminals) - 1L]]
        if (length(headers) && token$closer && length(previous) &&
                data$token[[previous]] %in% c("')'", "ELSE", "REPEAT")) {
            candidate <- content
            substr(candidate[[line]], token$start, token$end) <- " "
            expr <- parse_provider_projection(candidate)
            if (!is.null(expr)) projected <- candidate
        }
        # A closing delimiter on the error line may finish a real block or
        # call begun on an earlier line. Removing it would silently move
        # later code under that opener if another closer happened to follow.
        closers <- which(data$parent > 0L & data$line1 == line & data$col1 >= start &
                data$col2 <= token$end & data$token %in% c("'}'", "')'", "']'"))
        groups <- match(data$parent[closers], data$id)
        crosses_group <- any(data$line1[groups] < line |
                data$line1[groups] == line & data$col1[groups] < start)
        if (is.null(expr) && !drops_header && !crosses_group) {
            candidate <- content
            substr(candidate[[line]], start, token$end) <- strrep(" ", token$end - start + 1L)
            expr <- parse_provider_projection(candidate)
            if (!is.null(expr) && projection_statement_gap(expr, line, start)) {
                projected <- candidate
            } else {
                expr <- NULL
            }
        }
    }
    if (is.null(expr) && length(data$id)) {
        projected <- partial_prefix_content(content, data)
        # A stray closer or a namespace operator at a newline can stop R
        # before it reads the subsequent top-level code. Only resume when
        # all enclosing delimiters have closed; a suffix inside an unfinished
        # function or string must never become a new global declaration.
        if (!prefix_only && !length(headers) && length(terminals)) {
            last <- terminals[[length(terminals)]]
            line <- data$line2[[last]]
            token <- data$token[terminals]
            at_top <- sum(token == "'{'") <= sum(token == "'}'") &&
                sum(token == "'('") <= sum(token == "')'") &&
                sum(token == "'['") + 2L * sum(token == "LBB") <= sum(token == "']'")
            boundary <- data$text[[last]] %in% c(")", "]", "}") ||
                data$token[[last]] %in% c("NS_GET", "NS_GET_INT")
            if (at_top && boundary && line >= 1L && line < length(content) &&
                    grepl("^[[:space:]]*(#|$)", substring(content[[line]], data$col2[[last]] + 1L))) {
                candidate <- projected
                after <- seq.int(line + 1L, length(content))
                candidate[after] <- content[after]
                expr <- parse_provider_projection(candidate)
                if (!is.null(expr)) projected <- candidate
            }
        }
        if (is.null(expr)) expr <- parse_provider_projection(projected)
    }
    if (is.null(expr)) {
        projected <- rep("", length(content))
        expr <- expression()
    }
    incomplete <- which(content != projected) - 1L
    if (!is.null(attr(expr, "srcfile"))) projected <- attr(expr, "srcfile")$lines
    list(content = projected, expr = expr, token_data = tokens, incomplete_lines = incomplete)
}

# Keep scoped classifications and suffix positions from the structural tree,
# and actual lexer tokens (including dangling operators) on discarded lines.
merge_partial_tokens <- function(current, partial, incomplete_lines) {
    keep <- !current$lines %in% incomplete_lines
    add <- partial$lines %in% incomplete_lines
    if (all(keep) && !any(add)) return(current)
    fields <- c("lines", "cols", "lengths", "types", "modifiers")
    result <- Map(function(x, y) c(x[keep], y[add]), current[fields], partial[fields])
    order <- order(result$lines, result$cols)
    result <- lapply(result, `[`, order)
    result$encoded <- .Call("encode_semantic_tokens_c", result$lines, result$cols,
        result$lengths, result$types, result$modifiers, PACKAGE = "languageserver")
    result
}

merge_partial_strings <- function(current, partial, incomplete_lines) {
    keep <- !(current$line - 1L) %in% incomplete_lines
    add <- (partial$line - 1L) %in% incomplete_lines
    if (all(keep) && !any(add)) return(current)
    strings <- Map(function(x, y) c(x[keep], y[add]), current, partial)
    lapply(strings, `[`, order(strings$line, strings$col1))
}
