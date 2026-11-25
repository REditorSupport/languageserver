# Some lexical and pipe errors discard the entire parse table. On R versions
# that supply a physical error location, retain tokens strictly before its line.
parse_prefix_data <- function(content, error) {
    line <- error$lineno
    if (!identical(error$filename, "<text>") || length(line) != 1L ||
            is.na(line) || line < 2L || line > length(content) ||
            any(grepl("^#line[[:space:]]*[0-9]", content))) return(NULL)
    prefix <- content[seq_len(line - 1L)]
    srcfile <- srcfilecopy("<text>", prefix)
    tryCatch(parse(text = prefix, srcfile = srcfile, keep.source = TRUE), error = function(e) NULL)
    utils::getParseData(srcfile)
}

# A normalized parser column counts each tab once. Restore the original tab
# characters at surviving physical positions, including in multiline strings.
restore_parse_tabs <- function(projected, original, rows) {
    for (i in rows) {
        chars <- utf8ToInt(projected[[i]])
        tabs <- which(utf8ToInt(enc2utf8(original[[i]])) == 9L)
        chars[tabs[tabs <= length(chars)]] <- 9L
        projected[[i]] <- intToUtf8(chars)
    }
    projected
}

# Tabs and spaces can be distinct parts of two quoted parameter names. If
# replacing tabs by spaces creates a duplicate, try form feed (one R
# whitespace column) for positions. Values still come from the original R.
parse_tab_template <- function(content, original, srcfile, expr) {
    if (inherits(expr, "repeatedFormal") && !identical(content, original)) {
        rows <- which(grepl("\t", original, fixed = TRUE))
        if (length(rows)) {
            raw <- restore_parse_tabs(content, original, rows)
            actual <- tryCatch(parse(text = raw, keep.source = FALSE), error = identity)
            if (!inherits(actual, "repeatedFormal")) {
                alternate <- chartr("\t", "\f", raw)
                file <- srcfilecopy("<text>", alternate)
                parsed <- tryCatch(parse(text = alternate, srcfile = file, keep.source = TRUE), error = identity)
                if (!inherits(parsed, "repeatedFormal")) {
                    return(list(content = alternate, srcfile = file, expr = parsed))
                }
            }
        }
    }
    list(content = content, srcfile = srcfile, expr = expr)
}

# Keep every source reference (including nested function and block refs)
# from the position-preserving tree while taking literal values and argument
# names from a source-free parse of the corresponding original statement.
physical_expr_values <- function(physical, original) {
    if (identical(physical, original)) return(physical)
    if (!is.call(physical) && !is.pairlist(physical)) return(original)
    for (i in seq_along(original)) {
        if (identical(original[[i]], quote(expr = )) || inherits(physical[[i]], "srcref")) next
        if (!identical(physical[[i]], original[[i]])) {
            physical[[i]] <- Recall(physical[[i]], original[[i]])
        }
    }
    if (length(physical) == length(original)) names(physical) <- names(original)
    physical
}

restore_tabbed_parse <- function(expr, data, projected, original) {
    rows <- which(grepl("\t", original, fixed = TRUE))
    if (!length(rows) || !length(expr) || !length(data$id)) return(list(expr = expr, data = data))
    literal <- data$token %in% c("STR_CONST", "SPECIAL") | startsWith(data$text, "`")
    literal <- which(data$terminal & literal &
            findInterval(data$line2, rows) > findInterval(data$line1 - 1L, rows))
    if (!length(literal)) return(list(expr = expr, data = data))
    source <- restore_parse_tabs(projected, original, rows)
    spellings <- vapply(literal, function(i) {
        spelling <- get_range_text(source, data$line1[[i]], data$col1[[i]], data$line2[[i]], data$col2[[i]])
        paste(spelling, collapse = "\n")
    }, character(1L))
    changed <- grepl("\t", spellings, fixed = TRUE)
    if (!any(changed)) return(list(expr = expr, data = data))
    affected <- unique(data$line1[literal[changed]])
    # R abbreviates unusually long literal text in the token table.
    # Keep that representation; the AST retains the complete value.
    spelled <- changed & (data$token[literal] != "STR_CONST" | grepl("^[rR]?['\"]", data$text[literal]))
    data$text[literal[spelled]] <- spellings[spelled]
    refs <- attr(expr, "srcref")
    bounds <- vapply(refs, function(ref) as.integer(ref[c(1L, 3L)]), integer(2L))
    selected <- which(findInterval(bounds[2L, ], affected) > findInterval(bounds[1L, ] - 1L, affected))
    statements <- vapply(selected, function(i) {
        ref <- refs[[i]]
        paste(get_range_text(source, ref[[1L]], ref[[5L]], ref[[3L]], ref[[6L]]), collapse = "\n")
    }, character(1L))
    values <- tryCatch(parse(text = statements, keep.source = FALSE), error = function(e) NULL)
    rejected <- integer()
    for (j in seq_along(selected)) {
        i <- selected[[j]]
        ref <- refs[[i]]
        value <- if (length(values) == length(selected)) values[j] else
            tryCatch(parse(text = statements[[j]], keep.source = FALSE), error = function(e) NULL)
        if (length(value) == 1L) {
            expr[i] <- list(physical_expr_values(expr[[i]], value[[1L]]))
        } else {
            # Normalization can also disguise a duplicate (an escaped \t and
            # a literal tab). Omit the unverified root, even if its normalized
            # spelling parses, and retain the surrounding original statements.
            lines <- seq.int(ref[[1L]], ref[[3L]])
            last_col <- ref[[6L]]
            after <- substring(projected[[ref[[3L]]]], last_col + 1L)
            separator <- regexpr("^[[:blank:]]*;", after)
            if (separator > 0L) last_col <- last_col + attr(separator, "match.length")
            for (line in lines) {
                start <- if (line == ref[[1L]]) ref[[5L]] else 1L
                end <- if (line == ref[[3L]]) last_col else nchar(projected[[line]])
                if (end >= start) substr(projected[[line]], start, end) <- strrep(" ", end - start + 1L)
            }
            rejected <- union(rejected, lines - 1L)
        }
    }
    if (length(rejected)) {
        expr <- parse_provider_projection(projected)
        restored <- restore_tabbed_parse(expr, utils::getParseData(expr), projected, original)
        restored$incomplete_lines <- sort(unique(c(rejected, restored$incomplete_lines)))
        restored$content <- projected
        return(restored)
    }
    list(expr = expr, data = data)
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
partial_semantic_data <- function(data, content, token, terminals, gap_data = NULL) {
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
    if (!is.null(gap_data) && data$token[[token$row]] %in% c("NS_GET", "NS_GET_INT")) {
        # A lone receiver inserted in a verified statement gap is a plain
        # symbol unless an enclosing function binds it as a formal. Its line
        # can then be classified exactly without reparsing the entire buffer.
        rows <- terminals[data$line1[terminals] == line]
        if (length(rows) == 2L && data$token[[rows[[1L]]]] == "SYMBOL" &&
                grepl("^[[:space:]]*$", substr(content[[line]], 1L, data$col1[[rows[[1L]]]] - 1L))) {
            formals <- gap_data$text[gap_data$token == "SYMBOL_FORMALS"]
            receiver <- reference_symbol_names(data$text[rows[[1L]]])
            if (!receiver %in% reference_symbol_names(formals)) {
                one <- utils::getParseData(parse(text = content[[line]], keep.source = TRUE))
                one$line1 <- one$line1 + line - 1L
                one$line2 <- one$line2 + line - 1L
                return(one)
            }
        }
    }
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
parse_provider_projection <- function(content, expr = NULL) {
    parse_source <- function(text) {
        tryCatch(parse(text = text, keep.source = TRUE), error = function(e) NULL)
    }
    if (is.null(expr)) expr <- parse_source(content)
    if (!is.null(expr)) {
        physical <- physical_parse_content(content, expr)
        if (!identical(physical, content)) expr <- parse_source(physical)
    }
    expr
}

# Keep whole, gap-free roots for edits spanning a block. Member providers also
# need the exact leaf spans for unconditional statements inside a block.
partial_complete_roots <- function(syntax, gaps, statements) {
    expr_tokens <- c("expr", "expr_or_assign_or_help")
    roots <- syntax$roots[syntax$token[syntax$roots] %in% expr_tokens]
    complete <- roots[findInterval(syntax$line1[roots] - 2L, gaps) ==
            findInterval(syntax$line2[roots] - 1L, gaps)]
    refs <- attr(statements, "srcref")
    if (!length(refs)) return(complete)
    # A standalone statement has its own effective file-scope range. Match
    # both ends, in physical code-point coordinates, against the recovered AST.
    rows <- which(syntax$scope_row == seq_along(syntax$token) - 1L & syntax$token %in% expr_tokens)
    spans <- vapply(refs, function(ref) as.integer(ref[c(7L, 5L, 8L, 6L)]), integer(4L))
    start <- function(line, col) as.double(line) * syntax$maxcol + col
    rows <- rows[match(start(spans[1L, ], spans[2L, ]), start(syntax$line1[rows], syntax$col1[rows]))]
    exact <- which(!is.na(rows) & syntax$line2[rows] == spans[3L, ] & syntax$col2[rows] == spans[4L, ])
    rows <- union(complete, rows[exact])
    rows[order(syntax$order_rank[rows])]
}

# A gap at top level or between statements in a brace block can be removed.
# Reject gaps that would join an operand, call argument or control-flow body
# to code following the error (e.g. x <- / pkg:: / unrelated()).
projection_statement_gap <- function(data, line, col) {
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

# Only affected lines take tokens from the failed/repaired token tree. Keep
# the enclosing top-level expressions so parameter and function bindings
# resolve exactly as they do in the full tree. Rebase parser IDs rather than
# sizing the classifier's ID tables to the token's offset in a large file.
partial_token_context <- function(data, incomplete_lines) {
    if (!length(data$id) || !length(incomplete_lines)) return(NULL)
    first <- incomplete_lines[[1L]] + 1L
    last <- incomplete_lines[[length(incomplete_lines)]] + 1L
    roots <- which(data$parent == 0L & data$line1 <= last & data$line2 >= first)
    first <- min(first, data$line1[roots])
    last <- max(last, data$line2[roots])
    keep <- data$line1 <= last & data$line2 >= first
    if (all(keep)) return(data)
    data <- data[keep, , drop = FALSE]
    data$parent <- match(data$parent, data$id, nomatch = 0L)
    data$id <- seq_len(nrow(data))
    data
}

recover_partial_content <- function(content, srcfile, error, include_tokens = TRUE, member_hints = FALSE, row_offset = 0L) {
    data <- utils::getParseData(srcfile)
    prefix_only <- is.null(data)
    if (prefix_only) data <- parse_prefix_data(content, error)
    if (length(data$id)) {
        # An error can leave positive parent IDs whose rows were never reduced.
        missing_parent <- data$parent > 0L & !(data$parent %in% data$id)
        data$parent[missing_parent] <- 0L
    }
    terminals <- which(data$terminal & !data$token %in% c("COMMENT", "LINE_DIRECTIVE"))
    member_packages <- if (member_hints && !prefix_only) member_cell_packages(content, data, terminals)
    token <- if (!prefix_only) partial_failure_token(data, content, terminals)
    gap_data <- NULL
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
            candidate_data <- if (!is.null(expr)) utils::getParseData(expr)
            if (!is.null(expr) && projection_statement_gap(candidate_data, line, start)) {
                projected <- candidate
                gap_data <- candidate_data
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
            kinds <- data$token[terminals]
            at_top <- sum(kinds == "'{'") <= sum(kinds == "'}'") &&
                sum(kinds == "'('") <= sum(kinds == "')'") &&
                sum(kinds == "'['") + 2L * sum(kinds == "LBB") <= sum(kinds == "']'")
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
    tokens <- if (include_tokens) partial_semantic_data(data, content, token, terminals, gap_data)
    # A repaired trailing operator can preserve its function in the token
    # tree even when the structural projection discards the whole statement.
    type_hints <- if (include_tokens) member_roxygen_type_index(content, tokens, row_offset)
    syntax <- if (!is.null(gap_data)) gap_data else if (include_tokens) utils::getParseData(expr)
    inlay_lines <- incomplete
    if (length(inlay_lines) && (inherits(error, c("invalidRawLiteral", "unrecognizedEscape")) ||
                grepl("^[^\n]*\\bINCOMPLETE_STRING\\b", conditionMessage(error), perl = TRUE))) {
        # Editing a raw delimiter can turn unchanged calls into string text.
        # Only rebase old hints on complete lines reached by the lexer.
        through <- max(c(0L, data$line2[data$terminal])) - 1L
        inlay_lines <- inlay_lines[inlay_lines < through]
    }
    list(content = projected, expr = expr, syntax = syntax,
        token_data = partial_token_context(tokens, incomplete), incomplete_lines = incomplete,
        inlay_fallback_lines = inlay_lines, member_packages = member_packages, type_hints = type_hints)
}

# Keep scoped classifications and suffix positions from the structural tree,
# and actual lexer tokens (including dangling operators) on discarded lines.
merge_partial_tokens <- function(current, partial, incomplete_lines) {
    keep <- !current$lines %in% incomplete_lines
    add <- partial$lines %in% incomplete_lines
    if (all(keep) && !any(add)) {
        if (!is.null(current$encoded)) return(current)
        result <- current
    } else {
        fields <- c("lines", "cols", "lengths", "types", "modifiers")
        result <- Map(function(x, y) c(x[keep], y[add]), current[fields], partial[fields])
        # Both inputs are ordered. Appending a trailing operator needs no sort.
        if (is.unsorted(result$lines)) result <- lapply(result, `[`, order(result$lines, result$cols))
    }
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
