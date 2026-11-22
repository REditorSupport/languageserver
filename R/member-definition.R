# The R AST drops positions of named arguments and written member keys. Join
# those declarations to their parser tokens. The resulting attributes travel
# with inferred fields without retaining a srcfile or parse tree.
member_origin_tree <- function(data, syntax = NULL) {
    token <- data$token
    callees <- which(token == "SYMBOL_FUNCTION_CALL")
    names <- reference_symbol_names(data$text[callees])
    classes <- callees[names %in% c("R6Class", "setClass", "setRefClass", "new_class", "deprecated_class")]
    lists <- callees[names %in% c("list", "list2", "data.frame", "c", "representation")]
    operators <- which(token %in% c("'$'", "LBB"))
    if (!length(lists) && !length(classes) && !length(operators)) return(list())
    if (is.null(syntax)) syntax <- syntax_parse_data(data)
    parent <- syntax$parent_row
    calls <- intersect(parent[parent[lists]], parent[token == "EQ_SUB"])
    # A member access on the RHS can have the same parent assignment. Its tag is
    # harmless and never consumed: mutation inference reads only the LHS.
    assignments <- parent[token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")]
    accesses <- parent[operators]
    writes <- accesses[parent[accesses] %in% assignments]
    class_calls <- parent[parent[classes]]
    class_token <- if (length(classes)) {
        positions <- integer(length(token))
        positions[class_calls] <- classes
        positions
    }
    targets <- unique(c(calls, writes, class_calls))
    if (!length(targets)) return(list())
    needed <- logical(length(token))
    current <- targets
    while (length(current)) {
        needed[current] <- TRUE
        current <- unique(parent[current])
        current <- current[current > 0L]
        current <- current[!needed[current]]
    }
    named <- write <- logical(length(token))
    named[calls] <- TRUE
    write[writes] <- TRUE
    list(syntax = syntax, needed = needed, named = named, write = write, class_token = class_token,
        placeholders = parent[token == "PLACEHOLDER"],
        roots = syntax$roots[token[syntax$roots] %in% c("expr", "expr_or_assign_or_help")])
}

member_source_origins <- function(parsed, data, uri, row_offset = 0L, col_offset = 0L,
    tree = NULL, item = NULL, edit = NULL) {
    if (is.null(tree)) tree <- member_origin_tree(if (is.null(data)) utils::getParseData(parsed) else data)
    if (!length(tree)) return(parsed)
    syntax <- tree$syntax
    token <- syntax$token
    needed <- tree$needed
    roots <- if (is.null(item)) tree$roots else tree$roots[item]
    if (anyNA(roots) || length(roots) != length(parsed) || !any(needed[roots])) return(parsed)
    origin <- function(i) {
        first <- syntax$line1[[i]]
        last <- syntax$line2[[i]]
        start <- syntax$col1[[i]] - 1L + if (first == 1L) col_offset else 0L
        end <- syntax$col2[[i]] + if (last == 1L) col_offset else 0L
        first <- row_offset + first - 1L
        last <- row_offset + last - 1L
        if (!is.null(edit) && first <= edit$row && edit$row <= last &&
                (edit$row < last || end > edit$start)) {
            if (first < edit$row || start < edit$start + edit$inserted) return(NULL)
            shift <- edit$inserted - (edit$end - edit$start)
            start <- start - shift
            if (last == edit$row) end <- end - shift
        }
        result <- list(uri = uri, range = c(first, start, end))
        if (first != last) result$end_row <- last
        result
    }
    expr_tokens <- c("expr", "expr_or_assign_or_help")
    walk <- function(node, row, inserted = 0L) {
        if (!needed[[row]] || !is.call(node)) return(node)
        if (!is.null(tree$class_token) && tree$class_token[[row]]) attr(node, "class_origin") <- origin(tree$class_token[[row]])
        children <- syntax_children(syntax, row)
        children <- children[token[children] != "COMMENT"]
        kinds <- token[children]
        if (tree$named[[row]] && !is.null(names(node))) {
            equals <- which(kinds == "EQ_SUB")
            argument <- cumsum(kinds == "','") + 1L + inserted
            tags <- lapply(children[equals - 1L], origin)
            names(tags) <- as.character(argument[equals])
            attr(node, "member_origins") <- tags
        }
        positions <- which(kinds %in% expr_tokens)
        if (tree$write[[row]]) {
            if (member_head(node, "$")) {
                attr(node, "member_origin") <- origin(utils::tail(children, 1L))
            } else if (member_head(node, "[[") && length(positions) == 2L && length(node) == 3L &&
                    is.character(node[[3L]])) {
                key <- syntax_children(syntax, children[[positions[[2L]]]])
                key <- key[token[key] == "STR_CONST"]
                if (length(key) == 1L) attr(node, "member_origin") <- origin(key)
            }
        }
        if ("forcond" %in% kinds) {
            cond <- syntax_children(syntax, children[kinds == "forcond"][[1L]])
            rows <- c(cond[token[cond] %in% expr_tokens], children[positions])
            if (length(rows) == 2L) {
                for (i in which(needed[rows])) node[[i + 2L]] <- walk(node[[i + 2L]], rows[[i]])
            }
            return(node)
        }
        rows <- children[positions]
        if (!any(needed[rows])) return(node)
        if (member_head(node, "function")) {
            defaults <- which(!vapply(as.list(node[[2L]]), identical, logical(1L), quote(expr = )))
            if (length(rows) == length(defaults) + 1L) {
                for (i in which(needed[rows])) {
                    if (i > length(defaults)) {
                        node[[3L]] <- walk(node[[3L]], rows[[i]])
                    } else {
                        formal <- defaults[[i]]
                        node[[2L]][[formal]] <- walk(node[[2L]][[formal]], rows[[i]])
                    }
                }
            }
            return(node)
        }
        if ("PIPE" %in% kinds) {
            # In the AST a native pipe has become its right-hand call. The
            # left-hand expression occupies a named placeholder or a new arg.
            right <- syntax_children(syntax, rows[[2L]])
            right <- right[token[right] != "COMMENT"]
            if (length(right) < 2L || token[right[[2L]]] != "'('") return(node)
            place <- which(right %in% tree$placeholders)
            added <- as.integer(!length(place))
            argument <- if (added) 2L else 2L + sum(token[right[seq_len(place[[1L]])]] == "','")
            if (argument > length(node)) return(node)
            if (needed[[rows[[1L]]]]) node[[argument]] <- walk(node[[argument]], rows[[1L]])
            return(walk(node, rows[[2L]], inserted = added))
        }
        if (length(kinds) >= 2L && kinds[[1L]] %in% expr_tokens && kinds[[2L]] %in% c("'('", "'['", "LBB")) {
            first <- if (kinds[[2L]] == "'('") 1L else 2L
            argument <- cumsum(kinds == "','") + first + 1L + inserted
            indices <- c(first, argument[positions[-1L]])
        } else if (member_head(node, "$") || member_head(node, "@")) {
            indices <- 2L
        } else {
            indices <- seq_along(node)[-1L]
            # R parses value -> target as target <- value. The token rows
            # still follow the source spelling, so visit the operands in reverse.
            if ("RIGHT_ASSIGN" %in% kinds) indices <- rev(indices)
        }
        if (length(rows) != length(indices) || any(indices > length(node))) return(node)
        for (i in which(needed[rows])) node[[indices[[i]]]] <- walk(node[[indices[[i]]]], rows[[i]])
        node
    }
    for (i in which(needed[roots])) parsed[[i]] <- walk(parsed[[i]], roots[[i]])
    parsed
}

# Only the originating statement needs coordinates. Reuse its accepted syntax
# tree or reparse the known-good statement from an otherwise incomplete file.
# Keep this memo on the definition request, bounded by the inference budget.
member_declaration_expr <- function(document, i, budget) {
    item <- document$parse_data$member_data$items[[i]]
    node <- item$expr
    if (is.null(budget$origin_name) || !is.call(node)) return(node)
    memo <- budget$declarations[[document$uri]]
    if (is.null(memo)) {
        memo <- new.env(parent = emptyenv())
        syntax <- document$parse_data$syntax_data
        if (!isTRUE(document$parse_data$parse_error) && length(syntax$roots)) {
            memo$tree <- member_origin_tree(syntax, syntax)
        }
        budget$declarations[[document$uri]] <- memo
    }
    key <- as.character(i)
    if (exists(key, memo, inherits = FALSE)) return(memo[[key]])
    if (!is.null(memo$tree)) {
        expr <- member_source_origins(as.expression(list(node)), NULL, document$uri, tree = memo$tree, item = i)
    } else {
        start <- item$start
        end <- item$end
        if (start[[1L]] < 0L || end[[1L]] >= document$nline) return(node)
        lines <- get_range_text(document$content, start[[1L]] + 1L, start[[2L]] + 1L, end[[1L]] + 1L, end[[2L]])
        parsed <- tryCatch(parse(text = normalize_parse_content(lines), keep.source = TRUE), error = function(e) NULL)
        if (length(parsed) != 1L) return(node)
        expr <- member_source_origins(as.expression(list(node)), utils::getParseData(parsed), document$uri,
            start[[1L]], start[[2L]])
    }
    memo[[key]] <- expr[[1L]]
    expr[[1L]]
}

member_binding_declaration <- function(entry, budget) {
    if (is.null(budget$origin_name) || (is.null(entry$field) && !is.call(entry$expr))) return(entry)
    source <- entry$declaration
    document <- if (is.null(source)) budget$document else budget$documents$get(source$uri, NULL)
    if (is.null(document)) return(entry)
    i <- if (is.null(source)) member_history_position(document$parse_data$member_data$items, entry$end) else source$item
    if (!i) return(entry)
    expr <- member_declaration_expr(document, i, budget)
    if (member_head(expr, ":=")) {
        # The binding resolver still verifies the operator before naming its RHS.
        entry$expr <- expr
    } else if (member_head(expr, "<-") || member_head(expr, "=")) {
        entry$expr <- expr[[3L]]
        if (!is.null(entry$field)) entry$origin <- attr(expr[[2L]], "member_origin", exact = TRUE)
    }
    entry
}

# Recovery normally needs only an AST. Collect member token ranges when that
# fragment contains a potential declaration; its first line may start mid-line.
member_parse_recovered <- function(lines, uri, row, col = 0L, edit = NULL) {
    if (is.null(uri) || !any(grepl("=|<-|->", lines))) return(str2expression(lines))
    parsed <- parse(text = normalize_parse_content(lines), keep.source = TRUE)
    parsed <- member_source_origins(parsed, NULL, uri, row, col, edit = edit)
    member_strip_srcref(parsed)
}

member_definition_location <- function(uri, workspace, document, point, audit = NULL) {
    location <- member_hover_location(document, point)
    if (is.null(location)) return(NULL)
    if (!is.null(audit)) audit$fields <- NULL
    resolved <- member_resolve_cursor(uri, workspace, document, location$point,
        location$cursor, location$cursor$token, origins = TRUE, audit = audit)
    if (is.null(resolved) && !check_scope(uri, document, point)) return(NULL)
    origin <- resolved$value$origin
    if (!is.null(audit)) {
        # Only the parser's physical (row, column, end-column) origin can
        # participate in cross-request member-navigation proofs. Installed
        # metadata, on-disk srcrefs and inconclusive budgets are rechecked.
        audit$source <- !is.null(origin) && is.numeric(origin$range) && length(origin$range) == 3L &&
            is.null(origin$source_text) && !isTRUE(resolved$budget$exhausted) &&
            !isTRUE(resolved$budget$transient) && !isTRUE(resolved$value$possible_receiver)
        if (!audit$source) audit$fields <- NULL
    }
    if (is.null(origin) && !is.null(resolved$value$function_key) && !is.null(resolved$value$metadata)) {
        owner <- member_package_context(resolved$index, resolved$value$metadata)
        origin <- member_lookup(owner$locations, resolved$value$function_key)
    }
    result <- NULL
    if (is.list(origin) && !is.null(origin$uri)) {
        if (!is.null(origin$source_text)) {
            result <- member_file_definition(origin, workspace)
        } else {
            target <- if (identical(origin$uri, uri)) document else workspace$documents$get(origin$uri, NULL)
            at <- origin$range
            if (is.list(at) && !is.null(at$start)) {
                result <- list(uri = origin$uri, range = at)
            } else if (!is.null(target) && length(at) == 3L) {
                last <- if (is.null(origin$end_row)) at[[1L]] else origin$end_row
                if (at[[1L]] >= 0L && last < target$nline) {
                    result <- list(uri = origin$uri, range = range(target$to_lsp_position(at[[1L]], at[[2L]]),
                            target$to_lsp_position(last, at[[3L]])))
                }
            }
        }
    }
    # A recognized property owns this request even if it has no definition.
    # A generic symbol lookup could jump to an unrelated package or local.
    list(result = result)
}

# Retain a physical source reference only from an ordinary srcfile binding.
# Verify its text again on navigation; installed packages often point to a
# build machine or to a source tree that has since changed.
member_function_origin <- function(fn) {
    ref <- attr(fn, "srcref", exact = TRUE)
    if (!inherits(ref, "srcref") || !is.integer(ref)) return(NULL)
    ref <- unclass(ref)
    if (length(ref) < 6L) return(NULL)
    file <- attr(ref, "srcfile", exact = TRUE)
    if (!is.environment(file)) return(NULL)
    binding <- function(key) {
        value <- member_binding(file, key)
        if (identical(value[[1L]], "value")) value[[2L]]
    }
    path <- binding("filename")
    if (!identical(binding("isFile"), TRUE) || !is.character(path) || is.object(path) || length(path) != 1L ||
            is.na(path) || !nzchar(path)) return(NULL)
    if (!fs::is_absolute_path(path)) {
        wd <- binding("wd")
        if (!is.character(wd) || is.object(wd) || length(wd) != 1L || is.na(wd) || !fs::is_absolute_path(wd)) return(NULL)
        path <- file.path(wd, path)
    }
    lines <- binding("lines")
    bounds <- if (length(ref) >= 8L) ref[c(7L, 8L)] else ref[c(1L, 3L)]
    if (!is.character(lines) || is.object(lines) || anyNA(bounds) || any(bounds < 1L) ||
            bounds[[1L]] > bounds[[2L]] || bounds[[2L]] > length(lines) || !file.exists(path)) return(NULL)
    line <- enc2utf8(lines[[bounds[[1L]]]])
    # Depending on how the package was sourced, the R parser counts either
    # UTF-8 bytes or characters in columns and expands tabs to eight stops.
    # Locate the function keyword only when its physical position is unique.
    spelling <- gregexpr(r"(function|\\)", line, perl = TRUE)[[1L]]
    if (spelling[[1L]] < 0L) return(NULL)
    positions <- as.integer(spelling) - 1L
    display_width <- function(prefix, type) {
        parts <- strsplit(paste0(prefix, " "), "\t", fixed = TRUE)[[1L]]
        widths <- nchar(parts, type = type)
        col <- 0L
        for (width in utils::head(widths, -1L)) col <- ((col + width) %/% 8L + 1L) * 8L
        col + utils::tail(widths, 1L) - 1L
    }
    matches <- vapply(positions, function(col) {
        prefix <- substr(line, 1L, col)
        display_width(prefix, "chars") == ref[[5L]] - 1L || display_width(prefix, "bytes") == ref[[5L]] - 1L
    }, logical(1L))
    if (sum(matches) != 1L) return(NULL)
    start <- code_point_to_unit(line, positions[matches])
    end <- start + attr(spelling, "match.length")[matches]
    list(uri = path_to_uri(path), range = range(position(bounds[[1L]] - 1L, start),
            position(bounds[[1L]] - 1L, end)), source_end = bounds[[2L]],
        source_text = digest::digest(enc2utf8(lines[seq.int(bounds[[1L]], bounds[[2L]])]), algo = "xxhash64"))
}

member_file_definition <- function(origin, workspace) {
    start <- origin$range$start$line + 1L
    end <- origin$source_end
    open <- workspace$documents$get(origin$uri, NULL)
    lines <- if (!is.null(open) && isTRUE(open$is_open)) {
        open$content
    } else {
        path <- path_from_uri(origin$uri)
        if (!fs::is_file(path) || file.access(path, 4L) != 0L) return(NULL)
        tryCatch(readLines(path, n = end, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    }
    if (length(lines) < end ||
            !identical(digest::digest(enc2utf8(lines[seq.int(start, end)]), algo = "xxhash64"), origin$source_text)) return(NULL)
    list(uri = origin$uri, range = origin$range)
}
