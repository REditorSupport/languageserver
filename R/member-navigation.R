# Find possible member syntax once per parse. Receiver identities are resolved
# against the current workspace only for the requested name.
member_navigation_syntax <- function(parsed) {
    if (!is.null(parsed$member_navigation_syntax)) return(parsed$member_navigation_syntax)
    s <- parsed$syntax_data
    tok <- s$token
    children <- function(row) {
        kids <- syntax_children(s, row)
        kids[tok[kids] != "COMMENT"]
    }
    records <- collections::queue()
    dynamic <- list()
    add <- function(row, expr, kind, value = 0L) {
        text <- s$text[[row]]
        name <- if (tok[[row]] == "STR_CONST" || startsWith(text, "`") ||
                startsWith(text, "'") || startsWith(text, '"')) {
            tryCatch(member_name(str2lang(text)), error = function(e) NULL)
        } else {
            text
        }
        if (is.null(name) || !nzchar(name)) return()
        records$push(list(row = row, name = name, expr = expr, kind = kind, value = value))
    }
    for (op in which(tok %in% c("'$'", "'@'", "'['", "LBB"))) {
        expr <- s$parent_row[[op]]
        kids <- children(expr)
        if (tok[[op]] %in% c("'['", "LBB")) {
            args <- kids[-1L]
            values <- args[tok[args] == "expr"]
            if (!length(values)) next
            tokens <- lapply(values, children)
            literals <- vapply(tokens, function(x) length(x) == 1L && tok[x] == "STR_CONST", logical(1L))
            canonical <- tok[[op]] == "LBB" && identical(tok[kids], c("expr", "LBB", "expr", "']'", "']'"))
            if (!canonical || !literals[[1L]]) {
                # These subscripts do not expose a proven, editable field token.
                # Remember the receiver and possible key spellings for rename.
                numeric <- vapply(tokens, function(x) length(x) == 1L && tok[x] == "NUM_CONST", logical(1L))
                if (all(numeric)) next
                keys <- if (all(literals | numeric)) class_token_text(s$text[unlist(tokens[literals], use.names = FALSE)])
                exact <- which(tok[args] == "SYMBOL_SUB" & s$text[args] == "exact")
                optional <- if (length(exact) == 1L && exact + 2L <= length(args)) children(args[[exact + 2L]])
                partial <- tok[[op]] == "LBB" && length(exact) > 0L && !identical(s$text[optional], "TRUE")
                dynamic[[length(dynamic) + 1L]] <- list(receiver = kids[[1L]], keys = keys, partial = partial)
                next
            }
            key <- tokens[[1L]]
        } else {
            if (length(kids) != 3L) next
            key <- kids[[3L]]
        }
        parent <- s$parent_row[[expr]]
        outer <- if (parent) children(parent) else integer()
        kind <- "read"
        value <- 0L
        if (length(outer) >= 2L && outer[[1L]] == expr && tok[[outer[[2L]]]] == "'('") {
            kind <- "call"
        } else if (length(outer) == 3L) {
            assign <- tok[[outer[[2L]]]]
            if (outer[[1L]] == expr && assign %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
                kind <- "write"
                value <- outer[[3L]]
            } else if (outer[[3L]] == expr && assign == "RIGHT_ASSIGN") {
                kind <- "write"
                value <- outer[[1L]]
            }
        }
        add(key, expr, kind, value)
    }
    # Tags in a long list share a parent. Locate their neighbors in a single
    # pass rather than walking that entire sibling list for every argument.
    equals <- which(tok == "EQ_SUB")
    if (length(equals)) {
        siblings <- s$children[tok[s$children] != "COMMENT"]
        parents <- s$parent_row[siblings]
        first <- match(s$parent_row[equals], parents)
        at <- match(equals, siblings)
        keys <- siblings[at - 1L]
        values <- siblings[at + 1L]
        callees <- siblings[first]
        named <- !is.na(values) & tok[keys] %in% c("SYMBOL_SUB", "STR_CONST") &
            tok[siblings[first + 1L]] == "'('" & s$parent_row[values] == s$parent_row[equals]
        for (i in which(named)) add(keys[[i]], callees[[i]], "tag", values[[i]])
    }
    # Literal property and slot names are editable uses. The member resolver
    # verifies package identity and formal matching at the use site.
    helper_names <- names(member_key_formals)
    helpers <- which(tok == "SYMBOL_FUNCTION_CALL" & s$text %in% c(helper_names, paste0("`", helper_names, "`")))
    props_rhs <- integer(length(helpers))
    for (i in seq_along(helpers)) {
        helper <- helpers[[i]]
        call_row <- s$parent_row[[s$parent_row[[helper]]]]
        if (!call_row) next
        call <- syntax_call_arguments(s, call_row)
        if (is.null(call)) next
        key <- member_key_formals[[call$name]]
        rhs <- if (call$name == "slot") 0L else member_replacement_rhs(s, call_row)
        replacement <- rhs > 0L
        if (replacement && call$name == "props" && (is.null(call$package) || call$package == "S7")) props_rhs[[i]] <- rhs
        if (replacement && call$name != "prop") next
        args <- class_matched_arguments(call, c("object", key, if (replacement) "check"))
        arg <- args[[key]]
        if (is.null(arg)) next
        row <- class_literal_row(s, arg, "STR_CONST")
        if (row) add(row, call_row, "read")
    }
    records <- records$as_list()
    index <- if (length(records)) {
        data.frame(row = vapply(records, `[[`, 0L, "row"), name = vapply(records, `[[`, "", "name"),
            expr = vapply(records, `[[`, 0L, "expr"), kind = vapply(records, `[[`, "", "kind"),
            value = vapply(records, `[[`, 0L, "value"), stringsAsFactors = FALSE)
    } else {
        data.frame(row = integer(), name = character(), expr = integer(), kind = character(), value = integer())
    }
    for (class in parsed$class_data$declarations) {
        rows <- class$member_rows
        if (!length(rows)) next
        existing <- match(rows, index$row)
        index$kind[stats::na.omit(existing)] <- "field"
        missing <- which(is.na(existing))
        if (length(missing)) {
            index <- rbind(index, data.frame(row = rows[missing],
                    name = vapply(class$children[missing], `[[`, "", "name"), expr = 0L,
                    kind = "field", value = class$member_values[missing]))
        }
        replacements <- class$property_replacements
        if (length(replacements)) {
            targets <- class_token_text(s$text[replacements])
            declared <- match(targets, vapply(class$children, `[[`, "", "name"), nomatch = 0L)
            found <- which(declared > 0L | class$property_parent > 0L)
            if (length(found)) index <- rbind(index, data.frame(row = replacements[found],
                    name = targets[found], expr = class$property_parent, kind = "property_alias",
                    value = c(0L, rows)[declared[found] + 1L]))
        }
    }
    # Candidate property names in S7 updates. Preserve the exact outer
    # callee site for both dots and the documented single named-list form.
    updates <- which(tok == "SYMBOL_FUNCTION_CALL" & s$text %in% c("set_props", "`set_props`"))
    replacements <- which(props_rhs > 0L)
    if (length(updates) || length(replacements)) {
        caller <- integer(length(tok))
        nested <- logical(length(tok))
        if (length(replacements)) {
            replacement <- character(length(tok))
            for (i in replacements) {
                values <- syntax_call_arguments(s, props_rhs[[i]])
                if (!identical(values$name, "list") || (!is.null(values$package) && values$package != "base")) next
                caller[values$tags] <- helpers[[i]]
                nested[values$tags] <- TRUE
                replacement[values$tags] <- if (is.null(values$package)) "list" else "base::list"
            }
            index$replacement_list <- replacement[index$row]
        }
        for (helper in updates) {
            row <- s$parent_row[[s$parent_row[[helper]]]]
            if (!row) next
            args <- syntax_call_arguments(s, row)
            if (is.null(args) || (!is.null(args$package) && args$package != "S7")) next
            caller[args$tags] <- helper
            for (arg in args$values[args$tags == 0L & args$values > 0L]) {
                values <- syntax_call_arguments(s, arg)
                if (!identical(values$name, "list") || (!is.null(values$package) && values$package != "base")) next
                caller[values$tags] <- helper
                nested[values$tags] <- TRUE
            }
        }
        index$update_call <- caller[index$row]
        index$update_list <- nested[index$row]
    }
    index <- index[order(index$row), , drop = FALSE]
    index$group <- member_navigation_groups(s, index)
    attr(index, "literal_calls") <- member_navigation_literal_calls(s, index)
    by_name <- split(seq_len(nrow(index)), index$name)
    attr(index, "by_name") <- list2env(by_name, hash = TRUE, parent = emptyenv())
    attr(index, "dynamic_receivers") <- dynamic
    if (is.environment(parsed)) parsed$member_navigation_syntax <- index
    index
}

# Runs of direct, statically identical paths share one receiver proof within
# this request. Use the parser's block identity and the exact operator/key
# sequence. Calls, writes, control flow and nested blocks end a run. A bare
# name or scalar literal between two paths leaves their bindings intact.
member_navigation_groups <- function(s, index) {
    groups <- rep("", nrow(index))
    reads <- which(index$kind == "read")
    if (!length(reads)) return(groups)
    t <- s$token
    expr <- index$expr[reads]
    second <- s$child_start[expr] + 1L
    access <- which(second <= s$child_end[expr] & t[s$children[second]] %in% c("'$'", "'@'", "LBB"))
    reads <- reads[access]
    expr <- expr[access]
    if (!length(reads)) return(groups)
    # Intern path components from shortest to longest; no concatenation of
    # arbitrary member names can collide. Only accepted, literal accesses
    # already have a row in the member index.
    first <- s$children[s$child_start[expr]]
    token <- s$children[s$child_start[first]]
    simple <- which(s$child_start[first] == s$child_end[first] & t[token] == "SYMBOL")
    paths <- integer(length(t))
    paths[first[simple]] <- match(s$text[token[simple]], unique(s$text[token[simple]]))
    next_id <- max(c(0L, paths[first[simple]]))
    unresolved <- seq_along(reads)
    operator <- t[s$children[s$child_start[expr] + 1L]]
    # Receiver inference also has a depth budget. Extremely deep syntax can
    # still be examined per occurrence, without making indexing quadratic.
    for (depth in seq_len(128L)) {
        if (!length(unresolved)) break
        layer <- unresolved[paths[first[unresolved]] > 0L & operator[unresolved] %in% c("'$'", "'@'", "LBB")]
        if (!length(layer)) break
        # Numeric IDs and a fixed grammar delimiter precede the unrestricted
        # decoded key, so e.g. x$`a$b` and x$a$b remain distinct.
        keys <- paste(paths[first[layer]], operator[layer], index$name[reads[layer]], sep = ":")
        ids <- match(keys, unique(keys))
        paths[expr[layer]] <- next_id + ids
        next_id <- next_id + max(ids)
        unresolved <- setdiff(unresolved, layer)
    }
    if (!any(paths[expr])) return(groups)

    statements <- which(t %in% c("expr", "expr_or_assign_or_help"))
    parent <- s$parent_row[statements]
    blocks <- unique(s$parent_row[t == "'{'"])
    statements <- statements[parent == 0L | parent %in% blocks]
    if (!length(statements)) return(groups)
    statements <- statements[order(s$parent_row[statements], s$order_rank[statements])]
    blocks <- s$parent_row[statements]
    head <- s$children[s$child_start[statements]]
    atom <- s$child_start[statements] == s$child_end[statements] &
        t[head] %in% c("SYMBOL", "NUM_CONST", "STR_CONST", "NULL_CONST")
    boundary <- paths[statements] == 0L & !atom
    runs <- cumsum(boundary | c(TRUE, diff(blocks) != 0L))
    # Inner accesses in a direct path have the same lexical environment.
    # Accesses inside arguments, assignments and other expressions stay
    # independent and take the complete inference path.
    direct <- statements[paths[statements] > 0L]
    run <- integer(length(t))
    run[statements] <- runs
    while (length(direct)) {
        match_read <- match(direct, expr)
        keep <- which(!is.na(match_read))
        if (!length(keep)) break
        row <- match_read[keep]
        groups[reads[row]] <- paste(run[direct[keep]], paths[direct[keep]], sep = ":")
        inner <- first[row]
        run[inner] <- run[direct[keep]]
        direct <- inner
    }
    groups
}

# Possible consecutive, top-level calls on one simple receiver. This index
# proves only syntax. A run becomes reusable at request time after proving
# that its actual source member and each argument are inert. Every other
# statement, including a call on a different member, separates runs.
member_navigation_literal_calls <- function(s, index) {
    runs <- integer(nrow(index))
    calls <- which(index$kind == "call")
    if (!length(calls)) return(runs)
    expr <- index$expr[calls]
    call <- s$parent_row[expr]
    direct <- s$parent_row[call] == 0L
    head <- s$children[s$child_start[expr]]
    receiver <- s$children[s$child_start[head]]
    dollar <- s$token[s$children[s$child_start[expr] + 1L]] == "'$'"
    simple <- which(direct & dollar & s$child_start[head] == s$child_end[head] & s$token[receiver] == "SYMBOL")
    if (!length(simple)) return(runs)
    children <- s$children[s$token[s$children] != "COMMENT"]
    parents <- s$parent_row[children]
    roots <- s$roots[s$token[s$roots] != "COMMENT"]
    selected <- call[simple]
    arguments <- children[parents %in% selected]
    arguments <- arguments[!arguments %in% expr[simple]]
    kinds <- s$token[arguments]
    values <- arguments[kinds == "expr"]
    literal <- s$child_start[values] == s$child_end[values] &
        s$token[s$children[s$child_start[values]]] %in% c("NUM_CONST", "STR_CONST", "NULL_CONST")
    bad <- c(s$parent_row[values[!literal]], s$parent_row[arguments[!kinds %in% c("expr", "'('", "')'", "','")]])
    # An empty argument (e.g. f(,)) can force a default just as f() can.
    n <- tabulate(match(s$parent_row[values], selected), length(selected))
    commas <- tabulate(match(s$parent_row[arguments[kinds == "','"]], selected), length(selected))
    keep <- which(!selected %in% bad & (n == commas + 1L | (n == 0L & commas == 0L)))
    if (!length(keep)) return(runs)
    simple <- simple[keep]
    root_id <- match(call[simple], roots)
    receiver <- s$text[receiver[simple]]
    name <- index$name[calls[simple]]
    arity <- n[keep]
    same <- diff(root_id) == 1L & receiver[-1L] == receiver[-length(receiver)] &
        name[-1L] == name[-length(name)] & diff(arity) == 0L
    runs[calls[simple]] <- cumsum(c(TRUE, !same))
    counts <- integer(nrow(index))
    counts[calls[simple]] <- arity
    attr(runs, "arity") <- counts
    runs
}

member_navigation_indices <- function(index, names) {
    if (!length(names) || !nrow(index)) return(integer())
    catalog <- attr(index, "by_name", exact = TRUE)
    if (is.null(catalog)) return(which(index$name %in% names))
    if (length(names) == 1L) return(get0(names, envir = catalog, inherits = FALSE, ifnotfound = integer()))
    hits <- mget(unique(names), catalog, ifnotfound = list(integer()), inherits = FALSE)
    sort.int(as.integer(unlist(hits, use.names = FALSE)))
}

member_navigation_prefix_indices <- function(index, name) {
    n <- nchar(name)
    if (!n) return(integer())
    # A protocol rename can supply a very long non-syntactic name. Avoid
    # constructing all of its prefixes (quadratic total bytes).
    if (n > 128L) return(which(startsWith(rep.int(name, nrow(index)), index$name)))
    prefixes <- substring(name, 1L, seq_len(n))
    member_navigation_indices(index, prefixes)
}

member_navigation_key <- function(loc) {
    if (is.null(loc)) return("")
    paste(loc$uri, loc$range$start$line, loc$range$start$character,
        loc$range$end$line, loc$range$end$character, sep = ":")
}

member_navigation_token <- function(parsed, pos) {
    row <- navigation_token(parsed, pos$row + 1L, pos$col + 1L)
    if (is.null(row)) return(0L)
    # The general token lookup also accepts the preceding token's right edge.
    # A declaration range starts exactly on its first character, often after
    # an opening parenthesis or accessor. Prefer the identifier there.
    s <- parsed$syntax_data
    if (row && s$col2[[row]] == pos$col && s$line2[[row]] == pos$row + 1L) {
        next_row <- navigation_token(parsed, pos$row + 1L, pos$col + 2L)
        if (next_row && s$col1[[next_row]] == pos$col + 1L &&
                s$token[[next_row]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS",
                    "SYMBOL_SUB", "SLOT", "STR_CONST")) return(next_row)
    }
    row
}

member_navigation_point <- function(s, row) {
    list(row = s$line1[[row]] - 1L, col = s$col1[[row]] - 1L +
            as.integer(s$line1[[row]] == s$line2[[row]] && s$col2[[row]] > s$col1[[row]]))
}

member_replacement_rhs <- function(s, row) {
    parent <- s$parent_row[[row]]
    if (!parent) return(0L)
    # Only an edge of the parent expression can be the assignment target.
    # Inspect the indexed edges directly: one brace can contain many reads.
    first <- s$child_start[[parent]]
    last <- s$child_end[[parent]]
    kids <- s$children
    while (first < last && s$token[[kids[[first]]]] == "COMMENT") first <- first + 1L
    while (last > first && s$token[[kids[[last]]]] == "COMMENT") last <- last - 1L
    left <- kids[[first]]
    right <- kids[[last]]
    if (last - first < 2L || (row != left && row != right)) return(0L)
    op <- first + 1L
    while (op < last && s$token[[kids[[op]]]] == "COMMENT") op <- op + 1L
    operator <- s$text[[kids[[op]]]]
    if (row == left && operator %in% c("<-", "=")) return(right)
    if (row == right && operator == "->") return(left)
    0L
}

# Determine whether this tag declares a member or supplies a named argument to
# a user function. A locally shadowed list constructor has ordinary formals.
member_navigation_field_tag <- function(workspace, doc, rec) {
    if (rec$kind == "field") return(TRUE)
    if (rec$kind != "tag") return(FALSE)
    parsed <- doc$parse_data
    s <- parsed$syntax_data
    callee <- syntax_children(s, rec$expr)
    name <- s$text[utils::tail(callee, 1L)]
    if (!length(name) || !name %in% c("list", "data.frame", "list2")) return(FALSE)
    if (length(callee) == 3L) {
        return(s$token[[callee[[2L]]]] == "NS_GET" &&
                s$text[[callee[[1L]]]] == if (name == "list2") "rlang" else "base")
    }
    if (length(callee) != 1L) return(FALSE)
    if (length(navigation_definitions(parsed, s$line1[[callee]], s$col1[[callee]], name, token = callee))) return(FALSE)
    if (is.function(workspace$get_namespace)) {
        scope <- workspace$get_namespace(WORKSPACE, uri = doc$uri)
        if (!is.null(scope) && !is.null(scope$get_definition(name))) return(FALSE)
    }
    name != "list2" || "rlang" %in% doc$loaded_packages
}

member_navigation_access <- function(workspace, doc, rec, audit = NULL) {
    s <- doc$parse_data$syntax_data
    if (rec$kind == "property_alias") {
        if (rec$value) return(location(doc$uri, navigation_range(doc, s, rec$value)))
        # An alias can forward to a property inherited from a source parent.
        loc <- class_navigation_symbol(doc, doc$parse_data, member_navigation_point(s, rec$expr))$location
        if (is.null(loc)) return(NULL)
        gen <- member_resolve_cursor(doc$uri, workspace, doc, loc$point, loc$cursor, loc$cursor$token,
            origins = TRUE, origin_name = rec$name)$value$s7_generator
        return(member_navigation_origin(workspace, gen$properties[[rec$name]]$origin))
    }
    point <- member_navigation_point(s, rec$row)
    member_definition_location(doc$uri, workspace, doc, point, audit = audit)$result
}

member_navigation_accepted <- function(doc) {
    parsed <- doc$parse_data
    !is.null(doc$version) && is.environment(parsed) && identical(doc$version, parsed$version) &&
        identical(doc$content, doc$parsed_content) && !isTRUE(parsed$parse_error) && !length(parsed$partial_incomplete_lines)
}

# Only a real revisioned workspace can attest that an earlier per-site proof
# still describes the same caller, source dependencies, packages and indexes.
# Create this bounded store on a request; edits only advance existing epochs.
member_navigation_proof_cache <- function(workspace, doc, domain) {
    if (!member_navigation_accepted(doc) || !is.numeric(workspace$s3_revision) ||
            !is.numeric(workspace$index$revision) || !is.function(workspace$member_metadata$revision) ||
            !is.function(workspace$member_metadata$identity)) return(NULL)
    context <- list(uri = doc$uri, version = doc$version, generation = doc$parse_generation,
        parsed = doc$parse_data, workspace = workspace$documents, documents = workspace$documents$size(),
        revision = workspace$s3_revision, index = workspace$index, index_revision = workspace$index$revision,
        topology = workspace$index$topology_revision, packages = workspace$index$package_revision,
        metadata = workspace$member_metadata$identity(), metadata_revision = workspace$member_metadata$revision(),
        startup = workspace$startup_packages, imported = workspace$imported_packages)
    analysis_cache(doc, domain, context, max_bytes = 256L * 1024L, max_entries = 16L)
}


# Get the source function expression and its actual range, distinct from its
# declaration's name token. This is also the item's body for call hierarchy.
member_navigation_function <- function(doc, definition) {
    if (is.null(doc) || is.null(definition)) return(0L)
    parsed <- doc$parse_data
    s <- parsed$syntax_data
    if (is.null(s)) return(0L)
    point <- doc$from_lsp_position(definition$range$start)
    row <- member_navigation_token(parsed, point)
    if (!row) return(0L)
    index <- member_navigation_syntax(parsed)
    value <- index$value[match(row, index$row)]
    if (length(value) && !is.na(value) && value) {
        if (s$function_close[[value]]) return(value)
        kids <- syntax_children(s, value)
        if (length(kids) == 3L && identical(s$token[kids], c("'('", "expr", "')'")) &&
                s$function_close[[kids[[2L]]]]) return(kids[[2L]])
    }
    # A lexical function definition denotes the complete assignment range.
    node <- s$parent_row[[row]]
    while (node) {
        if (equal_range(navigation_range(doc, s, node), definition$range)) {
            children <- syntax_children(s, node)
            fn <- children[s$function_close[children] > 0L]
            if (length(fn) == 1L) return(fn)
        }
        node <- s$parent_row[[node]]
    }
    0L
}

member_navigation_masked_callee <- function(parsed, expr) {
    if (!length(parsed$data_mask$column)) return(FALSE)
    s <- parsed$syntax_data
    leaf <- class_literal_row(s, expr, "SYMBOL_FUNCTION_CALL")
    leaf > 0L && !is.null(data_mask_receiver(parsed$data_mask, leaf))
}

member_navigation_formals <- function(workspace, doc, rec) {
    parsed <- doc$parse_data
    if (member_navigation_masked_callee(parsed, rec$expr)) return(NULL)
    s <- parsed$syntax_data
    callee <- syntax_children(s, rec$expr)
    callee <- callee[s$token[callee] != "COMMENT"]
    if (!length(callee)) return(NULL)
    index <- member_navigation_syntax(parsed)
    access <- index[index$expr == rec$expr & index$kind == "call", , drop = FALSE]
    if (nrow(access) == 1L) {
        def <- member_navigation_access(workspace, doc, access)
    } else {
        if (length(callee) != 1L || s$token[[callee]] != "SYMBOL_FUNCTION_CALL") return(NULL)
        name <- s$text[[callee]]
        point <- member_navigation_point(s, callee)
        local <- navigation_definitions(parsed, point$row + 1L, point$col + 1L, name, token = callee, last = TRUE)
        if (length(local)) {
            def <- location(doc$uri, navigation_range(doc, s, local[[1L]]))
        } else {
            scope <- if (is.function(workspace$get_namespace)) workspace$get_namespace(WORKSPACE, uri = doc$uri)
            def <- if (!is.null(scope)) scope$get_definition(name)
        }
    }
    if (is.null(def)) return(NULL)
    owner <- workspace$documents$get(def$uri, NULL)
    fn <- member_navigation_function(owner, def)
    if (!fn) return(NULL)
    source <- owner$parse_data$syntax_data
    kids <- syntax_children(source, fn)
    formals <- kids[source$token[kids] == "SYMBOL_FORMALS"]
    list(owner = owner, rows = formals, names = reference_symbol_names(source$text[formals]))
}

# A named argument can reach the selected formal only if its callee does.
# Proven mask functions have no lexical tags; an ambiguous call can still
# use the caller's function. Likewise, a function stored in a named field
# may supply the masked callee even though its declaration is elsewhere.
member_masked_formal_conflict <- function(doc, owner, formal, old, new) {
    parsed <- doc$parse_data
    mask <- parsed$data_mask
    if (!length(mask$column)) return(FALSE)
    s <- parsed$syntax_data
    calls <- mask$column[s$token[mask$column] == "SYMBOL_FUNCTION_CALL"]
    heads <- s$parent_row[calls]
    calls <- calls[s$child_start[heads] == s$child_end[heads]]
    if (!length(calls)) return(FALSE)
    source <- owner$parse_data
    row <- member_navigation_token(source, owner$from_lsp_position(formal$range$start))
    if (!row) return(FALSE)
    value <- source$syntax_data$parent_row[[row]]
    # Parentheses around the function do not change the name that receives it.
    parent <- source$syntax_data$parent_row[[value]]
    while (parent) {
        parts <- syntax_children(source$syntax_data, parent)
        if (!identical(source$syntax_data$token[parts], c("'('", "expr", "')'"))) break
        value <- parent
        parent <- source$syntax_data$parent_row[[value]]
    }
    nav <- source$navigation_data
    names <- reference_symbol_names(source$syntax_data$text[nav$name[nav$value == value]])
    lexical <- length(names) > 0L
    if (lexical) {
        calls <- intersect(calls, unlist(mask$fallback[names], use.names = FALSE))
    } else {
        fields <- member_navigation_syntax(source)
        names <- fields$name[fields$value == value & fields$kind %in% c("tag", "field", "write")]
        calls <- calls[reference_symbol_names(s$text[calls]) %in% names]
    }
    if (!length(calls)) return(FALSE)
    tags <- member_navigation_syntax(parsed)
    hits <- member_navigation_prefix_indices(tags, old)
    if (!is.null(new)) hits <- union(hits, member_navigation_prefix_indices(tags, new))
    calls <- calls[s$parent_row[calls] %in% tags$expr[hits[tags$kind[hits] == "tag"]]]
    if (!lexical) return(length(calls) > 0L)
    for (call in calls) {
        # A preceding, unconditional function binding in the call's own block
        # rules out a different function of the same name. If the intervening
        # uses cannot be verified, the callee can still fall through.
        bound <- data_column_binding(parsed, call)
        if (!bound || !s$function_close[[bound]] || (doc$uri == owner$uri && bound == value)) return(TRUE)
    }
    FALSE
}

member_navigation_formal <- function(workspace, doc, rec) {
    info <- member_navigation_formals(workspace, doc, rec)
    if (is.null(info)) return(NULL)
    names <- info$names
    if (rec$name == "...") return(NULL)
    matched <- match(rec$name, names)
    if (is.na(matched)) {
        # Partial argument matching stops at dots. An exact tag elsewhere in
        # this call claims its formal before any partial tags are matched.
        index <- member_navigation_syntax(doc$parse_data)
        before_dots <- seq_len(match("...", names, nomatch = length(names) + 1L) - 1L)
        tags <- index$name[index$kind == "tag" & index$expr == rec$expr & index$row != rec$row]
        candidates <- before_dots[!names[before_dots] %in% tags & startsWith(names[before_dots], rec$name)]
        if (length(candidates) != 1L) return(NULL)
        matched <- candidates[[1L]]
    }
    owner <- info$owner
    location(owner$uri, navigation_range(owner, owner$parse_data$syntax_data, info$rows[[matched]]))
}

# R matches exact tags before partial tags and only then assigns positional
# arguments. Renaming a formal must preserve the mapping of untouched tags,
# including labels previously captured by dots.
member_navigation_tag_positions <- function(tags, formals) {
    args <- stats::setNames(seq_along(tags), tags)
    matched <- class_matched_arguments(list(values = args), formals)
    if (!length(matched)) return(NULL)
    mapping <- rep.int(match("...", formals), length(tags))
    for (key in setdiff(names(matched), "...")) mapping[matched[[key]]] <- match(key, formals)
    mapping
}

member_navigation_formal_conflict <- function(workspace, uris, target, old, new) {
    for (uri in uris) {
        doc <- workspace$documents$get(uri, NULL)
        if (is.null(doc)) next
        index <- member_navigation_syntax(doc$parse_data)
        possible <- which(index$kind == "tag" & startsWith(rep.int(new, nrow(index)), index$name))
        for (i in possible[!duplicated(index$expr[possible])]) {
            rec <- index[i, , drop = FALSE]
            if (member_navigation_field_tag(workspace, doc, rec)) next
            info <- member_navigation_formals(workspace, doc, rec)
            if (is.null(info) || !identical(info$owner$uri, target$uri)) next
            formal <- match(old, info$names)
            if (is.na(formal)) next
            owner <- info$owner
            if (!equal_range(target$range, navigation_range(owner, owner$parse_data$syntax_data, info$rows[[formal]]))) next
            tags <- index$name[index$kind == "tag" & index$expr == rec$expr]
            before <- member_navigation_tag_positions(tags, info$names)
            if (is.null(before)) return(TRUE)
            tags[before == formal] <- new
            renamed <- info$names
            renamed[[formal]] <- new
            if (!identical(before, member_navigation_tag_positions(tags, renamed))) return(TRUE)
        }
    }
    FALSE
}


member_navigation_origin <- function(workspace, origin) {
    if (is.null(origin$uri) || is.null(origin$range)) return(NULL)
    target <- workspace$documents$get(origin$uri, NULL)
    if (is.null(target)) return(NULL)
    rng <- origin$range
    if (!is.list(rng)) {
        if (length(rng) != 3L) return(NULL)
        last <- if (is.null(origin$end_row)) rng[[1L]] else origin$end_row
        rng <- range(target$to_lsp_position(rng[[1L]], rng[[2L]]), target$to_lsp_position(last, rng[[3L]]))
    }
    location(origin$uri, rng)
}

# Check the replacement key in the call's lexical scope. Only a dot argument
# (or an entry of the single accepted list) can name a property.
member_s7_update_receiver <- function(node, name, nested, index, env, budget) {
    if (is.character(nested)) {
        if (!identical(member_s7_method(node, index, env), "props<-")) return(NULL)
        if (nested == "list" && !member_s4_unshadowed("list", index, env)) return(NULL)
        args <- member_s7_arguments(node, c("object", "check"))
        object <- member_infer(args$object, index, env, budget = budget)
        return(if (isTRUE(object$s7) && is.null(object$s7_generator)) object)
    }
    if (!identical(member_s7_method(node, index, env), "set_props")) return(NULL)
    modes <- member_s7_modes(index)
    args <- member_s7_match(as.list(node)[-1L], modes$arguments$set_props)
    if (is.null(args)) return(NULL)
    if (nested) {
        if (!isTRUE(modes$lists) || length(args$dots) != 1L || any(nzchar(names(args$dots)))) return(NULL)
        values <- args$dots[[1L]]
        if (!is.call(values)) return(NULL)
        if (!identical(values[[1L]], quote(base::list))) {
            owner <- index$attached_roots$list$metadata
            if (!member_head(values, "list") || member_operator_masked("list", index, env) ||
                    (!is.null(owner) && !identical(owner, "base"))) return(NULL)
        }
        if (!name %in% names(values)[-1L]) return(NULL)
    } else if (!name %in% names(args$dots)) {
        return(NULL)
    }
    object <- member_infer(args$args[[modes$object]], index, env, budget = budget)
    if (isTRUE(object$s7) && is.null(object$s7_generator)) object
}

member_navigation_constructor_tag <- function(workspace, doc, rec) {
    if (isTRUE(rec$update_call > 0L)) {
        parsed <- doc$parse_data
        point <- member_navigation_point(parsed$syntax_data, rec$update_call)
        loc <- class_navigation_symbol(doc, parsed, point)$location
        if (is.null(loc)) return(NULL)
        loc$cursor$s7_update_list <- if (isTRUE(nzchar(rec$replacement_list))) rec$replacement_list else rec$update_list
        value <- member_resolve_cursor(doc$uri, workspace, doc, loc$point, loc$cursor, rec$name, origins = TRUE)$value
        return(member_navigation_origin(workspace, value$s7_descriptor$properties[[rec$name]]$origin))
    }
    point <- member_navigation_point(doc$parse_data$syntax_data, rec$row)
    origin <- member_constructor_parameter_origin(doc$uri, workspace, doc, point, rec$name)
    member_navigation_origin(workspace, origin)
}

member_navigation_at <- function(workspace, document, point) {
    parsed <- document$parse_data
    if (!identical(parsed$version, document$version) && !is.null(parsed$version)) return(NULL)
    row <- member_navigation_token(parsed, point)
    if (!row) return(NULL)
    s <- parsed$syntax_data
    kind <- s$token[[row]]
    if (!kind %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB", "SLOT", "STR_CONST")) return(NULL)
    if (kind %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") && !navigation_after_dollar(s, row)) return(NULL)
    if (kind == "STR_CONST" && !reference_member_literal_candidate(s, row)) return(NULL)
    index <- member_navigation_syntax(parsed)
    at <- match(row, index$row)
    if (is.na(at)) return(NULL)
    rec <- index[at, , drop = FALSE]
    rng <- navigation_range(document, s, row)
    if (rec$kind == "tag" && member_navigation_masked_callee(parsed, rec$expr)) {
        return(list(name = rec$name, definition = NULL, range = rng, formal = TRUE, kind = "tag"))
    }
    field <- member_navigation_field_tag(workspace, document, rec)
    formal <- rec$kind == "tag" && !field
    property <- if (formal || isTRUE(rec$update_call > 0L)) member_navigation_constructor_tag(workspace, document, rec)
    if (!is.null(property)) formal <- FALSE
    def <- if (!is.null(property)) {
        property
    } else if (field) {
        location(document$uri, rng)
    } else if (formal) {
        member_navigation_formal(workspace, document, rec)
    } else if (rec$kind == "write") {
        # A write starts a new reaching definition. Confirm from a following
        # read before treating an unknown receiver as an editable declaration.
        location(document$uri, rng)
    } else {
        member_navigation_access(workspace, document, rec)
    }
    list(name = rec$name, definition = def, range = rng, formal = formal, kind = rec$kind)
}

member_navigation_format <- function(doc, row) {
    text <- doc$parse_data$syntax_data$text[[row]]
    if (startsWith(text, "'")) "'" else if (startsWith(text, '"') ||
            doc$parse_data$syntax_data$token[[row]] == "STR_CONST") '"' else ""
}

member_navigation_uris <- function(workspace, target, context_uri) {
    if (workspace$documents$size() == 1L) return(workspace$documents$keys())
    workspace_reference_document_uris(workspace, target$uri, context_uri)
}

# Every returned use must resolve to exactly the selected source declaration.
# Keep its spelling on the server so rename can distinguish a key string from
# an identifier, including a backtick name that needs escaping.
member_navigation_occurrences <- function(workspace, owner, name, uris, formal = FALSE, declarations = TRUE, calls_only = FALSE) {
    if (is.null(owner)) return(list())
    property <- FALSE
    if (!formal && !calls_only) {
        source <- workspace$documents$get(owner$uri, NULL)
        for (class in source$parse_data$class_data$declarations) {
            if (!class$type %in% c("S4", "S7")) next
            matches <- Filter(function(x) x$name == name && equal_range(x$selectionRange, owner$range), class$children)
            if (length(matches)) property <- TRUE
        }
    }
    files <- vector("list", length(uris))
    for (f in seq_along(uris)) {
        uri <- uris[[f]]
        doc <- workspace$documents$get(uri, NULL)
        if (is.null(doc)) next
        parsed <- doc$parse_data
        if (is.null(parsed) || (!is.null(parsed$version) && !identical(parsed$version, doc$version))) next
        idx <- member_navigation_syntax(parsed)
        if (calls_only) {
            # R6 new() forwards to the source initializer. F12 verifies it.
            names <- if (name == "initialize") c(name, "new") else name
            candidates <- member_navigation_indices(idx, names)
            candidates <- candidates[idx$kind[candidates] == "call"]
            tags <- integer()
        } else if (formal) {
            candidates <- member_navigation_prefix_indices(idx, name)
            candidates <- candidates[idx$kind[candidates] == "tag"]
            tags <- integer()
        } else {
            hits <- member_navigation_indices(idx, name)
            candidates <- hits[idx$kind[hits] %in% c("read", "write", "call", if (property) c("tag", "property_alias"))]
            mask <- parsed$data_mask
            if (length(mask$env) || length(mask$column)) {
                candidates <- candidates[!idx$row[candidates] %in% c(mask$env, mask$column)]
            }
            tags <- if (declarations) hits[idx$kind[hits] %in% c("tag", "field")]
        }
        if (!length(candidates) && !length(tags)) next

        # Prove a group only once, including a failed proof. Ungrouped sites
        # remain independent. Defer URI/range objects until the reaching
        # definition graph selects the occurrences belonging to this owner.
        group <- idx$group[candidates]
        retained <- !nzchar(group) | !duplicated(group)
        chosen <- candidates[retained]
        # Opaque calls/expressions remain independent, even with identical
        # spelling. Retain only completed positive proofs, keyed by their
        # exact parser site for subsequent requests on this accepted snapshot.
        # Cheap grouped and name-miss paths need no persistent store.
        ungrouped <- !nzchar(group[retained]) & idx$kind[chosen] %in% c("read", "call", "write")
        use_cache <- !formal && sum(ungrouped) >= 2L
        cache <- if (use_cache) member_navigation_proof_cache(workspace, doc, "member-source-origins")
        cache_key <- paste(if (calls_only) "calls" else "members", name, sep = ":")
        cached <- if (!is.null(cache)) analysis_cache_get(cache, cache_key)$value
        origins <- rep("", length(chosen))
        fields <- vector("list", length(chosen))
        hit <- logical(length(chosen))
        if (length(cached$sites)) {
            # A peer can be in the middle of a reparse or have been reopened
            # at the same version. Check each target buffer once, including
            # its content hash, before accepting its stored physical offsets.
            current <- vapply(seq_along(cached$uris), function(j) {
                target <- workspace$documents$get(cached$uris[[j]], NULL)
                member_navigation_accepted(target) && identical(target$parse_data$content_hash, cached$hashes[[j]])
            }, logical(1L))
            found <- match(idx$row[chosen], cached$sites, nomatch = 0L)
            hit <- found > 0L & c(FALSE, current[cached$owners])[found + 1L]
            origins[hit] <- cached$origins[found[hit]]
            if (length(cached$field_ids)) fields[hit] <- cached$fields[cached$field_ids[found[hit]]]
        }
        audit <- if (!formal) new.env(parent = emptyenv())
        target_uris <- rep("", length(chosen))
        literal_calls <- attr(idx, "literal_calls", exact = TRUE)
        runs <- literal_calls[chosen]
        arity <- attr(literal_calls, "arity", exact = TRUE)
        certified <- 0L
        representative <- 0L
        for (j in which(!hit)) {
            i <- chosen[[j]]
            if (certified && runs[[j]] == certified) {
                origins[[j]] <- origins[[representative]]
                fields[j] <- fields[representative]
                target_uris[[j]] <- target_uris[[representative]]
                next
            }
            rec <- idx[i, , drop = FALSE]
            if (!is.null(audit)) {
                audit$source <- FALSE
                audit$fields <- NULL
                audit$literal_arity <- if (length(runs) && runs[[j]]) arity[[i]]
                audit$inert <- FALSE
            }
            target <- if (formal) {
                if (!member_navigation_field_tag(workspace, doc, rec)) member_navigation_formal(workspace, doc, rec)
            } else if (rec$kind == "tag") {
                member_navigation_constructor_tag(workspace, doc, rec)
            } else {
                member_navigation_access(workspace, doc, rec, audit)
            }
            origins[[j]] <- member_navigation_key(target)
            if (isTRUE(audit$source) && !is.null(target)) {
                if (!is.null(audit$fields)) fields[j] <- list(audit$fields)
                if (ungrouped[[j]]) target_uris[[j]] <- target$uri
                if (isTRUE(audit$inert)) {
                    certified <- runs[[j]]
                    representative <- j
                }
            }
        }
        if (!is.null(cache) && any(nzchar(target_uris))) {
            if (any(hit)) target_uris[hit] <- cached$uris[cached$owners[found[hit]]]
            uris_seen <- unique(target_uris[nzchar(target_uris)])
            hashes <- vapply(uris_seen, function(target_uri) {
                target <- workspace$documents$get(target_uri, NULL)
                if (!member_navigation_accepted(target)) return("")
                target$parse_data$content_hash %s3_or% ""
            }, character(1L), USE.NAMES = FALSE)
            positive <- nzchar(hashes)
            uris_seen <- uris_seen[positive]
            owners <- match(target_uris, uris_seen)
            save <- which(!is.na(owners))
            if (length(save)) {
                record <- list(sites = idx$row[chosen[save]], origins = origins[save],
                    owners = owners[save], uris = uris_seen, hashes = hashes[positive])
                # Keep one copy of identical closed field sets. The temporary
                # integer ID string is unambiguous even for unusual R names.
                ids <- fields[save]
                if (any(lengths(ids))) {
                    all_names <- unique(unlist(ids, use.names = FALSE))
                    labels <- vapply(ids, function(x) paste(match(x, all_names), collapse = ","), "")
                    first <- !duplicated(labels)
                    record$fields <- ids[first]
                    record$field_ids <- match(labels, labels[first])
                }
                analysis_cache_set(cache, cache_key, record)
            }
        }
        if (any(!retained)) {
            source <- match(group, group[retained])
            source[retained] <- seq_along(chosen)
            origins <- origins[source]
            fields <- fields[source]
        }
        writes <- rep("", length(candidates))
        prefix <- which(idx$kind[candidates] == "write")
        if (length(prefix)) {
            writes[prefix] <- member_navigation_site_keys(doc, idx$row[candidates[prefix]])
        }
        files[[f]] <- list(doc = doc, index = idx, candidates = candidates, origins = origins, writes = writes, tags = tags, fields = fields)
    }
    files <- Filter(Negate(is.null), files)
    origins <- unlist(lapply(files, `[[`, "origins"), use.names = FALSE)
    writes <- unlist(lapply(files, `[[`, "writes"), use.names = FALSE)
    group <- member_navigation_key(owner)
    edges <- which(nzchar(writes) & nzchar(origins) & writes != origins)
    if (length(edges)) {
        # Find the connected component of writes in either direction. A chain
        # of N successive writes is visited once rather than rescanned N times.
        from <- origins[edges]
        to <- writes[edges]
        neighbors <- list2env(split(c(to, from), c(from, to)), hash = TRUE, parent = emptyenv())
        seen <- new.env(hash = TRUE, parent = emptyenv())
        seen[[group]] <- TRUE
        queue <- character(length(ls(neighbors, all.names = TRUE)) + 1L)
        queue[[1L]] <- group
        head <- tail <- 1L
        while (head <= tail) {
            for (next_key in neighbors[[queue[[head]]]]) {
                if (is.null(seen[[next_key]])) {
                    seen[[next_key]] <- TRUE
                    tail <- tail + 1L
                    queue[[tail]] <- next_key
                }
            }
            head <- head + 1L
        }
        group <- queue[seq_len(tail)]
    }
    roots <- setdiff(group, writes[nzchar(origins) & writes != origins])
    result <- declaration_sites <- list()
    for (file in files) {
        idx <- file$index
        selected <- file$origins %in% group | file$writes %in% group
        root_write <- nzchar(file$writes) & file$writes %in% roots
        if (!declarations) selected <- selected & !root_write
        hits <- which(selected)
        if (length(hits)) {
            rows <- idx$row[file$candidates[hits]]
            roles <- idx$kind[file$candidates[hits]]
            roles[root_write[hits]] <- "declaration"
            result[[length(result) + 1L]] <- member_navigation_locations(file$doc, rows, roles, file$fields[hits])
        }
        if (length(file$tags)) {
            rows <- idx$row[file$tags]
            rows <- rows[member_navigation_site_keys(file$doc, rows) %in% roots]
            if (length(rows)) declaration_sites[[length(declaration_sites) + 1L]] <-
                member_navigation_locations(file$doc, rows, rep("declaration", length(rows)))
        }
    }
    c(rev(unlist(declaration_sites, recursive = FALSE)), unlist(result, recursive = FALSE), list())
}

member_navigation_columns <- function(doc, rows) {
    s <- doc$parse_data$syntax_data
    starts <- s$line1[rows]
    ends <- s$line2[rows]
    lines <- unique(c(starts, ends))
    text <- doc$content[lines]
    non_ascii <- nchar(text, type = "bytes") != nchar(text, type = "chars")
    cols <- c(s$col1[rows] - 1L, s$col2[rows])
    if (any(non_ascii)) {
        all <- c(starts, ends)
        affected <- which(all %in% lines[non_ascii])
        for (i in split(affected, all[affected])) {
            cols[i] <- code_point_to_unit(doc$content[[all[[i[[1L]]]]]], cols[i])
        }
    }
    n <- seq_along(rows)
    list(start = cols[n], end = cols[length(rows) + n])
}

member_navigation_site_keys <- function(doc, rows) {
    s <- doc$parse_data$syntax_data
    cols <- member_navigation_columns(doc, rows)
    paste(doc$uri, s$line1[rows] - 1L, cols$start, s$line2[rows] - 1L, cols$end, sep = ":")
}

member_navigation_locations <- function(doc, rows, roles, fields = NULL) {
    s <- doc$parse_data$syntax_data
    cols <- member_navigation_columns(doc, rows)
    lapply(seq_along(rows), function(i) {
        row <- rows[[i]]
        rng <- range(position(s$line1[[row]] - 1L, cols$start[[i]]),
            position(s$line2[[row]] - 1L, cols$end[[i]]))
        loc <- location(doc$uri, rng)
        attr(loc, "spelling") <- member_navigation_format(doc, row)
        attr(loc, "role") <- roles[[i]]
        attr(loc, "member_row") <- row
        if (!is.null(fields)) attr(loc, "member_fields") <- fields[[i]]
        loc
    })
}

member_navigation_formal_owner <- function(parsed, uri, name, key) {
    if (is.null(key)) return(NULL)
    index <- parsed$reference_index
    rows <- reference_indices(index, name, key)
    rows <- rows[index$is_definition[rows] & index$definition_kind[rows] == "formal"]
    if (length(rows) == 1L) indexed_reference_locations(index, uri, rows)[[1L]]
}

member_navigation_writable <- function(workspace, target) {
    !is.null(target) && is.null(attr(target, "namespace", exact = TRUE)) &&
        !is.null(workspace$documents$get(target$uri, NULL))
}

member_navigation_references <- function(id, workspace, document, member, include_declaration, for_rename, new_name = NULL) {
    target <- member$definition
    if (is.null(target) || (for_rename && !member_navigation_writable(workspace, target))) {
        return(Response$new(id, result = list()))
    }
    if (member$formal) {
        owner <- workspace$documents$get(target$uri)
        pos <- owner$from_lsp_position(target$range$start)
        row <- member_navigation_token(owner$parse_data, pos)
        if (!row) return(Response$new(id, result = list()))
        pos <- member_navigation_point(owner$parse_data$syntax_data, row)
        if (owner$parse_data$syntax_data$col1[[row]] == owner$parse_data$syntax_data$col2[[row]]) {
            pos$col <- pos$col + 1L
        }
        return(references_reply(id, target$uri, workspace, owner, pos, include_declaration, for_rename, new_name))
    }
    uris <- member_navigation_uris(workspace, target, document$uri)
    if (for_rename) {
        token <- list(token = member$name)
        glue_uris <- character()
        for (uri in uris) {
            doc <- workspace$documents$get(uri)
            if (!rename_document_complete(doc, document$uri, token, "member")) {
                return(rename_incomplete_error(id))
            }
            fragments <- doc$parse_data$glue_templates
            if (length(fragments) || length(attr(fragments, "unmapped", exact = TRUE))) glue_uris <- c(glue_uris, uri)
        }
        # A generated accessor can navigate to an implementation with a
        # different name (for example R6's new and initialize).
        owner <- workspace$documents$get(target$uri)
        i <- member_navigation_token(owner$parse_data, owner$from_lsp_position(target$range$start))
        names <- member_navigation_syntax(owner$parse_data)
        if (!any(names$row == i & names$name == member$name)) return(Response$new(id, result = list()))
        if (data_column_member_conflict(workspace, owner, target, i, member$name, uris)) return(data_column_rename_error(id))
    }
    result <- member_navigation_occurrences(workspace, target, member$name, uris, declarations = include_declaration)
    if (member$kind == "write" && !any(vapply(result, function(x) !identical(attr(x, "role"), "declaration"), logical(1L)))) {
        return(Response$new(id, result = list()))
    }
    if (for_rename && length(result)) {
        conflict <- member_navigation_rename_conflict(workspace, uris, result, member$name, new_name)
        if (!is.null(conflict)) return(ResponseErrorMessage$new(id, "RequestFailed", conflict))
        if (length(glue_uris)) return(glue_member_rename(id, workspace, member, glue_uris, result, new_name))
    } else if (!for_rename) {
        result <- lapply(result, function(x) location(x$uri, x$range))
    }
    Response$new(id, result = result)
}

member_navigation_methods <- function(workspace, doc) {
    if (is.null(doc)) return(list())
    parsed <- doc$parse_data
    s <- parsed$syntax_data
    idx <- member_navigation_syntax(parsed)
    rows <- which(idx$value > 0L & idx$kind %in% c("field", "tag", "write"))
    rows <- rows[s$function_close[idx$value[rows]] > 0L]
    for (class in parsed$class_data$declarations) {
        nonmethods <- class$member_rows[vapply(class$children, function(x) x$kind != SymbolKind$Method, logical(1L))]
        rows <- rows[!idx$row[rows] %in% nonmethods]
    }
    methods <- list()
    for (i in rows) {
        rec <- idx[i, , drop = FALSE]
        if (rec$kind == "tag" && !member_navigation_field_tag(workspace, doc, rec)) next
        select <- navigation_range(doc, s, rec$row)
        fn <- navigation_range(doc, s, rec$value)
        first <- if (s$order_rank[[rec$row]] < s$order_rank[[rec$value]]) select$start else fn$start
        last <- if (s$order_rank[[rec$row]] < s$order_rank[[rec$value]]) fn$end else select$end
        methods[[length(methods) + 1L]] <- list(name = rec$name, type = "function", range = range(first, last),
            memberDefinition = location(doc$uri, select))
    }
    methods
}

member_navigation_method <- function(workspace, definition) {
    if (is.null(definition)) return(NULL)
    doc <- workspace$documents$get(definition$uri, NULL)
    for (method in member_navigation_methods(workspace, doc)) {
        if (equal_range(method$memberDefinition$range, definition$range)) return(method)
    }
    NULL
}

member_navigation_call_item <- function(method, context_uri) {
    def <- method$memberDefinition
    list(name = method$name, kind = SymbolKind$Method, uri = def$uri,
        range = method$range, selectionRange = def$range,
        data = list(definition = def, member = TRUE, contextUri = context_uri))
}

member_navigation_calls <- function(workspace, definition, name, context_uri) {
    uris <- member_navigation_uris(workspace, definition, context_uri)
    member_navigation_occurrences(workspace, definition, name, uris, declarations = FALSE, calls_only = TRUE)
}

member_navigation_outgoing <- function(workspace, doc, item, nested = list()) {
    if (is.null(doc)) return(list())
    s <- doc$parse_data$syntax_data
    idx <- member_navigation_syntax(doc$parse_data)
    calls <- which(idx$kind == "call" & s$line1[idx$row] >= item$range$start$line + 1L &
            s$line1[idx$row] <= item$range$end$line + 1L)
    if (length(nested) && length(calls)) {
        rows <- idx$row[calls]
        units <- syntax_unit_columns(doc$content, s$line1[rows], s$col1[rows] - 1L, s$col2[rows])
        sites <- list(line = s$line1[rows] - 1L, end_line = s$line2[rows] - 1L,
            col = units$start, end_col = units$end)
        calls <- calls[indexed_call_containers(sites, seq_along(calls), nested) == 0L]
    }
    outgoing <- list()
    for (i in calls) {
        rec <- idx[i, , drop = FALSE]
        site <- navigation_range(doc, s, rec$row)
        if (!lsp_range_contains(item$range, site$start) || !lsp_range_contains(item$range, site$end)) next
        target <- member_navigation_access(workspace, doc, rec)
        if (is.null(target) || (!is.null(item$data$definition) && equal_definition(target, item$data$definition))) next
        key <- paste(target$uri, target$range$start$line, target$range$start$character, sep = ":")
        if (is.null(outgoing[[key]])) {
            method <- member_navigation_method(workspace, target)
            if (is.null(method)) next
            outgoing[[key]] <- list(to = member_navigation_call_item(method, item$uri), fromRanges = list())
        }
        outgoing[[key]]$fromRanges <- c(outgoing[[key]]$fromRanges, list(site))
    }
    unname(outgoing)
}

member_navigation_rename_conflict <- function(workspace, uris, refs, old, new) {
    # Keep all reaching source locations, including assignments, so an alias
    # after a write receives the same protection as its original receiver.
    memo <- new.env(parent = emptyenv())
    by_uri <- if (!is.null(new) && !identical(new, old)) split(refs, vapply(refs, `[[`, "", "uri"))
    same_origin <- function(origin) {
        loc <- member_navigation_origin(workspace, origin)
        if (is.null(loc)) return(FALSE)
        if (is.null(memo$origin_keys)) {
            spellings <- vapply(refs, member_navigation_key, "")
            memo$origin_keys <- list2env(stats::setNames(rep(list(TRUE), length(spellings)), spellings), parent = emptyenv())
        }
        exists(member_navigation_key(loc), memo$origin_keys, inherits = FALSE)
    }
    for (uri in uris) {
        doc <- workspace$documents$get(uri)
        parsed <- doc$parse_data
        s <- parsed$syntax_data
        index <- member_navigation_syntax(parsed)
        for (subscript in attr(index, "dynamic_receivers")) {
            keys <- subscript$keys
            if (length(keys) && !any(keys %in% c(old, new)) &&
                    !(subscript$partial && (any(startsWith(rep(old, length(keys)), keys)) ||
                                (!is.null(new) && any(startsWith(rep(new, length(keys)), keys)))))) next
            receiver <- subscript$receiver
            kids <- syntax_children(s, receiver)
            if (!length(kids)) next
            last <- utils::tail(kids, 1L)
            point <- member_navigation_point(s, last)
            loc <- if (length(kids) == 1L && s$token[[last]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
                member_symbol_location(doc, point)
            } else {
                member_hover_location(doc, point)
            }
            if (is.null(loc)) next
            value <- member_resolve_cursor(doc$uri, workspace, doc, loc$point, loc$cursor, loc$cursor$token,
                origins = TRUE, origin_name = old)$value
            field <- member_lookup(value$fields, old)
            if (same_origin(field$origin)) return("A subscript on this object may refer to the member")
        }
        # The receiver can acquire the proposed name later, including through
        # a list copy. Checking only at old-name reads misses that collision.
        prefixes <- member_navigation_prefix_indices(index, old)
        if (!is.null(new)) prefixes <- union(prefixes, member_navigation_prefix_indices(index, new))
        uses <- prefixes[index$name[prefixes] != old & index$kind[prefixes] %in% c("read", "call", "write")]
        if (length(uses)) {
            for (i in uses) {
                rec <- index[i, , drop = FALSE]
                loc <- member_hover_location(doc, member_navigation_point(s, rec$row))
                if (is.null(loc)) next
                exact <- identical(rec$name, new)
                if (!exact && !identical(loc$cursor$accessor, "$")) next
                resolved <- member_resolve_cursor(uri, workspace, doc, loc$point, loc$cursor,
                    origins = TRUE, origin_name = old)
                value <- resolved$value
                if (!exact && (!any(value$type %in% c("list", "pairlist", "data.frame")) ||
                            rec$name %in% names(value$fields))) next
                field <- if (identical(loc$cursor$accessor, "@")) {
                    member_slot(value, old, resolved$index, resolved$bindings, resolved$budget)
                } else {
                    member_lookup(value$fields, old)
                }
                if (same_origin(field$origin)) return(if (exact) "The new name is already used on this object" else
                        "A partial member access on this object could change meaning")
            }
        }
        local <- by_uri[[uri]]
        if (!length(local)) next
        rows <- vapply(local, function(ref) {
            row <- attr(ref, "member_row", exact = TRUE)
            if (!is.null(row)) return(row)
            member_navigation_token(parsed, doc$from_lsp_position(ref$range$start))
        }, integer(1L))
        hits <- match(rows, index$row, nomatch = 0L)
        fields <- lapply(local, attr, which = "member_fields", exact = TRUE)
        confirmed <- lengths(fields) > 0L
        if (any(confirmed) && new %in% unlist(fields[confirmed], use.names = FALSE)) {
            return("Another member of this object already has the new name")
        }
        hits <- unique(hits[hits > 0L & !confirmed])
        # A proved group has the same receiver state at every read. Additional
        # spellings and assignments that could change it form separate groups.
        groups <- index$group[hits]
        hits <- hits[!nzchar(groups) | !duplicated(groups)]
        for (i in hits) {
            row <- index$row[[i]]
            if (index$kind[[i]] %in% c("field", "tag")) {
                if (!is.null(index$update_call) && index$update_call[[i]] && !index$update_list[[i]]) {
                    metadata <- workspace$member_metadata
                    pkg <- if (is.function(metadata$catalog)) metadata$catalog("S7") else metadata$get("S7")
                    formals <- pkg$s7_capabilities$arguments$set_props
                    before <- seq_len(match("...", formals, nomatch = 1L) - 1L)
                    if (new %in% formals || any(startsWith(formals[before], new))) {
                        return("The new property name would match a set_props() parameter")
                    }
                }
                same_name <- member_navigation_indices(index, new)
                if (any(s$parent_row[index$row[same_name]] == s$parent_row[[row]])) {
                    return("Another member of this object already has the new name")
                }
                for (class in parsed$class_data$declarations) {
                    if (row %in% class$member_rows && any(vapply(class$children, function(x) x$name == new, logical(1L)))) {
                        return("Another member of this class already has the new name")
                    }
                }
            } else {
                loc <- member_hover_location(doc, member_navigation_point(s, row))
                if (is.null(loc)) next
                receiver <- member_resolve_cursor(uri, workspace, doc, loc$point, loc$cursor)
                if (!is.null(receiver) && new %in% names(member_members(receiver$value, receiver$index,
                            receiver$bindings, loc$cursor$accessor))) {
                    return("Another member of this object already has the new name")
                }
            }
        }
    }
    NULL
}
