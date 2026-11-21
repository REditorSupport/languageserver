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
    # slot() and prop() have literal key arguments. The member resolver
    # verifies package identity and formal matching at the use site.
    helpers <- which(tok == "SYMBOL_FUNCTION_CALL" & s$text %in% c("slot", "prop"))
    for (helper in helpers) {
        call_row <- s$parent_row[[s$parent_row[[helper]]]]
        if (!call_row) next
        call <- syntax_call_arguments(s, call_row)
        if (is.null(call)) next
        args <- class_matched_arguments(call, c("object", "name"))
        arg <- args$name
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
    }
    index <- index[order(index$row), , drop = FALSE]
    # Consecutive top-level reads of the same simple receiver share the
    # same source bindings. A declaration, nested call or expression with
    # uncertain effects ends the group, as does a class registration.
    index$group <- rep("", nrow(index))
    if (nrow(index)) {
        roots <- s$roots
        top <- c(0L, roots)[1L + findInterval(s$order_rank, s$order_rank[roots])]
        root <- top[index$row]
        read <- index$kind == "read" & index$expr == root
        call <- index$kind == "call" & s$parent_row[pmax(1L, index$expr)] == root
        simple <- which((read | call) & !index$name %in% c("set", "register"))
        first <- s$children[s$child_start[index$expr[simple]]]
        binding <- s$children[s$child_start[first]]
        simple_head <- s$child_end[first] == s$child_start[first] & tok[binding] == "SYMBOL"
        simple <- simple[which(simple_head)]
        binding <- binding[which(simple_head)]
        bad <- top[tok %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "FUNCTION", "OP-LAMBDA",
                "SPECIAL", "PIPE", "SYMBOL_PACKAGE", "IF", "FOR", "WHILE", "REPEAT")]
        calls <- tabulate(top[tok == "SYMBOL_FUNCTION_CALL"], nbins = length(tok))
        keep <- which(!root[simple] %in% bad & calls[root[simple]] <= 1L)
        simple <- simple[keep]
        binding <- binding[keep]
        runs <- cumsum(!roots %in% root[simple])
        accessor <- tok[s$children[s$child_start[index$expr[simple]] + 1L]]
        receiver <- which(accessor %in% c("'$'", "'@'", "LBB"))
        simple <- simple[receiver]
        index$group[simple] <- paste(runs[match(root[simple], roots)], s$text[binding[receiver]], accessor[receiver],
            index$name[simple], sep = ":")
    }
    attr(index, "dynamic_receivers") <- dynamic
    if (is.environment(parsed)) parsed$member_navigation_syntax <- index
    index
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

member_navigation_access <- function(workspace, doc, rec) {
    s <- doc$parse_data$syntax_data
    point <- member_navigation_point(s, rec$row)
    member_definition_location(doc$uri, workspace, doc, point)$result
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

member_navigation_formals <- function(workspace, doc, rec) {
    parsed <- doc$parse_data
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

member_navigation_constructor_tag <- function(workspace, doc, rec) {
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
    index <- member_navigation_syntax(parsed)
    at <- match(row, index$row)
    if (is.na(at)) return(NULL)
    rec <- index[at, , drop = FALSE]
    rng <- navigation_range(document, s, row)
    field <- member_navigation_field_tag(workspace, document, rec)
    formal <- rec$kind == "tag" && !field
    property <- if (formal) member_navigation_constructor_tag(workspace, document, rec)
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
    key <- function(loc) {
        if (is.null(loc)) return("")
        paste(loc$uri, loc$range$start$line, loc$range$start$character,
            loc$range$end$line, loc$range$end$character, sep = ":")
    }
    property <- FALSE
    if (!formal && !calls_only) {
        source <- workspace$documents$get(owner$uri, NULL)
        for (class in source$parse_data$class_data$declarations) {
            if (!class$type %in% c("S4", "S7")) next
            matches <- Filter(function(x) x$name == name && equal_range(x$selectionRange, owner$range), class$children)
            if (length(matches)) property <- TRUE
        }
    }
    uses <- list()
    origins <- writes <- character()
    for (uri in uris) {
        doc <- workspace$documents$get(uri, NULL)
        if (is.null(doc)) next
        parsed <- doc$parse_data
        if (is.null(parsed) || (!is.null(parsed$version) && !identical(parsed$version, doc$version))) next
        idx <- member_navigation_syntax(parsed)
        candidates <- if (calls_only) {
            # An R6 constructor forwards to its (possibly inherited) initializer.
            # F12 verifies the receiver and the physical initializer declaration.
            calls <- if (name == "initialize") c(name, "new") else name
            which(idx$kind == "call" & idx$name %in% calls)
        } else if (formal) {
            which(idx$kind == "tag" & startsWith(rep.int(name, nrow(idx)), idx$name))
        } else {
            which(idx$name == name & idx$kind %in% c("read", "write", "call", if (property) "tag"))
        }
        known <- new.env(hash = TRUE, parent = emptyenv())
        for (i in candidates) {
            rec <- idx[i, , drop = FALSE]
            if (formal && member_navigation_field_tag(workspace, doc, rec)) next
            target <- if (formal) {
                member_navigation_formal(workspace, doc, rec)
            } else if (rec$kind == "tag") {
                member_navigation_constructor_tag(workspace, doc, rec)
            } else if (nzchar(rec$group) && exists(rec$group, known, inherits = FALSE)) {
                known[[rec$group]]
            } else {
                resolved <- member_navigation_access(workspace, doc, rec)
                if (nzchar(rec$group)) assign(rec$group, resolved, known)
                resolved
            }
            if (is.null(target) && rec$kind != "write") next
            rng <- navigation_range(doc, parsed$syntax_data, rec$row)
            loc <- location(uri, rng)
            attr(loc, "spelling") <- member_navigation_format(doc, rec$row)
            attr(loc, "role") <- rec$kind
            j <- length(uses) + 1L
            uses[[j]] <- loc
            origins[[j]] <- key(target)
            writes[[j]] <- if (rec$kind == "write") key(loc) else ""
        }
    }
    # F12 returns the most recent assignment. Bridge successive writes only
    # when the earlier source location was proved at the assignment itself.
    # Rebinding the receiver to a different object starts a separate group.
    group <- key(owner)
    edges <- which(nzchar(writes) & nzchar(origins) & writes != origins)
    repeat {
        incident <- edges[origins[edges] %in% group | writes[edges] %in% group]
        if (!length(incident)) break
        group <- unique(c(group, writes[incident], origins[incident]))
        edges <- setdiff(edges, incident)
    }
    selected <- origins %in% group | writes %in% group
    result <- uses[selected]
    # Each independent starting point has a source tag or initial write.
    # Later writes are references even when declarations have been excluded.
    roots <- setdiff(group, writes[nzchar(origins) & writes != origins])
    if (length(result)) {
        root_use <- vapply(result, function(loc) key(loc) %in% roots, logical(1L))
        if (!declarations) result <- result[!root_use]
        else for (i in which(root_use)) attr(result[[i]], "role") <- "declaration"
    }
    if (declarations && !formal) {
        for (uri in uris) {
            doc <- workspace$documents$get(uri, NULL)
            if (is.null(doc)) next
            idx <- member_navigation_syntax(doc$parse_data)
            rows <- idx$row[idx$name == name & idx$kind %in% c("tag", "field")]
            for (row in rows) {
                loc <- location(uri, navigation_range(doc, doc$parse_data$syntax_data, row))
                if (!key(loc) %in% roots) next
                attr(loc, "spelling") <- member_navigation_format(doc, row)
                attr(loc, "role") <- "declaration"
                result <- c(list(loc), result)
            }
        }
    }
    result
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
        for (uri in uris) {
            if (!rename_document_complete(workspace$documents$get(uri), document$uri, token, "member")) {
                return(rename_incomplete_error(id))
            }
        }
        # A generated accessor can navigate to an implementation with a
        # different name (for example R6's new and initialize).
        owner <- workspace$documents$get(target$uri)
        i <- member_navigation_token(owner$parse_data, owner$from_lsp_position(target$range$start))
        names <- member_navigation_syntax(owner$parse_data)
        if (!any(names$row == i & names$name == member$name)) return(Response$new(id, result = list()))
    }
    result <- member_navigation_occurrences(workspace, target, member$name, uris, declarations = include_declaration)
    if (member$kind == "write" && !any(vapply(result, function(x) !identical(attr(x, "role"), "declaration"), logical(1L)))) {
        return(Response$new(id, result = list()))
    }
    if (for_rename && length(result)) {
        conflict <- member_navigation_rename_conflict(workspace, uris, result, member$name, new_name)
        if (!is.null(conflict)) return(ResponseErrorMessage$new(id, "RequestFailed", conflict))
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
    same_origin <- function(origin) {
        loc <- member_navigation_origin(workspace, origin)
        !is.null(loc) && any(vapply(refs, function(ref) equal_definition(ref, loc), logical(1L)))
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
        uses <- which(index$kind %in% c("read", "call", "write") & index$name != old)
        if (length(uses)) {
            spellings <- index$name[uses]
            prefix <- startsWith(rep(old, length(uses)), spellings)
            if (!is.null(new)) prefix <- prefix | startsWith(rep(new, length(uses)), spellings)
            for (i in uses[prefix]) {
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
        if (is.null(new) || identical(new, old)) next
        for (ref in refs) {
            if (!identical(ref$uri, uri)) next
            pos <- doc$from_lsp_position(ref$range$start)
            row <- member_navigation_token(parsed, pos)
            rec <- index[match(row, index$row), , drop = FALSE]
            if (!nrow(rec) || is.na(rec$row)) next
            if (rec$kind %in% c("field", "tag")) {
                if (any(index$name == new & s$parent_row[index$row] == s$parent_row[[row]])) {
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
