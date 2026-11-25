# A snapshot retains source facts only. Resolve them in the current workspace
# on demand so an edit to a sourced file can change an embedded use's owner.
glue_inner_callee_shadowed <- function(syntax, index, row, name) {
    if (is.null(index)) return(FALSE)
    head <- syntax_children(syntax, row)
    head <- head[syntax$token[head] != "COMMENT"][[1L]]
    callee <- utils::tail(syntax_children(syntax, head), 1L)
    point <- member_navigation_point(syntax, callee)
    key <- reference_key_at(index, point, name)
    if (!length(key)) return(FALSE)
    startsWith(key, "local:") || any(index$is_definition[reference_indices(index, name, key)])
}

# Filter by both decoded fragment text and the original call (which contains
# its tags and delayed RHSs). Exact token parsing below verifies every hit.
glue_reference_catalog <- function(document) {
    parsed <- document$parse_data
    fragments <- parsed$glue_templates
    if (!length(fragments) || !identical(document$version, parsed$version) ||
            !identical(document$content, document$parsed_content)) return(NULL)
    if (!is.null(parsed$glue_reference_catalog)) return(parsed$glue_reference_catalog)
    rows <- vapply(fragments, "[[", integer(1L), "call_row")
    calls <- unique(rows)
    group <- match(rows, calls)
    decoded <- vapply(fragments, function(f) paste(f$content, collapse = "\n"), "")
    source <- rep("", length(calls))
    valid <- which(calls > 0L)
    syntax <- parsed$syntax_data
    r <- calls[valid]
    source[valid] <- substr(document$content[syntax$line1[r]], syntax$col1[r], syntax$col2[r])
    for (i in which(syntax$line1[r] != syntax$line2[r])) {
        row <- r[[i]]
        source[[valid[[i]]]] <- paste(get_range_text(document$content, syntax$line1[[row]], syntax$col1[[row]],
                syntax$line2[[row]], syntax$col2[[row]]), collapse = "\n")
    }
    text <- c(decoded, source)
    owners <- c(group, seq_along(calls))
    words <- strsplit(text, "[^A-Za-z0-9._]+", perl = TRUE)
    names <- unlist(words, use.names = FALSE)
    selected <- nzchar(names)
    word_groups <- rep.int(owners, lengths(words))
    hits <- split(word_groups[selected], names[selected])
    # In damaged code the R lexer can split a contiguous word, e.g. 1Lname
    # or _name. Whole-word filtering is conclusive only for ordinary names.
    joined <- grepl("^(_.|[0-9]|\\.[0-9])", names) & grepl("[A-Za-z_]", names)
    numbers <- "^(0[xX][[:xdigit:]]+(\\.[[:xdigit:]]*)?([pP][0-9]+)?|([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][0-9]+)?)[Li]?$"
    joined[joined] <- !grepl(numbers, names[joined])
    # Backtick and escaped spellings may decode to a name absent as a literal
    # word. Keep these groups, including Unicode names, for exact checking.
    uncertain <- grepl("[`\\\\]", text) | nchar(text, "bytes") != nchar(text, "chars")
    call_group <- integer(length(syntax$token))
    call_group[calls[valid]] <- valid
    catalog <- list(groups = unname(split(seq_along(fragments), group)), call_group = call_group,
        by_name = list2env(lapply(hits, unique), parent = emptyenv()), uncertain = unique(c(owners[uncertain], word_groups[joined])),
        decoded = match(decoded, unique(decoded)), mapped = new.env(parent = emptyenv()),
        tokens = new.env(parent = emptyenv()))
    parsed$glue_reference_catalog <- catalog
    catalog
}

# Fully parse only the selected expressions. Plain reads and calls need a
# token table; closure/assignment scopes and recovery use the provider parser.
glue_reference_tokens <- function(document, fragment) {
    source <- enc2utf8(fragment$content)
    expr <- tryCatch(parse(text = if (any(grepl("\t", source, fixed = TRUE))) chartr("\t", " ", source) else source,
            keep.source = TRUE, encoding = "UTF-8"), error = function(e) NULL)
    data <- if (!is.null(expr)) utils::getParseData(expr)
    simple <- !is.null(expr) && !any(data$token %in% c("FUNCTION", "'\\\\'", "LEFT_ASSIGN", "RIGHT_ASSIGN",
            "EQ_ASSIGN", "forcond")) && !any(grepl("\t", source, fixed = TRUE))
    if (simple) {
        syntax <- syntax_parse_data(data)
        index <- NULL
        parse_error <- FALSE
    } else {
        parsed <- glue_fragment_document(document, fragment)$parse_data
        syntax <- parsed$syntax_data
        index <- parsed$reference_index
        parse_error <- parsed$parse_error
    }
    mutations <- index$name[index$is_definition & startsWith(as.character(index$definition_key), "global:")]
    # A superclass assignment and member replacement can be omitted from
    # the ordinary declaration table. Conservatively mark their receiver.
    for (op in which(syntax$token %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN"))) {
        group <- syntax_children(syntax, syntax$parent_row[[op]])
        group <- group[syntax$token[group] != "COMMENT"]
        if (length(group) != 3L) next
        lhs <- group[[if (syntax$token[[op]] == "RIGHT_ASSIGN") 3L else 1L]]
        i <- lhs
        while (!syntax$terminal[[i]]) {
            children <- syntax_children(syntax, i)
            if (!length(children)) break
            i <- children[[1L]]
        }
        if (syntax$token[[i]] != "SYMBOL") next
        name <- class_token_text(syntax$text[[i]])
        local <- reference_key_at(index, member_navigation_point(syntax, i), name)
        if (class_literal_row(syntax, lhs, "SYMBOL") && length(local) && startsWith(local, "local:") &&
                !syntax$text[[op]] %in% c("<<-", "->>")) next
        mutations <- c(mutations, name)
    }
    rows <- syntax$terminals[syntax$token[syntax$terminals] %in%
            c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS", "SYMBOL_SUB")]
    terms <- lapply(rows, function(row) {
        kind <- syntax$token[[row]]
        if (navigation_after_dollar(syntax, row)) return(NULL)
        parent <- syntax$parent_row[[row]]
        peers <- syntax_children(syntax, parent)
        if (any(syntax$token[peers] %in% c("'@'", "NS_GET", "NS_GET_INT"))) return(NULL)
        name <- class_token_text(syntax$text[[row]])
        pos <- member_navigation_point(syntax, row)
        i <- reference_index_at(index, pos, name)
        key <- if (i) index$definition_key[[i]]
        call <- if (kind == "SYMBOL_SUB") syntax_call_arguments(syntax, parent)
        if (length(call) && glue_inner_callee_shadowed(syntax, index, parent, call$name)) call$inner_callee <- parent
        list(name = name, row = row, call = call, kind = kind,
            start = list(row = syntax$line1[[row]] - 1L, col = syntax$col1[[row]] - 1L),
            end = list(row = syntax$line2[[row]] - 1L, col = syntax$col2[[row]]),
            local_key = if (length(key) && startsWith(key, "local:")) key,
            definition = i > 0L && index$is_definition[[i]])
    })
    terms <- Filter(Negate(is.null), terms)
    inner_names <- if (length(index$name)) unlist(lapply(terms, function(term) {
        if (term$definition && length(term$local_key)) term$name
    }), use.names = FALSE)
    list(tokens = terms, mutations = unique(mutations), inner_bindings = union(mutations, inner_names), parse_error = parse_error)
}

glue_reference_facts <- function(document, name = NULL, call_row = NULL) {
    catalog <- glue_reference_catalog(document)
    if (is.null(catalog)) return(list())
    groups <- if (is.null(name)) seq_along(catalog$groups) else union(catalog$uncertain,
        get0(name, catalog$by_name, inherits = FALSE, ifnotfound = integer()))
    call_row <- call_row[!is.na(call_row) & call_row > 0L]
    if (length(call_row)) groups <- intersect(groups, catalog$call_group[call_row])
    selected <- as.integer(unlist(catalog$groups[groups], use.names = FALSE))
    fragments <- document$parse_data$glue_templates
    lapply(selected, function(i) {
        id <- as.character(i)
        facts <- catalog$mapped[[id]]
        if (!is.null(facts)) return(facts)
        fragment <- fragments[[i]]
        key <- as.character(catalog$decoded[[i]])
        facts <- catalog$tokens[[key]]
        if (is.null(facts)) {
            facts <- glue_reference_tokens(document, fragment)
            catalog$tokens[[key]] <- facts
        }
        facts$fragment <- fragment
        facts$tokens <- lapply(facts$tokens, function(term) {
            a <- glue_source_point(fragment, term$start)
            b <- glue_source_point(fragment, term$end)
            term$range <- range(document$to_lsp_position(a$row, a$col), document$to_lsp_position(b$row, b$col))
            term
        })
        catalog$mapped[[id]] <- facts
        facts
    })
}

glue_occurrence <- function(name, target, selection, fragment, call_row, before = Inf, definition = FALSE,
    argument_tag = FALSE, function_position = FALSE, inner_bindings = NULL) {
    list(name = name, key = target$key, target = target, range = selection, fragment = fragment,
        call_row = call_row, before = before, definition = definition, argument_tag = argument_tag,
        function_position = function_position, inner_bindings = inner_bindings)
}

# A named argument to a call inside the interpolation belongs to that
# function's formal, even if the glue mask contains the same spelling.
glue_formal_target <- function(workspace, document, fragment, token) {
    call <- token$call
    if (!length(call) || !is.null(call$package)) return(NULL)
    if (length(call$inner_callee)) {
        inner <- glue_fragment_document(document, fragment)
        candidate <- glue_call_target(workspace, inner, list(call_row = call$inner_callee), call$name, list(), lexical_only = TRUE)
        if (!is.null(candidate)) return(NULL)
    }
    owner <- glue_context_at(workspace, document, fragment, call$name, function_position = TRUE)$target
    if (length(owner$callable)) owner <- owner$callable
    if (is.null(owner$range)) return(NULL)
    host <- workspace$documents$get(owner$uri, NULL)
    if (is.null(host)) return(NULL)
    parsed <- host$parse_data
    value <- owner$value_row
    if (!length(value) || !value) {
        pos <- host$from_lsp_position(owner$range$start)
        declared <- member_navigation_token(parsed, pos)
        nav <- parsed$navigation_data
        assignment <- which(nav$name == declared)
        if (length(assignment)) value <- nav$value[[assignment[[1L]]]]
    }
    if (!length(value) || !value || !parsed$syntax_data$function_close[[value]]) return(NULL)
    siblings <- syntax_children(parsed$syntax_data, value)
    rows <- siblings[parsed$syntax_data$token[siblings] == "SYMBOL_FORMALS"]
    names <- reference_symbol_names(parsed$syntax_data$text[rows])
    i <- match(token$name, names, nomatch = 0L)
    if (!i) return(NULL)
    row <- rows[[i]]
    selection <- navigation_range(host, parsed$syntax_data, row)
    key <- reference_key_at(parsed$reference_index, host$from_lsp_position(selection$start), token$name)
    list(kind = "lexical", name = token$name, key = key, uri = host$uri, range = selection, row = row,
        writable = !is.null(key), formal = TRUE)
}

glue_reference_occurrences <- function(workspace, document, name = NULL, call_row = NULL) {
    facts <- glue_reference_facts(document, name, call_row)
    if (!length(facts)) return(list())
    syntax <- document$parse_data$syntax_data
    groups <- split(seq_along(facts), vapply(facts, function(f) f$fragment$call_row, integer(1L)))
    result <- list()
    for (group in groups) {
        first <- facts[[group[[1L]]]]$fragment
        bindings <- glue_call_bindings(workspace, document, first)
        if (isTRUE(bindings$shadowed)) next
        unknown <- function(nm) list(kind = "unknown", name = nm, writable = FALSE)
        mutations <- unique(unlist(lapply(facts[group], "[[", "mutations"), use.names = FALSE))
        # Named arguments and .x fields have source locations outside of the
        # string. Reclassify reads in delayed RHSs in binding order.
        for (k in seq_along(bindings$named)) {
            target <- bindings$named[[k]]
            before <- bindings$tags[[k]]
            if (is.null(name) || identical(name, target$name)) {
                result[[length(result) + 1L]] <- glue_occurrence(target$name, target, target$range,
                    NULL, first$call_row, before, TRUE)
            }
            value <- target$value_row
            if (!length(value) || !value) next
            index <- document$parse_data$reference_index
            candidates <- if (is.null(name)) seq_along(index$name) else reference_indices(index, name)
            if (!length(candidates)) next
            a <- syntax$line1[[value]] - 1L
            b <- syntax$line2[[value]] - 1L
            within <- candidates[(index$line[candidates] > a | (index$line[candidates] == a &
                            index$code_point_col[candidates] >= syntax$col1[[value]] - 1L)) &
                    (index$line[candidates] < b | (index$line[candidates] == b &
                                index$code_point_end_col[candidates] <= syntax$col2[[value]]))]
            for (i in within[!nzchar(index$call_package[within])]) {
                nm <- index$name[[i]]
                # Nested functions, assignments and their locals use ordinary
                # R scopes inside the argument expression.
                if (index$is_definition[[i]]) next
                use <- indexed_reference_locations(index, document$uri, i)[[1L]]$range
                site <- member_navigation_token(document$parse_data, document$from_lsp_position(use$start))
                node <- site
                nested <- FALSE
                while (node && node != value) {
                    if (syntax$function_close[[node]]) nested <- TRUE
                    node <- syntax$parent_row[[node]]
                }
                if (syntax$function_close[[value]]) nested <- TRUE
                ctx <- list(call_row = first$call_row, before = before)
                function_position <- syntax$token[[site]] == "SYMBOL_FUNCTION_CALL"
                target <- if (nested) unknown(nm) else glue_context_at(workspace, document, ctx, nm, function_position)$target
                if (is.null(target)) next
                result[[length(result) + 1L]] <- glue_occurrence(nm, target, use, NULL, first$call_row, before, function_position = function_position)
            }
        }
        for (target in bindings$fields) {
            if (identical(target$uri, document$uri) && (is.null(name) || identical(name, target$name))) {
                result[[length(result) + 1L]] <- glue_occurrence(target$name, target, target$range,
                    NULL, first$call_row, definition = TRUE)
            }
        }
        for (i in group) {
            f <- facts[[i]]
            fragment <- f$fragment
            if (isTRUE(f$parse_error) && length(name) && (any(grepl(name, fragment$content, fixed = TRUE)) ||
                        any(grepl("\\", fragment$content, fixed = TRUE)))) {
                result[[length(result) + 1L]] <- glue_occurrence(name, unknown(name), NULL, fragment, fragment$call_row)
            }
            for (term in f$tokens) {
                nm <- term$name
                if (!is.null(name) && !identical(name, nm)) next
                target <- NULL
                if (!fragment$complete || isTRUE(f$parse_error) || nm %in% mutations) {
                    target <- unknown(nm)
                } else if (term$kind == "SYMBOL_SUB") {
                    if (length(term$call) && !term$call$name %in% mutations) {
                        target <- glue_formal_target(workspace, document, fragment, term)
                    }
                } else if (length(term$local_key)) {
                    key <- term$local_key
                    if (term$kind == "SYMBOL_FUNCTION_CALL") {
                        inner <- glue_fragment_document(document, fragment)
                        candidate <- glue_call_target(workspace, inner, list(call_row = term$row), nm, list(), lexical_only = TRUE)
                        key <- candidate$key
                        if (is.null(candidate)) target <- glue_context_at(workspace, document, fragment, nm, function_position = TRUE)$target
                    }
                    if (length(key)) {
                        declaration <- Filter(function(x) isTRUE(x$definition) && identical(x$local_key, key), f$tokens)
                        if (length(declaration)) target <- list(kind = "inner", name = nm,
                            uri = document$uri, range = declaration[[1L]]$range, writable = FALSE,
                            key = paste("glue", fragment$start$row, fragment$start$col, key, sep = ":"))
                    }
                } else {
                    target <- glue_context_at(workspace, document, fragment, nm, function_position = term$kind == "SYMBOL_FUNCTION_CALL")$target
                }
                if (is.null(target)) target <- unknown(nm)
                result[[length(result) + 1L]] <- glue_occurrence(nm, target, term$range, fragment,
                    fragment$call_row, definition = term$definition, argument_tag = term$kind == "SYMBOL_SUB",
                    function_position = term$kind == "SYMBOL_FUNCTION_CALL", inner_bindings = f$inner_bindings)
            }
        }
    }
    result
}

glue_reference_site <- function(workspace, document, point) {
    parsed <- document$parse_data
    if (!length(parsed$glue_templates) || !identical(document$version, parsed$version) ||
            !identical(document$content, document$parsed_content)) return(NULL)
    fragment <- glue_fragment_at(parsed, point)
    if (!is.null(fragment) && isTRUE(glue_call_bindings(workspace, document, fragment)$shadowed)) return(NULL)
    inner <- if (!is.null(fragment)) glue_fragment_document(document, fragment)
    call_row <- fragment$call_row
    if (is.null(inner)) {
        row <- member_navigation_token(parsed, point)
        syntax <- parsed$syntax_data
        if (!row || !syntax$token[[row]] %in% c("SYMBOL", "SYMBOL_SUB", "SYMBOL_FUNCTION_CALL", "STR_CONST")) return(NULL)
        name <- class_token_text(syntax$text[[row]])
        if (syntax$token[[row]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
            # Only named right-hand sides within a glue call override an
            # ordinary lexical occurrence. A declaration stays in host R.
            hit <- reference_index_at(parsed$reference_index, point, name)
            if (hit && parsed$reference_index$is_definition[[hit]]) return(NULL)
            groups <- glue_reference_catalog(document)$call_group
            node <- syntax$parent_row[[row]]
            while (node && !groups[[node]]) node <- syntax$parent_row[[node]]
            if (!node) return(NULL)
            call_row <- node
        }
    } else {
        row <- member_navigation_token(inner$parse_data, glue_inner_point(fragment, point))
        name <- if (row) class_token_text(inner$parse_data$syntax_data$text[[row]])
    }
    if (length(name) && nzchar(name)) {
        pos <- document$to_lsp_position(point$row, point$col)
        for (site in glue_reference_occurrences(workspace, document, name, call_row)) {
            if (!is.null(site$range) && lsp_range_contains(site$range, pos)) return(site)
        }
    }
    # Suppress the ordinary STR_CONST highlight/lookup on inner whitespace,
    # nested string/comment content and other non-identifiers.
    if (!is.null(inner)) glue_occurrence(name, NULL, NULL, fragment, fragment$call_row)
}

glue_overlay_locations <- function(workspace, document, name, key, ordinary, include_declaration = TRUE,
    occurrences = glue_reference_occurrences(workspace, document, name)) {
    if (!length(occurrences)) return(ordinary)
    additions <- list()
    for (site in occurrences) {
        if (!length(site$key) || !identical(key, site$key) || (!include_declaration && site$definition)) next
        loc <- location(document$uri, site$range)
        attr(loc, "glue_site") <- site
        attr(loc, "fragment") <- site$fragment
        additions[[length(additions) + 1L]] <- loc
    }
    blocked <- Filter(function(site) is.null(site$fragment), occurrences)
    if (length(ordinary) && length(blocked)) {
        blocked_ids <- vapply(blocked, function(site) member_navigation_key(location(document$uri, site$range)), character(1L))
        ordinary <- ordinary[!vapply(ordinary, member_navigation_key, character(1L)) %in% blocked_ids]
    }
    locs <- c(ordinary, additions)
    if (length(locs)) locs <- locs[!duplicated(vapply(locs, member_navigation_key, character(1L)))]
    locs
}

# Match field uses to the same member declaration that ordinary $ navigation
# found. This also reaches interpolations in a dependent source() file.
glue_member_locations <- function(workspace, owner, name, uris, ordinary, include_declaration = TRUE) {
    key <- paste("glue", "field", owner$uri, owner$range$start$line, owner$range$start$character, sep = ":")
    additions <- list()
    for (uri in uris) {
        doc <- workspace$documents$get(uri, NULL)
        if (!length(doc$parse_data$glue_templates)) next
        sites <- glue_reference_occurrences(workspace, doc, name)
        hits <- Filter(function(x) identical(key, x$key), sites)
        additions <- c(additions, glue_overlay_locations(workspace, doc, name, key, list(), include_declaration, hits))
    }
    if (!length(additions)) return(ordinary)
    ordinary <- c(ordinary, additions)
    ordinary <- ordinary[!duplicated(vapply(ordinary, member_navigation_key, character(1L)))]
    ordinary
}

glue_site_locations <- function(workspace, document, site, include_declaration = TRUE) {
    site <- reference_source_glue_site(workspace, document, site)
    target <- site$target
    if (!length(target$key)) return(list())
    if (identical(target$kind, "lexical")) {
        source <- workspace$documents$get(target$uri, NULL)
        if (!is.null(source)) {
            point <- source$from_lsp_position(target$range$start)
            # Include the first character after the boundary shared with an
            # opening parenthesis, even for a one-character formal.
            point$col <- point$col + 1L
            result <- references_reply(NULL, source$uri, workspace, source, point, include_declaration,
                glue_site = FALSE)$result
            return(result)
        }
    }
    if (identical(target$kind, "field")) {
        uris <- member_navigation_uris(workspace, target, document$uri)
        loc <- location(target$uri, target$range)
        refs <- member_navigation_occurrences(workspace, loc, site$name, uris, declarations = include_declaration)
        if (include_declaration) refs <- c(list(loc), refs)
        return(glue_member_locations(workspace, loc, site$name, uris, refs, include_declaration))
    }
    glue_overlay_locations(workspace, document, site$name, site$key, list(), include_declaration)
}
