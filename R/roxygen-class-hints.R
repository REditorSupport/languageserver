# Function literals have srcrefs of their own. Factory calls do not, so keep
# their path from a sourced statement while the parser's syntax tree is live.
# Only declarations with documentation need these paths.
roxygen_class_route <- function(s, row, row_offset, col_offset) {
    path <- integer()
    anchors <- list()
    repeat {
        parent <- s$parent_row[[row]]
        children <- if (parent) syntax_children(s, parent)
        children <- children[s$token[children] != "COMMENT"]
        kinds <- s$token[children]
        key <- paste(s$line1[[row]] + row_offset, s$col1[[row]] + if (s$line1[[row]] == 1L) col_offset else 0L, sep = ":")
        # Recovery can parse a whole file, a brace statement or the factory
        # call itself. Each starts at one of the same physical coordinates.
        anchors[[key]] <- path
        if (!parent || length(path) >= 128L) break
        i <- match(row, children)
        expressions <- which(kinds %in% c("expr", "expr_or_assign_or_help"))
        nth <- match(i, expressions)
        if (is.na(nth) || "PIPE" %in% kinds || "forcond" %in% kinds) break
        if (kinds[[1L]] %in% c("FUNCTION", r"('\\')")) {
            if (nth == length(expressions)) {
                slot <- 3L
            } else {
                slot <- c(2L, sum(kinds[seq_len(i)] == "SYMBOL_FORMALS"))
            }
        } else if (length(kinds) >= 2L && kinds[[1L]] == "expr" && kinds[[2L]] %in% c("'('", "'['", "LBB")) {
            head <- if (kinds[[2L]] == "'('") 1L else 2L
            slot <- if (nth == 1L) head else head + 1L + sum(kinds[seq_len(i)] == "','")
        } else {
            slot <- if ("RIGHT_ASSIGN" %in% kinds) 4L - nth else nth + 1L
        }
        path <- c(slot, path)
        row <- parent
    }
    anchors
}

roxygen_class_factories <- c("R6Class", "setClass", "new_class", "new_generic")

roxygen_helper_shadowed <- function(s, name, token, memo = NULL) {
    shadows <- memo$helper_shadows
    if (is.null(shadows)) {
        writes <- .Call("symbol_assignment_rows_c", s, s, PACKAGE = "languageserver")$name
        rows <- c(writes, which(s$token == "SYMBOL_FORMALS"))
        labels <- reference_symbol_names(s$text[rows])
        keep <- labels %in% c(roxygen_class_factories, names(roxygen_registration_argument))
        shadows <- split(rows[keep], labels[keep])
        if (!is.null(memo)) memo$helper_shadows <- shadows
    }
    visible <- shadows[[name]]
    visible <- visible[s$order_rank[visible] < s$order_rank[[token]] | s$token[visible] == "SYMBOL_FORMALS"]
    if (!length(visible)) return(FALSE)
    if (!length(s$function_scope)) return(TRUE)
    scopes <- 0L
    scope <- s$function_scope[[token]]
    while (scope) {
        scopes <- c(scopes, scope)
        scope <- s$function_scope[[scope]]
    }
    any(s$function_scope[visible] %in% scopes)
}

roxygen_class_factory <- function(s, row, memo = NULL) {
    head <- syntax_children(s, row)
    head <- head[s$token[head] != "COMMENT"]
    if (length(head) < 3L || s$token[[head[[2L]]]] != "'('") return(NULL)
    target <- syntax_children(s, head[[1L]])
    name <- if (length(target) && s$token[[utils::tail(target, 1L)]] == "SYMBOL_FUNCTION_CALL") {
        class_token_text(s$text[[utils::tail(target, 1L)]])
    }
    if (!length(name) || !name %in% roxygen_class_factories) return(NULL)
    if (length(target) == 1L) {
        if (roxygen_helper_shadowed(s, name, target, memo)) return(NULL)
    } else if (length(target) != 3L || s$token[[target[[2L]]]] != "NS_GET" ||
            !identical(class_token_text(s$text[[target[[1L]]]]), class_helper_packages[[name]])) {
        return(NULL)
    }
    name
}

roxygen_class_target <- function(s, first, statement, inline, lists) {
    if (s$token[[first]] %in% c("SYMBOL_SUB", "STR_CONST")) {
        if (!inline) return(NULL)
        group <- s$parent_row[[first]]
        if (!group) return(NULL)
        key <- as.character(group)
        if (!exists(key, lists, inherits = FALSE)) {
            factory <- s$parent_row[[group]]
            found <- FALSE
            name <- if (factory) roxygen_class_factory(s, factory, lists)
            if (!is.null(name) && name != "new_generic") {
                args <- syntax_call_arguments(s, factory)$values
                section <- switch(name, R6Class = c("public", "private", "active"),
                    setClass = c("slots", "representation"), new_class = "properties")
                parts <- syntax_children(s, group)
                head <- if (length(parts)) syntax_children(s, parts[[1L]])
                accepted <- if (name == "setClass") c("c", "list", "representation") else "list"
                if (name == "setClass" && length(head) == 3L && s$token[[head[[2L]]]] == "NS_GET") {
                    owner <- s$text[[head[[1L]]]]
                    helper <- s$text[[head[[3L]]]]
                    if ((owner == "base" && helper %in% c("c", "list")) ||
                            (owner == "methods" && helper == "representation")) head <- head[[3L]]
                }
                if (length(head) == 1L && s$text[[head]] %in% accepted && group %in% args[names(args) %in% section]) {
                    found <- list(row = factory, name = name)
                }
            }
            assign(key, found, lists)
        }
        target <- lists[[key]]
        if (!isFALSE(target)) {
            target$field <- class_token_text(s$text[[first]])
            return(target)
        }
        return(NULL)
    }
    row <- statement
    repeat {
        children <- syntax_children(s, row)
        children <- children[s$token[children] != "COMMENT"]
        if (length(children) != 3L) break
        kinds <- s$token[children]
        if (kinds[[2L]] %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")) {
            value <- if (kinds[[2L]] == "RIGHT_ASSIGN") 1L else 3L
            row <- children[[value]]
        } else if (identical(kinds[c(1L, 3L)], c("'('", "')'"))) {
            row <- children[[2L]]
        } else {
            break
        }
    }
    name <- roxygen_class_factory(s, row, lists)
    if (!is.null(name)) list(row = row, name = name)
}

roxygen_class_name <- function(s, target) {
    call <- syntax_call_arguments(s, target$row)
    name <- call$values["name"]
    if (is.na(name) && length(call$values) && !nzchar(names(call$values)[[1L]])) name <- call$values[[1L]]
    literal <- if (!is.na(name)) class_literal_row(s, name, "STR_CONST")
    if (isTRUE(literal > 0L)) return(class_token_text(s$text[[literal]]))
    parent <- s$parent_row[[target$row]]
    if (parent && !("name" %in% names(call$values))) {
        parts <- syntax_children(s, parent)
        parts <- parts[s$token[parts] != "COMMENT"]
        if (length(parts) == 3L && s$text[[parts[[2L]]]] == ":=" && parts[[3L]] == target$row) {
            symbol <- class_literal_row(s, parts[[1L]], "SYMBOL")
            if (symbol) class_token_text(s$text[[symbol]])
        }
    }
}

roxygen_class_block <- function(s, target, comments, tags, typed, rows, cols, record) {
    factory <- target$name
    directives <- if (!is.null(target$field)) {
        switch(factory, R6Class = "field", setClass = "slot", new_class = "prop")
    } else {
        switch(factory, R6Class = c("field", "param"), setClass = "slot", new_class = c("param", "prop"),
            new_generic = c("param", "return", "returns"))
    }
    cls <- if ("prop" %in% directives && any(grepl("@prop", comments, fixed = TRUE))) {
        roxygen_class_name(s, target)
    }
    values <- if (typed) roxygen_type_block(comments, rows = rows, columns = cols,
        directives = directives, field = target$field, class_name = cls)
    documented <- if (factory == "R6Class" && is.null(target$field)) tags$documented
    fields <- if ("field" %in% directives && any(grepl("@field", comments, fixed = TRUE))) {
        pattern <- paste0("^#'\\s*@field\\s+(", roxygen_param_spelling, ")(?:\\s|$)")
        matches <- regmatches(comments, regexec(pattern, comments, perl = TRUE))
        field_names <- reference_symbol_names(vapply(matches[lengths(matches) > 0L], `[[`, "", 2L))
        if (!is.null(target$field)) intersect(field_names, target$field) else field_names
    }
    roxygen_class_record(factory, record, values, documented, fields)
}

# A field can be documented both on its class and beside its declaration.
# A matching explicit annotation already vouches for the linked spelling.
roxygen_merge_member_types <- function(previous, incoming) {
    if (is.null(previous) || identical(previous, incoming)) return(incoming)
    if (!identical(as.vector(previous), as.vector(incoming))) return(character())
    links <- intersect(attr(previous, "roxygen_class_links", exact = TRUE), attr(incoming, "roxygen_class_links", exact = TRUE))
    attr(incoming, "roxygen_class_links") <- if (length(links)) links
    incoming
}

roxygen_class_record <- function(factory, record, values, documented = NULL, fields = NULL) {
    if (!length(values) && !length(documented) && !length(fields)) return(NULL)
    if (is.null(record)) record <- list(factory = factory, hints = list())
    for (tag in names(values)[lengths(values) > 0L]) {
        if (tag == "returns") {
            record$hints$returns <- values$returns
        } else {
            incoming <- values[[tag]]
            current <- record$hints[[tag]]
            for (name in intersect(names(current), names(incoming))) {
                incoming[[name]] <- roxygen_merge_member_types(current[[name]], incoming[[name]])
            }
            current[names(incoming)] <- incoming
            record$hints[[tag]] <- current
        }
    }
    if (length(documented)) attr(record$hints, "roxygen_documented") <- documented
    if (length(fields)) {
        attr(record$hints, "roxygen_documented_fields") <- union(attr(record$hints, "roxygen_documented_fields", exact = TRUE), fields)
    }
    list(record = record, sites = attr(values, "occurrences", exact = TRUE))
}

# A sequence of simple @field blocks shares its class declaration and section.
# Decode those names/locations together; complex spellings still use the full
# block parser. Batches never cross another documented member or declaration.
roxygen_field_batches <- function(s, groups, terminals, text, content, source, row_offset, col_offset, lists, typed_lines) {
    keys <- names(groups)
    first <- terminals[as.integer(keys)]
    candidates <- which(lengths(groups) == 1L & s$token[first] == "SYMBOL_SUB")
    if (length(candidates) < 8L) return(NULL)
    comments <- unlist(groups[candidates], use.names = FALSE)
    line <- s$line1[terminals[comments]]
    indent <- as.integer(regexpr("#'", content[line], fixed = TRUE)) - 1L
    tagged <- if (is.null(source)) text[comments] else substring(source[line], indent + 1L)
    id <- roxygen_type_identifier
    type <- paste0("(?<type>(?:", id, "::)?(?<class>", id, "))")
    # Branch-reset captures expose the same type and class columns for all
    # single-atom spellings. Unions and prose use the full block grammar.
    marker <- paste0("(?|\\(`", type, "`\\)|\\\\?\\[`", type, "`\\]|\\[\\\\code\\{", type, "\\}\\])")
    pattern <- paste0("(*UCP)^#'\\s*@field\\s+(?<field>", id, ")(?=\\s|$)",
        "(?:\\s+", marker, "(?=\\h*$|\\.\\h*$|\\h+(?!(?i:or|and)\\b)[[:alpha:]]))?")
    match <- regexpr(pattern, tagged, perl = TRUE)
    start <- attr(match, "capture.start")
    size <- attr(match, "capture.length")
    field <- substring(tagged, start[, "field"], start[, "field"] + size[, "field"] - 1L)
    typed <- size[, "type"] > 0L
    use <- which(match > 0L & field == s$text[first[candidates]] & (typed | !typed_lines[line]))
    if (length(use) < 8L) return(NULL)
    section <- s$parent_row[first[candidates[use]]]
    runs <- split(use, cumsum(c(TRUE, diff(candidates[use]) != 1L | diff(section) != 0L)))
    records <- list()
    skip <- character()
    for (run in runs[lengths(runs) >= 8L]) {
        entries <- candidates[run]
        row <- first[[entries[[1L]]]]
        target <- roxygen_class_target(s, row, row, TRUE, lists)
        if (!identical(target$name, "R6Class") || anyDuplicated(field[run])) next
        typed_rows <- run[typed[run]]
        values <- NULL
        if (length(typed_rows)) {
            i <- typed_rows
            types <- substring(tagged[i], start[i, "type"], start[i, "type"] + size[i, "type"] - 1L)
            values <- as.list(types)
            # Unescaped Markdown links require class evidence, unlike the
            # explicit escaped bracket and Rd forms.
            at <- start[i, "type"]
            opening <- substring(tagged[i], at - 2L, at - 1L)
            weak <- (opening == "[`" & substring(tagged[i], at - 3L, at - 3L) != "\\") |
                (opening != "(`" & types %in% roxygen_bracket_labels)
            for (pos in which(weak)) attr(values[[pos]], "roxygen_class_links") <- types[[pos]]
            values <- list(field = stats::setNames(values, field[i]))
            offset <- indent[i] + (line[i] == 1L) * col_offset - 1L
            pos <- start[i, "class"] + offset
            source_row <- line[i] + row_offset - 1L
            attr(values, "occurrences") <- data.frame(type = types, row = source_row, start = pos, end = pos + size[i, "class"],
                directive = "field", parameter = field[i], parameter_row = source_row, parameter_start = start[i, "field"] + offset,
                stringsAsFactors = FALSE)
        }
        records[[keys[[entries[[1L]]]]]] <- list(values = values, fields = field[run])
        skip <- c(skip, keys[entries[-1L]])
    }
    list(records = records, skip = skip)
}

member_attach_class_hints <- function(parsed, classes, row_offset, col_offset) {
    refs <- attr(parsed, "srcref", exact = TRUE)
    if (!length(refs) || length(refs) != length(parsed)) return(parsed)
    lines <- vapply(refs, `[[`, integer(1L), 7L)
    cols <- vapply(refs, `[[`, integer(1L), 5L)
    keys <- paste(lines + row_offset, cols + (lines == 1L) * col_offset, sep = ":")
    anchors <- lapply(classes, function(record) names(record$anchors))
    roots <- utils::relist(match(unlist(anchors, use.names = FALSE), keys, nomatch = 0L), anchors)
    used <- vector("list", length(classes))
    for (j in seq_along(classes)) {
        record <- classes[[j]]
        root <- roots[[j]]
        anchor <- match(TRUE, root > 0L)
        if (is.na(anchor)) next
        statement <- root[[anchor]]
        path <- record$anchors[[anchor]]
        nodes <- vector("list", length(path))
        child <- parsed[[statement]]
        for (i in seq_along(path)) {
            if ((!is.call(child) && !is.expression(child) && !is.pairlist(child)) || path[[i]] > length(child)) break
            nodes[[i]] <- child
            child <- child[[path[[i]]]]
        }
        if ((length(path) && is.null(nodes[[length(path)]])) || !is.call(child)) next
        head <- child[[1L]]
        if (member_head(head, "::")) head <- head[[3L]]
        if (!identical(head, as.name(record$factory))) next
        attr(child, "member_class_hints") <- record$hints
        dependencies <- member_type_dependencies(record$hints)
        used[[j]] <- dependencies
        attr(child, "member_type_names") <- union(attr(child, "member_type_names", exact = TRUE), dependencies)
        for (i in rev(seq_along(path))) {
            parent <- nodes[[i]]
            parent[[path[[i]]]] <- child
            attr(parent, "member_type_names") <- union(attr(parent, "member_type_names", exact = TRUE), dependencies)
            child <- parent
        }
        parsed[[statement]] <- child
    }
    attr(parsed, "member_type_names") <- union(attr(parsed, "member_type_names", exact = TRUE), unlist(used, use.names = FALSE))
    parsed
}
