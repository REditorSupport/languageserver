# Argument expressions with data-masked evaluation. Keep package identity and
# the receiver separate from the names of columns available at runtime.
data_mask_base <- c("with", "within", "subset", "transform")
data_mask_dplyr <- c("filter", "mutate", "transmute", "summarise", "summarize", "reframe",
    "arrange", "group_by", "rowwise", "select", "rename", "relocate", "distinct", "slice",
    "count", "add_count")

data_mask_table_calls <- c("data.table", "as.data.table", "fread", "setDT")

data_mask_index <- function(parsed) {
    s <- parsed$syntax_data
    tok <- s$token
    functions <- which(tok == "SYMBOL_FUNCTION_CALL")
    names <- reference_symbol_names(s$text[functions])
    bracket <- which(tok == "'['")
    verbs <- functions[names %in% c(data_mask_base, data_mask_dplyr)]
    if (!length(verbs) && !length(bracket)) return(NULL)
    # Single-subscript indexing is lexical for ordinary objects. A bare i
    # is also evaluated in the caller by [.data.table, except special names.
    maybe_table <- any(names %in% data_mask_table_calls) || "data.table" %in% parsed$member_data$packages
    if (length(bracket) && !maybe_table) {
        bracket <- bracket[s$parent_row[bracket] %in% s$parent_row[tok == "','"]]
    }
    if (!length(verbs) && !length(bracket)) return(NULL)

    binding <- data_mask_bindings(parsed)
    owns <- data_mask_call_owner(parsed, binding)
    masks <- data_mask_call_roots(s, verbs, owns)
    if (length(bracket)) {
        table_kind <- data_mask_table_kind(s, functions, names, binding, owns)
        masks <- data_mask_bracket_roots(s, bracket, table_kind, masks)
    }
    kind <- masks$kind
    if (!any(kind != "")) return(NULL)
    context <- data_mask_context(s, kind)
    escapes <- data_mask_escapes(s, masks, context, binding)
    if (length(escapes$rows)) {
        # An escape inside a nested data mask may still see outer columns.
        parent <- s$parent_row[context[escapes$rows]]
        kind[escapes$rows] <- ifelse(c(0L, context)[parent + 1L] > 0L, "ambiguous", "lexical")
        masks$receiver[escapes$rows] <- 0L
        context <- data_mask_context(s, kind)
    }
    mask <- data_mask_occurrences(parsed, kind, masks$receiver, context, binding, escapes$keys)
    heads <- data_mask_call_heads(parsed, kind, masks$receiver, context, binding)
    data_mask_fallbacks(parsed, mask, heads, kind, context, binding, owns)
}

# Resolve only unconditional, preceding simple assignments. A formal or a
# conditional binding leaves the receiver unknown. Cache per-name histories
# so repeated brackets do not rescan all assignments in a large document.
data_mask_bindings <- function(parsed) {
    s <- parsed$syntax_data
    nav <- parsed$navigation_data
    groups <- new.env(hash = TRUE, parent = emptyenv())
    function(name, at) {
        history <- groups[[name]]
        if (is.null(history)) {
            ids <- get0(name, nav$by_name, inherits = FALSE, ifnotfound = integer())
            history <- split(ids, s$function_scope[nav$node[ids]])
            groups[[name]] <- history
        }
        scope <- s$function_scope[[at]]
        repeat {
            ids <- history[[as.character(scope)]]
            lo <- 1L
            hi <- length(ids)
            while (lo <= hi) {
                middle <- (lo + hi) %/% 2L
                node <- nav$node[[ids[[middle]]]]
                before <- s$line2[[node]] < s$line1[[at]] ||
                    (s$line2[[node]] == s$line1[[at]] && s$col2[[node]] < s$col1[[at]])
                if (before) lo <- middle + 1L else hi <- middle - 1L
            }
            if (hi) {
                id <- ids[[hi]]
                parent <- s$parent_row[[nav$node[[id]]]]
                while (parent && parent != scope) {
                    if (s$token[[s$first_child[[parent]]]] != "'{'") return(-1L)
                    parent <- s$parent_row[[parent]]
                }
                return(nav$value[[id]])
            }
            if (!scope) return(0L)
            if (length(nav$by_scope_name[[paste(scope, name, sep = "\r")]])) return(-1L)
            parent <- s$parent_row[[scope]]
            scope <- if (parent) s$function_scope[[parent]] else 0L
        }
    }
}

data_mask_call_owner <- function(parsed, binding) {
    s <- parsed$syntax_data
    imports <- parsed$member_data$imports
    # In a package with wildcard imports, a spelling alone cannot identify
    # the supplying namespace. Explicit importFrom declarations take priority.
    directives <- parsed$member_data$namespace_imports
    # The last new attachment has priority. Repeated library calls leave
    # search order alone, and tidyverse also attaches dplyr when necessary.
    latest <- character(length(imports))
    seen <- character()
    owner <- NA_character_
    for (i in seq_along(imports)) {
        item <- imports[[i]]
        package <- if (!isTRUE(item$uncertain)) member_name(item$expr[[2L]])
        if (!length(package)) {
            owner <- NA_character_
        } else if (!package %in% seen) {
            seen <- c(seen, package)
            owner <- package
            if (package == "tidyverse" && !"dplyr" %in% seen) {
                seen <- c(seen, "dplyr")
                owner <- "dplyr"
            }
        }
        latest[[i]] <- owner
    }
    events <- which(s$token == "SYMBOL_FUNCTION_CALL" & s$text %in% c("attach", "detach", "source", "sys.source"))
    uncertain_after <- if (length(events)) min(s$order_rank[events]) else Inf
    function(call, package, row, unknown = FALSE) {
        if (!is.null(call$package)) return(identical(call$package, package))
        value <- binding(call$name, row)
        if (value) {
            if (value > 0L && s$function_close[[value]]) return(FALSE)
            return(unknown)
        }
        wildcards <- character()
        for (item in directives) {
            if (item$type == "importFrom") {
                at <- match(call$name, item$symbols, nomatch = 0L)
                if (at) return(identical(item$package, package) && identical(item$targets[[at]], call$name))
            } else if (item$type == "import" && !call$name %in% item$except) {
                wildcards <- union(wildcards, item$packages)
            }
        }
        if (length(wildcards)) return(if (identical(wildcards, package)) TRUE else unknown)
        if (package == "base") return(TRUE)
        if (s$order_rank[[row]] > uncertain_after) return(unknown)
        point <- c(s$line1[[row]] - 1L, s$col1[[row]] - 1L)
        at <- member_history_position(imports, point)
        if (at && identical(latest[[at]], package)) return(TRUE)
        unknown
    }
}

# 0 is unknown, 1 is a data.table, 2 is a standard, lexically indexed
# object. Checking a receiver never executes the program or dispatches [.
# A by-reference conversion or class replacement may change bracket dispatch.
data_mask_table_kind <- function(s, functions, names, binding, owns) {
    tok <- s$token
    setters <- c("setDT", "setattr", "class<-", "attr<-", "attributes<-", "class", "attr", "attributes")
    changed <- vapply(functions[names %in% setters], function(i) {
        row <- s$parent_row[[s$parent_row[[i]]]]
        if (!row || (s$text[[i]] %in% c("class", "attr", "attributes") && !member_replacement_rhs(s, row))) return("")
        call <- syntax_call_arguments(s, row)
        if (!length(call$values)) return(NA_character_)
        arg <- class_literal_row(s, call$values[[1L]], "SYMBOL")
        if (arg) class_token_text(s$text[[arg]]) else NA_character_
    }, character(1L))
    class_cache <- rep(NA_integer_, length(tok))
    table_kind <- function(row, depth = 0L) {
        if (depth > 8L) return(0L)
        id <- s$first_child[[row]]
        if (id && !s$next_sibling[[id]] && tok[[id]] %in% c("NUM_CONST", "STR_CONST", "NULL_CONST")) return(2L)
        if (id && (tok[[id]] != "SYMBOL" || s$next_sibling[[id]])) id <- 0L
        if (id) {
            name <- s$text[[id]]
            if (startsWith(name, "`")) name <- class_token_text(name)
            if (length(changed) && (anyNA(changed) || name %in% changed)) return(0L)
            value <- binding(name, id)
            if (value <= 0L) return(0L)
            if (is.na(class_cache[[value]])) class_cache[[value]] <<- table_kind(value, depth + 1L)
            return(class_cache[[value]])
        }
        call <- syntax_call_arguments(s, row)
        if (is.null(call)) {
            parts <- syntax_children(s, row)
            if (length(parts) == 3L && s$text[[parts[[2L]]]] %in% c("+", "-", "*", "/", ":") &&
                    binding(s$text[[parts[[2L]]]], row) == 0L &&
                    table_kind(parts[[1L]], depth + 1L) == 2L && table_kind(parts[[3L]], depth + 1L) == 2L) return(2L)
            return(0L)
        }
        if (call$name %in% data_mask_table_calls && owns(call, "data.table", row)) return(1L)
        if (call$name %in% c("matrix", "as.matrix", "data.frame", "list", "array", "integer", "as.integer",
                "numeric", "character", "logical", "complex", "raw", "vector") && owns(call, "base", row)) return(2L)
        if (call$name %in% c("c", "cbind", "rbind", "unique") && owns(call, "base", row)) {
            # These generics may preserve or create a data.table. Only keep
            # the ordinary bracket semantics when the inputs establish them.
            args <- call$values
            if (call$name == "unique") args <- class_matched_arguments(call, c("x", "..."))$x
            if (length(args) && all(args > 0L) &&
                    all(vapply(args, function(arg) table_kind(arg, depth + 1L) == 2L, logical(1L)))) return(2L)
        }
        0L
    }
    table_kind
}

# Only argument expressions evaluated in a mask start a new lookup context.
data_mask_call_roots <- function(s, verbs, owns) {
    tok <- s$token
    kind <- character(length(tok))
    receiver <- integer(length(tok))
    selection <- logical(length(tok))
    pipes <- s$parent_row[tok == "PIPE" | (tok == "SPECIAL" & s$text %in% c("%>%", "%T>%", "%<>%"))]
    # Direct, unnamed calls have the same argument positions across all these
    # verbs. Handle them in batches; tagged and piped calls use the matcher.
    heads <- s$parent_row[verbs]
    calls <- s$parent_row[heads]
    simple <- which(calls > 0L)
    simple <- simple[s$child_end[calls[simple]] - s$child_start[calls[simple]] == 5L &
            !s$parent_row[calls[simple]] %in% pipes]
    if (length(simple)) {
        first <- s$child_start[calls[simple]]
        root <- s$first_child[heads[simple]]
        ok <- s$children[first] == heads[simple] & tok[root] == "SYMBOL_PACKAGE" & s$text[root] == "dplyr" &
            s$text[verbs[simple]] %in% data_mask_dplyr & tok[s$children[first + 1L]] == "'('" &
            tok[s$children[first + 2L]] == "expr" & tok[s$children[first + 3L]] == "','" &
            tok[s$children[first + 4L]] == "expr" & tok[s$children[first + 5L]] == "')'"
        arg <- s$children[first[ok] + 4L]
        kind[arg] <- "dplyr"
        receiver[arg] <- s$children[first[ok] + 2L]
        selection[arg] <- s$text[verbs[simple[ok]]] %in% c("select", "rename", "relocate", "rowwise")
        if (any(ok)) verbs <- verbs[-simple[ok]]
    }
    for (symbol in verbs) {
        row <- s$parent_row[[s$parent_row[[symbol]]]]
        if (!row) next
        call <- syntax_call_arguments(s, row)
        if (is.null(call)) next
        base <- call$name %in% data_mask_base
        verified <- owns(call, if (base) "base" else "dplyr", row, unknown = NA)
        if (identical(verified, FALSE)) next
        if (s$parent_row[[row]] %in% pipes) call <- syntax_pipe_arguments(s, row, call)
        if (!length(call$values)) next
        formals <- switch(call$name, with = , within = c("data", "expr", "..."),
            subset = c("x", "subset", "select", "drop", "..."),
            transform = c("_data", "..."), count = , add_count = c("x", "..."),
            rowwise = c("data", "..."), c(".data", "..."))
        keys <- names(call$values)
        first <- match(formals[[1L]], keys, nomatch = 0L)
        matched <- NULL
        if (call$name %in% c("with", "within", "subset") ||
                any(nzchar(keys) & startsWith(formals[[1L]], keys) & keys != formals[[1L]])) {
            matched <- class_matched_arguments(call, formals)
            data <- matched[[formals[[1L]]]]
        } else {
            if (!first) first <- match("", keys, nomatch = 0L)
            data <- call$values[first]
        }
        if (!length(data) || !data) next
        args <- if (call$name %in% c("with", "within", "subset")) {
            unlist(matched[intersect(c("expr", "subset", "select"), names(matched))], use.names = FALSE)
        } else {
            # Only the documented controls have special meaning: a new
            # column named e.g. .custom in mutate still has a masked RHS.
            controls <- switch(call$name, filter = , slice = ".preserve", distinct = ".keep_all",
                arrange = c(".by_group", ".locale"),
                mutate = , transmute = ".keep", summarise = , summarize = ".groups", group_by = c(".add", ".drop"),
                count = , add_count = c("sort", "name", ".drop"), character())
            call$values[call$values != data & !keys %in% controls]
        }
        args <- unname(args[args > 0L])
        if (!length(args)) next
        kind[args] <- if (is.na(verified)) "unknown_call" else if (base) "base" else "dplyr"
        if (isTRUE(verified)) receiver[args] <- data
        selection[args] <- call$name %in% c("select", "rename", "relocate", "rowwise") |
            args %in% call$values[keys %in% c(".by", ".before", ".after")]
        if (call$name %in% c("mutate", "transmute", "summarise", "summarize", "reframe", "group_by", "within")) {
            # Later expressions can see newly created columns.
            receiver[if (call$name == "within") args else args[-1L]] <- 0L
        }
    }
    list(kind = kind, receiver = receiver, selection = selection)
}

# [.data.table evaluates .SDcols in its caller, except for a top-level
# first:last expression with non-call endpoints. Parentheses and one
# leading ! or - are stripped before it tests for this range.
data_mask_sdcols_range <- function(s, row) {
    negated <- FALSE
    repeat {
        if (s$child_start[[row]] == s$child_end[[row]]) return(FALSE)
        parts <- syntax_children(s, row)
        parts <- parts[s$token[parts] != "COMMENT"]
        tokens <- s$token[parts]
        if (length(parts) < 2L) return(FALSE)
        if (length(parts) == 3L && tokens[[1L]] == "'('") {
            operator <- "("
            args <- parts[[2L]]
        } else if (length(parts) == 2L && tokens[[1L]] %in% c("'!'", "'-'")) {
            operator <- s$text[[parts[[1L]]]]
            args <- parts[[2L]]
        } else if (length(parts) == 3L && tokens[[2L]] == "':'") {
            operator <- ":"
            args <- parts[c(1L, 3L)]
        } else {
            # Handle prefix spellings such as `:`(a, b). Most other calls
            # are selectors such as patterns(), whose arguments stay lexical.
            if (tokens[[2L]] != "'('") return(FALSE)
            head <- syntax_children(s, parts[[1L]])
            head <- head[s$token[head] != "COMMENT"]
            if (length(head) != 1L && !(length(head) == 3L && s$token[[head[[2L]]]] %in% c("NS_GET", "NS_GET_INT"))) return(FALSE)
            operator <- class_token_text(s$text[[head[[length(head)]]]])
            if (!operator %in% c(":", "(", "!", "-")) return(FALSE)
            args <- parts[tokens == "expr"][-1L]
            if (length(args) != sum(tokens == "','") + 1L) return(FALSE)
        }
        if (length(args) == 1L && args[[1L]] > 0L &&
                (operator == "(" || (!negated && operator %in% c("!", "-")))) {
            negated <- negated || operator != "("
            row <- args[[1L]]
            next
        }
        if (operator != ":" || length(args) != 2L || any(args == 0L)) return(FALSE)
        return(all(vapply(args, function(arg) {
            leaf <- syntax_children(s, arg)
            leaf <- leaf[s$token[leaf] != "COMMENT"]
            length(leaf) == 1L && s$terminal[[leaf]]
        }, logical(1L))))
    }
}

data_mask_bracket_roots <- function(s, bracket, table_kind, masks) {
    tok <- s$token
    kind <- masks$kind
    receiver <- masks$receiver
    for (op in bracket) {
        node <- s$parent_row[[op]]
        data <- s$first_child[[node]]
        after <- s$next_sibling[[op]]
        if (after && tok[[after]] == "','" && data && tok[[data]] == "expr") {
            value <- s$next_sibling[[after]]
            if (value && tok[[value]] == "expr" && tok[[s$next_sibling[[value]]]] == "']'") {
                mode <- table_kind(data)
                if (mode != 2L) {
                    kind[[value]] <- if (mode == 1L) "bracket" else "unknown_bracket"
                    if (mode == 1L) receiver[[value]] <- data
                }
                next
            }
        }
        parts <- syntax_children(s, node)
        parts <- parts[tok[parts] != "COMMENT"]
        args <- parts[-seq_len(2L)]
        values <- args[tok[args] == "expr"]
        if (!length(values)) next
        data <- parts[[1L]]
        modes <- table_kind(data)
        if (modes == 2L) next
        groups <- cumsum(tok[args] == "','") + 1L
        keys <- rep("", max(groups))
        tags <- which(tok[args] == "EQ_SUB")
        keys[groups[tags]] <- class_token_text(s$text[args[tags - 1L]])
        if (modes == 1L && length(tags)) {
            keys[startsWith(keys, ".") & startsWith(".SDcols", keys)] <- ".SDcols"
        }
        positional <- which(!nzchar(keys))
        formals <- c("i", "j", "by", "keyby")
        if (modes == 1L) {
            formals <- c(formals, "with", "nomatch", "mult", "roll", "rollends", "which", ".SDcols")
        }
        remaining <- setdiff(formals, keys)
        n <- min(length(positional), length(remaining))
        keys[positional[seq_len(n)]] <- remaining[seq_len(n)]
        names(values) <- keys[groups[tok[args] == "expr"]]
        cols <- values[names(values) %in% c("i", "j", "by", "keyby", "on", ".SDcols")]
        sdcols <- values[".SDcols"]
        if (modes == 1L && !is.na(sdcols) && !data_mask_sdcols_range(s, sdcols)) {
            cols <- cols[names(cols) != ".SDcols"]
        }
        i <- values["i"]
        if (!is.na(i)) {
            sym <- class_literal_row(s, i, "SYMBOL")
            if (sym && !s$text[[sym]] %in% c(".N", ".I", ".GRP")) cols <- cols[names(cols) != "i"]
        }
        with <- values["with"]
        if (!is.na(with) && identical(s$text[class_literal_row(s, with, "NUM_CONST")], "FALSE")) {
            cols <- cols[names(cols) != "j"]
        }
        kind[cols] <- if (modes == 1L) "bracket" else "unknown_bracket"
        if (modes == 1L && is.na(values["on"]) && is.na(values["env"]) && is.na(with)) {
            if (any(names(cols) %in% c("by", "keyby"))) cols <- cols[names(cols) != "j"]
            receiver[cols] <- data
        }
    }
    list(kind = kind, receiver = receiver, selection = masks$selection)
}

# Each marked argument is its own ancestor. Doubling the jump on each pass
# finds the nearest such ancestor for every row in logarithmically many passes.
data_mask_context <- function(s, kind) {
    links <- c(0L, s$parent_row)
    fixed <- which(kind != "")
    links[fixed + 1L] <- fixed
    repeat {
        parents <- links[links + 1L]
        if (identical(parents, links)) return(links[-1L])
        links <- parents
    }
}

# Unquoting, .data[[name]] and tidyselect's external-vector helpers look up
# their arguments in the caller. Keep literal .env keys for Rename safety.
data_mask_escapes <- function(s, masks, context, binding) {
    tok <- s$token
    active <- which(context > 0L)
    tidy <- active[masks$kind[context[active]] == "dplyr"]
    possible <- active[masks$kind[context[active]] == "unknown_call" & tok[active] == "LBB"]
    if (!length(tidy) && !length(possible)) return(list(rows = integer(), keys = integer()))
    escaped <- logical(length(tok))
    literal_keys <- integer(length(tok))
    escape <- tidy[tok[tidy] %in% c("'!'", "'{'", "LBB")]
    selectors <- tidy[tok[tidy] == "SYMBOL_FUNCTION_CALL" & masks$selection[context[tidy]] &
            s$text[tidy] %in% c("all_of", "any_of")]
    for (row in c(escape, selectors, possible)) {
        parent <- s$parent_row[[row]]
        if (tok[[row]] == "LBB") {
            id <- class_literal_row(s, s$first_child[[parent]], "SYMBOL")
            if (id && s$text[[id]] %in% c(".data", ".env")) {
                parts <- syntax_children(s, parent)
                arg <- setdiff(parts[tok[parts] == "expr"], s$first_child[[parent]])
                if (s$text[[id]] == ".data") {
                    if (masks$kind[[context[[row]]]] == "dplyr") escaped[arg] <- TRUE
                } else if (length(arg) == 1L) {
                    key <- class_literal_row(s, arg, "STR_CONST")
                    literal_keys[[row]] <- key
                }
            }
        } else if (tok[[row]] == "SYMBOL_FUNCTION_CALL") {
            call_row <- s$parent_row[[parent]]
            call <- syntax_call_arguments(s, call_row)
            if (length(call) && ((is.null(call$package) && !binding(call$name, row)) ||
                        isTRUE(call$package %in% c("tidyselect", "dplyr")))) {
                escaped[call$values] <- TRUE
            }
        } else {
            outer <- s$parent_row[[parent]]
            if (outer && s$token[[s$first_child[[outer]]]] == tok[[row]]) {
                if (tok[[row]] == "'!'" || length(syntax_children(s, outer)) == 3L) escaped[[parent]] <- TRUE
            }
        }
    }
    list(rows = which(escaped), keys = literal_keys[literal_keys > 0L])
}

# Look up members only on a verified receiver. A bare obj inside a mask
# can itself be a column, so obj$field and obj[["field"]] cannot be
# attributed to the same-named lexical object. Follow the receiver through
# nested access and calls, preserving the lexical route via .env$obj.
data_mask_member_rows <- function(s, kind, context, symbols) {
    tok <- s$token
    active <- which(context > 0L)
    column <- logical(length(tok))
    column[symbols] <- TRUE
    ordinary <- active[tok[active] %in% c("'$'", "'@'", "LBB") & kind[context[active]] != "lexical"]
    for (op in ordinary) {
        parent <- s$parent_row[[op]]
        left <- s$first_child[[parent]]
        root <- left
        while (!s$terminal[[root]]) {
            root <- s$first_child[[root]]
            if (tok[[root]] == "'('" && s$next_sibling[[root]]) root <- s$next_sibling[[root]]
        }
        if (!column[[root]] || (kind[[context[[root]]]] == "dplyr" && s$text[[root]] == ".env")) next
        key <- s$next_sibling[[op]]
        if (tok[[op]] == "LBB") key <- if (key) class_literal_row(s, key, "STR_CONST") else 0L
        if (key && tok[[key]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "STR_CONST", "SLOT")) column[[key]] <- TRUE
    }
    which(column)
}

data_mask_occurrences <- function(parsed, kind, receiver, context, binding, indirect_rows) {
    s <- parsed$syntax_data
    tok <- s$token
    dollars <- which(tok == "'$'")
    prop <- s$next_sibling[dollars]
    literals <- which(prop > 0L & tok[pmax(1L, prop)] == "STR_CONST" & context[dollars] > 0L)
    if (length(literals)) {
        op <- dollars[literals[kind[context[dollars[literals]]] %in% c("dplyr", "unknown_call")]]
        prefix <- s$first_child[s$first_child[s$parent_row[op]]]
        key <- s$next_sibling[op[s$text[prefix] == ".env"]]
        indirect_rows <- c(indirect_rows, key)
    }
    reads <- tok == "SYMBOL"
    method <- prop[prop > 0L & tok[pmax(1L, prop)] == "SYMBOL_FUNCTION_CALL"]
    reads[method] <- TRUE
    symbols <- which(reads & context > 0L)
    symbols <- symbols[kind[context[symbols]] != "lexical"]
    owner <- context[symbols]
    sources <- receiver[owner]
    tidy <- kind[owner] == "dplyr"
    # Only the direct property of a verified dplyr pronoun has a known
    # lookup rule here. Other receiver chains are considered below.
    member <- match(s$parent_row[symbols], s$parent_row[dollars], nomatch = 0L)
    properties <- which(member > 0L)
    env <- integer()
    keep <- rep(TRUE, length(symbols))
    if (length(properties)) {
        left <- s$first_child[s$parent_row[symbols[properties]]]
        names <- s$text[s$first_child[left]]
        pronoun <- tidy[properties] & names %in% c(".env", ".data")
        keep[properties] <- pronoun
        possible <- kind[owner[properties]] == "unknown_call" & names == ".env"
        indirect_rows <- c(indirect_rows, symbols[properties[possible]])
        lookups <- properties[pronoun & names == ".env"]
        outer <- c(0L, context)[s$parent_row[owner[lookups]] + 1L]
        lexical <- lookups[outer == 0L & s$function_scope[symbols[lookups]] == s$function_scope[owner[lookups]]]
        # An assignment inside the predicate can shadow either the pronoun
        # or the caller's name in the lexical declaration index.
        targets <- parsed$navigation_data$name
        written <- targets[context[targets] %in% unique(owner[lexical])]
        if (length(written)) {
            names <- reference_symbol_names(s$text[written])
            changed <- paste(context[written], names)
            conflict <- owner[lexical] %in% context[written[names == ".env"]] |
                paste(owner[lexical], reference_symbol_names(s$text[symbols[lexical]])) %in% changed
            lexical <- lexical[!conflict]
        }
        indirect_rows <- c(indirect_rows, symbols[setdiff(lookups, lexical)])
        env <- symbols[lexical]
        keep[lexical] <- FALSE
        sources[lookups] <- 0L
    }
    # A function created inside a mask still captures it. Its own local
    # bindings take precedence; other captured names have no proven column.
    nested <- which(s$function_scope[symbols] != s$function_scope[owner])
    for (i in nested) {
        if (binding(class_token_text(s$text[[symbols[[i]]]]), symbols[[i]]) != 0L) {
            scope <- s$function_scope[[symbols[[i]]]]
            name <- class_token_text(s$text[[symbols[[i]]]])
            locals <- parsed$navigation_data$by_name[[name]]
            if (length(parsed$navigation_data$by_scope_name[[paste(scope, name, sep = "\r")]]) ||
                    any(s$function_scope[parsed$navigation_data$node[locals]] == scope)) keep[[i]] <- FALSE
        }
    }
    sources[nested] <- 0L
    sources[tidy & s$text[symbols] %in% c(".env", ".data")] <- 0L
    # An assignment target may introduce a fresh column.
    assignments <- which(tok == "LEFT_ASSIGN" & s$text == ":=")
    if (length(assignments)) {
        lhs <- s$first_child[s$parent_row[assignments]]
        sources[s$parent_row[symbols] %in% lhs] <- 0L
    }
    glue <- unique(unlist(lapply(parsed$glue_templates, "[[", "call_row"), use.names = FALSE))
    glue <- glue[glue > 0L]
    glue <- glue[context[glue] > 0L]
    rows <- data_mask_member_rows(s, kind, context, symbols[keep])
    target <- integer(length(rows))
    target[match(symbols[keep], rows)] <- sources[keep]
    lookup <- keep & kind[owner] %in% c("unknown_bracket", "unknown_call") & tok[symbols] == "SYMBOL"
    if (any(lookup)) {
        # A possible column in an unverified mask may also be an actual
        # lexical read. Keep the occurrence so Rename can decline partial edits.
        qualified <- s$parent_row[tok %in% c("NS_GET", "NS_GET_INT")]
        indirect_rows <- c(indirect_rows, symbols[lookup & !s$parent_row[symbols] %in% qualified])
    }
    bracket <- symbols[keep & kind[owner] %in% c("bracket", "unknown_bracket")]
    names <- class_token_text(s$text[bracket])
    dots <- which(startsWith(names, "..") & nchar(names) > 2L)
    indirect <- c(class_token_text(s$text[indirect_rows]), substring(names[dots], 3L))
    list(column = rows, receiver = target, env = env, bracket = bracket,
        indirect = indirect, indirect_row = c(indirect_rows, bracket[dots]),
        glue = glue[kind[context[glue]] != "lexical"])
}

# R skips non-functions during function lookup. An ordinary bare call can
# therefore use either a function supplied by the mask or one in the caller.
# Qualified calls and member calls already have an explicit receiver.
data_mask_call_heads <- function(parsed, kind, receiver, context, binding) {
    s <- parsed$syntax_data
    rows <- which(s$token == "SYMBOL_FUNCTION_CALL" & context > 0L)
    parent <- s$parent_row[rows]
    rows <- rows[s$first_child[parent] == rows & s$next_sibling[rows] == 0L &
            kind[context[rows]] != "lexical"]
    owner <- context[rows]
    data <- receiver[owner]
    if (!length(rows)) return(list(rows = rows, receiver = data))

    # A fallback from an inner mask can still encounter an outer mask.
    outer <- c(0L, context)[s$parent_row[owner] + 1L]
    data[outer > 0L] <- 0L
    # Assignments in the evaluated expression can introduce further bindings.
    writes <- s$token %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN") |
        (s$token == "SYMBOL_FUNCTION_CALL" & s$text %in% c("assign", "rm", "remove", "eval", "evalq", "source", "sys.source"))
    data[owner %in% context[writes]] <- 0L

    # A closure created in the mask can capture its functions. A verified
    # function declared in an intervening lexical scope takes precedence.
    nested <- which(s$function_scope[rows] != s$function_scope[owner])
    keep <- rep(TRUE, length(rows))
    for (i in nested) {
        row <- rows[[i]]
        value <- binding(reference_symbol_names(s$text[[row]]), row)
        if (value > 0L && s$function_close[[value]] && context[[value]] == owner[[i]] &&
                s$function_scope[[value]] != s$function_scope[[owner[[i]]]]) keep[[i]] <- FALSE
    }
    local <- which(!keep)
    if (length(local)) {
        keep[local] <- data_mask_ambiguous_callees(parsed, rows[local], reference_symbol_names(s$text[rows[local]]), binding)
    }
    data[nested] <- 0L
    list(rows = rows[keep], receiver = data[keep])
}

data_mask_receiver <- function(mask, row) {
    lo <- 1L
    hi <- length(mask$column)
    while (lo <= hi) {
        at <- (lo + hi) %/% 2L
        key <- mask$column[[at]]
        if (key == row) return(mask$receiver[[at]])
        if (key < row) lo <- at + 1L else hi <- at - 1L
    }
    NULL
}
