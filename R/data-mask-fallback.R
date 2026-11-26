# Bare names may fall through a mask to the caller. Record uncertain reads
# by spelling once in the worker, so Rename can check just its own binding.
data_mask_fallbacks <- function(parsed, mask, heads, kind, context, binding, owns) {
    s <- parsed$syntax_data
    rows <- mask$column
    owners <- kind[context[rows]]
    names <- reference_symbol_names(s$text[rows])
    parent <- s$parent_row[rows]
    bare <- s$token[rows] == "SYMBOL" & s$child_start[parent] == s$child_end[parent] &
        owners %in% c("base", "dplyr", "bracket") & !rows %in% parsed$navigation_data$name &
        !(owners == "dplyr" & names %in% c(".env", ".data")) &
        !(owners == "bracket" & (startsWith(names, "..") | names %in% c(".SD", ".N", ".I", ".GRP", ".BY", ".EACHI")))
    # In a brace expression, a direct local assignment binds its name for
    # the following statement. No lookup can fall through on a bare read
    # at that position (e.g. with(df, {v <- 4; v})).
    nav <- parsed$navigation_data
    written <- which(context[nav$name] > 0L)
    if (length(written)) {
        statement <- nav$node[written]
        block <- s$parent_row[statement]
        direct <- block > 0L & s$token[pmax(1L, s$first_child[pmax(1L, block)])] == "'{'"
        written <- written[direct]
        statement <- statement[direct]
        next_expr <- s$next_sibling[statement]
        punctuation <- next_expr > 0L & s$token[pmax(1L, next_expr)] == "';'"
        next_expr[punctuation] <- s$next_sibling[next_expr[punctuation]]
        leaf <- c(0L, s$first_child)[next_expr + 1L]
        op <- s$next_sibling[s$first_child[statement]]
        local <- op > 0L & s$text[pmax(1L, op)] %in% c("<-", "=") & leaf > 0L &
            s$next_sibling[pmax(1L, leaf)] == 0L & s$text[pmax(1L, leaf)] == s$text[nav$name[written]]
        bare <- bare & !rows %in% leaf[local]
    }
    if (!any(bare) && !length(heads$rows)) return(mask)
    rows <- c(rows[bare], heads$rows)
    names <- c(names[bare], reference_symbol_names(s$text[heads$rows]))
    receiver <- c(mask$receiver[bare], heads$receiver)
    call_head <- seq_along(rows) > sum(bare)
    sites <- unique(receiver[receiver > 0L])
    confirmed <- caller <- logical(length(rows))
    if (length(sites)) {
        constructor <- data_mask_constructors(parsed, sites, mask, binding)
        origin <- constructor[match(receiver, sites)]
        known <- which(!is.na(origin) & origin > 0L)
        for (at in split(known, origin[known])) {
            source <- origin[[at[[1L]]]]
            call <- syntax_call_arguments(s, source)
            table <- data_column_constructor(s, source, owns, call)
            if (!is.null(table)) {
                confirmed[at] <- names[at] %in% names(table$values)
                # Only ordinary, atomic fields satisfy this constructor proof.
                caller[at] <- call_head[at]
                next
            }
            calls <- at[call_head[at]]
            if (!length(calls)) next
            fields <- data_mask_list_functions(s, source, owns, call)
            if (is.null(fields)) next
            confirmed[calls] <- names[calls] %in% fields$functions
            lexical <- calls[!names[calls] %in% fields$possible]
            caller[lexical] <- TRUE
            if (length(lexical) && length(fields$possible)) {
                mask$call_capture[[length(mask$call_capture) + 1L]] <- list(rows = rows[lexical], names = fields$possible)
            }
        }
    }
    # The lexical index binds by name. A call can skip that binding if it
    # holds a non-function, even after it has fallen through the data mask.
    lexical <- which(caller)
    ambiguous <- lexical[data_mask_ambiguous_callees(parsed, rows[lexical], names[lexical], binding)]
    caller[ambiguous] <- confirmed[ambiguous] <- FALSE
    uncertain <- !confirmed & !caller
    if (any(uncertain)) mask$fallback <- split(rows[uncertain], names[uncertain])
    masked <- rows[call_head & !caller]
    if (length(masked)) {
        combined <- c(mask$column, masked)
        order <- order(combined)
        mask$column <- combined[order]
        mask$receiver <- c(mask$receiver, integer(length(masked)))[order]
    }
    mask
}

data_mask_ambiguous_callees <- function(parsed, rows, names, binding) {
    s <- parsed$syntax_data
    nav <- parsed$navigation_data
    declared <- reference_symbol_names(s$text[nav$name])
    possible <- c(declared[nav[["function"]] == 0L | duplicated(declared)], reference_symbol_names(s$text[nav$scope_name]))
    check <- which(names %in% possible)
    ambiguous <- logical(length(rows))
    for (at in split(check, names[check])) {
        name <- names[[at[[1L]]]]
        values <- vapply(rows[at], function(row) binding(name, row), integer(1L))
        unsure <- values < 0L | (values > 0L & s$function_close[pmax(1L, values)] == 0L)
        ambiguous[at[unsure]] <- TRUE
        # A later local assignment also appears in the lexical index before
        # it has run. Check intervening scopes up to the reaching function.
        remaining <- at[!unsure]
        values <- values[!unsure]
        target <- ifelse(values > 0L, s$function_scope[pmax(1L, values)], -1L)
        scope <- s$function_scope[rows[remaining]]
        scopes <- unique(s$function_scope[nav$node[nav$by_name[[name]]]])
        while (length(remaining)) {
            blocked <- scope != target & scope %in% scopes
            ambiguous[remaining[blocked]] <- TRUE
            more <- scope != target & scope > 0L & !blocked
            remaining <- remaining[more]
            target <- target[more]
            scope <- s$function_scope[scope[more]]
        }
    }
    ambiguous
}

# A literal list can supply a function directly. Unknown field values may
# also be callable. Duplicate, unnamed and spliced arguments remain ambiguous.
data_mask_list_functions <- function(s, row, owns, call) {
    if (!identical(call$name, "list") || !owns(call, "base", row)) return(NULL)
    args <- call$values
    keys <- names(args)
    if (any(args == 0L) || any(!nzchar(keys)) || anyDuplicated(keys)) return(NULL)
    spread <- s$first_child[args]
    if (any(s$token[spread] == "SYMBOL" & s$text[spread] == "...")) return(NULL)
    callable <- s$function_close[args] > 0L
    ordinary <- logical(length(args))
    ordinary[!callable] <- vapply(args[!callable], function(arg) {
        class_literal_row(s, arg, "NULL_CONST") > 0L || data_column_atomic(arg, s, owns)
    }, logical(1L))
    list(functions = keys[callable], possible = keys[!ordinary])
}

# Keep a constructor's schema only through uses as a mask receiver. Passing
# a table to other code, taking an alias, or modifying columns by reference
# can invalidate it. Group the checks by receiver name and constructor.
data_mask_constructors <- function(parsed, sites, mask, binding) {
    s <- parsed$syntax_data
    nav <- parsed$navigation_data
    leaf <- s$first_child[sites]
    symbol <- leaf > 0L & s$token[pmax(1L, leaf)] == "SYMBOL" & s$child_start[sites] == s$child_end[sites]
    out <- sites
    out[symbol] <- 0L
    ids <- which(symbol & !leaf %in% mask$column)
    if (!length(ids)) return(out)
    spelling <- reference_symbol_names(s$text[leaf[ids]])
    groups <- split(ids, spelling)
    all_receivers <- unique(c(mask$receiver[mask$receiver > 0L], sites))
    mentions <- setdiff(which(s$token == "SYMBOL"), s$first_child[all_receivers])
    words <- reference_symbol_names(s$text[mentions])
    group_names <- names(groups)
    mentions <- split(mentions, factor(words, levels = group_names))
    writers <- which(s$text %in% c(":=", "`:=`") |
            (s$token == "SYMBOL_FUNCTION_CALL" & s$text %in% c("assign", "rm", "remove", "load", "source", "sys.source")))
    stride <- as.double(s$maxcol) + 1
    pos <- function(r) s$line1[r] * stride + s$col1[r]
    end_pos <- function(r) s$line2[r] * stride + s$col2[r]
    ends <- end_pos(nav$node)
    writes <- sort(pos(writers))
    next_write <- c(writes, Inf)[findInterval(ends, writes) + 1L]
    for (i in seq_along(groups)) {
        name <- group_names[[i]]
        defs <- nav$by_name[[name]]
        if (length(defs) != 1L) next
        definition <- nav$value[[defs]]
        if (!definition) next
        at <- groups[[i]]
        at <- at[s$function_scope[leaf[at]] == s$function_scope[[definition]] &
                s$order_rank[leaf[at]] > s$order_rank[[definition]]]
        if (!length(at) || binding(name, leaf[[at[[1L]]]]) != definition) next
        # A later occurrence inside the RHS cannot see the new binding.
        end <- ends[[defs]]
        other <- pos(mentions[[i]])
        cutoff <- min(c(next_write[[defs]], other[other > end]))
        safe <- pos(leaf[at]) > end & end_pos(s$parent_row[sites[at]]) < cutoff
        out[at[safe]] <- definition
    }
    out
}
