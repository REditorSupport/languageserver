# Bare names may fall through a mask to the caller. Record uncertain reads
# by spelling once in the worker, so Rename can check just its own binding.
data_mask_fallbacks <- function(parsed, mask, kind, context, binding, owns) {
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
    if (!any(bare)) return(NULL)
    rows <- rows[bare]
    names <- names[bare]
    receiver <- mask$receiver[bare]
    sites <- unique(receiver[receiver > 0L])
    confirmed <- logical(length(rows))
    if (length(sites)) {
        constructor <- data_mask_constructors(parsed, sites, mask, binding)
        origin <- constructor[match(receiver, sites)]
        known <- which(!is.na(origin) & origin > 0L)
        for (at in split(known, origin[known])) {
            call <- data_column_constructor(s, origin[[at[[1L]]]], owns)
            if (is.null(call)) next
            confirmed[at] <- names[at] %in% names(call$values)
        }
    }
    if (all(confirmed)) return(NULL)
    split(rows[!confirmed], names[!confirmed])
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
    all_receivers <- unique(mask$receiver[mask$receiver > 0L])
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
