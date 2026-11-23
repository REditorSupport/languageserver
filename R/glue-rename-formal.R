# Bare named arguments already have compact call facts in the host reference
# index. Quoted argument labels and pipes need the original physical tree.
glue_rename_tagged_calls <- function(document, fact, old, new) {
    lines <- fact$fragment$content
    if (!any(grepl("=", lines, fixed = TRUE))) return(list())
    tags <- Filter(function(t) identical(t$kind, "SYMBOL_SUB") && length(t$call) > 0L, fact$tokens)
    if (!any(grepl("['\"|%]", lines))) {
        calls <- lapply(tags, "[[", "call")
        return(unique(calls))
    }
    spellings <- vapply(tags, "[[", "", "name")
    possible <- startsWith(rep.int(old, length(spellings)), spellings) | startsWith(rep.int(new, length(spellings)), spellings)
    # A quoted tag can be the sole tag of a call and can contain R escapes.
    if (!any(possible) && !any(grepl("['\"]", lines))) return(list())
    data <- glue_fragment_document(document, fact$fragment)$parse_data
    s <- data$syntax_data
    results <- lapply(unique(s$parent_row[s$token == "EQ_SUB"]), function(row) {
        call <- syntax_call_arguments(s, row)
        if (is.null(call) || !is.null(call$package) || glue_inner_callee_shadowed(s, data$reference_index, row, call$name)) return(NULL)
        matched <- syntax_pipe_arguments(s, row, call)
        # Only SYMBOL_SUB labels participate in the exact-tag edit set.
        labeled <- which(call$tags > 0L)
        call$tags[labeled[s$token[call$tags[labeled]] != "SYMBOL_SUB"]] <- 0L
        inserted <- length(matched$values) - length(call$values)
        matched$tags <- c(integer(inserted), call$tags)
        matched
    })
    Filter(Negate(is.null), results)
}

# Compare R's argument matching before and after the exact tag edits the
# provider can make. A partial tag may stay as written if it still matches
# the same parameter. A tag collected by ... must also keep that position.
glue_rename_formal_problem <- function(workspace, document, formal, old, new) {
    fragments <- document$parse_data$glue_templates
    if (is.null(new) || identical(old, new) || !length(fragments) || is.null(formal$range)) return(NULL)
    owner <- workspace$documents$get(formal$uri, NULL)
    if (is.null(owner)) return(NULL)
    parsed <- owner$parse_data
    syntax <- parsed$syntax_data
    declaration <- member_navigation_token(parsed, owner$from_lsp_position(formal$range$start))
    if (!declaration || syntax$token[[declaration]] != "SYMBOL_FORMALS") return(NULL)
    siblings <- syntax_children(syntax, syntax$parent_row[[declaration]])
    rows <- siblings[syntax$token[siblings] == "SYMBOL_FORMALS"]
    parameters <- reference_symbol_names(syntax$text[rows])
    k <- match(declaration, rows)
    renamed <- parameters
    renamed[[k]] <- new

    facts <- glue_reference_facts(document)
    call_rows <- vapply(facts, function(f) as.character(f$fragment$call_row), "")
    mutations <- lapply(split(facts, call_rows), function(group) unique(unlist(lapply(group, "[[", "mutations"))))
    for (i in seq_along(facts)) {
        fact <- facts[[i]]
        for (call in glue_rename_tagged_calls(document, fact, old, new)) {
            if (!is.null(call$package) || call$name %in% mutations[[call_rows[[i]]]]) next
            supplied <- names(call$values)
            tags <- supplied[nzchar(supplied)]
            relevant <- startsWith(rep.int(old, length(tags)), tags) | startsWith(rep.int(new, length(tags)), tags)
            if (!any(relevant)) next
            target <- glue_formal_target(workspace, document, fact$fragment, list(name = old, call = call))
            if (!identical(target$uri, formal$uri) || !equal_range(target$range, formal$range)) next
            before <- member_navigation_tag_positions(supplied, parameters)
            supplied[supplied == old & call$tags > 0L] <- new
            after <- member_navigation_tag_positions(supplied, renamed)
            if (is.null(before) || !identical(before, after)) {
                return("Renaming would change argument matching inside a glue expression")
            }
        }
    }
    NULL
}
