# Locate a lexical binding at the cursor, including operators and replacement
# functions whose dispatch name need not occur as a symbol token there.
s4_definition_binding_row <- function(parsed, token, name) {
    syntax <- parsed$syntax_data
    index <- parsed$navigation_data
    ancestors <- integer(syntax$max_depth + 1L)
    depth <- 0L
    row <- token
    while (row) {
        depth <- depth + 1L
        ancestors[[depth]] <- row
        row <- syntax$parent_row[[row]]
    }
    ancestors <- ancestors[seq_len(depth)]
    scoped <- mget(paste(ancestors, name, sep = "\r"), index$by_scope_name,
        ifnotfound = list(integer()), inherits = FALSE)
    targets <- index$scope_name[unlist(scoped, use.names = FALSE)]
    assignments <- get0(name, index$by_name, inherits = FALSE, ifnotfound = integer())
    nodes <- index$node[assignments]
    # The RHS of x <- x refers to the earlier binding. A function body can
    # refer recursively to the name being assigned to the function.
    preceding <- syntax$order_rank[nodes] <= syntax$order_rank[[token]]
    functions <- ancestors[syntax$function_close[ancestors] > 0L]
    own <- index$name[assignments] == token | index$value[assignments] %in% functions
    targets <- c(targets, nodes[preceding & (!nodes %in% ancestors | own)])
    if (!length(targets)) return(NULL)

    scope <- vapply(syntax$parent_row[targets], function(row) {
        while (row && !syntax$function_close[[row]]) row <- syntax$parent_row[[row]]
        row
    }, integer(1L))
    priority <- match(scope, c(functions, 0L))
    visible <- which(!is.na(priority))
    if (!length(visible)) return(NULL)
    best <- order(priority[visible], -syntax$order_rank[targets[visible]])[[1L]]
    targets[[visible[[best]]]]
}

# Only a literal occupying the generic-name argument of a known helper refers
# to a generic. Other strings, including class signatures, retain file lookup.
s4_definition_literal <- function(parse_data, token) {
    syntax <- parse_data$syntax_data
    argument <- syntax$parent_row[[token]]
    if (!argument || !identical(syntax_children(syntax, argument), token)) return(NULL)
    call <- syntax$parent_row[[argument]]
    if (!call) return(NULL)
    children <- syntax_children(syntax, call)
    children <- children[syntax$token[children] != "COMMENT"]
    kinds <- syntax$token[children]
    if (length(kinds) < 4L || kinds[[2L]] != "'('" || children[[1L]] == argument) return(NULL)
    head <- syntax_children(syntax, children[[1L]])
    head <- head[syntax$token[head] != "COMMENT"]
    if (!length(head)) return(NULL)
    callee <- utils::tail(head, 1L)
    if (syntax$token[[callee]] != "SYMBOL_FUNCTION_CALL") return(NULL)
    helper <- reference_symbol_names(syntax$text[[callee]])
    if (!helper %in% c(s4_registration_helpers, "standardGeneric", "getGeneric", "getMethod", "selectMethod",
            "hasMethod", "hasMethods", "existsMethod", "isGeneric", "findMethods", "showMethods", "removeMethod",
            "removeMethods", "removeGeneric")) return(NULL)

    package <- if (helper == "standardGeneric") "base" else "methods"
    if (length(head) == 1L) {
        if (!is.null(s4_definition_binding_row(parse_data, callee, helper))) return(NULL)
    } else if (length(head) != 3L || !syntax$token[[head[[2L]]]] %in% c("NS_GET", "NS_GET_INT") ||
            !identical(class_token_text(syntax$text[[head[[1L]]]]), package)) {
        return(NULL)
    }

    # A pipe can insert a positional argument before those written in the call.
    # Resolve only calls whose written arguments have their ordinary positions.
    parent <- syntax$parent_row[[call]]
    if (parent) {
        siblings <- syntax_children(syntax, parent)
        if (call == utils::tail(siblings, 1L) && any(syntax$text[siblings] %in% c("|>", "%>%", "%T>%", "%<>%"))) return(NULL)
    }

    groups <- cumsum(kinds == "','") + 1L
    actuals <- as.list(seq_len(max(groups)))
    equals <- which(kinds == "EQ_SUB")
    tags <- rep("", length(actuals))
    tags[groups[equals]] <- class_token_text(syntax$text[children[equals - 1L]])
    names(actuals) <- tags
    target <- groups[match(argument, children)]
    if (helper == "standardGeneric") {
        # The primitive checks supplied names in positional order.
        if (target != 1L || !tags[[1L]] %in% c("", "f")) return(NULL)
    } else {
        fn <- getExportedValue(package, if (helper == "setReplaceMethod") "setMethod" else helper)
        matched <- tryCatch(match.call(fn, as.call(c(list(as.name(helper)), actuals)), expand.dots = FALSE),
            error = function(e) NULL)
        generic <- names(formals(fn))[[1L]]
        external <- if (helper %in% s4_registration_helpers) "where" else c("where", "package", "mlist", "fdef")
        if (!identical(matched[[generic]], target) || any(external %in% names(matched))) return(NULL)
    }
    s4_registration_name(helper, class_token_text(syntax$text[[token]]))
}

# Replacement requires a function name: handle ordinary and qualified calls.
s4_replacement_callee <- function(syntax, row) {
    kinds <- syntax$token[syntax_children(syntax, row)]
    kinds <- kinds[kinds != "COMMENT"]
    (length(kinds) == 1L && kinds %in% c("SYMBOL_FUNCTION_CALL", "STR_CONST")) ||
        (length(kinds) == 3L && kinds[[2L]] %in% c("NS_GET", "NS_GET_INT"))
}

# R writes a compound target back through the first argument of each call.
# In value(x)[i] <- y this invokes both `[<-` and `value<-`.
s4_definition_replacement <- function(syntax, token) {
    head <- syntax$parent_row[[token]]
    if (!head) return(FALSE)
    if (syntax$token[[token]] %in% c("'['", "LBB")) {
        call <- head
    } else {
        call <- syntax$parent_row[[head]]
        if (!call) return(FALSE)
        children <- syntax_children(syntax, call)
        children <- children[syntax$token[children] != "COMMENT"]
        if (length(children) < 3L || children[[1L]] != head || syntax$token[[children[[2L]]]] != "'('" ||
                !s4_replacement_callee(syntax, head)) return(FALSE)
    }
    repeat {
        parent <- syntax$parent_row[[call]]
        if (!parent) return(FALSE)
        siblings <- syntax_children(syntax, parent)
        siblings <- siblings[syntax$token[siblings] != "COMMENT"]
        if (length(siblings) < 3L) return(FALSE)
        kinds <- syntax$token[siblings]
        operator <- kinds[[2L]]
        if (operator %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) return(siblings[[1L]] == call)
        if (operator == "RIGHT_ASSIGN") return(siblings[[3L]] == call)
        if (operator %in% c("'['", "LBB", "'$'", "'@'")) {
            receiver <- siblings[[1L]]
        } else if (operator == "'('") {
            # The first written argument receives the updated object, even
            # when named. A computed function has no replacement name in R.
            if (!s4_replacement_callee(syntax, siblings[[1L]])) return(FALSE)
            first <- if (length(kinds) >= 5L && kinds[[4L]] == "EQ_SUB") 5L else 3L
            receiver <- siblings[[first]]
        } else {
            return(FALSE)
        }
        if (receiver != call) return(FALSE)
        call <- parent
    }
}
