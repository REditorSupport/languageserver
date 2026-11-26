# A roxygen block immediately above a literal R6 $set() documents the new
# method. An unrelated object with a method named set has no such contract.
roxygen_r6_set_function_row <- function(s, row, memo = NULL) {
    if (!class_top_level_statement(s, row)) return(0L)
    call <- syntax_children(s, row)
    head <- syntax_children(s, call[[1L]])
    kinds <- s$token[head]
    key <- if (length(head) == 3L && kinds[[2L]] == "'$'") head[[3L]] else if (
        length(head) == 5L && kinds[[2L]] == "LBB") class_literal_row(s, head[[3L]], "STR_CONST") else 0L
    if (!key || !identical(class_token_text(s$text[[key]]), "set")) return(0L)
    receiver <- class_literal_row(s, head[[1L]], "SYMBOL")
    if (!receiver) return(0L)
    assignments <- memo$r6_assignments
    if (is.null(assignments)) {
        writes <- .Call("symbol_assignment_rows_c", s, s, PACKAGE = "languageserver")
        global <- if (length(s$function_scope)) s$function_scope[writes$name] == 0L else rep(TRUE, length(writes$name))
        assignments <- split(writes$operator[global], class_token_text(s$text[writes$name[global]]))
        if (!is.null(memo)) memo$r6_assignments <- assignments
    }
    writes <- assignments[[class_token_text(s$text[[receiver]])]]
    before <- writes[writes < receiver]
    if (!length(before)) return(0L)
    operator <- before[[length(before)]]
    binding <- s$parent_row[[operator]]
    if (!class_top_level_statement(s, binding)) return(0L)
    sides <- syntax_children(s, binding)
    sides <- sides[s$token[sides] != "COMMENT"]
    rhs <- if (s$token[[operator]] == "RIGHT_ASSIGN") sides[[1L]] else sides[[3L]]
    if (!identical(roxygen_class_factory(s, rhs, memo), "R6Class")) return(0L)

    args <- class_matched_arguments(syntax_call_arguments(s, row, computed_name = "set"), c("which", "name", "value", "overwrite"))
    which <- class_literal_row(s, args$which, "STR_CONST")
    name <- class_literal_row(s, args$name, "STR_CONST")
    if (!which || !name || !class_token_text(s$text[[which]]) %in% c("public", "private", "active") ||
            !length(args$value) || !args$value) return(0L)
    fn <- syntax_children(s, args$value)
    fn <- fn[s$token[fn] != "COMMENT"]
    if (length(fn) && s$token[[fn[[1L]]]] %in% c("FUNCTION", r"('\\')")) fn[[1L]] else 0L
}

# Keep the names of prose-only declarations as masks, alongside lazily
# resolved types captured at the class declaration.
member_r6_doc_params <- function(types, documented, index, bindings) {
    keys <- union(names(types), documented)
    if (!length(keys)) return(NULL)
    specs <- stats::setNames(vector("list", length(keys)), keys)
    for (name in names(types)) specs[[name]] <- list(r6_hint = member_class_hint(types[[name]], index, bindings))
    specs
}

member_r6_document_method <- function(value, name, class_params, declared_fields, parent, index, bindings) {
    fn <- value$function_expr
    if (is.null(fn)) return(value)
    hints <- attr(fn, "member_type_hints", exact = TRUE)
    parent_params <- attr(parent$function_expr, "member_type_hints", exact = TRUE)$params
    fields <- if (identical(name, "initialize")) declared_fields
    if (!length(hints$params) && !length(class_params) && !length(fields) && !length(parent_params)) return(value)
    # A child overriding the method may use a differently named or shadowed
    # binding. Capture direct method documentation before publishing it.
    params <- lapply(hints$params, function(types) {
        if (!is.character(types)) return(types)
        list(r6_hint = member_class_hint(types, index, bindings))
    })
    missing <- setdiff(names(fn[[2L]]), c("...", names(params), attr(hints, "roxygen_documented", exact = TRUE)))
    for (source in list(class_params, fields, parent_params)) {
        use <- intersect(missing, names(source))
        if (length(use)) {
            params[use] <- source[use]
            missing <- setdiff(missing, use)
        }
    }
    if (length(params)) {
        hints$params <- params
        attr(fn, "member_type_hints") <- hints
        value$function_expr <- fn
    }
    value
}

member_r6_field_default <- function(value, hint, placeholder = FALSE) {
    if (is.null(hint)) return(value)
    if (placeholder) value <- member_value(reason = "documented_field")
    if (member_needs_call_hint(value)) value$r6_field_hint <- hint
    value
}

member_r6_read_field <- function(value, index, budget = NULL) {
    if (is.null(index)) return(value)
    result <- member_refine_class_hint(value, value$r6_field_hint, index, budget)
    if (!length(result$type) && !length(result$possible_returns)) return(value)
    for (key in c("origin", "r6_owner", "r6_member_kind")) {
        if (!is.null(value[[key]])) result[[key]] <- value[[key]]
    }
    result
}

# A documented $set() method has the same scoped receiver while editing
# its body as it does when selected from an instance.
member_r6_set_cursor <- function(node, selected, index, bindings, budget) {
    control <- member_r6_control(node)
    if (is.null(control) || !identical(member_name(control$call[[1L]][[3L]]), "set")) return(NULL)
    args <- member_s7_arguments(control$call, c("which", "name", "value", "overwrite"))
    if (!identical(args$value, selected) || !is.character(args$which) || length(args$which) != 1L ||
            !args$which %in% c("public", "private", "active") || !is.character(args$name) || length(args$name) != 1L) return(NULL)
    generator <- member_infer(as.name(control$receiver), index, bindings, budget = budget)
    if (!identical(generator$r6_role, "generator")) return(NULL)
    updated <- member_r6_apply_control(generator, control$call, index, bindings, budget)
    instance <- updated$fields$new$result_shape
    if (is.null(instance) || identical(updated, generator)) return(NULL)
    target <- if (args$which == "private") instance$r6_private else instance
    fn <- target$fields[[args$name]]$function_expr
    if (!is.null(fn)) attr(selected, "member_type_hints") <- attr(fn, "member_type_hints", exact = TRUE)
    list(method = selected, scope = member_r6_instance_scope(instance, length(instance$r6_layers) > 0L || isTRUE(instance$r6_class$incomplete_parents)))
}
