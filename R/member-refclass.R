# Model the fields and methods declared by a source reference class. Custom
# field accessors and initialize bodies are never invoked.
member_refclass_generator <- function(expr, index, bindings, budget, trail = character()) {
    if (!identical(member_s4_method(expr, index, bindings), "setRefClass")) return(NULL)
    args <- tryCatch(match.call(methods::setRefClass, expr), error = function(e) NULL)
    class <- args$Class
    if (!is.character(class) || length(class) != 1L || !nzchar(class) ||
            class %in% trail || length(trail) >= 24L || !is.null(args$where)) {
        return(member_value(reason = "dynamic_reference_class"))
    }
    members <- list()
    field_names <- character()
    if (budget$remaining <= 0L) return(member_value(reason = "reference_class_limit"))
    budget$remaining <- budget$remaining - 1L
    parents <- member_s4_strings(args$contains, index, bindings)
    if (length(parents) > 16L) return(member_value(reason = "reference_class_limit"))
    # Merge in declaration order; the first superclass takes precedence.
    for (parent in rev(parents)) {
        history <- member_lookup(index$document_refclasses, parent)
        at <- bindings$.__member_position__
        position <- if (is.null(at)) length(history) else member_history_position(history, at)
        if (!position) next
        entry <- history[[position]]
        env <- bindings
        env$.__member_position__ <- entry$start
        parent_expr <- entry$expr
        if (!is.null(budget$origin_name) && !is.null(budget$document)) {
            i <- member_history_position(budget$document$parse_data$member_data$items, entry$end)
            if (i) {
                parent_expr <- member_declaration_expr(budget$document, i, budget)
                if (member_head(parent_expr, "<-") || member_head(parent_expr, "=")) parent_expr <- parent_expr[[3L]]
            }
        }
        generator <- member_refclass_generator(parent_expr, index, env, budget, c(trail, class))
        field_names <- union(field_names, generator$refclass_instance$refclass_fields)
        inherited <- generator$refclass_instance$fields
        members[names(inherited)] <- inherited
    }
    field_origins <- attr(args$fields, "member_origins", exact = TRUE)
    method_origins <- attr(args$methods, "member_origins", exact = TRUE)
    fields <- args$fields
    if (is.character(fields)) fields <- as.list(fields) else if (
        member_s4_literal_call(fields, "list", index, bindings) || member_s4_literal_call(fields, "c", index, bindings)
    ) fields <- as.list(fields)[-1L] else fields <- list()
    methods <- args$methods
    if (member_s4_literal_call(methods, "list", index, bindings)) methods <- as.list(methods)[-1L] else methods <- list()
    if (length(fields) + length(methods) > 256L || budget$remaining <= 0L) {
        return(member_value(reason = "reference_class_limit"))
    }
    budget$remaining <- budget$remaining - length(fields) - length(methods) - 1L
    keys <- names(fields)
    for (i in seq_along(fields)) {
        if (identical(fields[[i]], quote(expr = ))) next
        key <- if (length(keys)) keys[[i]] else ""
        type <- fields[[i]]
        if (!nzchar(key) && is.character(type) && length(type) == 1L) {
            key <- type
            type <- NULL
        }
        if (nzchar(key)) {
            field_names <- union(field_names, key)
            field <- member_value(type = if (is.character(type) && length(type) == 1L) type)
            if (identical(budget$origin_name, key)) field$origin <- field_origins[[as.character(i)]]
            members[[key]] <- field
        }
    }
    for (key in names(methods)) {
        if (nzchar(key) && !identical(methods[[key]], quote(expr = ))) {
            method <- member_infer(methods[[key]], index, bindings, budget = budget)
            if (identical(budget$origin_name, key)) method$origin <- method_origins[[as.character(match(key, names(methods)))]]
            members[[key]] <- method
        }
    }
    instance <- member_value(type = class, fields = members)
    instance$refclass_origin <- attr(expr, "class_origin", exact = TRUE)
    instance$refclass_fields <- setdiff(field_names, names(methods))
    initialize <- member_lookup(members, "initialize")
    constructor <- if (is.null(initialize)) quote(function(...) NULL) else initialize$function_expr
    new <- member_value(type = "function", function_expr = constructor)
    if (is.null(initialize)) new$constructor_slots <- instance$refclass_fields
    new$refclass_instance <- instance
    generator <- new
    generator$fields <- list(new = new)
    generator
}

# Named S4 signature entries identify a method formal directly. A positional
# class is unambiguous when the method has a single dispatchable formal.
member_s4_method_formals <- function(expr, node, index, bindings, budget) {
    if (!member_head(node, "function") ||
            !isTRUE(member_s4_method(expr, index, bindings) %in% c("setMethod", "setReplaceMethod"))) return(NULL)
    args <- tryCatch(match.call(methods::setMethod, expr), error = function(e) NULL)
    if (!identical(args$definition, node) || !is.null(args$where)) return(NULL)
    signature <- args$signature
    entries <- if (is.character(signature)) as.list(signature) else if (
        member_s4_literal_call(signature, "c", index, bindings) ||
            identical(member_s4_method(signature, index, bindings), "signature")
    ) as.list(signature)[-1L]
    if (!length(entries) || length(entries) > 32L) return(NULL)
    formals <- setdiff(names(node[[2L]]), "...")
    keys <- names(entries)
    if (is.null(keys)) keys <- rep("", length(entries))
    if (length(formals) == 1L && length(entries) == 1L && !nzchar(keys[[1L]])) keys <- formals
    shapes <- list()
    for (i in which(keys %in% formals)) {
        if (identical(entries[[i]], quote(expr = ))) next
        type <- entries[[i]]
        if (!is.character(type) || length(type) != 1L || !nzchar(type)) next
        shape <- member_s4_shape(type, index, bindings, budget = budget)
        if (!is.null(shape$slot_types)) shapes[[keys[[i]]]] <- shape
    }
    shapes
}

# The generated S4 class function forwards to new(), which dispatches
# initialize() for .Object. Reserve the class argument only for new() itself.
member_s4_constructor <- function(target, index, bindings, budget, new = FALSE) {
    if (!is.null(target$recipe)) bindings$.__s4_classes__[[target$name]] <- target$recipe
    descriptor <- member_s4_descriptor(target$name, index, bindings, target$package)
    if (is.null(descriptor) || isTRUE(descriptor$virtual)) return(NULL)
    shape <- member_s4_shape(target$name, index, bindings, target$package, budget = budget)
    if (is.null(shape$slot_types)) return(NULL)
    formals <- alist(... = )
    # Package snapshots expose slot layouts but can omit initialize methods.
    # Offer optional slot arguments only for a visible source declaration.
    slots <- if (!is.null(target$recipe) || length(member_lookup(index$document_s4, target$name)) ||
            !is.null(member_lookup(bindings$.__s4_classes__, target$name))) names(shape$slot_types)
    classes <- target$name
    for (i in seq_len(16L)) {
        parents <- unlist(lapply(classes, function(name) member_s4_descriptor(name, index, bindings)$contains),
            use.names = FALSE)
        more <- setdiff(parents, classes)
        if (!length(more)) break
        classes <- unique(c(classes, more))
        if (length(classes) > 64L) return(NULL)
    }
    at <- bindings$.__member_position__
    classes <- c(classes, "ANY")
    matches <- list()
    for (entry in rev(index$document_initializers)) {
        if (!is.null(at) && !member_before(entry$end, at)) next
        env <- bindings
        env$.__member_position__ <- entry$start
        if (!identical(member_s4_method(entry$expr, index, env), "setMethod")) next
        args <- tryCatch(match.call(methods::setMethod, entry$expr), error = function(e) NULL)
        if (!identical(args$f, "initialize") || !is.null(args$where)) next
        sig <- args$signature
        if (is.call(sig) && (member_s4_literal_call(sig, "c", index, env) ||
                    identical(member_s4_method(sig, index, env), "signature"))) {
            parts <- as.list(sig)[-1L]
            sig <- if (length(parts) == 1L) parts[[1L]]
        }
        if (!is.character(sig) || length(sig) != 1L) {
            # A computed registration might bind this class.
            slots <- character()
            next
        }
        if (!sig %in% classes || sig %in% names(matches)) next
        definition <- args$definition
        matches[[sig]] <- if (member_head(definition, "function") &&
                identical(utils::head(names(definition[[2L]]), 1L), ".Object")) {
            as.list(definition[[2L]])[-1L]
        } else {
            # Keep an opaque match to suppress default slot suggestions.
            list(quote(.__unknown_initializer__))
        }
    }
    if (length(matches)) {
        match <- if (target$name %in% names(matches)) matches[[target$name]] else if (length(matches) == 1L) matches[[1L]]
        slots <- character()
        if (!is.null(match) && (length(match) == 0L || !is.null(names(match)))) formals <- match
    }
    if (new) formals <- c(alist(Class = ), formals)
    fn <- as.call(list(as.name("function"), as.pairlist(formals), NULL))
    value <- member_value(type = "function", function_expr = fn)
    value$s4_constructor <- TRUE
    value$constructor_slots <- slots
    if (length(shape$slot_origins)) value$constructor_origins <- shape$slot_origins[slots]
    value
}
