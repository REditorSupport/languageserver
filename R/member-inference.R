# Static receiver analysis. All inputs are syntax or inert metadata.
# No document expression, method, getter, or default is evaluated.

member_head <- function(x, name) {
    !missing(x) && is.call(x) && is.symbol(x[[1L]]) && identical(as.character(x[[1L]]), name)
}

unwrap_single_statement <- function(body) {
    while (member_head(body, "{") && length(body) == 2L) body <- body[[2L]]
    body
}

member_name <- function(x) {
    if (missing(x)) {
        return(NULL)
    }
    if (is.symbol(x)) {
        return(as.character(x))
    }
    if (is.character(x) && length(x) == 1L) {
        return(x)
    }
    NULL
}

member_key <- function(x) {
    if (is.symbol(x)) {
        return(as.character(x))
    }
    if (member_head(x, "$") && length(x) == 3L) {
        lhs <- member_key(x[[2L]])
        rhs <- member_name(x[[3L]])
        if (!is.null(lhs) && !is.null(rhs)) {
            return(paste(lhs, rhs, sep = "$"))
        }
    }
    NULL
}

member_walk <- function(x, visit, descend_functions = TRUE) {
    if (missing(x)) {
        return(invisible(NULL))
    }
    visit(x)
    if (is.call(x) || is.expression(x)) {
        if (!descend_functions && member_head(x, "function")) {
            return(invisible(NULL))
        }
        for (child in as.list(x)) member_walk(child, visit, descend_functions)
    }
    invisible(NULL)
}

member_strings <- function(x) {
    if (is.character(x)) {
        return(x)
    }
    if (member_head(x, "c")) {
        args <- as.list(x)[-1L]
        if (all(vapply(args, is.character, logical(1L)))) {
            return(unlist(args))
        }
    }
    character()
}

member_lookup <- function(x, key) {
    if (is.null(x) || !is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
        return(NULL)
    }
    # Lists and environments return NULL for absent names. Avoid scanning
    # large namespace maps twice for each syntax node.
    if (is.list(x) || is.environment(x)) return(x[[key]])
    if (!key %in% names(x)) return(NULL)
    x[[key]]
}

member_namespace_index <- function(index, package) {
    value <- member_lookup(index$namespace_indices, package)
    if (is.null(value) && identical(package, index$package)) return(index)
    if (isTRUE(value$.metadata_lazy)) {
        value <- index$namespace_metadata$get(package)
        index$namespace_indices[package] <- list(value)
    }
    value
}

# Member tables belong to the receiver's package, while lexical shadowing and
# attached namespaces still come from the document containing the request.
member_package_context <- function(index, package) {
    if (is.null(package) || identical(package, index$package)) return(index)
    owner <- member_namespace_index(index, package)
    if (is.null(owner)) return(NULL)
    context <- list2env(as.list(owner), parent = emptyenv())
    for (name in c("namespace_indices", "namespace_metadata", "document_bindings", "document_s4", "hint_packages", "document_package",
            "document_binding_cache", "document_context_key", "document_cache",
            "attached_roots", "attached_s4", "methods_attached")) context[[name]] <- index[[name]]
    context
}

member_subset_method <- function(value, operator, index, bindings, budget, depth, trail) {
    owner <- member_package_context(index, value$member_package)
    if (is.null(owner)) return(NULL)
    if (identical(value$reason, "ambiguous_class")) return(member_value(reason = "unresolved_subset_dispatch"))
    if (!length(value$classes) && !length(member_lookup(owner$classes, value$type))) return(NULL)
    classes <- member_classes(value, owner)
    if (!length(classes)) return(NULL)
    registries <- c(list(owner$subset_registry), lapply(index$namespace_indices, function(candidate) candidate$subset_registry))
    for (key in paste0(operator, ".", c(classes, "default"))) {
        history <- member_lookup(index$document_bindings, key)
        at <- bindings$.__member_position__
        if (key %in% names(bindings) || (length(history) && (is.null(at) || member_history_position(history, at)))) {
            return(member_infer(as.name(key), index, bindings, depth = depth + 1L, trail = trail, budget = budget))
        }
        registered <- unique(unlist(lapply(registries, function(registry) member_lookup(registry, key)), use.names = FALSE))
        definition <- member_definition(owner$definitions, key)
        if (length(registered)) {
            # Registrations can come from another namespace, or replace a
            # method after this receiver's package snapshot was prepared.
            if (identical(registered, "base")) return(NULL)
            expected <- if (member_head(definition, "function")) {
                paste0("namespace:", owner$package, ":", digest::digest(definition, algo = "xxhash64"))
            }
            if (length(registered) != 1L || is.null(expected) || !identical(registered, expected)) {
                return(member_value(reason = "unresolved_subset_dispatch"))
            }
            return(member_value(function_key = key, metadata = owner$package))
        }
        if (!is.null(definition)) return(member_value(function_key = key, metadata = owner$package))
    }
    if (!is.null(value$member_package) && !isTRUE(owner$subset_registry_complete)) {
        return(member_value(reason = "unresolved_subset_dispatch"))
    }
    NULL
}

member_value <- function(
  type = NULL, function_key = NULL, receiver = NULL,
  fields = NULL, function_expr = NULL, closure = NULL, receiver_name = NULL,
  receiver_value = NULL, literal = NULL, known_literal = FALSE,
  elements = NULL, classes = NULL, reason = NULL, element_shape = NULL,
  open = FALSE, result_shape = NULL, binding_expr = NULL, binding_env = NULL,
  metadata = NULL, slots = NULL, slot_types = NULL, s4_class = NULL,
  s4_generator = NULL, r6_private = NULL, r6_super = NULL, r6_self = NULL,
  r6_class = NULL, r6_role = NULL, default_dollar = FALSE, named_elements = FALSE,
  member_package = NULL, column_names_only = FALSE, possible_returns = NULL
) {
    value <- list(
        type = type, function_key = function_key, receiver = receiver,
        fields = fields, function_expr = function_expr, closure = closure,
        receiver_name = receiver_name, receiver_value = receiver_value,
        literal = literal, known_literal = known_literal, elements = elements,
        classes = classes, reason = reason, element_shape = element_shape,
        open = open, result_shape = result_shape,
        binding_expr = binding_expr, binding_env = binding_env, metadata = metadata,
        slots = slots, slot_types = slot_types, s4_class = s4_class,
        s4_generator = s4_generator, r6_private = r6_private,
        r6_super = r6_super, r6_self = r6_self,
        r6_class = r6_class, r6_role = r6_role
    )
    if (isTRUE(default_dollar)) value$default_dollar <- TRUE
    if (isTRUE(named_elements)) value$named_elements <- TRUE
    if (!is.null(member_package)) value$member_package <- member_package
    if (isTRUE(column_names_only)) value$column_names_only <- TRUE
    if (length(possible_returns)) value$possible_returns <- possible_returns
    value
}

member_literal <- function(x) {
    type <- if (is.null(x)) "NULL" else typeof(x)
    member_value(type = type, literal = x, known_literal = TRUE)
}

member_join <- function(a, b) {
    if (identical(a$type, ".never")) {
        return(b)
    }
    if (identical(b$type, ".never")) {
        return(a)
    }
    # Two branches can establish the same value at different declarations.
    # Preserve an agreed signature or shape without selecting either source.
    if (!identical(a$origin, b$origin)) a$origin <- b$origin <- NULL
    # Compacted snapshots and source values can omit different optional NULLs.
    if (identical(a, b) || (isTRUE(a$known_literal) && isTRUE(b$known_literal) &&
                identical(a[!vapply(a, is.null, logical(1L))], b[!vapply(b, is.null, logical(1L))]))) {
        return(a)
    }
    if (identical(a$reason, "type_hint_union") || identical(b$reason, "type_hint_union")) {
        return(member_possible_result(list(a, b), closed = TRUE))
    }
    r6_cmp <- NULL
    if (any(c(a$r6_role, b$r6_role) %in% c("instance", "private", "super"))) {
        r6_cmp <- member_r6_comparator(a, b, 0L)
        if (member_r6_value_equiv(a, b, 0L, r6_cmp)) return(a)
    } else if (member_r6_value_equiv(a, b, 0L)) {
        return(a)
    }
    if (!length(a$type) || !length(b$type)) {
        return(member_value(reason = "unknown_branch"))
    }
    fields <- NULL
    if (!is.null(a$fields) && !is.null(b$fields)) {
        common <- intersect(names(a$fields), names(b$fields))
        fields <- stats::setNames(lapply(common, function(n) member_join(a$fields[[n]], b$fields[[n]])), common)
    }
    slots <- NULL
    slot_types <- NULL
    if (!is.null(a$slot_types) && !is.null(b$slot_types)) {
        common <- intersect(names(a$slot_types), names(b$slot_types))
        slot_types <- stats::setNames(lapply(common, function(n) {
            member_s4_join_types(a$slot_types[[n]], b$slot_types[[n]], a$s4_class$package, b$s4_class$package)
        }), common)
        slots <- stats::setNames(lapply(common, function(n) {
            member_join(member_lookup(a$slots, n), member_lookup(b$slots, n))
        }), common)
    }
    value <- member_value(
        type = sort(unique(c(a$type, b$type))), fields = fields,
        open = isTRUE(a$open) || isTRUE(b$open) || !identical(names(a$fields), names(b$fields)) ||
            !is.null(a$elements) || !is.null(b$elements),
        slots = slots, slot_types = slot_types,
        s4_class = if (identical(a$s4_class, b$s4_class)) a$s4_class else NULL,
        element_shape = if (!is.null(a$element_shape) && !is.null(b$element_shape)) member_join(a$element_shape, b$element_shape) else NULL,
        classes = if (identical(a$classes, b$classes)) a$classes else if (
            isTRUE(a$default_dollar) || isTRUE(b$default_dollar)
        ) unique(c(a$classes, b$classes)) else NULL,
        default_dollar = isTRUE(a$default_dollar) && isTRUE(b$default_dollar),
        named_elements = isTRUE(a$named_elements) && isTRUE(b$named_elements) && identical(names(a$fields), names(b$fields)),
        member_package = if (identical(a$member_package, b$member_package)) a$member_package else NULL,
        column_names_only = isTRUE(a$column_names_only) || isTRUE(b$column_names_only)
    )
    if (!identical(a$classes, b$classes) && (length(a$classes) || length(b$classes))) value$reason <- "ambiguous_class"
    if (!identical(a$member_package, b$member_package) &&
            !is.null(a$member_package) && !is.null(b$member_package) && !is.null(fields)) {
        # Object names are local to each namespace; a joined receiver can only
        # retain its common structural fields, not either package's registry.
        value$type <- if (all(c(a$type, b$type) %in% c("list", "pairlist")) ||
                (isTRUE(a$named_elements) && isTRUE(b$named_elements))) "list" else "environment"
    }
    if (isTRUE(a$s7) && isTRUE(b$s7)) value$s7 <- TRUE
    if (length(a$slot_hints) && length(b$slot_hints)) {
        for (key in intersect(names(a$slot_hints), names(b$slot_hints))) {
            if (!identical(a$slot_hints[[key]], b$slot_hints[[key]])) next
            left <- a$slots[[key]]
            right <- b$slots[[key]]
            # A concrete value or a deferred default on either branch can
            # contradict the documented type on the other branch.
            if (identical(left, right) || (is.null(left$binding_expr) && is.null(right$binding_expr) &&
                        member_needs_call_hint(left) && member_needs_call_hint(right))) {
                value$slot_hints[[key]] <- a$slot_hints[[key]]
            }
        }
    }
    if (identical(a$r6_class, b$r6_class)) value$r6_class <- a$r6_class
    if (identical(a$r6_role, b$r6_role)) value$r6_role <- a$r6_role
    if (!is.null(a$r6_class_locked) && !is.null(b$r6_class_locked)) {
        value$r6_class_locked <- if (identical(a$r6_class_locked, b$r6_class_locked)) a$r6_class_locked else NA
    }
    if (identical(a$r6_owner, b$r6_owner)) value$r6_owner <- a$r6_owner
    if (identical(a$r6_member_kind, b$r6_member_kind)) value$r6_member_kind <- a$r6_member_kind
    if (identical(a$r6_locked, b$r6_locked)) value$r6_locked <- a$r6_locked
    if (!is.null(a$r6_bindings) || !is.null(b$r6_bindings)) {
        bindings <- c(a$r6_bindings, b$r6_bindings)
        # A binding must be writable on every branch to infer replacement.
        bindings <- c(bindings[bindings %in% c("method", "active", "locked")], bindings)
        value$r6_bindings <- bindings[!duplicated(names(bindings))]
    }
    if (identical(a$r6_identity, b$r6_identity)) value$r6_identity <- a$r6_identity
    if (!is.null(r6_cmp)) value <- member_r6_join(value, a, b, r6_cmp)
    value
}

member_classes <- function(value, index) {
    if (length(value$type) != 1L) {
        return(character())
    }
    if (length(value$classes)) {
        return(value$classes)
    }
    classes <- member_lookup(index$classes, value$type)
    if (is.null(classes)) value$type else classes
}

# An unknown alternative can reach a return indirectly through an assignment,
# a nested branch or a wrapper. Keep the proven part until the function exits.
# Intermediate expressions still see the original unknown value.
member_join_return_evidence <- function(a, b) {
    out <- member_join(a, b)
    if (length(out$type) || !isTRUE(out$reason %in% c("unknown_branch", "unknown_union_result"))) return(out)
    concrete <- function(x) {
        if (length(x$type) || !is.null(x$function_key) || !is.null(x$function_expr) ||
                identical(x$reason, "type_hint_union")) x else x$return_evidence
    }
    x <- concrete(a)
    y <- concrete(b)
    out$return_evidence <- if (is.null(x)) y else if (is.null(y)) x else member_join(x, y)
    out
}

member_hint_dispatch_classes <- function(value, index) {
    if (identical(value$r6_role, "generator")) return("R6ClassGenerator")
    if (identical(value$r6_role, "instance")) {
        if (is.null(value$r6_class)) return(character())
        if (isFALSE(value$r6_class$classed)) return("environment")
        if (!is.null(value$r6_class$classed) && is.na(value$r6_class$classed)) return(character())
        parents <- lapply(rev(value$r6_layers), function(layer) layer$r6_class$name)
        classes <- unique(c(value$r6_class$name, unlist(parents, use.names = FALSE)))
        if (isTRUE(value$r6_class$incomplete_parents)) {
            return(if (length(classes)) structure(classes, incomplete_s3_path = TRUE) else character())
        }
        return(unique(c(classes, "R6")))
    }
    if (isTRUE(value$s7)) {
        if (!is.null(value$s7_generator)) return(c("S7_class", "S7_object"))
        descriptor <- value$s7_descriptor
        classes <- character()
        while (identical(descriptor$kind, "class")) {
            name <- descriptor$name
            if (!is.null(descriptor$package)) name <- paste(descriptor$package, name, sep = "::")
            classes <- c(classes, name)
            descriptor <- descriptor$parent
        }
        # An S4 bridge can add inherited S4 dispatch classes that are absent
        # from the S7 descriptor. Avoid selecting a default method in that case.
        if (!is.null(descriptor) && !identical(descriptor$kind, "base")) return(character())
        return(unique(c(classes, descriptor$name, "S7_object")))
    }
    if (!is.null(value$s4_generator) || !is.null(value$refclass_instance)) return(character())
    if (!is.null(value$s4_class) || !is.null(value$refclass_fields)) {
        path <- if (!is.null(value$s4_class)) value$s4_class$dispatch_path else value$refclass_dispatch_path
        if (is.null(path) && !is.null(value$s4_class$package)) {
            path <- member_s4_descriptor(value$s4_class$name, index, list(), value$s4_class$package)$dispatch_path
        }
        if (length(path)) return(names(path))
        name <- if (!is.null(value$s4_class)) value$s4_class$name else value$type
        return(structure(name, incomplete_s3_path = TRUE))
    }
    if (!is.null(value$member_package) && !identical(value$member_package, index$package)) {
        index <- member_lookup(index$namespace_indices, value$member_package)
    }
    classes <- member_classes(value, index)
    if (!length(value$classes) && length(classes) == 1L && classes %in% c("double", "integer")) {
        return(s3_implicit_classes(classes))
    }
    classes
}

# Resolve a document generic with the same source and registration precedence
# used by signature help, then analyze the selected method in its own document.
member_source_s3_result <- function(generic, classes, actuals, index, caller, at, budget, depth, trail, owner = "source") {
    unknown <- member_value(reason = "unknown_s3_method")
    document <- index$dispatch_document
    workspace <- index$dispatch_workspace
    if (!length(classes) || isTRUE(budget$exhausted)) return(unknown)
    if (is.null(at)) at <- c(Inf, Inf)
    context <- budget$member_s3_context
    if (is.null(context) || !identical(context$document, document) || !identical(context$at, at)) {
        context <- s3_context(document$uri, workspace, document, at)
        budget$member_s3_context <- context
    }
    scope <- new.env(parent = context$root)
    keys <- paste(generic, s3_dispatch_path(classes), sep = ".")
    local <- intersect(keys, names(caller))
    for (name in local) {
        value <- member_local_value(caller[[name]], index, budget, depth, trail)
        assign(name, list(fn = value$function_expr), scope)
    }
    method <- s3_lookup_method(generic, classes, scope, context, owner)
    # For a package generic, its own verified method can use the existing
    # namespace interpreter and native return summaries.
    if (owner != "source" && is.null(method$fn) && identical(method$package, owner)) return(NULL)
    if (owner != "source" && is.null(method) && !isTRUE(context$registration_unknown) && !isTRUE(context$exhausted)) {
        # A registration for a different namespace can share the same
        # generic.class spelling. It does not override this package's method.
        registered <- get0(".registrations", scope, inherits = TRUE)
        qualified <- paste(owner, keys, sep = "::")
        if (!any(qualified %in% names(registered)) &&
                !any(c(qualified, keys) %in% names(context$namespace_records)) &&
                !any(vapply(keys, function(key) !is.null(s3_get_value(key, scope, context, source_only = TRUE)), logical(1L)))) return(NULL)
    }
    if (is.null(method$fn) || isTRUE(context$exhausted)) return(unknown)
    env <- list(.__member_position__ = at)
    registration <- method$registration
    if (is.null(registration) && is.null(method$source) && method$name %in% local) {
        callee <- member_local_value(caller[[method$name]], index, budget, depth, trail)
    } else {
        source <- if (is.null(registration)) method$source else registration
        target <- if (!is.null(registration)) registration$expr else if (!is.null(source$binding)) as.name(source$binding)
        if (is.null(target) || is.null(source$uri)) return(unknown)
        if (!identical(source$uri, document$uri)) {
            if (is.null(workspace$documents)) return(unknown)
            method_document <- workspace$documents$get(source$uri, NULL)
            if (is.null(method_document) || !identical(method_document$version, method_document$parse_data$version)) return(unknown)
            env$.__member_position__ <- c(Inf, Inf)
            index <- member_context_index(workspace, source$uri, method_document, env$.__member_position__, target)
            if (!is.null(budget$origin_name)) {
                previous <- budget$document
                budget$document <- method_document
                on.exit(budget$document <- previous)
            }
        }
        # Registration retains the closure from this binding, including the
        # arguments captured by a factory. A later assignment may have the
        # same function body and a different set of captures.
        definition_env <- env
        if (!is.null(registration)) definition_env$.__member_position__ <- registration$at
        callee <- member_infer(target, index, definition_env, depth = depth + 1L, trail = trail, budget = budget)
        if (is.null(callee$function_expr) || (!identical(callee$function_expr, method$fn) &&
                    !identical(s3_function_identity(callee$function_expr), method$body_hash))) return(unknown)
        callee$closure$.__member_position__ <- env$.__member_position__
    }
    env$.__member_s3_method__ <- callee
    arguments <- sprintf(".__member_s3_arg%d", seq_along(actuals))
    env[arguments] <- actuals
    call <- as.call(c(list(as.name(".__member_s3_method__")), stats::setNames(lapply(arguments, as.name), names(actuals))))
    member_infer(call, index, env, depth = depth + 1L, trail = trail, budget = budget)
}

# Inspect inexpensive source histories before constructing a dispatch context.
# Package calls without visible overrides retain ordinary namespace inference.
member_package_s3_overrides <- function(generic, classes, owner, index, caller, at, budget) {
    document <- index$dispatch_document
    workspace <- index$dispatch_workspace
    keys <- paste(generic, s3_dispatch_path(classes), sep = ".")
    if (any(keys %in% names(caller))) return(TRUE)
    sources <- budget$member_s3_sources
    if (!identical(sources$uri, document$uri)) {
        document$member_s3_dispatch <- TRUE
        root <- s3_package_root(workspace, document$uri)
        namespace <- if (!is.null(root)) s3_namespace_snapshot(root, workspace)
        sources <- list(uri = document$uri, views = s3_context_sources(document$uri, workspace, document),
            records = namespace$records, uncertain = isFALSE(namespace$valid))
        budget$member_s3_sources <- sources
        if (is.null(workspace$s3_revision) && length(sources$views) > 1L) budget$unversioned_s3_sources <- TRUE
    }
    if (sources$uncertain) return(TRUE)
    for (record in sources$records) {
        if (identical(record$generic, generic) && record$class %in% s3_dispatch_path(classes) &&
                (is.null(record$package) || identical(record$package, owner))) return(TRUE)
    }
    if (is.null(at)) at <- c(Inf, Inf)
    for (source in sources$views) {
        position <- if (identical(source$uri, document$uri)) at else c(Inf, Inf)
        for (i in source$index$registrations) {
            item <- source$items[[i]]
            if (!member_before(item$end, position)) next
            args <- as.list(item$expr)[-1L]
            matched <- s3_match_arguments(alist(genname = , class = , method = , envir = ), args)
            if (is.null(matched) || !all(1:2 %in% matched)) return(TRUE)
            target <- args[[match(1L, matched)]]
            class <- args[[match(2L, matched)]]
            if ((!is.character(target) || identical(target, generic)) &&
                    (!is.character(class) || class %in% s3_dispatch_path(classes))) return(TRUE)
        }
        for (key in keys) {
            if (member_history_position(source$index$histories[[key]], position)) return(TRUE)
        }
    }
    FALSE
}

# A worker snapshot can verify ordinary environment access. A method declared
# in the document or current lexical scope invalidates that package-time fact.
member_default_dollar <- function(value, index, bindings) {
    if (!isTRUE(value$default_dollar)) return(FALSE)
    if (!length(value$classes)) return(TRUE)
    methods <- paste0("$.", c(value$classes, "default"))
    if (any(methods %in% names(bindings))) return(FALSE)
    for (method in methods) {
        if (!is.null(member_lookup(index$definitions, method))) return(FALSE)
        history <- member_lookup(index$document_bindings, method)
        at <- bindings$.__member_position__
        if (length(history) && (is.null(at) || member_history_position(history, at))) return(FALSE)
    }
    TRUE
}

member_elements <- function(value) {
    if (!is.null(value$elements)) return(value$elements)
    if (isTRUE(value$named_elements) && !isTRUE(value$open)) return(value$fields)
    NULL
}

# Dollar and slot names are literal; a double-bracket name must be a string literal.
member_write_field <- function(lhs) {
    if (!is.call(lhs) || length(lhs) != 3L || !is.symbol(lhs[[2L]])) return(NULL)
    if (!member_head(lhs, "$") && !member_head(lhs, "@") && !(member_head(lhs, "[[") && is.character(lhs[[3L]]))) return(NULL)
    field <- member_name(lhs[[3L]])
    if (length(field) && !is.na(field) && nzchar(field)) field
}

member_write_root <- function(lhs) {
    while (is.call(lhs) && length(lhs) >= 2L) lhs <- lhs[[2L]]
    member_name(lhs)
}

member_plain_container <- function(object) {
    length(object$type) == 1L && object$type %in% c("list", "environment") &&
        !is.null(object$fields) && !length(object$classes) && is.null(object$r6_role) &&
        is.null(object$member_package) && is.null(object$reason) && !isTRUE(object$default_dollar)
}

member_write_value <- function(value, name, origin) {
    if (!is.null(origin)) value$origin <- origin
    if (!is.null(value$function_expr) && name %in% names(value$closure)) {
        value$closure[name] <- NULL
        value$receiver_name <- name
    }
    value
}

member_operator_masked <- function(operator, index, bindings) {
    if (operator %in% names(bindings)) return(TRUE)
    history <- member_lookup(index$document_bindings, operator)
    at <- bindings$.__member_position__
    (length(history) && (is.null(at) || member_history_position(history, at))) ||
        (!identical(index$package, "base") && !is.null(member_lookup(index$definitions, operator)))
}

# Environment-backed S7 subclasses inherit reference semantics through their
# parent chain. Use the descriptor rather than the user-defined class name.
member_s7_reference <- function(object) {
    desc <- object$s7_descriptor
    while (identical(desc$kind, "class")) desc <- desc$parent
    identical(desc$kind, "base") && identical(desc$name, "environment")
}

member_write_container <- function(object, field, value, name, origin, index, bindings, subset = FALSE, slot = FALSE) {
    if (slot) {
        if (!isTRUE(object$s7) || !is.null(object$s7_generator) ||
                member_operator_masked("@<-", index, bindings)) return(NULL)
        return(member_s7_update_slots(object, stats::setNames(list(value), field)))
    }
    setter <- if (subset) "[[<-" else "$<-"
    if (isTRUE(object$s7_exact_class)) {
        storage <- member_s7_environment_storage(object, setter, index, bindings)
        if (is.null(storage)) return(NULL)
        storage$fields[field] <- list(member_write_value(value, name, origin))
        object$s7_storage <- storage
        return(object)
    }
    if (!member_plain_container(object) || member_operator_masked(setter, index, bindings)) return(NULL)
    value <- member_write_value(value, name, origin)
    if (identical(object$type, "environment")) {
        object$fields[field] <- list(value)
        return(object)
    }
    # A NULL RHS deletes a list element. An opaque RHS might do the same:
    # keep the certain siblings without promising a field or positional order.
    if ((!length(value$type) && is.null(value$function_expr) && is.null(value$function_key)) ||
            (length(value$type) > 1L && "NULL" %in% value$type)) {
        object$fields <- object$fields[names(object$fields) != field]
        object$elements <- object$named_elements <- NULL
        object$open <- TRUE
    } else if (identical(value$type, "NULL")) {
        object$fields[[field]] <- NULL
        if (!is.null(object$elements)) object$elements[[field]] <- NULL
    } else {
        object$fields[field] <- list(value)
        if (!is.null(object$elements)) object$elements[field] <- list(value)
    }
    object
}

member_dataframe_columns <- function(value) {
    "data.frame" %in% value$classes && !is.null(value$fields)
}

member_members <- function(value, index, bindings = list(), accessor = "$") {
    if (!length(value$type) || any(value$type %in% c(".never", ".missing"))) {
        return(character())
    }
    if (identical(accessor, "@")) {
        slots <- names(value$slot_types)
        return(stats::setNames(rep(NA_character_, length(slots)), slots))
    }
    if (identical(accessor, "[[")) {
        fields <- unique(names(value$fields))
        return(stats::setNames(rep(NA_character_, length(fields)), fields))
    }
    index <- member_package_context(index, value$member_package)
    if (is.null(index)) return(character())
    columns <- member_dataframe_columns(value)
    if (!columns && isTRUE(value$default_dollar) && !member_default_dollar(value, index, bindings)) return(character())
    if (!isTRUE(value$default_dollar) && length(value$classes) &&
            !all(value$classes %in% c("data.frame", "list", "environment", "R6")) &&
            !any(value$classes %in% names(index$members)) && !columns) {
        return(character())
    }
    sets <- lapply(value$type, function(type) {
        out <- member_lookup(index$members, type)
        if (is.null(out)) character() else out
    })
    common <- Reduce(intersect, lapply(sets, names))
    out <- sets[[1L]][common]
    dynamic <- member_lookup(bindings, ".__member_properties__")
    if (length(value$type) == 1L) {
        extra <- setdiff(names(member_lookup(dynamic, value$type)), names(out))
        out <- c(out, stats::setNames(rep(NA_character_, length(extra)), extra))
    }
    if (!is.null(value$fields)) {
        extra <- setdiff(names(value$fields), names(out))
        out <- c(out, stats::setNames(rep(NA_character_, length(extra)), extra))
    }
    out
}

# Possible receivers keep their inferred type unresolved. All providers use
# the same intersection and reject conflicting member implementations.
member_possible_receiver <- function(value, index, bindings = list(), accessor = "$", inspect = NULL, budget = NULL) {
    if (length(value$type) || !length(value$possible_returns)) return(NULL)
    candidates <- value$possible_returns
    sets <- lapply(candidates, function(candidate) member_members(candidate, index, bindings, accessor))
    common <- Reduce(intersect, lapply(sets, names))
    labels <- if (is.null(inspect)) common else intersect(inspect, common)
    ambiguous <- member_ambiguous_names(value, sets, labels, index, bindings, accessor, budget)
    list(value = candidates[[1L]], members = common, ambiguous = ambiguous, sets = sets)
}

# Resolving a slot can infer a prototype expression. Only inspect labels the
# provider will actually use, sharing its budget across all alternatives.
member_ambiguous_names <- function(value, sets, labels, index, bindings, accessor, budget) {
    if (!length(labels)) return(character())
    candidates <- value$possible_returns
    owners <- vapply(candidates, function(candidate) {
        if (is.null(candidate$member_package)) "" else candidate$member_package
    }, character(1L))
    if (length(unique(owners)) > 1L) return(labels)
    labels[vapply(labels, function(name) {
        if (isTRUE(budget$exhausted) || !all(vapply(sets, function(set) identical(set[[name]], sets[[1L]][[name]]), logical(1L)))) {
            return(TRUE)
        }
        origins <- lapply(candidates, function(candidate) {
            if (identical(accessor, "@")) {
                return(if (isTRUE(candidate$s7)) candidate$s7_descriptor$properties[[name]]$origin else candidate$slot_origins[[name]])
            }
            member_lookup(candidate$fields, name)$origin
        })
        if (!all(vapply(origins, identical, logical(1L), origins[[1L]]))) return(TRUE)
        if (all(vapply(sets, function(set) is.na(set[[name]]), logical(1L)))) {
            if (identical(value$reason, "type_hint_union") && !identical(accessor, "@")) {
                # Sibling instances bind the same inherited method to different
                # self/private environments. Compare its declaration first.
                declarations <- lapply(candidates, function(candidate) member_lookup(candidate$fields, name))
                first <- declarations[[1L]]
                if (!is.null(first$r6_owner) && !is.null(first$function_expr) &&
                        all(vapply(declarations, identical, logical(1L), first))) return(FALSE)
            }
            fields <- lapply(candidates, function(candidate) {
                if (identical(accessor, "@")) return(member_slot(candidate, name, index, bindings, budget))
                if (identical(accessor, "[[")) return(member_lookup(candidate$fields, name))
                member_access(candidate, name, index, bindings, budget)
            })
            return(isTRUE(budget$exhausted) || !all(vapply(fields, member_r6_value_equiv, logical(1L), fields[[1L]])))
        }
        FALSE
    }, logical(1L))]
}

member_possible_result <- function(values, closed = FALSE) {
    candidates <- list()
    for (value in values) {
        if (!length(value$type) && length(value$possible_returns)) {
            # An open generic describes only known alternatives. It cannot
            # establish a common result for every branch of a declared union.
            if (closed && !identical(value$reason, "type_hint_union")) return(member_value(reason = "unknown_union_result"))
            candidates <- c(candidates, value$possible_returns)
        } else if ((length(value$type) && !any(value$type %in% c(".never", ".missing"))) ||
                !is.null(value$function_key) || !is.null(value$function_expr) || !is.null(value$result_shape)) {
            candidates <- c(candidates, list(value))
        } else if (closed && !identical(value$type, ".never")) {
            return(member_value(reason = "unknown_union_result"))
        }
        if (closed && length(candidates) > max_type_hint_alternatives) {
            candidates <- unique(candidates)
            if (length(candidates) > max_type_hint_alternatives) return(member_value(reason = "type_union_limit"))
        }
    }
    candidates <- candidates[!duplicated(candidates)]
    if (closed) {
        if (!length(candidates)) return(member_value(type = ".never"))
        if (length(candidates) == 1L) return(candidates[[1L]])
        return(member_value(reason = "type_hint_union", possible_returns = candidates))
    }
    member_value(reason = "possible_result", possible_returns = candidates)
}

# A bounded structural cache key omits irrelevant literals/large closure maps.
# Function identity and the metadata generation isolate package summaries.
member_shape_key <- function(value, depth = 0L) {
    if (is.null(value)) {
        return(NULL)
    }
    if (depth > 4L) {
        return("...")
    }
    list(
        type = value$type, classes = value$classes, default_dollar = value$default_dollar,
        named_elements = value$named_elements, member_package = value$member_package,
        column_names_only = isTRUE(value$column_names_only),
        class_ambiguous = identical(value$reason, "ambiguous_class"),
        literal = if (isTRUE(value$known_literal) &&
            length(value$literal) <= 8L) {
            value$literal
        } else {
            NULL
        },
        known = value$known_literal, function_key = value$function_key,
        fields = lapply(value$fields, member_shape_key, depth + 1L),
        slots = lapply(value$slots, member_shape_key, depth + 1L),
        slot_types = value$slot_types, slot_hints = value$slot_hints, s4_class = value$s4_class,
        s4_generator = value$s4_generator, s4_contains = value$s4_contains,
        s7 = value$s7, s7_descriptor = value$s7_descriptor,
        s7_storage = if (!is.null(value$s7_storage)) member_shape_key(value$s7_storage, depth + 1L),
        r6_private = member_shape_key(value$r6_private, depth + 1L),
        # Class-shape instances omit `r6_super` and build it on demand; key the
        # same surface `r6_super` held before so fixed-point checks match.
        r6_super = member_shape_key(
            if (!is.null(value$r6_super)) {
                value$r6_super
            } else if (identical(value$r6_role, "instance") && !is.null(value$r6_level)) {
                member_r6_super(value$r6_layers, value$r6_level)
            },
            depth + 1L
        ),
        r6_self = member_shape_key(value$r6_self, depth + 1L),
        r6_locked = value$r6_locked, r6_bindings = value$r6_bindings,
        r6_identity = value$r6_identity, r6_class = value$r6_class,
        r6_role = value$r6_role,
        r6_owner = value$r6_owner, r6_member_kind = value$r6_member_kind,
        r6_static_bound = isTRUE(value$r6_static_bound),
        r6_private_self = isTRUE(value$r6_private_self),
        elements = lapply(value$elements, member_shape_key, depth + 1L),
        element_shape = if (!is.null(value$element_shape)) member_shape_key(value$element_shape, depth + 1L) else NULL
    )
}

member_cacheable <- function(value, depth = 0L) {
    if (is.null(value)) {
        return(TRUE)
    }
    if (depth > 4L || !is.null(value$origin) || isTRUE(value$open) || length(value$possible_returns) || !is.null(value$function_expr) || !is.null(value$closure) ||
            !is.null(value$binding_cache) || !is.null(value$binding_expr) || !is.null(value$hint_types) || !is.null(value$return_evidence) ||
            !is.null(value$slot_hints)) {
        return(FALSE)
    }
    if (value$known_literal && length(value$literal) > 8L) {
        return(FALSE)
    }
    all(vapply(
        c(value$fields, value$slots, value$elements, value[c("r6_private", "r6_super", "r6_self", "s7_storage")],
            if (!is.null(value$element_shape)) list(value$element_shape)),
        member_cacheable, logical(1L), depth + 1L
    ))
}

member_assigned_names <- function(node) {
    if (missing(node)) return(character())
    calls <- .Call("member_assignment_calls_c", node, PACKAGE = "languageserver")
    if (!length(calls)) return(character())
    roots <- lapply(calls, function(call) member_write_root(call[[2L]]))
    unique(c(character(), unlist(roots)))
}

# Retain lexical inputs used by the function (including nested closures and
# defaults), rather than copying an entire instance into every generated method.
member_function_closure <- function(expr, bindings) {
    if (!length(bindings)) return(list())
    # Replacement syntax implicitly invokes a lexical setter.
    internal <- intersect(names(bindings), c("$<-", "[[<-", "@<-", member_s7_setters, ".__member_position__", ".__member_properties__",
            ".__s4_classes__", ".__s7_package__", ".__s7_constructing__"))
    if (length(internal) == length(bindings)) return(bindings)
    referenced <- setdiff(member_syntax_names(expr), names(expr[[2L]]))
    hint_names <- attr(expr, "member_type_names", exact = TRUE)
    if (length(hint_names)) referenced <- union(referenced, hint_names)
    bindings[intersect(names(bindings), c(referenced, internal))]
}

# Select a member from an already inferred receiver. This does not traverse
# syntax, so a known declaration remains usable after inference hits a limit.
member_slot <- function(value, name, index, bindings, budget = NULL) {
    if (identical(value$reason, "type_hint_union")) {
        return(member_possible_result(lapply(value$possible_returns, member_slot, name = name, index = index,
                    bindings = bindings, budget = budget), closed = TRUE))
    }
    result <- if (isTRUE(value$s7)) member_s7_slot(value, name, index, bindings, budget) else
        member_s4_slot(value, name, index, bindings, budget)
    if (identical(budget$origin_name, name)) {
        origin <- if (isTRUE(value$s7) && is.null(value$s7_generator)) {
            value$s7_descriptor$properties[[name]]$origin
        } else {
            value$slot_origins[[name]]
        }
        result$origin <- origin
    }
    result
}

# Closures retain their receiver when selected from the object's stored fields.
member_stored_field <- function(lhs, value, index = NULL, budget = NULL) {
    if (!is.null(value$r6_field_hint)) value <- member_r6_read_field(value, index, budget)
    if (identical(value$r6_role, "clone")) {
        if (is.null(value$receiver_value)) {
            value$receiver_value <- if (is.null(lhs$r6_self)) lhs else lhs$r6_self
        }
        return(value)
    }
    value <- member_r6_member_info(value, lhs)
    if (!is.null(value$receiver_name)) {
        receiver <- if (is.null(lhs$r6_self)) lhs else lhs$r6_self
        layers <- member_r6_levels(lhs)
        if (is.null(layers)) layers <- member_r6_levels(receiver)
        value$receiver_value <- receiver
        if (isTRUE(value$r6_private_self)) {
            value$closure$private <- lhs
        } else if (!isTRUE(value$r6_static_bound) && !is.null(receiver$r6_private)) {
            value$closure$private <- receiver$r6_private
            if (is.null(value$closure$private$r6_layers)) {
                value$closure$private <- member_r6_private_layers(
                    value$closure$private, member_r6_levels(receiver))
            }
        } else if (!is.null(value$r6_owner_level) &&
                !identical(lhs$r6_declared_level, value$r6_owner_level)) {
            # No single instance: the table of the declaring class. Its
            # own private methods were recorded before they had one.
            source <- if (!is.null(lhs$r6_layers) || identical(lhs$r6_role, "instance")) lhs else receiver
            value$closure$private <- member_r6_declared_private(source, value$r6_owner_level)
        }
        # Contexts of a stored instance's methods are kept once, on it, and
        # carried on bound contexts so container-held methods still find
        # them. Intra-instance context cycles are cut dynamically here.
        contexts <- if (!is.null(lhs$r6_contexts)) lhs$r6_contexts else receiver$r6_contexts
        visited <- lhs$r6_visited
        for (role in names(value$r6_context_ids)) {
            if (!is.null(value$closure[[role]])) next
            key <- value$r6_context_ids[[role]]
            context <- member_lookup(contexts, key)
            if (is.null(context)) next
            if (key %in% visited) {
                cut <- member_value(reason = "cycle")
                cut[c("r6_class", "r6_role", "r6_locked", "r6_bindings")] <-
                    context[c("r6_class", "r6_role", "r6_locked", "r6_bindings")]
                context <- cut[!vapply(cut, is.null, logical(1L))]
            } else if (!identical(context$reason, "cycle")) {
                context <- member_r6_attach_stored_contexts(context, contexts, c(visited, key))
            }
            value$closure[role] <- list(context)
        }
        if (!is.null(contexts) && !is.null(value$closure$private) &&
                !identical(value$closure$private$reason, "cycle") &&
                is.null(value$closure$private$r6_contexts)) {
            value$closure$private <- member_r6_attach_stored_contexts(
                value$closure$private, contexts, visited)
        }
        if (!is.null(value$closure$private)) {
            value$closure$private$r6_self <- receiver
        }
        if (isTRUE(value$r6_uses_super) || (!is.null(value$r6_owner_level) && value$r6_owner_level > 1L)) {
            value$closure$super <- member_r6_super(layers, value$r6_owner_level)
        }
        if (!is.null(value$closure$super)) value$closure$super$r6_self <- receiver
        # A bound method keeps these contexts wherever it is stored next.
        value$r6_owner_level <- value$r6_uses_super <- value$r6_context_ids <- NULL
    }
    if (!is.null(value$function_key)) value$receiver_value <- lhs
    value
}

member_access <- function(lhs, name, index, bindings, budget = NULL) {
    unknown <- member_value(reason = "unsupported")
    if (is.null(name)) return(unknown)
    if (identical(lhs$reason, "type_hint_union")) {
        return(member_possible_result(lapply(lhs$possible_returns, member_access, name = name,
                    index = index, bindings = bindings, budget = budget), closed = TRUE))
    }
    if (!is.null(lhs$s7_storage) && member_s7_reference(lhs)) {
        storage <- member_s7_environment_storage(lhs, "$", index, bindings)
        return(if (is.null(storage)) unknown else member_access(storage, name, index, bindings, budget))
    }
    possible <- member_possible_receiver(lhs, index, bindings, inspect = character())
    if (!is.null(possible)) {
        if (!name %in% possible$members) return(unknown)
        return(member_possible_result(lapply(lhs$possible_returns, function(candidate) {
            member_access(candidate, name, index, bindings, budget)
        })))
    }
    if (isTRUE(lhs$column_names_only) || identical(lhs$reason, "unresolved_dollar_dispatch")) {
        return(member_value(reason = "unresolved_dollar_dispatch"))
    }
    index <- member_package_context(index, lhs$member_package)
    if (is.null(index)) return(unknown)
    if (isTRUE(lhs$default_dollar) && !member_default_dollar(lhs, index, bindings)) {
        return(member_value(reason = "unresolved_dollar_dispatch"))
    }
    dispatch_classes <- c(
        names(index$members), names(index$classes),
        unlist(lapply(index$namespace_indices, function(x) names(x$classes)))
    )
    if (!is.null(lhs$fields) && any(lhs$classes %in% index$record_dispatch_classes)) {
        dispatch_classes <- c(dispatch_classes, lhs$classes)
    }
    if (!isTRUE(lhs$default_dollar) && length(lhs$classes) &&
            !all(lhs$classes %in% c("list", "environment", "data.frame", "R6")) &&
            !any(lhs$classes %in% dispatch_classes)) {
        return(member_value(reason = "unresolved_dollar_dispatch"))
    }
    if (is.null(name)) {
        return(unknown)
    }
    dynamic <- member_lookup(member_lookup(member_lookup(bindings, ".__member_properties__"), lhs$type), name)
    if (!is.null(dynamic)) {
        return(dynamic)
    }
    value <- member_lookup(lhs$fields, name)
    if (!is.null(value)) {
        value <- member_stored_field(lhs, value, index, budget)
        if (length(value$type) || !is.null(value$function_expr) || !is.null(value$function_key) ||
                !is.null(value$r6_owner) || !is.null(value$origin)) {
            return(value)
        }
    }
    if (length(lhs$type) > 1L) {
        return(Reduce(member_join, lapply(lhs$type, function(type) member_access(member_value(type = type), name, index, bindings, budget))))
    }
    property <- member_lookup(member_lookup(index$properties, lhs$type), name)
    if (!is.null(property)) {
        value <- if (is.character(property)) member_value(type = property) else property
        if (is.null(value$member_package) && length(value$type) == 1L && value$type %in% names(index$members)) {
            value$member_package <- index$package
        }
        return(value)
    }
    raw <- member_lookup(member_lookup(index$raw_fields, lhs$type), name)
    if (!is.null(raw)) {
        return(member_value(type = raw, member_package = index$package))
    }
    members <- member_lookup(index$members, lhs$type)
    if (name %in% names(members) && !is.na(members[[name]])) {
        return(member_value(function_key = members[[name]], receiver_value = lhs, metadata = lhs$member_package))
    }
    unknown
}

member_extracted_callable <- function(value) {
    if (!is.null(value$function_expr) || !is.null(value$function_key)) value$extracted_callable <- TRUE
    value
}

member_infer <- function(
  expr, index, bindings = list(), receiver = NULL,
  depth = 0L, trail = character(), budget = NULL, context = NULL
) {
    unknown <- member_value(reason = "unsupported")
    if (missing(expr)) {
        return(member_value(type = ".missing"))
    }
    if (is.null(budget)) {
        budget <- new.env(parent = emptyenv())
        budget$remaining <- 20000L
        budget$exhausted <- FALSE
        budget$transient <- FALSE
    }
    if (isTRUE(budget$exhausted)) return(member_value(reason = "budget"))
    budget$remaining <- budget$remaining - 1L
    # Package installation byte-compiles this engine. In development sessions,
    # start timing after R's first-call JIT compilation, before traversing ASTs.
    if (!is.null(budget$time_limit) && is.null(budget$deadline)) {
        budget$deadline <- proc.time()[[3L]] + budget$time_limit
        budget$next_time_check <- budget$remaining - 128L
    }
    # Reading the clock for every AST node can consume most of the request's
    # time limit itself. Check periodically; node and depth bounds still apply
    # on every visit, and an externally supplied deadline is checked first.
    timed_out <- FALSE
    if (!is.null(budget$deadline) && (is.null(budget$next_time_check) ||
                budget$remaining <= budget$next_time_check)) {
        budget$next_time_check <- budget$remaining - 128L
        timed_out <- member_budget_expired(budget, proc.time())
    }
    depth_limit <- if (is.null(budget$depth_limit)) 64L else budget$depth_limit
    if (isTRUE(budget$exhausted) || depth > depth_limit || budget$remaining < 0L || timed_out) {
        budget$exhausted <- TRUE
        if (timed_out) budget$timed_out <- TRUE
        return(member_value(reason = "budget"))
    }
    infer <- function(x, env = bindings, ctx = context) {
        # Only the initializer's outer body returns its instance effects.
        # Nested expressions and called functions keep ordinary return values.
        if (isTRUE(ctx$r6_initialize)) ctx$r6_initialize <- FALSE
        member_infer(x, index, env, receiver, depth + 1L, trail, budget, ctx)
    }
    join <- if (isTRUE(budget$track_returns)) member_join_return_evidence else member_join
    local_object <- function(env, name) {
        member_local_value(member_lookup(env, name), index, budget, depth, trail, receiver, context)
    }
    qualified <- NULL
    if (is.call(expr) && member_head(expr[[1L]], "::") && identical(expr[[1L]][[2L]], as.name("base"))) {
        target <- member_name(expr[[1L]][[3L]])
        if (length(target) && target %in% c("list", "data.frame", "new.env", "structure")) qualified <- target
    }
    intrinsic <- function(name) {
        if (identical(name, qualified)) return(TRUE)
        at <- bindings$.__member_position__
        history <- member_lookup(index$document_bindings, name)
        shadowed <- length(history) && (is.null(at) || member_history_position(history, at) > 0L)
        !name %in% names(bindings) &&
            !shadowed &&
            ((is.null(member_lookup(index$definitions, name)) && name %in% member_base_intrinsics) ||
                    (name %in% index$intrinsics && (is.null(index$document_bindings) || !is.null(context$key))))
    }
    package_env <- if (!is.null(index$package_roots)) index$package_roots else index$roots
    join_env <- function(a, b) {
        if (identical(a, b)) return(a)
        keys <- union(names(a), names(b))
        xs <- a[match(keys, names(a))]
        ys <- b[match(keys, names(b))]
        stats::setNames(lapply(seq_along(keys), function(i) {
            x <- xs[[i]]
            y <- ys[[i]]
            if (identical(x, y)) return(x)
            if (!is.list(x) || !is.list(y)) return(unknown)
            if (!is.null(x$binding_cache) || !is.null(y$binding_cache)) {
                return(list(binding_choices = list(x, y), binding_cache = new.env(parent = emptyenv())))
            }
            join(x, y)
        }), keys)
    }
    join_effects <- function(a, b) {
        if (is.null(a)) return(b)
        if (is.null(b)) return(a)
        member_join(a, b)
    }
    update_object <- function(env, name, object) {
        identity <- object$r6_identity
        if (!is.null(identity)) {
            for (alias in names(env)) {
                if (is.list(env[[alias]]) && identical(env[[alias]]$r6_identity, identity)) {
                    env[alias] <- list(object)
                }
            }
        }
        env[name] <- list(object)
        env
    }
    # Flow records keep early returns separate from fall-through values.
    flow <- function(node, env) {
        if (isTRUE(budget$exhausted)) {
            return(list(value = member_value(reason = "budget"), env = env,
                    returns = member_value(type = ".never"), falls = TRUE))
        }
        env <- member_s4_effect(node, index, env)
        if (member_head(node, "{")) {
            state <- list(
                value = member_literal(NULL), env = env,
                returns = member_value(type = ".never"), falls = TRUE
            )
            children <- as.list(node)[-1L]
            for (child in children) {
                if (!state$falls || isTRUE(budget$exhausted)) break
                next_state <- flow(child, state$env)
                state$returns <- join(state$returns, next_state$returns)
                if (isTRUE(context$r6_initialize)) {
                    state$r6_returns <- join_effects(state$r6_returns, next_state$r6_returns)
                }
                state$value <- next_state$value
                state$env <- next_state$env
                state$falls <- next_state$falls
            }
            return(state)
        }
        if (member_head(node, "return")) {
            if (!isTRUE(context$r6_initialize)) {
                result <- if (length(node) > 1L) infer(node[[2L]], env) else member_literal(NULL)
                return(list(value = result, env = env, returns = result, falls = FALSE))
            }
            state <- if (length(node) > 1L) flow(node[[2L]], env) else
                list(value = member_literal(NULL), env = env, falls = TRUE)
            returns <- if (is.null(state$returns)) member_value(type = ".never") else state$returns
            result <- if (state$falls) member_join(returns, state$value) else returns
            effects <- state$r6_returns
            if (state$falls) effects <- join_effects(effects, state$env$.__r6_instance__)
            return(list(value = result, env = state$env, returns = result, falls = FALSE, r6_returns = effects))
        }
        if (member_head(node, "if")) {
            cond <- infer(node[[2L]], env)
            yes <- !isTRUE(cond$known_literal) || !isFALSE(cond$literal)
            no <- !isTRUE(cond$known_literal) || !identical(cond$literal, TRUE)
            a <- if (yes) flow(node[[3L]], env) else NULL
            b <- if (no) {
                if (length(node) == 4L) {
                    flow(node[[4L]], env)
                } else {
                    list(
                        value = member_literal(NULL), env = env,
                        returns = member_value(type = ".never"), falls = TRUE
                    )
                }
            } else {
                NULL
            }
            if (is.null(a)) {
                return(b)
            }
            if (is.null(b)) {
                return(a)
            }
            falls <- a$falls || b$falls
            out_env <- if (!a$falls) b$env else if (!b$falls) a$env else join_env(a$env, b$env)
            value <- if (!a$falls) b$value else if (!b$falls) a$value else join(a$value, b$value)
            return(list(
                value = value, env = out_env,
                returns = join(a$returns, b$returns), falls = falls,
                r6_returns = if (isTRUE(context$r6_initialize)) join_effects(a$r6_returns, b$r6_returns)
            ))
        }
        if (member_head(node, "for") || member_head(node, "while")) {
            # A literal named collection copied field-for-field has a finite
            # surface. Other loops retain the conservative invalidation below.
            if (member_head(node, "for") && member_head(node[[3L]], "names") && length(node[[3L]]) == 2L &&
                    intrinsic("names") && !"names" %in% names(env)) {
                body <- unwrap_single_statement(node[[4L]])
                variable <- member_name(node[[2L]])
                if (member_head(body, "<-") && member_head(body[[2L]], "[[") &&
                        member_head(body[[3L]], "[[") && length(body[[2L]]) == 3L && length(body[[3L]]) == 3L &&
                        identical(member_name(body[[2L]][[3L]]), variable) &&
                        identical(member_name(body[[3L]][[3L]]), variable) &&
                        identical(body[[3L]][[2L]], node[[3L]][[2L]])) {
                    target <- member_name(body[[2L]][[2L]])
                    object <- local_object(env, target)
                    source <- infer(node[[3L]][[2L]], env)
                    if (identical(object$type, "environment") && identical(source$type, "list") &&
                            !isTRUE(source$open) && !is.null(source$fields) && length(source$fields) <= 256L) {
                        for (name in unique(names(source$fields))) {
                            if (!member_r6_writable(object, name)) next
                            value <- source$fields[[name]]
                            if (!is.null(value$function_expr) && target %in% names(value$closure)) {
                                value$closure[target] <- NULL
                                value$receiver_name <- target
                            }
                            object$fields[name] <- list(value)
                        }
                        env <- update_object(env, target, object)
                        env[variable] <- list(member_value(type = "character"))
                        return(list(value = member_literal(NULL), env = env,
                                returns = member_value(type = ".never"), falls = TRUE))
                    }
                }
            }
            # Preserve bindings not assigned by the loop. Never assume one
            # iteration represents all iterations of an unknown loop.
            writes <- member_assigned_names(node)
            if (member_head(node, "for")) writes <- c(writes, member_name(node[[2L]]))
            for (name in writes) env[name] <- list(unknown)
            has_return <- FALSE
            member_walk(node, function(x) {
                if (member_head(x, "return")) has_return <<- TRUE
            }, descend_functions = FALSE)
            returns <- member_value(type = ".never")
            if (has_return) {
                returns <- if (isTRUE(budget$track_returns)) flow(node[[length(node)]], env)$returns else unknown
            }
            return(list(
                value = member_literal(NULL), env = env,
                returns = returns, falls = TRUE
            ))
        }
        if (member_head(node, ":=")) {
            bound <- member_s7_bind(node, index, env)
            if (!is.null(bound)) node <- bound
        }
        assignment <- member_head(node, "<-") || member_head(node, "=")
        # Arbitrary reflection can mutate local bindings. Do not retain types
        # through it. Reviewed package metadata wrappers have their own rules.
        if (!assignment && (member_head(node, "assign") || member_head(node, "eval") || member_head(node, "source"))) {
            env <- lapply(env, function(x) unknown)
        }
        value <- infer(if (assignment) node[[3L]] else node, env)
        control <- member_r6_control(node)
        if (!is.null(control)) {
            receiver <- control$receiver
            generator <- infer(as.name(receiver), env)
            if (!is.null(generator$r6_class_locked)) {
                updated <- member_r6_apply_control(generator, control$call, index, env, budget)
                for (alias in c(receiver, member_aliases(attr(env, "member_aliases"), receiver))) env[[alias]] <- updated
            }
        }
        if (assignment) {
            lhs <- node[[2L]]
            if (is.symbol(lhs)) {
                name <- as.character(lhs)
                attr(env, "member_aliases") <- member_alias_assignment(attr(env, "member_aliases"), name, node[[3L]])
                env[name] <- list(value)
            } else if (member_head(lhs, "$") || member_head(lhs, "[[") || member_head(lhs, "@")) {
                name <- member_name(lhs[[2L]])
                if (is.null(name)) {
                    # A nested replacement can change the root but cannot be
                    # treated as a direct write to its outermost field name.
                    root <- member_write_root(lhs)
                    if (member_plain_container(local_object(env, root))) name <- root
                }
                subset <- member_head(lhs, "[[")
                direct <- member_write_field(lhs)
                field <- if (subset) direct else member_name(lhs[[3L]])
                object <- local_object(env, name)
                surface <- if (member_head(lhs, "@")) "slots" else "fields"
                if (surface == "slots" && isTRUE(object$s7)) {
                    reference <- member_s7_reference(object)
                    object <- if (!is.null(direct)) member_write_container(object, direct, value, name, NULL,
                        index, env, slot = TRUE)
                    if (is.null(object)) object <- unknown
                    env[name] <- list(object)
                    if (reference) {
                        for (alias in member_aliases(attr(env, "member_aliases"), name)) env[alias] <- list(object)
                    }
                } else if (surface == "fields" && (member_plain_container(object) || isTRUE(object$s7_exact_class))) {
                    reference <- identical(object$type, "environment") || member_s7_reference(object)
                    object <- if (!is.null(direct)) member_write_container(object, direct, value, name,
                        if (identical(budget$origin_name, direct)) attr(lhs, "member_origin", exact = TRUE), index, env, subset)
                    if (is.null(object)) object <- unknown
                    env[name] <- list(object)
                    if (reference) {
                        for (alias in member_aliases(attr(env, "member_aliases"), name)) env[alias] <- list(object)
                    }
                } else if (!is.null(object[[surface]]) && !is.null(field)) {
                    if (!member_r6_writable(object, field)) {
                        return(list(value = value, env = env, returns = member_value(type = ".never"), falls = TRUE))
                    }
                    object[[surface]][field] <- list(member_write_value(value, name, NULL))
                    env <- update_object(env, name, object)
                } else if (!is.null(name)) {
                    env[name] <- list(unknown)
                }
            } else if (!is.null(setter <- member_s7_property_setter(lhs))) {
                name <- member_name(lhs[[2L]])
                object <- local_object(env, name)
                updated <- member_s7_property_write(setter, object, value, index, env, budget)
                env[[name]] <- if (is.null(updated)) unknown else updated
                if (isTRUE(object$s7) && member_s7_reference(object)) {
                    for (alias in member_aliases(attr(env, "member_aliases"), name)) env[alias] <- env[name]
                }
            } else if (member_head(lhs, "class")) {
                name <- member_name(lhs[[2L]])
                object <- local_object(env, name)
                classes <- member_strings(node[[3L]])
                if (!is.null(object) && length(classes)) {
                    object$type <- classes[[1L]]
                    object$classes <- classes
                    object$default_dollar <- FALSE
                    env[name] <- list(object)
                } else if (!is.null(name)) env[name] <- list(unknown)
            }
            return(list(value = value, env = env, returns = member_value(type = ".never"), falls = TRUE))
        }
        list(
            value = value, env = env, returns = member_value(type = ".never"),
            falls = !identical(value$type, ".never")
        )
    }
    analyze <- function(body, env) {
        state <- flow(body, env)
        if (isTRUE(context$r6_initialize)) {
            return(join_effects(state$r6_returns, if (state$falls) state$env$.__r6_instance__ else NULL))
        }
        if (state$falls) join(state$returns, state$value) else state$returns
    }
    bind_arguments <- function(fn, actuals, env, defaults = TRUE, param_hints = NULL) {
        if (length(param_hints)) lexical <- env
        formals <- as.list(fn[[2L]])
        keys <- names(formals)
        # An open forwarded dots list can fill any formal. Preparation must
        # explore unknown values instead of treating it as one supplied item.
        if (isTRUE(attr(actuals, "member_open_dots"))) {
            for (name in keys) env[name] <- list(unknown)
            for (name in names(param_hints)) env[[name]] <- member_hinted_argument(unknown, param_hints[[name]], lexical)
            if ("..." %in% keys) env["..."] <- list(member_value(type = "list", open = TRUE))
            return(env)
        }
        dots <- match("...", keys)
        before_dots <- if (is.na(dots)) keys else keys[seq_len(dots - 1L)]
        supplied <- names(actuals)
        if (is.null(supplied)) supplied <- rep("", length(actuals))
        matched <- rep(FALSE, length(actuals))
        used <- character()
        for (i in which(nzchar(supplied))) {
            name <- supplied[[i]]
            candidates <- if (name %in% keys) name else before_dots[startsWith(before_dots, name)]
            if (length(candidates) == 1L && candidates != "...") {
                if (candidates %in% used) {
                    return(NULL)
                }
                env[candidates] <- list(actuals[[i]])
                used <- c(used, candidates)
                matched[[i]] <- TRUE
            } else if (length(candidates) > 1L) {
                return(NULL)
            }
        }
        available <- setdiff(before_dots, used)
        for (i in which(!matched & !nzchar(supplied))) {
            if (!length(available)) break
            name <- available[[1L]]
            available <- available[-1L]
            env[name] <- list(actuals[[i]])
            used <- c(used, name)
            matched[[i]] <- TRUE
        }
        if (any(!matched) && is.na(dots)) {
            return(NULL)
        }
        if (!is.na(dots)) env["..."] <- list(member_value(type = "list", elements = actuals[!matched]))
        if (length(param_hints)) {
            for (name in intersect(used, names(param_hints))) {
                env[[name]] <- member_hinted_argument(env[[name]], param_hints[[name]], lexical)
            }
        }
        pending <- setdiff(keys, c(used, "..."))
        if (length(param_hints)) {
            env[pending] <- rep(list(unknown), length(pending))
            forward <- list()
        }
        for (name in pending) {
            env[name] <- list(if (!defaults) {
                unknown
            } else if (identical(formals[[name]], quote(expr = ))) {
                member_value(type = ".missing")
            } else if (length(param_hints)) {
                binding <- member_local_binding(formals[[name]], env)
                dependencies <- intersect(names(binding$binding_env), pending)
                if (length(dependencies)) forward[[name]] <- list(cache = binding$binding_cache, names = dependencies)
                binding
            } else {
                infer(formals[[name]], env)
            })
            if (!is.null(param_hints[[name]])) env[[name]] <- member_hinted_argument(env[[name]], param_hints[[name]], lexical)
        }
        if (length(param_hints)) {
            # Defaults can refer to later formals. Link only actual lexical
            # dependencies once all the argument bindings exist.
            for (entry in forward) entry$cache$defaults <- env[entry$names]
        }
        env
    }
    apply_function <- function(key = NULL, self = NULL, actuals = list(),
        fn = NULL, lexical = package_env, dispatch = NULL, r6_initialize = FALSE, fallback_result = NULL) {
        if (!is.null(key)) {
            aliases <- character()
            while (is.symbol(member_lookup(index$definitions, key))) {
                if (key %in% c(aliases, trail)) {
                    return(member_value(reason = "alias_cycle"))
                }
                aliases <- c(aliases, key)
                key <- as.character(index$definitions[[key]])
            }
            if (key %in% trail) {
                budget$transient <- TRUE
                return(member_value(reason = "recursion"))
            }
            definition <- member_lookup(index$definitions, key)
            fn <- definition
        }
        shape <- member_lookup(index$constructor_types, key)
        if (!is.null(shape)) {
            return(member_value(type = shape, member_package = index$package))
        }
        if (!member_head(fn, "function")) {
            return(unknown)
        }
        # S3 dispatch invokes methods directly, without the ordinary callee
        # shortcut below. Reuse a proven return shape after argument matching.
        summary_key <- if (is.null(self)) key else paste(self$type, key, sep = "|")
        summary <- member_lookup(index$method_results, summary_key)
        if (is.null(summary)) summary <- member_lookup(index$method_results, key)
        if (!is.null(summary) && !isTRUE(attr(actuals, "member_open_dots"))) {
            if (is.null(bind_arguments(fn, actuals, list(), defaults = FALSE))) {
                return(member_value(reason = "argument_matching"))
            }
            return(summary)
        }
        # Package syntax and captures are immutable. A cached invocation has
        # already passed matching; look it up before defaults and delegation.
        cache_values <- c(list(self), actuals)
        cache_key <- if (is.null(budget$origin_name) && is.null(budget$hint_wait) && !is.null(key) && !isTRUE(attr(actuals, "member_open_dots")) &&
                all(vapply(cache_values, member_cacheable, logical(1L)))) {
            digest::digest(list(key, lapply(cache_values, member_shape_key), dispatch), algo = "xxhash64")
        } else {
            NULL
        }
        if (!is.null(cache_key) && isTRUE(budget$track_returns)) cache_key <- paste0("returns:", cache_key)
        if (!is.null(cache_key) && exists(cache_key, index$cache, inherits = FALSE)) {
            return(get(cache_key, index$cache, inherits = FALSE))
        }
        # A native instance binding exposes the inner method's formals. The
        # outer factory takes the pointer and is never called for inference.
        body <- fn[[3L]]
        if (!is.null(self) && !is.null(key) && key %in% index$native_factories) {
            returned <- member_returned_function(fn)
            if (is.null(returned)) {
                return(unknown)
            }
            fn <- returned$fn
        }
        hints <- attr(fn, "member_type_hints", exact = TRUE)
        params <- if (!is.null(hints)) member_parameter_hints(hints, names(fn[[2L]]))
        param_hints <- if (length(params)) params[intersect(names(params), setdiff(names(fn[[2L]]), "..."))]
        env <- bind_arguments(fn, actuals, lexical, param_hints = param_hints)
        if (is.null(env)) {
            return(member_value(reason = "argument_matching"))
        }
        if (!is.null(self)) {
            bound <- member_lookup(index$method_receivers, paste(self$type, key, sep = "|"))
            if (is.null(bound)) bound <- "self"
            env[bound] <- list(self)
        }
        delegated <- member_lookup(index$delegation, key)
        if (!is.null(delegated) && !is.null(self)) {
            scope <- package_env
            scope[delegated$receiver] <- list(self)
            if (!is.null(delegated$variable)) scope[delegated$variable] <- list(member_value(function_key = delegated$original))
            for (name in names(delegated$captures)) scope[name] <- list(infer(delegated$captures[[name]], scope))
            for (node in delegated$prelude) {
                if (member_head(node, "<-") && is.symbol(node[[2L]])) {
                    scope[as.character(node[[2L]])] <- list(infer(node[[3L]], scope))
                }
            }
            env <- utils::modifyList(scope, env)
            for (name in setdiff(names(scope), names(fn[[2L]]))) env[name] <- list(scope[[name]])
        }
        # Only package summaries are cached. Local closures can share syntax
        # while capturing different values and must keep their lexical identity.
        ctor <- member_lookup(index$constructors, key)
        summary_index <- index
        if (!is.null(key) && !is.null(index$document_bindings)) {
            summary_index <- list2env(as.list(index), parent = emptyenv())
            summary_index$document_bindings <- NULL
            summary_index$attached_roots <- NULL
        }
        if (!is.null(ctor)) {
            fields <- list()
            for (name in names(ctor$fields)) {
                recipe <- ctor$fields[[name]]
                factory <- member_lookup(index$members[[ctor$type]], name)
                if (!is.null(factory) && !is.na(factory) && factory %in% index$native_factories) {
                    value <- member_value(function_key = factory, metadata = index$package)
                } else if (isTRUE(recipe$active)) {
                    body <- recipe$expr
                    if (member_head(body, "function")) body <- body[[3L]]
                    value <- member_infer(body, summary_index, c(env, stats::setNames(list(member_value(type = ctor$type)), ctor$target)),
                        depth = depth + 1L, trail = c(trail, key), budget = budget
                    )
                } else {
                    value <- member_infer(recipe$expr, summary_index, env,
                        depth = depth + 1L, trail = c(trail, key), budget = budget
                    )
                }
                fields[name] <- list(value)
            }
            result <- member_value(type = ctor$type, classes = ctor$classes, fields = fields,
                member_package = index$package)
        } else {
            body <- fn[[3L]]
            ctx <- list(key = key, actuals = actuals, formals = names(fn[[2L]]), parameters = fn[[2L]], dispatch = dispatch,
                r6_initialize = r6_initialize,
                namespace_code = !is.null(index$package) && is.null(index$document_bindings))
            if (is.null(key) && !is.null(index$dispatch_document)) ctx$caller <- bindings
            if (!r6_initialize && (length(hints$returns) || !is.null(fallback_result)) && !isTRUE(budget$track_returns)) {
                budget$track_returns <- TRUE
                on.exit(budget$track_returns <- NULL, add = TRUE)
            }
            result <- member_infer(body, summary_index, env,
                depth = depth + 1L,
                trail = if (is.null(key)) trail else c(trail, key), budget = budget, context = ctx
            )
            if (!r6_initialize && length(hints$returns)) {
                result <- member_refine_call_hint(result, hints$returns, index, lexical, budget)
            } else if (!is.null(fallback_result)) {
                result <- member_refine_class_hint(result, fallback_result, index, budget)
            }
        }
        if (!is.null(cache_key) && !budget$exhausted && !isTRUE(budget$transient) && is.null(result$function_expr)) {
            size <- as.numeric(utils::object.size(result))
            if (size <= 1024^2) {
                if (is.null(index$cache$.bytes)) index$cache$.bytes <- 0
                if (length(index$cache) >= 512L || index$cache$.bytes + size > 4 * 1024^2) {
                    rm(list = setdiff(ls(index$cache, all.names = TRUE), ".reserve"), envir = index$cache)
                    index$cache$.bytes <- 0
                }
                if (!is.function(index$cache$.reserve) || index$cache$.reserve(size)) {
                    assign(cache_key, result, index$cache)
                    index$cache$.bytes <- index$cache$.bytes + size
                }
            }
        }
        result
    }
    if (is.symbol(expr)) {
        key <- as.character(expr)
        if (key %in% names(bindings)) {
            value <- bindings[[key]]
            if (!is.null(value$binding_cache)) {
                return(member_local_value(value, index, budget, depth, trail, receiver, context))
            }
            if (!is.null(value$binding_expr)) {
                return(infer(value$binding_expr, value$binding_env))
            }
            return(value)
        }
        if (key %in% names(index$document_bindings) || key %in% names(index$attached_roots) ||
                (!is.null(index$source_workspace) && index$source_workspace$index$has_definition(key, index$source_uri))) {
            value <- member_resolve_document(key, index, bindings, budget, depth, trail)
            if (!is.null(value)) {
                return(value)
            }
        }
        if (identical(key, "self") && !is.null(receiver)) {
            return(member_value(type = receiver))
        }
        if (!is.null(member_lookup(index$definitions, key)) && (is.null(index$document_bindings) ||
                    !is.null(context$key))) {
            return(member_value(function_key = key))
        }
        # Imported roots must be supplied by the document/package lexical
        # context. Never resurrect an unattached or shadowed root by name.
        if (key %in% c("NA", "NA_real_", "NA_integer_", "NA_character_", "TRUE", "FALSE", "NULL")) {
            return(switch(key,
                `TRUE` = member_literal(TRUE),
                `FALSE` = member_literal(FALSE),
                `NULL` = member_literal(NULL),
                unknown
            ))
        }
        return(unknown)
    }
    if (!is.call(expr)) {
        return(member_literal(expr))
    }
    head <- if (is.null(qualified)) member_name(expr[[1L]]) else qualified
    if (is.null(head)) head <- ""
    if (member_head(expr, "(")) {
        return(infer(expr[[2L]]))
    }
    if (head %in% c("{", "if", "return")) {
        return(analyze(expr, bindings))
    }
    if (head == "switch" && intrinsic(head)) {
        selector <- infer(expr[[2L]])
        alternatives <- as.list(expr)[-c(1L, 2L)]
        keys <- names(alternatives)
        position <- NULL
        if (selector$known_literal && length(selector$literal) == 1L) {
            if (is.character(selector$literal)) {
                position <- match(selector$literal, keys)
            } else if (is.numeric(selector$literal)) position <- as.integer(selector$literal)
            if (length(position) == 1L && !is.na(position) && position >= 1L && position <= length(alternatives)) {
                while (position < length(alternatives) && identical(alternatives[[position]], quote(expr = ))) position <- position + 1L
                return(infer(alternatives[[position]]))
            }
            # An unnamed arm is the default for a character selector.
            defaults <- if (is.null(keys)) integer() else which(!nzchar(keys))
            return(if (length(defaults) == 1L) infer(alternatives[[defaults]]) else member_literal(NULL))
        }
        results <- lapply(seq_along(alternatives), function(i) {
            if (identical(alternatives[[i]], quote(expr = ))) return(member_value(type = ".never"))
            infer(alternatives[[i]])
        })
        return(if (length(results)) Reduce(join, results) else member_literal(NULL))
    }
    if (member_head(expr, "function")) {
        package <- if (isTRUE(context$namespace_code)) index$package else NULL
        return(member_value(function_expr = expr, closure = member_function_closure(expr, bindings), metadata = package))
    }
    if (member_is_r6_call(expr, index, bindings)) {
        return(member_r6_shape(expr, index, bindings, budget, depth, trail))
    }
    if (head == "setRefClass" || identical(expr[[1L]], quote(methods::setRefClass))) {
        generator <- member_refclass_generator(expr, index, bindings, budget)
        if (!is.null(generator)) return(generator)
    }
    s4 <- member_s4_call(expr, index, bindings, budget)
    if (!is.null(s4)) return(s4)
    if (head %in% member_s7_intrinsics || (member_head(expr[[1L]], "::") &&
                identical(member_name(expr[[1L]][[2L]]), "S7"))) {
        s7 <- member_s7_call(expr, index, bindings, budget)
        if (!is.null(s7)) return(s7)
    }
    if (member_head(expr, "::") || member_head(expr, ":::")) {
        package <- member_name(expr[[2L]])
        name <- member_name(expr[[3L]])
        private <- member_head(expr, ":::")
        root <- if (!private) member_lookup(index$namespace_roots, paste(package, name, sep = "::"))
        if (!is.null(root)) {
            return(root)
        }
        package_index <- member_namespace_index(index, package)
        if (private) {
            root <- member_lookup(package_index$package_roots, name)
            if (is.null(root) && identical(package, index$package)) root <- member_lookup(index$package_roots, name)
            if (!is.null(root)) return(root)
        }
        if (!is.null(package_index) && (private || name %in% package_index$exports) &&
            name %in% names(package_index$definitions)) {
            return(member_value(
                function_key = name, metadata = package
            ))
        }
        if (identical(package, index$package) && name %in% names(index$definitions) &&
                (private || is.null(index$exports) || name %in% index$exports)) {
            return(member_value(function_key = name))
        }
        return(unknown)
    }
    if (member_head(expr, "$")) {
        lhs <- infer(expr[[2L]])
        out <- member_access(lhs, member_name(expr[[3L]]), index, bindings, budget)
        key <- member_key(expr)
        if (!identical(lhs$reason, "type_hint_union") && !length(out$type) && is.null(out$function_key) && !is.null(key) &&
                key %in% names(index$definitions) && !member_name(expr[[2L]]) %in% names(bindings)) {
            return(member_value(function_key = key))
        }
        return(member_extracted_callable(out))
    }
    if (member_head(expr, "@")) {
        return(member_extracted_callable(member_slot(infer(expr[[2L]]), member_name(expr[[3L]]), index, bindings, budget)))
    }
    if (head %in% c("[[", "[")) {
        if (head == "[[" && is.null(qualified) && member_operator_masked(head, index, bindings)) return(unknown)
        lhs <- infer(expr[[2L]])
        if (identical(lhs$reason, "type_hint_union")) {
            subset <- expr
            subset[[2L]] <- as.name(".__member_subset_receiver__")
            values <- lapply(lhs$possible_returns, function(candidate) {
                env <- bindings
                env$.__member_subset_receiver__ <- candidate
                infer(subset, env)
            })
            return(member_possible_result(values, closed = TRUE))
        }
        if (!is.null(lhs$s7_storage) && head == "[[" && member_s7_reference(lhs)) {
            lhs <- member_s7_environment_storage(lhs, head, index, bindings)
            if (is.null(lhs)) return(unknown)
        }
        elements <- member_elements(lhs)
        selector <- if (length(expr) >= 3L && !identical(expr[[3L]], quote(expr = ))) infer(expr[[3L]]) else unknown
        method <- member_subset_method(lhs, head, index, bindings, budget, depth, trail)
        if (!is.null(method)) {
            env <- bindings
            env$.__member_subset_method__ <- method
            env$.__member_subset_receiver__ <- lhs
            call <- as.call(c(list(as.name(".__member_subset_method__"), as.name(".__member_subset_receiver__")),
                    as.list(expr)[-c(1L, 2L)]))
            return(member_infer(call,
                    index, env, depth = depth + 1L, trail = trail, budget = budget))
        }
        if (isTRUE(selector$known_literal) && length(selector$literal) == 1L) {
            name <- selector$literal
            if (is.character(name)) {
                if (head == "[[") {
                    # Default [[ selects exact storage. Special $ methods can
                    # expose additional names that do not exist in that storage.
                    field <- member_lookup(lhs$fields, name)
                    return(if (is.null(field)) unknown else member_extracted_callable(member_stored_field(lhs, field, index, budget)))
                }
                return(member_access(lhs, name, index, bindings, budget))
            }
            if (head == "[[" && !is.null(elements) && is.numeric(name) &&
                    name >= 1L && name <= length(elements)) {
                return(elements[[name]])
            }
        }
        if (head == "[" && length(lhs$type) == 1L && lhs$type %in% c("double", "integer", "character", "logical", "list", "data.frame")) {
            return(lhs)
        }
        if (!is.null(elements) && length(elements) && head == "[[") {
            return(Reduce(member_join, elements))
        }
        if (!is.null(lhs$element_shape) && head == "[[") {
            return(lhs$element_shape)
        }
        # Registered [ methods are analyzed with their receiver argument.
        classes <- member_classes(lhs, index)
        keys <- paste0(head, ".", classes)
        key <- keys[keys %in% names(index$definitions)]
        if (length(key)) {
            return(apply_function(key[[1L]], actuals = list(lhs, selector)))
        }
        return(unknown)
    }
    if (head %in% c("UseMethod", "NextMethod") && intrinsic(head) && !is.null(context)) {
        if (head == "UseMethod") {
            if (length(expr) < 2L || (length(expr) == 2L && !length(context$formals))) return(unknown)
            generic <- member_strings(expr[[2L]])
            input <- if (length(expr) > 2L) {
                infer(expr[[3L]])
            } else {
                infer(as.name(context$formals[[1L]]))
            }
            closed <- identical(input$reason, "type_hint_union")
            source <- !is.null(index$dispatch_document) && is.null(context$key) && !isTRUE(context$namespace_code)
            if ((source || closed || length(input$type) > 1L) && length(generic) == 1L) {
                inputs <- if (closed) {
                    input$possible_returns
                } else if (length(input$type) > 1L) {
                    lapply(input$type, function(type) member_value(type = type))
                } else {
                    list(input)
                }
                actuals <- context$actuals
                position <- 1L
                if (source || closed) {
                    # The dispatch argument may be supplied by name, in any
                    # order, or explicitly computed by UseMethod(g, object).
                    formal <- if (length(expr) > 2L) member_name(expr[[3L]]) else context$formals[[1L]]
                    matched <- s3_match_arguments(context$parameters, actuals)
                    position <- match(match(formal, context$formals), matched, nomatch = 0L)
                    if (!length(position) || !position) position <- NULL
                }
                values <- lapply(inputs, function(candidate) {
                    classes <- if (source || closed) member_hint_dispatch_classes(candidate, index) else member_classes(candidate, index)
                    if (!length(classes)) return(unknown)
                    if (is.integer(position)) actuals[[position]] <- candidate
                    if (source) {
                        return(member_source_s3_result(generic, classes, actuals, index, context$caller,
                                bindings$.__member_position__, budget, depth, trail))
                    }
                    keys <- paste(generic, s3_dispatch_path(classes), sep = ".")
                    keys <- keys[keys %in% names(index$definitions)]
                    if (!length(keys)) {
                        return(unknown)
                    }
                    apply_function(keys[[1L]], actuals = actuals, dispatch = keys[-1L])
                })
                return(if (closed) member_possible_result(values, closed = TRUE) else Reduce(member_join, values))
            }
            classes <- member_classes(input, index)
            if (!length(classes) || !length(generic)) {
                return(member_value(reason = "open_dispatch", possible_returns = member_lookup(index$generic_returns, generic)))
            }
            keys <- c(paste(generic, classes, sep = "."), paste0(generic, ".default"))
            keys <- keys[keys %in% names(index$definitions)]
        } else {
            keys <- context$dispatch
            if (!length(keys)) {
                return(unknown)
            }
        }
        if (!length(keys)) {
            return(unknown)
        }
        return(apply_function(keys[[1L]], actuals = context$actuals, dispatch = keys[-1L]))
    }
    if (head %in% index$nonreturning_functions && intrinsic(head)) {
        return(member_value(type = ".never"))
    }
    args <- as.list(expr)[-1L]
    callee <- NULL
    if (!intrinsic(head)) {
        callee <- infer(expr[[1L]])
        if (isTRUE(budget$exhausted)) return(member_value(reason = "budget"))
        if (!is.null(callee$function_key) && is.null(callee$metadata)) {
            self <- callee$receiver_value
            key <- if (is.null(self)) callee$function_key else paste(self$type, callee$function_key, sep = "|")
            summary <- member_lookup(index$method_results, key)
            if (is.null(summary)) summary <- member_lookup(index$method_results, callee$function_key)
            if (!is.null(summary) && !any(vapply(args, identical, logical(1L), as.name("...")))) {
                fn <- member_definition(index$definitions, callee$function_key)
                if (member_head(fn, "function")) {
                    actuals <- lapply(args, function(arg) unknown)
                    if (is.null(bind_arguments(fn, actuals, list(), defaults = FALSE))) {
                        return(member_value(reason = "argument_matching"))
                    }
                    return(summary)
                }
            }
        }
    }
    if (head == "missing" && intrinsic(head) && length(args) == 1L &&
        identical(args[[1L]], as.name("..."))) {
        dots <- member_lookup(bindings, "...")
        if (!is.null(dots$elements)) {
            return(member_literal(length(dots$elements) == 0L))
        }
    }
    actuals <- vector("list", length(args))
    names(actuals) <- names(args)
    track_args <- !is.null(callee$function_expr) && !isTRUE(budget$track_returns) && {
        hints <- attr(callee$function_expr, "member_type_hints", exact = TRUE)
        length(hints$params) > 0L || (!is.null(hints) && isTRUE(attr(hints, "roxygen_inherits", exact = TRUE)))
    }
    if (track_args) {
        budget$track_returns <- TRUE
        on.exit(budget$track_returns <- NULL, add = TRUE)
    }
    for (i in seq_along(args)) {
        actuals[[i]] <- if (identical(unname(args[i]), list(quote(expr = )))) member_value(type = ".missing") else infer(args[[i]])
        if (isTRUE(budget$exhausted)) return(member_value(reason = "budget"))
    }
    if (track_args) budget$track_returns <- NULL
    origins <- if (!is.null(budget$origin_name)) attr(expr, "member_origins", exact = TRUE)
    if (length(origins) && head %in% c("list", "list2", "data.frame") && intrinsic(head)) {
        for (i in which(names(args) == budget$origin_name)) {
            source <- origins[[as.character(i)]]
            if (!is.null(source)) actuals[[i]]$origin <- source
        }
    }
    # Expand syntactic dots from a known argument list without evaluating it.
    open_dots <- FALSE
    dots <- vapply(args, identical, logical(1L), as.name("..."))
    if (any(dots)) {
        open_dots <- any(dots & vapply(actuals, function(value) is.null(value$elements) && isTRUE(value$open), logical(1L)))
        pieces <- lapply(seq_along(args), function(i) {
            if (dots[[i]] && !is.null(actuals[[i]]$elements)) return(actuals[[i]]$elements)
            actuals[i]
        })
        actuals <- unlist(pieces, recursive = FALSE)
        if (is.null(actuals)) actuals <- list()
    }
    if (open_dots) attr(actuals, "member_open_dots") <- TRUE
    if (head == "|" && intrinsic(head) && length(actuals) == 2L && isTRUE(member_s7_modes(index)$union)) {
        classes <- lapply(actuals, function(value) member_s7_spec(value, index, bindings, budget, resolve = FALSE))
        if (any(vapply(actuals, function(value) !is.null(value$s7_descriptor), logical(1L))) &&
                !any(vapply(classes, is.null, logical(1L)))) {
            return(member_s7_value(type = "S7_union", s7_descriptor = list(kind = "union", classes = classes)))
        }
    }
    arg <- function(i) if (length(actuals) >= i) actuals[[i]] else unknown
    if (!is.null(head) && intrinsic(head)) {
        # Forwarded open dots have unknown cardinality and argument types.
        # Do not inspect the list placeholder as if it were one real argument.
        if (open_dots) {
            return(if (head %in% c("list", "list2")) {
                member_value(type = "list", open = TRUE)
            } else if (head == "missing") {
                member_value(type = "logical")
            } else {
                unknown
            })
        }
        if (head %in% c("list", "list2")) {
            if (length(actuals) > 256L) {
                return(member_value(type = "list", open = TRUE))
            }
            nms <- names(actuals)
            fields <- if (is.null(nms)) list() else actuals[nzchar(nms)]
            return(member_value(type = "list", fields = fields, elements = actuals))
        }
        if (head == "new.env") {
            return(member_value(type = "environment", fields = list()))
        }
        if (head %in% c("invisible", "identity", "arg_match0", "arg_match", "set_props", "try_fetch", "local", "withAutoprint")) {
            return(arg(1L))
        }
        if (head %in% c("head", "tail") && length(arg(1L)$type) == 1L &&
            arg(1L)$type %in% c("data.frame", "integer", "double", "character", "logical", "list")) {
            return(arg(1L))
        }
        if (head == "tryCatch") {
            result <- arg(1L)
            for (name in intersect(names(actuals), c("error", "warning", "interrupt"))) {
                handler <- actuals[[name]]
                branch <- if (!is.null(handler$function_expr)) {
                    apply_function(
                        fn = handler$function_expr,
                        actuals = list(unknown), lexical = handler$closure
                    )
                } else {
                    unknown
                }
                result <- join(result, branch)
            }
            return(result)
        }
        if (head %in% c("&&", "||") && isTRUE(arg(1L)$known_literal)) {
            if (head == "&&" && isFALSE(arg(1L)$literal)) {
                return(member_literal(FALSE))
            }
            if (head == "||" && identical(arg(1L)$literal, TRUE)) {
                return(member_literal(TRUE))
            }
        }
        if (head == "structure") {
            out <- arg(1L)
            classes <- actuals[["class"]]
            if (!is.null(classes)) {
                class_expr <- member_lookup(args, "class")
                empty_character <- FALSE
                if (is.call(class_expr) && length(class_expr) == 1L) {
                    empty_character <- member_head(class_expr, "character") && intrinsic("character")
                    if (member_head(class_expr[[1L]], "::")) {
                        empty_character <- identical(member_name(class_expr[[1L]][[2L]]), "base") &&
                            identical(member_name(class_expr[[1L]][[3L]]), "character")
                    }
                }
                if (empty_character) classes <- member_literal(character())
                if (!isTRUE(classes$known_literal)) return(unknown)
                if (is.null(classes$literal) || (is.character(classes$literal) && !length(classes$literal))) {
                    out$classes <- NULL
                    out$default_dollar <- NULL
                    out$column_names_only <- NULL
                    return(out)
                }
                if (!is.character(classes$literal) || anyNA(classes$literal)) return(unknown)
                out$type <- classes$literal[[1L]]
                out$classes <- classes$literal
                out$default_dollar <- FALSE
                if (!"data.frame" %in% out$classes) out$column_names_only <- NULL
            }
            return(out)
        }
        if (head == "data.frame") {
            keys <- names(actuals)
            columns <- nzchar(keys) & !keys %in% names(formals(base::data.frame))
            return(member_value(type = "data.frame", classes = "data.frame", fields = actuals[columns]))
        }
        if (head == "length") {
            x <- arg(1L)
            elements <- member_elements(x)
            if (!is.null(elements)) {
                return(member_literal(length(elements)))
            }
            if (x$known_literal) {
                return(member_literal(length(x$literal)))
            }
            return(member_value(type = "integer"))
        }
        if (head == "startsWith" && arg(1L)$known_literal && arg(2L)$known_literal) {
            return(member_literal(startsWith(arg(1L)$literal, arg(2L)$literal)))
        }
        if (head %in% c("c", ":", "seq", "seq_len", "seq_along", "rep", "rep.int")) {
            types <- unique(unlist(lapply(actuals, function(x) x$type)))
            if (any(!vapply(actuals, function(x) length(x$type) > 0L, logical(1L)))) {
                return(unknown)
            }
            type <- if ("character" %in% types) "character" else if (head %in% c(":", "seq_len", "seq_along")) "integer" else if ("double" %in% types) "double" else if ("integer" %in% types) "integer" else if ("logical" %in% types) "logical" else "list"
            if (head == "c" && all(vapply(actuals, function(x) isTRUE(x$known_literal), logical(1L)))) {
                # Concatenate literal data only, never eval the original call.
                return(member_literal(unlist(lapply(actuals, function(x) x$literal), recursive = FALSE)))
            }
            return(member_value(type = type))
        }
        simple_types <- c(
            as.character = "character", character = "character", as.integer = "integer",
            integer = "integer", as.numeric = "double", as.double = "double", numeric = "double",
            double = "double", as.logical = "logical", logical = "logical", as.raw = "raw", raw = "raw",
            charToRaw = "raw", as.Date = "Date", as.POSIXct = "POSIXct", factor = "factor",
            paste = "character", paste0 = "character", sprintf = "character", names = "character"
        )
        if (head %in% names(simple_types)) {
            return(member_value(type = unname(simple_types[[head]])))
        }
        if (head %in% c("is.null", "missing", "isTRUE", "isFALSE", "inherits", "is.character", "is.numeric", "is.integer", "is.logical", "is.list", "is_character", "is_list", "is_bool")) {
            x <- arg(1L)
            if (!length(x$type)) {
                return(member_value(type = "logical"))
            }
            if (length(x$type) != 1L) {
                return(member_value(type = "logical"))
            }
            yes <- switch(head,
                is.null = identical(x$type, "NULL"),
                missing = identical(x$type, ".missing") ||
                    (identical(member_name(args[[1L]]), "...") && identical(x$type, "list") &&
                        !is.null(x$elements) && length(x$elements) == 0L),
                isTRUE = if (x$known_literal) identical(x$literal, TRUE) else NA,
                isFALSE = if (x$known_literal) isFALSE(x$literal) else NA,
                inherits = if (isTRUE(arg(2L)$known_literal)) arg(2L)$literal %in% member_classes(x, index) else NA,
                is.character = identical(x$type, "character"),
                is.numeric = x$type %in% c("double", "integer"),
                is.integer = identical(x$type, "integer"),
                is.logical = identical(x$type, "logical"),
                is.list = identical(x$type, "list"),
                is_character = identical(x$type, "character"),
                is_list = identical(x$type, "list"),
                is_bool = if (x$known_literal) is.logical(x$literal) && length(x$literal) == 1L && !is.na(x$literal) else NA
            )
            return(if (length(yes) == 1L && !is.na(yes)) member_literal(yes) else member_value(type = "logical"))
        }
        if (head == "lapply") {
            xs <- member_elements(arg(1L))
            fn <- arg(2L)
            if (is.null(xs)) {
                input <- if (is.null(arg(1L)$element_shape)) unknown else arg(1L)$element_shape
                value <- if (!is.null(fn$function_expr)) {
                    apply_function(actuals = list(input), fn = fn$function_expr, lexical = fn$closure)
                } else {
                    apply_function(fn$function_key, actuals = list(input))
                }
                return(member_value(type = "list", element_shape = value))
            }
            if (length(xs) > 32L) {
                return(member_value(type = "list"))
            }
            elements <- lapply(xs, function(x) {
                if (!is.null(fn$function_expr)) {
                    apply_function(actuals = list(x), fn = fn$function_expr, lexical = fn$closure)
                } else {
                    apply_function(fn$function_key, actuals = list(x))
                }
            })
            return(member_value(type = "list", elements = elements))
        }
        if (head == "Reduce") {
            fn <- arg(1L)
            xs <- member_elements(arg(2L))
            init <- member_lookup(actuals, "init")
            if (is.null(init) && length(actuals) >= 3L) init <- arg(3L)
            if (is.null(xs)) {
                if (is.null(init)) {
                    return(unknown)
                }
                input <- if (is.null(arg(2L)$element_shape)) unknown else arg(2L)$element_shape
                next_shape <- if (!is.null(fn$function_expr)) {
                    apply_function(
                        fn = fn$function_expr,
                        actuals = list(init, input), lexical = fn$closure
                    )
                } else {
                    unknown
                }
                # Prove a shape-preserving transfer for any number of inputs.
                if (identical(member_shape_key(init), member_shape_key(next_shape))) {
                    return(init)
                }
                return(unknown)
            }
            if (length(xs) > 32L) {
                return(unknown)
            }
            if (is.null(init)) {
                if (!length(xs)) {
                    return(member_literal(NULL))
                }
                init <- xs[[1L]]
                xs <- xs[-1L]
            }
            for (input in xs) {
                init <- if (!is.null(fn$function_expr)) {
                    apply_function(
                        fn = fn$function_expr,
                        actuals = list(init, input), lexical = fn$closure
                    )
                } else {
                    apply_function(fn$function_key, actuals = list(init, input))
                }
            }
            return(init)
        }
    }
    operators <- c("+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "==", "!=", ">", "<", ">=", "<=", "&&", "||")
    if (head %in% operators && intrinsic(head)) {
        keys <- lapply(actuals[seq_len(min(2L, length(actuals)))], function(value) {
            candidates <- paste(head, member_classes(value, index), sep = ".")
            candidates <- candidates[candidates %in% names(index$definitions)]
            if (length(candidates)) candidates[[1L]] else character()
        })
        keys <- unique(unlist(keys))
        if (length(keys) == 1L) {
            return(apply_function(keys[[1L]], actuals = actuals))
        }
        if (length(keys) > 1L) {
            return(member_value(reason = "conflicting_dispatch"))
        }
        if (head %in% c("==", "!=", ">", "<", ">=", "<=") && length(actuals) == 2L &&
            all(vapply(actuals, function(x) x$known_literal && length(x$literal) == 1L && is.atomic(x$literal), logical(1L)))) {
            x <- arg(1L)$literal
            y <- arg(2L)$literal
            return(member_literal(switch(head,
                `==` = x == y,
                `!=` = x != y,
                `>` = x > y,
                `<` = x < y,
                `>=` = x >= y,
                `<=` = x <= y
            )))
        }
        if (head %in% c("!", "&&", "||") && all(vapply(actuals, function(x) x$known_literal && is.logical(x$literal) && length(x$literal) == 1L, logical(1L)))) {
            x <- arg(1L)$literal
            y <- arg(2L)$literal
            return(member_literal(switch(head,
                `!` = !x,
                `&&` = x && y,
                `||` = x || y
            )))
        }
        if (all(vapply(actuals, function(x) length(x$type) == 1L && x$type %in% c("integer", "double", "logical", "character"), logical(1L)))) {
            return(member_value(type = if (head %in% c("+", "-", "*", "/", "^", "%%", "%/%")) "double" else "logical"))
        }
        return(unknown)
    }
    if (is.null(callee)) callee <- infer(expr[[1L]])
    if (length(callee$possible_returns)) {
        if (open_dots) return(unknown)
        # Infer arguments once, then analyse each known callable against its
        # own receiver. Results remain possibilities through further chains.
        env <- bindings
        for (i in seq_along(actuals)) env[paste0(".__possible_arg", i)] <- list(actuals[[i]])
        call <- as.call(c(list(as.name(".__possible_callee__")),
            stats::setNames(lapply(seq_along(actuals), function(i) as.name(paste0(".__possible_arg", i))), names(actuals))))
        values <- lapply(callee$possible_returns, function(candidate) {
            env$.__possible_callee__ <- candidate
            member_infer(call, index, env, depth = depth + 1L, trail = trail, budget = budget)
        })
        return(member_possible_result(values, closed = identical(callee$reason, "type_hint_union")))
    }
    if (identical(callee$metadata, "S7") && callee$function_key %in% member_s7_intrinsics) {
        result <- member_s7_call(expr, index, bindings, budget, callee$function_key)
        if (!is.null(result)) return(result)
    }
    if (!is.null(callee$s7_generator)) {
        return(member_s7_construct(callee, actuals, index, bindings, budget))
    }
    if (!is.null(callee$s7_dispatch_args)) {
        fallback <- callee$s7_return_hint
        if (is.null(fallback) && !isTRUE(callee$s7_typed_methods)) return(unknown)
        if (open_dots) return(unknown)
        supplied <- member_s7_match(actuals, names(callee$function_expr[[2L]]))
        if (is.null(supplied) || !all(callee$s7_dispatch_args %in% names(supplied$args))) return(unknown)
        if (isTRUE(budget$class_hint_probe)) budget$used_method_result <- TRUE
        selected <- member_s7_exact_method(callee, supplied$args, budget, require_certain = TRUE)
        if (is.null(selected$implementation)) return(member_refine_class_hint(unknown, fallback, index, budget))
        callee <- selected$implementation
        if (!is.null(fallback)) callee$s7_call_return <- fallback
    }
    if (!is.null(callee$s4_generator)) {
        return(member_s4_construct(callee$s4_generator, actuals, index, bindings, budget))
    }
    if (!is.null(callee$refclass_instance)) return(callee$refclass_instance)
    if (identical(callee$r6_role, "clone")) {
        args <- bind_arguments(callee$function_expr, actuals, list())
        if (is.null(args) || is.null(callee$receiver_value)) return(unknown)
        return(member_r6_clone_result(callee$receiver_value, args$deep))
    }
    if (!is.null(callee$result_shape)) {
        return(member_r6_construct(callee, actuals, index, budget, depth, trail))
    }
    if (!is.null(callee$metadata)) {
        if (open_dots) return(unknown)
        package_index <- member_namespace_index(index, callee$metadata)
        if (is.null(package_index)) {
            return(unknown)
        }
        fn <- member_definition(package_index$definitions, callee$function_key)
        descriptor <- s3_generic_descriptor(fn)
        if (!is.null(descriptor) && length(package_index$generic_returns[[descriptor$name]])) {
            methods <- union(names(bindings), names(index$document_bindings))
            methods <- methods[startsWith(methods, paste0(descriptor$name, "."))]
            at <- bindings$.__member_position__
            overridden <- any(methods %in% names(bindings)) || any(vapply(methods, function(method) {
                history <- member_lookup(index$document_bindings, method)
                length(history) && (is.null(at) || member_history_position(history, at) > 0L)
            }, logical(1L)))
            if (overridden) {
                package_index <- list2env(as.list(package_index), parent = emptyenv())
                package_index$generic_returns[[descriptor$name]] <- NULL
                package_index$cache <- new.env(parent = emptyenv())
            }
        }
        # Keep package evaluation isolated, but resolve a visible source S3
        # method in the caller's dispatch context before crossing namespaces.
        namespace_call <- function(values) {
            env <- package_index$package_roots
            target <- callee
            target$metadata <- NULL
            env$.__member_callee__ <- target
            arg_names <- sprintf(".__member_arg%d", seq_along(values))
            env[arg_names] <- values
            call <- as.call(c(list(as.name(".__member_callee__")), stats::setNames(lapply(arg_names, as.name), names(values))))
            member_infer(call, package_index, env, depth = depth + 1L, budget = budget)
        }
        if (!is.null(descriptor) && !is.null(index$dispatch_document) && is.null(member_lookup(package_index$definitions, "UseMethod")) &&
                is.null(member_lookup(package_index$package_roots, "UseMethod"))) {
            matched <- s3_match_arguments(fn[[2L]], actuals)
            if (is.null(matched)) return(member_value(reason = "argument_matching"))
            position <- match(match(descriptor$formal, names(fn[[2L]])), matched)
            if (!is.na(position)) {
                input <- actuals[[position]]
                closed <- identical(input$reason, "type_hint_union")
                candidates <- if (closed) input$possible_returns else list(input)
                values <- lapply(candidates, function(candidate) {
                    actuals[[position]] <- candidate
                    classes <- member_hint_dispatch_classes(candidate, index)
                    at <- bindings$.__member_position__
                    if (length(classes) && member_package_s3_overrides(descriptor$name, classes, callee$metadata, index, bindings, at, budget)) {
                        result <- member_source_s3_result(descriptor$name, classes, actuals, index, bindings, at,
                            budget, depth, trail, callee$metadata)
                        if (!is.null(result)) return(result)
                    }
                    namespace_call(actuals)
                })
                return(if (closed) member_possible_result(values, closed = TRUE) else values[[1L]])
            }
        }
        return(namespace_call(actuals))
    }
    if (!is.null(callee$function_expr)) {
        if (isTRUE(budget$class_hint_probe) && !is.null(callee$r6_owner) &&
                length(attr(callee$function_expr, "member_type_hints", exact = TRUE)$returns)) budget$used_method_result <- TRUE
        if (!is.null(callee$function_uri) && !identical(callee$function_uri, index$source_uri)) {
            # Arguments were inferred at the call site. Interpret the source
            # body and its documented class names in the defining file.
            workspace <- index$source_workspace
            if (is.null(workspace)) return(unknown)
            doc <- workspace$documents$get(callee$function_uri, NULL)
            if (is.null(doc) || !identical(doc$version, doc$parse_data$version)) return(unknown)
            index <- member_context_index(workspace, doc$uri, doc, c(Inf, Inf), callee$function_expr)
            if (!is.null(budget$origin_name)) {
                previous <- budget$document
                budget$document <- doc
                on.exit(budget$document <- previous, add = TRUE)
            }
        }
        env <- callee$closure
        if (!is.null(callee$receiver_name)) env[callee$receiver_name] <- list(callee$receiver_value)
        return(apply_function(actuals = actuals, fn = callee$function_expr, lexical = env,
                r6_initialize = isTRUE(callee$r6_initialize), fallback_result = callee$s7_call_return))
    }
    key <- callee$function_key
    # Follow named aliases with the original abstract arguments intact.
    aliases <- character()
    while (!is.null(key) && is.symbol(member_lookup(index$definitions, key))) {
        if (key %in% aliases) {
            return(member_value(reason = "alias_cycle"))
        }
        aliases <- c(aliases, key)
        key <- as.character(index$definitions[[key]])
    }
    apply_function(key, callee$receiver_value, actuals)
}

# Apply only statically described registration effects to a document context.
# The registration call itself is never evaluated. Unknown name/factory stops.
member_registration_effect <- function(expr, index, bindings, budget = NULL) {
    # A registration needs an actual name and factory. Resolving the callee
    # of an argument-free method chain can otherwise consume the request budget.
    if (!is.call(expr) || length(expr) < 2L) {
        return(bindings)
    }
    callee <- member_infer(expr[[1L]], index, bindings, budget = budget)
    rule <- member_lookup(index$registration_rules, callee$function_key)
    if (is.null(rule)) {
        return(bindings)
    }
    args <- as.list(expr)[-1L]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    values <- list()
    positional <- 1L
    for (i in seq_along(args)) {
        name <- nms[[i]]
        if (!nzchar(name)) {
            if (positional > length(rule$formals)) {
                return(bindings)
            }
            name <- rule$formals[[positional]]
            positional <- positional + 1L
        }
        values[name] <- list(member_infer(args[[i]], index, bindings, budget = budget))
    }
    name <- values[[rule$name_arg]]
    factory <- values[[rule$value_arg]]
    if (!isTRUE(name$known_literal) || !is.character(name$literal) || length(name$literal) != 1L || is.null(factory)) {
        return(bindings)
    }
    for (type in rule$owners) {
        env <- bindings
        env$.__factory__ <- factory
        env$.__input__ <- member_value(type = type)
        value <- member_infer(quote(.__factory__(.__input__)), index, env, budget = budget)
        if (length(value$type)) bindings$.__member_properties__[[type]][name$literal] <- list(value)
    }
    bindings
}

member_generic_index <- function(code) {
    index <- new.env(parent = emptyenv())
    index$definitions <- list()
    for (expr in top_level_statements(str2expression(code))) {
        if (member_head(expr, "<-") && is.symbol(expr[[2L]])) {
            index$definitions[as.character(expr[[2L]])] <- list(expr[[3L]])
        }
    }
    index$generic <- TRUE
    index$package <- NULL
    index$cache <- new.env(parent = emptyenv())
    index$roots <- list()
    index$namespace_roots <- list()
    index$members <- list()
    index$properties <- list()
    index$raw_fields <- list()
    index$nonreturning_functions <- "stop"
    index$classes <- list()
    index$intrinsics <- character()
    index$constructors <- list()
    index$constructor_types <- list()
    index$native_factories <- character()
    index$registration_rules <- list()
    index$s4_classes <- list()
    index$methods_attached <- TRUE
    index
}
