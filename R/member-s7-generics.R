# Match the installed factory's formals. The dispatch parameters come from
# the user's declaration and the optional function supplies its exact defaults.
member_s7_generic <- function(expr, method, index, bindings, budget) {
    modes <- member_s7_modes(index)
    factory <- modes$arguments[[method]]
    if (is.null(factory)) return(member_value())
    args <- member_s7_arguments(expr, factory)
    if (!is.character(args$name) || length(args$name) != 1L || is.na(args$name) || !nzchar(args$name) ||
            (method == "deprecated_generic" && is.null(args$when))) return(member_value())
    infer <- function(node) member_infer(node, index, bindings, budget = budget)
    if (method == "deprecated_generic" && !is.null(args$new)) {
        target <- infer(args$new)
        if (any(c("dispatch_args", "fun") %in% names(args)) || is.null(target$s7_dispatch_args)) return(member_value())
        value <- member_value(type = "function", function_expr = target$function_expr)
        value$s7_dispatch_args <- target$s7_dispatch_args
        return(value)
    }
    dispatch <- infer(args$dispatch_args)$literal
    if (!is.character(dispatch) || !length(dispatch) || length(dispatch) > 256L ||
            anyNA(dispatch) || any(!nzchar(dispatch)) || anyDuplicated(dispatch) || "..." %in% dispatch) return(member_value())
    if (is.null(args$fun)) {
        params <- c(dispatch, "...")
        fn <- member_s7_function(stats::setNames(rep(alist(x = ), length(params)), params))
    } else {
        fn <- infer(args$fun)$function_expr
        if (!member_head(fn, "function") || !identical(names(fn[[2L]])[seq_along(dispatch)], dispatch)) return(member_value())
    }
    # The generic itself is an S7 function object. Its available properties
    # are described by the installed class, just like other S7 instances.
    class <- member_lookup(index$namespace_indices, "S7")$s7_capabilities$generic_class
    if (is.null(class)) class <- modes$generic_class
    value <- if (method == "new_generic" && !is.null(class)) member_s7_shape(class) else member_value(type = "function")
    if (isTRUE(value$s7)) {
        value$slots$name <- member_literal(args$name)
        value$slots$dispatch_args <- member_literal(dispatch)
    }
    value$function_expr <- fn
    value$s7_dispatch_args <- dispatch
    # A custom generic can transform its arguments before S7_dispatch().
    if (is.null(args$fun) && method == "new_generic") value$s7_methods <- list()
    value
}

# Replacement assignment resolves the setter binding. In the documented form
# the first argument is the generic binding to which R assigns its result.
member_s7_method_setter <- function(lhs) {
    if (!is.call(lhs) || length(lhs) < 3L || !is.symbol(lhs[[2L]])) return(NULL)
    head <- lhs[[1L]]
    if (identical(head, as.name("method"))) {
        lhs[[1L]] <- as.name("method<-")
    } else if (identical(head, quote(S7::method))) {
        lhs[[1L]] <- quote(S7::`method<-`)
    } else {
        return(NULL)
    }
    lhs
}

# Keep a bounded history of exact source registrations. A union registers
# the same method for each constituent class. An opaque registration can
# replace any earlier entry, while a later exact registration is still known.
member_s7_register_signature <- function(generic, setter, rhs, index, bindings, budget, source = NULL) {
    if (!length(member_s7_dispatch_args(generic))) {
        return(member_s7_s3_generic(generic, member_name(setter[[2L]]), index, bindings))
    }
    if (is.null(generic$s7_methods)) return(generic)
    fn <- if (member_head(rhs, "function")) rhs else member_infer(rhs, index, bindings, budget = budget)$function_expr
    params <- if (member_head(fn, "function")) fn[[2L]]
    if (!isTRUE(budget$s7_implementation) && !length(generic$s7_methods) &&
            !length(setdiff(names(params), c(generic$s7_dispatch_args, "...")))) return(generic)
    args <- member_s7_arguments(setter, setdiff(member_s7_modes(index)$arguments[["method<-"]], "value"))
    signature <- member_infer(args$signature, index, bindings, budget = budget)
    specs <- if (identical(signature$type, "list")) signature$elements else list(signature)
    types <- lapply(specs, function(spec) member_s7_dispatch_keys(member_s7_spec(spec, index, bindings, budget), budget))
    if (is.null(args$signature) || length(types) != length(generic$s7_dispatch_args) || any(!lengths(types))) {
        generic$s7_methods <- list()
        return(generic)
    }
    # A removal or an unknown RHS masks the preceding method for this tuple.
    record <- list(types = types, formals = params)
    if (!is.null(source) && member_head(rhs, "function")) record$source <- source
    generic$s7_methods <- c(utils::tail(generic$s7_methods, 63L), list(record))
    generic
}

member_s7_dispatch_keys <- function(descriptor, budget, depth = 0L) {
    budget$remaining <- budget$remaining - 1L
    if (budget$remaining < 0L || depth > 32L) return(NULL)
    if (identical(descriptor$kind, "any")) return(NA_character_)
    if (identical(descriptor$kind, "union")) {
        keys <- lapply(descriptor$classes, member_s7_dispatch_keys, budget, depth + 1L)
        return(if (all(lengths(keys) > 0L)) unique(unlist(keys, use.names = FALSE)))
    }
    if (identical(descriptor$kind, "class")) {
        return(paste(c(descriptor$package, descriptor$name), collapse = "::"))
    }
    if (identical(descriptor$kind, "base") && length(descriptor$name)) return(descriptor$name[[1L]])
    NULL
}

member_s7_cursor_call <- function(document, point, cursor, recovered) {
    path <- recovered$path
    if (length(path) >= 2L && path[[length(path)]] == 1L) {
        # The indexed scope already contains the complete call, including
        # arguments to the right of the cursor and native pipe expansion.
        node <- recovered$parsed[[path[-length(path)]]]
        if (member_head(node, recovered$sentinel)) return(node)
    }
    full <- recovered$context
    if (is.null(full)) full <- s3_recover_call(document, list(point = point, cursor = cursor), recovered)
    path <- member_cursor_path(full, recovered$sentinel, NULL)
    if (length(path) < 2L || utils::tail(path, 1L) != 1L) return(NULL)
    node <- full[[utils::head(path, -1L)]]
    if (is.call(node)) node else NULL
}

member_s7_call_signature <- function(generic, call, index, bindings, budget) {
    dispatch <- generic$s7_dispatch_args
    args <- member_s7_match(as.list(call)[-1L], c(dispatch, "..."))$args
    if (!all(dispatch %in% names(args))) return(generic)
    keys <- lapply(args[dispatch], function(arg) {
        if (identical(arg, quote(expr = ))) return(NULL)
        value <- member_infer(arg, index, bindings, budget = budget)
        if (!is.null(value$reason) || !is.null(value$s4_class)) return(NULL)
        if (isTRUE(budget$s7_implementation) && (isTRUE(value$possible_receiver) || length(value$possible_returns))) return(NULL)
        # structure() can retain its known base payload while changing the
        # dispatch class. Use the explicit class vector before its storage.
        if (length(value$classes)) return(value$classes[[1L]])
        # A declared method receiver or property may hold a subclass.
        if (isTRUE(value$s7_exact_class)) return(member_s7_dispatch_keys(value$s7_descriptor, budget))
        if (!isTRUE(value$s7) && isTRUE(value$known_literal) && !is.object(value$literal)) return(typeof(value$literal))
        NULL
    })
    if (any(lengths(keys) != 1L)) return(generic)
    for (method in rev(generic$s7_methods)) {
        if (!all(mapply(`%in%`, keys, method$types))) next
        params <- names(method$formals)
        if (identical(utils::head(params, length(dispatch)), dispatch)) {
            if (isTRUE(budget$s7_implementation)) generic$s7_implementation <- method$source
            if (length(setdiff(params, c(dispatch, "...")))) generic$function_expr <- member_s7_function(method$formals)
        }
        break
    }
    generic
}

member_s7_dispatch_args <- function(value) {
    if (!is.null(value$s7_dispatch_args)) return(value$s7_dispatch_args)
    if (!is.null(value$s7_s3_formal)) return(value$s7_s3_formal)
    # An external registration needs only the signature recorded in the
    # descriptor. The target package can be absent from the library.
    if (identical(value$type, "S7_external_generic")) {
        dispatch <- value$fields$dispatch_args$literal
        if (is.character(dispatch) && length(dispatch) <= 256L && !anyNA(dispatch) &&
                all(nzchar(dispatch)) && !anyDuplicated(dispatch)) return(dispatch)
    }
    NULL
}

# S7 can register a method on an ordinary S3 generic such as print().
# Use the verified function body to identify the dispatch argument.
member_s7_s3_generic <- function(generic, symbol, index, bindings) {
    fn <- generic$function_expr
    if (is.null(fn) && !is.null(generic$function_key)) {
        owner <- if (is.null(generic$metadata)) index else member_namespace_index(index, generic$metadata)
        fn <- member_definition(owner$definitions, generic$function_key)
    }
    if (is.null(fn) && is.null(generic$type) && member_s4_unshadowed(symbol, index, bindings) &&
            (is.null(index$source_workspace) || !member_package_shadows(index, symbol))) {
        record <- member_binding(baseenv(), symbol)
        if (identical(record[[1L]], "value") && typeof(record[[2L]]) == "closure") {
            fn <- member_function_syntax(record[[2L]])
        }
    }
    formal <- s3_generic_descriptor(fn)$formal
    if (is.null(formal)) return(NULL)
    if (is.null(generic$type)) generic <- member_value(type = "function")
    generic$function_expr <- fn
    generic$s7_s3_formal <- formal
    generic
}

member_s7_method_formals <- function(expr, fn, index, bindings, budget) {
    lhs <- expr[[2L]]
    setter <- member_s7_method_setter(lhs)
    if (is.null(setter) || !identical(member_s7_method(setter, index, bindings), "method<-")) return(NULL)
    args <- member_s7_arguments(lhs, setdiff(member_s7_modes(index)$arguments[["method<-"]], "value"))
    if (is.null(args$generic) || !identical(args$generic, lhs[[2L]])) return(NULL)
    infer <- function(node) member_infer(node, index, bindings, budget = budget)
    generic <- infer(args$generic)
    dispatch <- member_s7_dispatch_args(generic)
    if (is.null(dispatch)) dispatch <- member_s7_s3_generic(generic, member_name(args$generic), index, bindings)$s7_s3_formal
    if (!length(dispatch) || !identical(names(fn[[2L]])[seq_along(dispatch)], dispatch)) return(NULL)
    signature <- infer(args$signature)
    specs <- if (identical(signature$type, "list")) signature$elements else list(signature)
    if (length(specs) != length(dispatch)) return(NULL)
    shapes <- lapply(specs, function(spec) {
        descriptor <- member_s7_spec(spec, index, bindings, budget)
        member_s7_shape(descriptor, bindings, defaults = FALSE)
    })
    # convert() dispatches on the class supplied as to, and passes that
    # class object itself to the method. Its from argument is an instance.
    convert <- (identical(generic$metadata, "S7") && identical(generic$function_key, "convert")) ||
        (identical(generic$type, "S7_external_generic") && identical(generic$fields$package$literal, "S7") &&
                identical(generic$fields$name$literal, "convert"))
    if (convert && identical(dispatch, c("from", "to"))) {
        target <- specs[[2L]]
        descriptor <- member_s7_spec(target, index, bindings, budget)
        shapes[[2L]] <- switch(descriptor$kind %s3_or% "", class = member_s7_generator(descriptor, bindings),
            base = target, member_value())
    }
    stats::setNames(shapes, dispatch)
}

# Identify the callback by argument matching. Only an inline property within
# the declaring class supplies an owner for a getter or setter's first formal.
member_s7_callback_formals <- function(node, position, root, path, index, bindings, budget) {
    method <- member_s7_method(node, index, bindings)
    if (!isTRUE(method %in% c("new_class", "new_property"))) return(NULL)
    keys <- member_s7_modes(index)$arguments
    # Match positions, so two identical callback expressions can still
    # occupy different roles in the same declaration.
    indices <- function(call, name) {
        args <- as.list(call)[-1L]
        member_s7_match(stats::setNames(as.list(seq_along(args) + 1L), names(args)), keys[[name]])$args
    }
    positions <- indices(node, method)
    role <- names(positions)[vapply(positions, identical, logical(1L), position)]
    if (length(role) != 1L || !role %in% c("getter", "setter", "validator")) return(NULL)
    formals <- names(node[[position]][[2L]])
    if (!length(formals)) return(NULL)
    shapes <- list()
    if (method == "new_property") {
        if (!is.null(positions$class) && role %in% c("validator", "setter") && "value" %in% formals) {
            class <- member_infer(node[[positions$class]], index, bindings, budget = budget)
            descriptor <- member_s7_spec(class, index, bindings, budget)
            shapes$value <- member_s7_shape(descriptor, bindings, defaults = FALSE)
        }
        if (role %in% c("getter", "setter") && "name" %in% formals) shapes$name <- member_value(type = "character")
        if (role == "validator" || length(path) < 2L) return(shapes)
        properties <- root[[utils::head(path, -1L)]]
        if (!member_s4_literal_call(properties, "list", index, bindings)) return(shapes)
        section <- path[[length(path) - 1L]]
        path <- utils::head(path, -2L)
        node <- if (length(path)) root[[path]] else root
        if (!identical(member_s7_method(node, index, bindings), "new_class") ||
                !identical(indices(node, "new_class")$properties, section)) return(shapes)
    }
    if (length(path) && utils::tail(path, 1L) == 3L) {
        outer <- utils::head(path, -1L)
        binding <- if (length(outer)) root[[outer]] else root
        bound <- member_s7_bind(binding, index, bindings)
        if (!is.null(bound)) node <- bound[[3L]]
    }
    if ("self" %in% formals) {
        class <- member_infer(node, index, bindings, budget = budget)$s7_generator
        if (!is.null(class)) shapes$self <- member_s7_shape(class, bindings, defaults = FALSE)
    }
    shapes
}
