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
    value
}

# Replacement assignment resolves the setter binding. In the documented form
# the first argument is the generic binding to which R assigns its result.
member_s7_method_setter <- function(lhs) {
    if (!is.call(lhs) || length(lhs) < 3L || !is.symbol(lhs[[2L]])) return(NULL)
    head <- lhs[[1L]]
    if (identical(head, as.name("method"))) return(quote(`method<-`()))
    if (identical(head, quote(S7::method))) return(quote(S7::`method<-`()))
    NULL
}

member_s7_dispatch_args <- function(value) {
    if (!is.null(value$s7_dispatch_args)) return(value$s7_dispatch_args)
    # An external registration needs only the signature recorded in the
    # descriptor. The target package can be absent from the library.
    if (identical(value$type, "S7_external_generic")) {
        dispatch <- value$fields$dispatch_args$literal
        if (is.character(dispatch) && length(dispatch) <= 256L && !anyNA(dispatch) &&
                all(nzchar(dispatch)) && !anyDuplicated(dispatch)) return(dispatch)
    }
    NULL
}

member_s7_method_formals <- function(expr, fn, index, bindings, budget) {
    lhs <- expr[[2L]]
    setter <- member_s7_method_setter(lhs)
    if (is.null(setter) || !identical(member_s7_method(setter, index, bindings), "method<-")) return(NULL)
    args <- member_s7_arguments(lhs, setdiff(member_s7_modes(index)$arguments[["method<-"]], "value"))
    if (is.null(args$generic) || !identical(args$generic, lhs[[2L]])) return(NULL)
    infer <- function(node) member_infer(node, index, bindings, budget = budget)
    dispatch <- member_s7_dispatch_args(infer(args$generic))
    if (!length(dispatch) || !identical(names(fn[[2L]])[seq_along(dispatch)], dispatch)) return(NULL)
    signature <- infer(args$signature)
    specs <- if (identical(signature$type, "list")) signature$elements else list(signature)
    if (length(specs) != length(dispatch)) return(NULL)
    stats::setNames(lapply(specs, function(spec) {
        descriptor <- member_s7_spec(spec, index, bindings, budget)
        member_s7_shape(descriptor, bindings, defaults = FALSE)
    }), dispatch)
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
