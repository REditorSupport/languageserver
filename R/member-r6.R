# Recognize declarative R6 definitions without creating an instance or running
# initialize/active functions. Only explicit package calls or verified imports
# qualify; a document function called R6Class has ordinary function semantics.
member_is_r6_call <- function(expr, index, bindings) {
    if (!is.call(expr)) {
        return(FALSE)
    }
    head <- expr[[1L]]
    if (member_head(head, "::")) {
        return(identical(member_name(head[[2L]]), "R6") &&
                identical(member_name(head[[3L]]), "R6Class"))
    }
    identical(member_name(head), "R6Class") && !"R6Class" %in% names(bindings) &&
        !"R6Class" %in% names(index$document_bindings) &&
        isTRUE(index$r6_attached)
}

member_r6_shape <- function(expr, index, bindings, budget) {
    args <- as.list(expr)[-1L]
    active <- args$active
    inherited <- member_value()
    if (!is.null(args$inherit)) {
        generator <- member_infer(args$inherit, index, bindings, budget = budget)
        inherited <- generator$fields$new$result_shape
    }
    fields <- if (is.null(inherited$fields)) list() else inherited$fields
    # R6's super environment contains ancestor methods and active properties,
    # including private methods, but does not expose ancestor data fields.
    methods <- c(inherited$fields, inherited$r6_private$fields)
    methods <- Filter(function(value) !is.null(value$function_expr) || identical(value$reason, "active_property"), methods)
    super <- member_value(type = "environment", fields = methods)
    # Private fields are available inside methods, outside the public surface.
    private <- if (is.null(inherited$r6_private$fields)) list() else inherited$r6_private$fields
    own <- list(public = character(), private = character())
    for (section in c("private", "public")) {
        node <- args[[section]]
        if (!member_head(node, "list")) next
        values <- as.list(node)[-1L]
        for (name in names(values)) {
            value <- member_infer(values[[name]], index, bindings, budget = budget)
            if (!is.null(value$function_expr)) {
                value$receiver_name <- "self"
                value$closure$self <- NULL
                value$closure$super <- super
            }
            if (section == "public") {
                fields[name] <- list(value)
            } else {
                private[name] <- list(value)
            }
            own[[section]] <- c(own[[section]], name)
        }
    }
    if (member_head(active, "list")) {
        for (name in names(as.list(active)[-1L])) {
            fields[name] <- list(member_value(reason = "active_property"))
        }
    }
    private_value <- member_value(type = "environment", fields = private)
    for (name in own$public) {
        if (!is.null(fields[[name]]$function_expr)) fields[[name]]$closure$private <- private_value
    }
    for (name in own$private) {
        if (!is.null(private[[name]]$function_expr)) private[[name]]$closure$private <- private_value
    }
    instance <- member_value(type = "environment", fields = fields, open = TRUE,
        r6_private = member_value(type = "environment", fields = private), r6_super = super)
    generator <- member_value(type = "environment", fields = list(new = member_value(
        type = "function", result_shape = instance
    )))
    generator$fields$new$r6_initialize <- fields$initialize
    generator
}

# Initialization is inspected as syntax with inert self/private shapes. The
# declared surface survives opaque initialization; proven field assignments and
# finite named-list copies can add methods without constructing a live object.
member_r6_construct <- function(callee, actuals, index, budget, depth, trail) {
    instance <- callee$result_shape
    package <- callee$r6_package
    if (!is.null(package)) {
        package_index <- member_lookup(index$namespace_indices, package)
        if (!is.null(package_index)) index <- package_index
        if (!identical(package, index$package)) return(instance)
        if (!is.null(index$document_bindings)) {
            index <- list2env(as.list(index), parent = emptyenv())
            index$document_bindings <- index$attached_roots <- NULL
        }
    }
    initialize <- callee$r6_initialize
    fn <- initialize$function_expr
    if (!member_head(fn, "function")) return(instance)
    fn[[3L]] <- as.call(list(as.name("{"), fn[[3L]], as.name("self")))
    env <- index$package_roots
    for (name in names(initialize$closure)) env[name] <- initialize$closure[name]
    env$self <- instance
    env$private <- instance$r6_private
    env$.__r6_initialize__ <- member_value(function_expr = fn, closure = env)
    for (i in seq_along(actuals)) env[paste0(".__r6_arg", i)] <- list(actuals[[i]])
    call <- as.call(c(list(as.name(".__r6_initialize__")),
            stats::setNames(lapply(seq_along(actuals), function(i) as.name(paste0(".__r6_arg", i))), names(actuals))))
    result <- member_infer(call, index, env, budget = budget, depth = depth + 1L, trail = trail)
    if (identical(result$type, "environment") && !is.null(result$fields)) {
        for (name in names(result$fields)) instance$fields[name] <- result$fields[name]
    }
    instance
}

member_r6_context <- function(expr, index, bindings, budget) {
    instance <- member_r6_shape(expr, index, bindings, budget)$fields$new$result_shape
    super <- instance$r6_super
    super$r6_self <- instance
    private <- instance$r6_private
    private$r6_self <- instance
    list(self = instance, super = super, private = private)
}

member_r6_runtime_shape <- function(generator, index, trail = list()) {
    if (length(trail) > 16L || any(vapply(trail, identical, logical(1L), generator))) {
        return(member_value(reason = "inheritance_cycle"))
    }
    get_plain <- function(name) member_binding(generator, name)[[2L]]
    public <- get_plain("public_methods")
    public_fields <- get_plain("public_fields")
    private_methods <- get_plain("private_methods")
    private_fields <- get_plain("private_fields")
    active <- get_plain("active")
    inherited <- get_plain("inherit")
    if (!is.list(public) || is.object(public)) {
        return(member_value(reason = "unsupported_r6"))
    }
    convert <- function(values) {
        as.call(c(list(as.name("list")), lapply(values, function(x) {
            if (typeof(x) == "closure") {
                member_function_syntax(x)
            } else if (!is.object(x) && is.atomic(x) && length(x) <= 100L) {
                x
            } else {
                as.name(".__unknown_field__")
            }
        })))
    }
    bindings <- list()
    # Inheritance may be an expression resolved in the generator's parent scope.
    # Read a direct binding only; never evaluate get_inherit() or an expression.
    if (is.symbol(inherited)) {
        scope <- get_plain("parent_env")
        if (is.environment(scope)) {
            record <- member_binding(scope, as.character(inherited))
            parent <- record[[2L]]
            if (identical(record[[1L]], "value") && is.environment(parent)) {
                bindings$.__r6_parent__ <- member_r6_runtime_shape(parent, index, c(trail, list(generator)))
            }
        }
    }
    expr <- as.call(list(as.call(list(as.name("::"), as.name("R6"), as.name("R6Class"))),
            classname = "inspected", public = convert(c(public_fields, public)),
            private = convert(c(private_fields, private_methods)),
            active = convert(active), inherit = as.name(".__r6_parent__")
        ))
    shape <- member_r6_shape(expr, index, bindings, NULL)
    shape$fields$new$r6_package <- index$package
    shape
}
