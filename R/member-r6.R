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

member_r6_shape <- function(expr, index, bindings, budget, depth = 0L, trail = character()) {
    infer <- function(expr) {
        member_infer(expr, index, bindings,
            budget = budget, depth = depth + 1L, trail = trail)
    }
    args <- as.list(expr)[-1L]
    classname <- args$classname
    if (is.null(classname) && length(args) && (is.null(names(args)) || !nzchar(names(args)[[1L]]))) {
        classname <- args[[1L]]
    }
    if (!is.character(classname) && !is.null(classname)) {
        classname <- infer(classname)$literal
    }
    class <- list(name = if (is.character(classname) && length(classname) == 1L) classname else NULL,
        package = if (is.null(index$document_bindings)) index$package else "")
    active <- args$active
    inherited <- member_value()
    if (!is.null(args$inherit)) {
        generator <- infer(args$inherit)
        inherited <- generator$fields$new$result_shape
    }
    fields <- if (is.null(inherited$fields)) list() else inherited$fields
    bindings_public <- inherited$r6_bindings
    bindings_private <- inherited$r6_private$r6_bindings
    # R6's super environment contains ancestor methods and active properties,
    # including private methods, but does not expose ancestor data fields.
    methods <- c(inherited$fields, inherited$r6_private$fields)
    methods <- Filter(function(value) !is.null(value$function_expr) || identical(value$reason, "active_property"), methods)
    super <- member_value(type = "environment", fields = methods,
        r6_class = inherited$r6_class, r6_role = "super")
    # Private fields are available inside methods, outside the public surface.
    private <- if (is.null(inherited$r6_private$fields)) list() else inherited$r6_private$fields
    own <- list(public = character(), private = character())
    for (section in c("private", "public")) {
        node <- args[[section]]
        if (!member_head(node, "list")) next
        values <- as.list(node)[-1L]
        for (name in names(values)) {
            value <- infer(values[[name]])
            value$r6_owner <- class
            value$r6_member_kind <- paste(section,
                if (!is.null(value$function_expr) || "function" %in% value$type) "method" else "field")
            if (!is.null(value$function_expr)) {
                value$receiver_name <- "self"
                value$closure$self <- NULL
                value$closure$super <- super
            }
            if (section == "public") {
                fields[name] <- list(value)
                bindings_public[name] <- if (is.null(value$function_expr)) "data" else "method"
            } else {
                private[name] <- list(value)
                bindings_private[name] <- if (is.null(value$function_expr)) "data" else "method"
            }
            own[[section]] <- c(own[[section]], name)
        }
    }
    if (member_head(active, "list")) {
        for (name in names(as.list(active)[-1L])) {
            value <- member_value(reason = "active_property")
            value$r6_owner <- class
            value$r6_member_kind <- "active binding"
            fields[name] <- list(value)
            bindings_public[name] <- "active"
        }
    }
    # R6 locks instance environments before initialize() runs. Only an explicit
    # FALSE can allow new bindings; an unknown setting remains conservative.
    locked <- is.null(args$lock_objects) ||
        !identical(infer(args$lock_objects)$literal, FALSE)
    private_value <- member_value(type = "environment", fields = private,
        r6_class = class, r6_role = "private")
    private_value$r6_locked <- locked
    private_value$r6_bindings <- bindings_private
    for (name in own$public) {
        if (!is.null(fields[[name]]$function_expr)) fields[[name]]$closure$private <- private_value
    }
    for (name in own$private) {
        if (!is.null(private[[name]]$function_expr)) private[[name]]$closure$private <- private_value
    }
    instance <- member_value(type = "environment", fields = fields, open = TRUE,
        r6_private = member_value(type = "environment", fields = private,
            r6_class = class, r6_role = "private"), r6_super = super,
        r6_class = class, r6_role = "instance")
    instance$r6_private$r6_locked <- locked
    instance$r6_private$r6_bindings <- bindings_private
    instance$r6_locked <- locked
    instance$r6_bindings <- bindings_public
    instance$r6_cloneable <- !identical(args$cloneable, FALSE) &&
        !identical(inherited$r6_cloneable, FALSE)
    # new() forwards its arguments to the public (possibly inherited)
    # initialize method. Preserve defaults as syntax, without evaluating them.
    unknown_parent <- !is.null(args$inherit) && !identical(args$inherit, quote(NULL)) &&
        is.null(inherited$r6_class)
    initialize <- fields$initialize
    has_initializer <- !is.null(initialize) &&
        (!length(initialize$type) || "function" %in% initialize$type)
    constructor <- if (has_initializer || unknown_parent) quote(function(...) NULL) else quote(function() NULL)
    if (member_head(fields$initialize$function_expr, "function")) {
        constructor[[2L]] <- fields$initialize$function_expr[[2L]]
    }
    generator <- member_value(type = "environment", fields = list(new = member_value(
        type = "function", function_expr = constructor, result_shape = instance,
        r6_class = class, r6_role = "constructor"
    )), r6_class = class, r6_role = "generator")
    generator$fields$new$r6_initialize <- fields$initialize
    generator
}

# Members inferred from initialize() or inert runtime fields may not have a
# declaration owner. Use their R6 receiver when available, without changing
# the member's own type or class (a field can itself contain an R6 instance).
member_r6_member_info <- function(value, receiver) {
    if (is.null(receiver$r6_class) || !is.null(value$r6_owner) ||
            !any(receiver$r6_role %in% c("instance", "private", "super"))) return(value)
    value$r6_owner <- receiver$r6_class
    kind <- if (identical(value$reason, "active_property")) {
        "active binding"
    } else {
        paste(if (identical(receiver$r6_role, "private")) "private" else "public",
            if (!is.null(value$function_expr) || "function" %in% value$type) "method" else "field")
    }
    value$r6_member_kind <- kind
    value
}

# Binding kinds come from declarations, not their current values. R6 locks
# methods independently of lock_objects; active writes invoke opaque setters.
# A writable data field remains writable after receiving a function.
member_r6_writable <- function(object, name) {
    !any(member_lookup(object$r6_bindings, name) %in% c("method", "active")) &&
        (!isTRUE(object$r6_locked) || name %in% names(object$fields))
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
    # Track the original instance independently of the self binding and the
    # initializer's ignored return value. Aliases carry the same inert identity.
    budget$r6_identity <- if (is.null(budget$r6_identity)) 1L else budget$r6_identity + 1L
    instance$r6_identity <- budget$r6_identity
    fn[[3L]] <- as.call(list(as.name("{"), fn[[3L]]))
    env <- index$package_roots
    for (name in names(initialize$closure)) env[name] <- initialize$closure[name]
    env$self <- instance
    env$.__r6_instance__ <- instance
    env$private <- instance$r6_private
    env$.__r6_initialize__ <- member_value(function_expr = fn, closure = env)
    env$.__r6_initialize__$r6_initialize <- TRUE
    for (i in seq_along(actuals)) env[paste0(".__r6_arg", i)] <- list(actuals[[i]])
    call <- as.call(c(list(as.name(".__r6_initialize__")),
            stats::setNames(lapply(seq_along(actuals), function(i) as.name(paste0(".__r6_arg", i))), names(actuals))))
    result <- member_infer(call, index, env, budget = budget, depth = depth + 1L, trail = trail)
    if (identical(result$type, "environment") && !is.null(result$fields)) {
        for (name in names(result$fields)) instance$fields[name] <- result$fields[name]
    }
    instance$r6_identity <- NULL
    instance
}

member_r6_context <- function(expr, index, bindings, budget) {
    args <- as.list(expr)[-1L]
    instance <- member_r6_shape(expr, index, bindings, budget)$fields$new$result_shape
    super <- instance$r6_super
    super$r6_self <- instance
    private <- instance$r6_private
    private$r6_self <- instance
    scope <- list(self = instance)
    if (length(private$fields)) scope$private <- private
    if (!is.null(args$inherit) && !identical(args$inherit, quote(NULL))) scope$super <- super
    if (any(instance$r6_bindings == "active")) {
        scope$.__active__ <- member_value(type = "list", r6_class = instance$r6_class, r6_role = "active")
    }
    if (identical(args$portable, FALSE)) {
        # Non-portable methods enclose the public environment, whose parent
        # contains private bindings. Public bindings win on lookup.
        scope <- utils::modifyList(private$fields, utils::modifyList(instance$fields, scope))
        scope$.__enclos_env__ <- member_value(type = "environment", r6_class = instance$r6_class, r6_role = "enclosure")
        if (isTRUE(instance$r6_cloneable)) {
            scope$clone <- member_value(function_expr = quote(function(deep = FALSE) NULL),
                r6_class = instance$r6_class, r6_role = "clone")
        }
    }
    scope
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
            classname = get_plain("classname"), public = convert(c(public_fields, public)),
            private = convert(c(private_fields, private_methods)),
            active = convert(active), inherit = if (is.null(inherited)) NULL else as.name(".__r6_parent__"),
            lock_objects = get_plain("lock_objects"), cloneable = get_plain("cloneable")
        ))
    shape <- member_r6_shape(expr, index, bindings, NULL)
    shape$fields$new$r6_package <- index$package
    shape
}
