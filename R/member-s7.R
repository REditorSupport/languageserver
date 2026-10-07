# S7 descriptors are read as inert data. Source declarations are summarized
# from syntax; constructors, validators, getters and defaults are never called.
member_s7_intrinsics <- c("new_class", "new_property", "new_union", "new_object", "prop", "set_props")

member_s7_value <- function(..., s7 = FALSE, s7_descriptor = NULL, s7_generator = NULL, s7_property = NULL) {
    value <- member_value(...)
    value$s7 <- s7
    value$s7_descriptor <- s7_descriptor
    value$s7_generator <- s7_generator
    value$s7_property <- s7_property
    value
}

member_s7_method <- function(expr, index, bindings) {
    if (!is.call(expr)) return(NULL)
    head <- expr[[1L]]
    if (member_head(head, "::")) {
        if (identical(member_name(head[[2L]]), "S7")) return(member_name(head[[3L]]))
        return(NULL)
    }
    name <- member_name(head)
    if (is.null(name) || !name %in% member_s7_intrinsics || name %in% names(bindings)) return(NULL)
    at <- bindings$.__member_position__
    history <- member_lookup(index$document_bindings, name)
    if (length(history) && (is.null(at) || any(vapply(history, function(item) member_before(item$end, at), logical(1L))))) return(NULL)
    attached <- member_lookup(index$attached_roots, name)
    if ((!is.null(attached$metadata) && !identical(attached$metadata, "S7")) ||
            (is.null(index$document_bindings) && name %in% names(index$definitions) && !identical(index$package, "S7"))) return(NULL)
    if (isTRUE(index$s7_attached) || name %in% index$intrinsics || identical(attached$metadata, "S7")) name else NULL
}

member_s7_arguments <- function(expr, keys) {
    args <- as.list(expr)[-1L]
    if (any(vapply(args, function(arg) identical(arg, quote(expr = )), logical(1L)))) return(NULL)
    supplied <- names(args)
    if (is.null(supplied)) supplied <- rep("", length(args))
    result <- list()
    for (i in which(nzchar(supplied))) {
        key <- supplied[[i]]
        candidates <- if (key %in% keys) key else keys[startsWith(keys, key)]
        if (length(candidates) != 1L || candidates %in% names(result)) return(NULL)
        result[candidates] <- args[i]
    }
    available <- setdiff(keys, names(result))
    positional <- which(!nzchar(supplied))
    if (length(positional) > length(available)) return(NULL)
    for (i in seq_along(positional)) result[available[[i]]] <- args[positional[[i]]]
    result
}

member_s7_function <- function(formals) {
    syntax <- function(node) {
        if (missing(node)) return(quote(expr = ))
        if (typeof(node) == "closure") {
            name <- attr(node, "name", exact = TRUE)
            return(if (is.character(name) && length(name) == 1L) as.name(name) else as.name(".__unknown_s7_default__"))
        }
        if (is.call(node)) return(as.call(lapply(as.list(node), syntax)))
        node
    }
    formals <- lapply(formals, function(node) if (identical(node, quote(expr = ))) quote(expr = ) else syntax(node))
    as.call(list(as.name("function"), as.pairlist(formals), NULL))
}

member_s7_type <- function(descriptor) {
    if (identical(descriptor$kind, "union")) return(unique(unlist(lapply(descriptor$classes, member_s7_type))))
    if (identical(descriptor$kind, "any")) return("ANY")
    descriptor$name
}

member_s7_shape <- function(descriptor, bindings = list()) {
    if (is.null(descriptor)) return(member_value(reason = "unknown_s7_class"))
    if (identical(descriptor$kind, "union")) {
        shapes <- lapply(descriptor$classes, member_s7_shape, bindings)
        return(if (length(shapes)) Reduce(member_join, shapes) else member_value())
    }
    if (!identical(descriptor$kind, "class")) {
        return(member_value(type = if (!identical(descriptor$kind, "any")) descriptor$name))
    }
    properties <- descriptor$properties
    slots <- lapply(properties, function(property) {
        value <- member_s7_shape(property$class, bindings)
        if (!property$getter && !is.null(property$default)) {
            value$binding_expr <- property$default
            value$binding_env <- bindings
        }
        value
    })
    types <- lapply(properties, function(property) {
        type <- member_s7_type(property$class)
        if (length(type)) type else "ANY"
    })
    member_s7_value(type = descriptor$name, slots = slots, slot_types = types, s7 = TRUE,
        s7_descriptor = descriptor)
}

member_s7_generator <- function(descriptor, bindings = list()) {
    shape <- member_s7_shape(descriptor, bindings)
    constructor <- member_s7_value(type = "function", function_expr = descriptor$constructor,
        closure = bindings, s7_generator = descriptor)
    # Class objects themselves expose descriptor properties via @.
    constructor$slots <- list(name = member_literal(descriptor$name),
        parent = if (!is.null(descriptor$parent)) member_s7_generator(descriptor$parent, bindings) else member_literal(NULL),
        package = member_literal(descriptor$package), abstract = member_literal(isTRUE(descriptor$abstract)),
        properties = member_value(type = "list", fields = shape$slots),
        constructor = member_value(type = "function", function_expr = descriptor$constructor,
            closure = bindings),
        validator = member_value(type = "function"))
    constructor$slot_types <- lapply(constructor$slots, function(value) if (length(value$type)) value$type else "ANY")
    constructor$s7 <- TRUE
    constructor$s7_descriptor <- descriptor
    constructor
}

member_s7_default <- function(descriptor) {
    if (identical(descriptor$kind, "union")) {
        return(if (length(descriptor$classes)) member_s7_default(descriptor$classes[[1L]]))
    }
    if (identical(descriptor$kind, "class")) return(as.call(list(as.name(descriptor$name))))
    descriptor$default
}

member_s7_call <- function(expr, index, bindings, budget, method = member_s7_method(expr, index, bindings)) {
    if (is.null(method) || !method %in% member_s7_intrinsics) return(NULL)
    nesting <- budget$s7_depth
    if (is.null(nesting)) nesting <- 0L
    if (nesting >= 32L) return(member_value(reason = "s7_recursion"))
    budget$s7_depth <- nesting + 1L
    on.exit(budget$s7_depth <- nesting)
    infer <- function(node) member_infer(node, index, bindings, budget = budget)
    if (method == "new_union") {
        values <- lapply(as.list(expr)[-1L], infer)
        classes <- lapply(values, function(value) value$s7_descriptor)
        if (!length(classes) || length(classes) > 256L || any(vapply(classes, is.null, logical(1L)))) return(member_value())
        return(member_s7_value(type = "S7_union", s7_descriptor = list(kind = "union", classes = classes)))
    }
    if (method == "new_property") {
        args <- member_s7_arguments(expr, c("class", "getter", "setter", "validator", "default", "name"))
        if (is.null(args)) return(member_value())
        class <- if (is.null(args$class)) list(kind = "any") else infer(args$class)$s7_descriptor
        default <- args$default
        if (member_head(default, "quote") && length(default) == 2L) default <- default[[2L]]
        getter <- if (!is.null(args$getter)) infer(args$getter)
        setter <- if (!is.null(args$setter)) infer(args$setter)
        return(member_s7_value(type = "S7_property", s7_property = list(class = class, default = default,
                    getter = !is.null(getter) && !identical(getter$type, "NULL"),
                    setter = !is.null(setter) && !identical(setter$type, "NULL"), name = member_name(args$name))))
    }
    if (method == "new_class") {
        args <- member_s7_arguments(expr, c("name", "parent", "package", "properties", "abstract", "constructor", "validator"))
        if (is.null(args) || !is.character(args$name) || length(args$name) != 1L || !nzchar(args$name)) return(member_value())
        parent <- if (is.null(args$parent)) NULL else infer(args$parent)$s7_descriptor
        if (!is.null(args$parent) && is.null(parent)) return(member_value(reason = "unknown_s7_parent"))
        abstract <- if (is.null(args$abstract)) FALSE else infer(args$abstract)$literal
        if (!is.logical(abstract) || length(abstract) != 1L || is.na(abstract)) return(member_value(reason = "dynamic_s7_abstract"))
        properties <- if (is.null(parent$properties)) list() else parent$properties
        if (!is.null(args$properties)) {
            value <- infer(args$properties)
            values <- value$elements
            if (!identical(value$type, "list") || length(values) > 256L) return(member_value(reason = "dynamic_s7_properties"))
            keys <- names(values)
            if (is.null(keys)) keys <- rep("", length(values))
            for (i in seq_along(values)) {
                property <- values[[i]]$s7_property
                if (is.null(property)) property <- list(class = values[[i]]$s7_descriptor, getter = FALSE, setter = FALSE)
                name <- if (nzchar(keys[[i]])) keys[[i]] else property$name
                if (is.null(name) || !nzchar(name) || name %in% keys[seq_len(i - 1L)]) return(member_value(reason = "dynamic_s7_properties"))
                keys[[i]] <- name
                properties[name] <- list(property)
            }
        }
        if (length(properties) > 256L) return(member_value(reason = "s7_property_limit"))
        custom <- !is.null(args$constructor) && !identical(infer(args$constructor)$type, "NULL")
        constructor <- if (custom) infer(args$constructor)$function_expr
        if (custom && is.null(constructor)) return(member_value(reason = "dynamic_s7_constructor"))
        if (!custom) {
            formals <- if (!isTRUE(parent$abstract) && !is.null(parent$constructor)) as.list(parent$constructor[[2L]]) else list()
            own <- if (!isTRUE(parent$abstract)) setdiff(names(properties), names(parent$properties)) else names(properties)
            for (name in own) {
                property <- properties[[name]]
                if (property$getter && !property$setter) next
                default <- if (!is.null(property$default)) property$default else member_s7_default(property$class)
                formals[name] <- list(if (is.null(default) && is.null(property$class)) quote(expr = ) else default)
            }
            constructor <- member_s7_function(formals)
        }
        descriptor <- list(kind = "class", name = args$name, parent = parent, properties = properties,
            package = if (is.character(args$package)) args$package, abstract = abstract,
            constructor = constructor, custom = custom)
        return(member_s7_generator(descriptor, bindings))
    }
    if (method == "prop") {
        args <- member_s7_arguments(expr, c("object", "name"))
        if (is.null(args)) return(member_value())
        return(member_slot(infer(args$object), member_name(args$name), index, bindings, budget))
    }
    if (method == "set_props") {
        args <- as.list(expr)[-1L]
        if (!length(args)) return(member_value())
        value <- infer(args[[1L]])
        if (!isTRUE(value$s7)) return(member_value())
        for (name in intersect(names(args)[-1L], names(value$slot_types))) {
            property <- value$s7_descriptor$properties[[name]]
            if (!property$getter && !property$setter) value$slots[name] <- list(infer(args[[name]]))
        }
        return(value)
    }
    if (method == "new_object") {
        descriptor <- bindings$.__s7_constructing__
        if (is.null(descriptor)) return(member_value())
        value <- member_s7_shape(descriptor, bindings)
        args <- as.list(expr)[-1L]
        for (name in intersect(names(args)[-1L], names(value$slot_types))) {
            property <- descriptor$properties[[name]]
            if (!property$getter && !property$setter) value$slots[name] <- list(infer(args[[name]]))
        }
        return(value)
    }
    NULL
}

member_s7_construct <- function(callee, actuals, index, bindings, budget) {
    descriptor <- callee$s7_generator
    if (isTRUE(descriptor$abstract)) return(member_value(reason = "abstract_s7_class"))
    value <- member_s7_shape(descriptor, callee$closure)
    if (isTRUE(descriptor$custom)) {
        trail <- budget$s7_constructors
        if (descriptor$name %in% trail || length(trail) >= 32L) return(value)
        budget$s7_constructors <- c(trail, descriptor$name)
        on.exit(budget$s7_constructors <- trail)
        scope <- callee$closure
        scope$.__s7_constructing__ <- descriptor
        scope$.__s7_constructor__ <- member_value(function_expr = descriptor$constructor, closure = scope)
        call <- as.call(c(list(as.name(".__s7_constructor__")),
                stats::setNames(lapply(seq_along(actuals), function(i) as.name(paste0(".__s7_arg", i))), names(actuals))))
        for (i in seq_along(actuals)) scope[paste0(".__s7_arg", i)] <- list(actuals[[i]])
        result <- member_infer(call, index, scope, budget = budget)
        return(if (isTRUE(result$s7) && identical(result$s7_descriptor$name, descriptor$name)) result else value)
    }
    formals <- names(descriptor$constructor[[2L]])
    supplied <- names(actuals)
    if (is.null(supplied)) supplied <- rep("", length(actuals))
    used <- character()
    for (i in which(nzchar(supplied))) {
        key <- supplied[[i]]
        candidates <- if (key %in% formals) key else formals[startsWith(formals, key)]
        if (length(candidates) != 1L || candidates %in% used) return(member_value(reason = "s7_argument_matching"))
        used <- c(used, candidates)
        if (candidates %in% names(value$slots) && !descriptor$properties[[candidates]]$getter && !descriptor$properties[[candidates]]$setter) value$slots[candidates] <- actuals[i]
    }
    available <- setdiff(formals, used)
    positional <- which(!nzchar(supplied))
    if (length(positional) > length(available) && !"..." %in% available) return(member_value(reason = "s7_argument_matching"))
    for (i in seq_len(min(length(positional), length(available)))) {
        name <- available[[i]]
        if (name %in% names(value$slots) && !descriptor$properties[[name]]$getter && !descriptor$properties[[name]]$setter) value$slots[name] <- actuals[positional[[i]]]
    }
    value
}

# Read only known descriptor attributes/list records, never S7 accessors.
member_s7_runtime_descriptor <- function(value, depth = 0L) {
    if (depth > 32L) return(NULL)
    classes <- attr(value, "class", exact = TRUE)
    if ("S7_class" %in% classes && typeof(value) == "closure") {
        name <- attr(value, "name", exact = TRUE)
        properties <- attr(value, "properties", exact = TRUE)
        if (!is.character(name) || length(name) != 1L || !is.list(properties) || length(properties) > 256L) return(NULL)
        parent <- member_s7_runtime_descriptor(attr(value, "parent", exact = TRUE), depth + 1L)
        props <- lapply(properties, function(property) {
            if (typeof(property) != "list") return(list(class = NULL, getter = FALSE, setter = FALSE))
            property <- unclass(property)
            list(class = member_s7_runtime_descriptor(property$class, depth + 1L),
                getter = typeof(property$getter) == "closure", setter = typeof(property$setter) == "closure",
                default = if (!is.object(property$default) && !is.function(property$default)) property$default)
        })
        return(list(kind = "class", name = name, parent = parent, properties = props,
                package = attr(value, "package", exact = TRUE), abstract = isTRUE(attr(value, "abstract", exact = TRUE)),
                constructor = member_s7_function(formals(value)), custom = TRUE))
    }
    if (typeof(value) != "list") return(NULL)
    value <- unclass(value)
    if ("S7_union" %in% classes) {
        if (!is.list(value$classes) || length(value$classes) > 256L) return(NULL)
        return(list(kind = "union", classes = lapply(value$classes, member_s7_runtime_descriptor, depth + 1L)))
    }
    if (any(c("S7_base_class", "S7_S3_class") %in% classes)) {
        constructor <- value$constructor
        if (typeof(constructor) != "closure") return(NULL)
        formals <- as.list(formals(constructor))
        default <- if (identical(body(constructor), quote(.data)) && identical(names(formals), ".data")) formals[[1L]]
        return(list(kind = "base", name = value$class, constructor = member_s7_function(formals), default = default))
    }
    if ("S7_any" %in% classes) return(list(kind = "any"))
    NULL
}

member_slot <- function(value, name, index, bindings, budget = NULL) {
    if (!isTRUE(value$s7)) return(member_s4_slot(value, name, index, bindings, budget))
    property <- member_lookup(value$slots, name)
    if (is.null(property)) return(member_value(reason = "unknown_s7_property"))
    if (is.null(property$binding_expr)) return(property)
    if (is.null(budget)) {
        budget <- new.env(parent = emptyenv())
        budget$remaining <- 20000L
    }
    trail <- budget$s7_slots
    key <- paste(value$s7_descriptor$name, name, sep = "::")
    if (key %in% trail || length(trail) >= 32L) return(property)
    budget$s7_slots <- c(trail, key)
    on.exit(budget$s7_slots <- trail)
    result <- member_infer(property$binding_expr, index, property$binding_env, budget = budget)
    if (length(result$type) || !is.null(result$function_expr)) result else property
}
