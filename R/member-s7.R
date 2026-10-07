# S7 descriptors are read as inert data. Source declarations are summarized
# from syntax; constructors, validators, getters and defaults are never called.
member_s7_intrinsics <- c("new_class", "new_property", "new_union", "new_object", "prop", "set_props",
    "as_class", "new_external_class", "new_S3_class", "deprecated_class", "deprecated_property", ":=")


# Derive semantics in the metadata worker from the installed implementation.
# Unknown implementations keep the older, conservative summaries. No probe
# constructs an object or invokes package callbacks.
member_s7_capabilities <- function() {
    if (!"S7" %in% loadedNamespaces()) return(list())
    ns <- asNamespace("S7")
    read <- function(name) {
        record <- member_binding(ns, name, materialize = TRUE)
        if (identical(record[[1L]], "value") && typeof(record[[2L]]) == "closure") record[[2L]]
    }
    uses <- function(name, symbol) {
        fn <- read(name)
        !is.null(fn) && symbol %in% all.names(body(fn))
    }
    args_fn <- read("constructor_args")
    first <- function(name) names(formals(read(name)))[1L]
    list(forward = uses("new_constructor", "constructor_forward"),
        overrides = !is.null(args_fn) && !uses("constructor_args", "setdiff"),
        class_ref = uses("new_object", "get_class_ref"),
        lists = uses("set_props", "collect_dots"),
        s4 = uses("new_constructor", "is_S4_class"),
        bind = uses(":=", "S7_eval_bare_"),
        union = uses("|.S7_class", "new_union"),
        exports = getNamespaceExports(ns),
        object = first("set_props"), parent = first("new_object"))
}

member_s7_modes <- function(index) {
    if (!is.null(index$s7_capabilities)) return(index$s7_capabilities)
    member_lookup(index$namespace_indices, "S7")$s7_capabilities
}

# := is a declaration only when its binding belongs to S7. Other packages
# use the same spelling with unrelated semantics.
member_s7_bind <- function(expr, index, bindings) {
    if (!member_head(expr, ":=") || length(expr) != 3L || !is.symbol(expr[[2L]]) ||
            !is.call(expr[[3L]]) || !isTRUE(member_s7_modes(index)$bind) ||
            !identical(member_s7_method(expr, index, bindings), ":=")) return(NULL)
    rhs <- expr[[3L]]
    if ("name" %in% names(rhs)) return(NULL)
    rhs$name <- as.character(expr[[2L]])
    call("<-", expr[[2L]], rhs)
}

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

# Exact matching precedes partial matching, and positional/partial matching
# stops at dots, as in R. Keep unmatched dots in order for parent forwarding.
member_s7_match <- function(args, keys) {
    supplied <- names(args)
    if (is.null(supplied)) supplied <- rep("", length(args))
    dots <- match("...", keys, nomatch = length(keys) + 1L)
    before <- keys[seq_len(dots - 1L)]
    result <- list()
    used <- rep(FALSE, length(args))
    for (i in which(nzchar(supplied) & supplied %in% setdiff(keys, "..."))) {
        key <- supplied[[i]]
        if (key %in% names(result)) return(NULL)
        result[key] <- args[i]
        used[[i]] <- TRUE
    }
    for (i in which(nzchar(supplied) & !used)) {
        candidates <- before[startsWith(before, supplied[[i]])]
        if (length(candidates) > 1L || (length(candidates) == 1L && candidates %in% names(result))) return(NULL)
        if (length(candidates) == 1L) {
            result[candidates] <- args[i]
            used[[i]] <- TRUE
        }
    }
    available <- setdiff(before, names(result))
    positional <- which(!nzchar(supplied) & !used)
    for (i in seq_len(min(length(positional), length(available)))) {
        result[available[[i]]] <- args[positional[[i]]]
        used[[positional[[i]]]] <- TRUE
    }
    if (any(!used) && !"..." %in% keys) return(NULL)
    list(args = result, dots = args[!used])
}

member_s7_arguments <- function(expr, keys) {
    args <- as.list(expr)[-1L]
    if (any(vapply(args, function(arg) identical(arg, quote(expr = )), logical(1L)))) return(NULL)
    member_s7_match(args, keys)$args
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

member_s7_shape <- function(descriptor, bindings = list(), depth = 0L) {
    if (depth >= 32L) return(member_value(reason = "s7_recursion"))
    if (is.null(descriptor)) return(member_value(reason = "unknown_s7_class"))
    if (identical(descriptor$kind, "union")) {
        shapes <- lapply(descriptor$classes, member_s7_shape, bindings, depth + 1L)
        return(if (length(shapes)) Reduce(member_join, shapes) else member_value())
    }
    if (identical(descriptor$kind, "s4")) {
        return(member_value(type = descriptor$name, slot_types = descriptor$slots,
                s4_class = list(name = descriptor$name, package = descriptor$package)))
    }
    if (!identical(descriptor$kind, "class")) {
        return(member_s7_value(type = if (!identical(descriptor$kind, "any")) descriptor$name,
                s7_descriptor = if (identical(descriptor$kind, "external")) descriptor))
    }
    properties <- descriptor$properties
    slots <- lapply(properties, function(property) {
        value <- member_s7_shape(property$class, bindings, depth + 1L)
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

member_s7_generator <- function(descriptor, bindings = list(), depth = 0L) {
    if (depth >= 32L) return(member_value(reason = "s7_recursion"))
    shape <- member_s7_shape(descriptor, bindings, depth)
    constructor <- member_s7_value(type = "function", function_expr = descriptor$constructor,
        closure = bindings, s7_generator = descriptor)
    # Class objects themselves expose descriptor properties via @.
    constructor$slots <- list(name = member_literal(descriptor$name),
        parent = if (!is.null(descriptor$parent)) member_s7_generator(descriptor$parent, bindings, depth + 1L) else member_literal(NULL),
        package = member_literal(descriptor$package), abstract = member_literal(isTRUE(descriptor$abstract)),
        properties = member_value(type = "list", fields = shape$slots),
        constructor = member_s7_value(type = "function", function_expr = descriptor$constructor,
            closure = bindings, s7_generator = if (isTRUE(descriptor$modes$class_ref)) descriptor),
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
    if (identical(descriptor$kind, "external") || isTRUE(descriptor$external)) {
        ref <- call("::", as.name(descriptor$package), as.name(descriptor$name))
        return(as.call(list(as.call(list(quote(S7::as_class), ref)))))
    }
    if (identical(descriptor$kind, "s4")) return(as.call(list(call("::", as.name("methods"), as.name("new")), descriptor$name)))
    if (identical(descriptor$kind, "class")) {
        ref <- if (is.null(descriptor$package)) as.name(descriptor$name) else
            call("::", as.name(descriptor$package), as.name(descriptor$name))
        return(as.call(list(ref)))
    }
    descriptor$default
}

member_s7_call <- function(expr, index, bindings, budget, method = member_s7_method(expr, index, bindings)) {
    if (is.null(method) || !method %in% member_s7_intrinsics) return(NULL)
    if (method %in% c("new_external_class", "deprecated_class", "deprecated_property", ":=") &&
            !method %in% member_s7_modes(index)$exports) return(member_value(reason = "unavailable_s7_api"))
    nesting <- budget$s7_depth
    if (is.null(nesting)) nesting <- 0L
    if (nesting >= 32L) return(member_value(reason = "s7_recursion"))
    budget$s7_depth <- nesting + 1L
    on.exit(budget$s7_depth <- nesting)
    infer <- function(node) member_infer(node, index, bindings, budget = budget)
    modes <- member_s7_modes(index)
    if (method == "as_class") {
        args <- member_s7_arguments(expr, c("x", "arg"))
        if (is.null(args)) return(member_value())
        value <- infer(args$x)
        descriptor <- member_s7_spec(value, index, bindings, budget)
        return(if (is.null(descriptor)) member_value() else if (descriptor$kind == "class")
                member_s7_generator(descriptor, bindings) else member_s7_value(s7_descriptor = descriptor))
    }
    if (method == "new_external_class") {
        args <- member_s7_arguments(expr, c("package", "name", "version"))
        if (is.null(args) || !is.character(args$package) || length(args$package) != 1L ||
                !is.character(args$name) || length(args$name) != 1L) return(member_value())
        return(member_s7_value(type = "S7_external_class", s7_descriptor = c(list(kind = "external"), args)))
    }
    if (method == "new_S3_class") {
        args <- member_s7_arguments(expr, c("class", "constructor", "validator", "default"))
        if (is.null(args) || !is.character(args$class) || !length(args$class)) return(member_value())
        constructor <- infer(args$constructor)$function_expr
        default <- args$default
        if (member_head(default, "quote")) default <- default[[2L]]
        return(member_s7_value(type = "S7_S3_class", s7_descriptor = list(kind = "base", name = args$class,
                    constructor = constructor, custom = TRUE, s3 = TRUE, default = default)))
    }
    if (method == "deprecated_class") {
        args <- as.list(expr)[-1L]
        args[c("when", "new", "method")] <- NULL
        return(member_s7_call(as.call(c(list(quote(S7::new_class)), args)), index, bindings, budget, "new_class"))
    }
    if (method == "deprecated_property") {
        args <- member_s7_arguments(expr, c("old", "new", "when", "method", "class", "default", "validator"))
        if (is.null(args) || !is.character(args$old) || length(args$old) != 1L) return(member_value())
        default <- args$default
        if (member_head(default, "quote")) default <- default[[2L]]
        if (is.null(default) && is.character(args$new) && length(args$new) == 1L) default <- as.name(args$new)
        return(member_s7_value(type = "S7_property", s7_property = list(name = args$old,
                    class = if (is.null(args$class)) list(kind = "any") else member_s7_spec(infer(args$class), index, bindings, budget, resolve = FALSE),
                    default = default, getter = !is.null(args$new), setter = !is.null(args$new), alias = args$new)))
    }
    if (method == "new_union") {
        values <- lapply(as.list(expr)[-1L], infer)
        classes <- lapply(values, function(value) member_s7_spec(value, index, bindings, budget))
        if (!length(classes) || length(classes) > 256L || any(vapply(classes, is.null, logical(1L)))) return(member_value())
        return(member_s7_value(type = "S7_union", s7_descriptor = list(kind = "union", classes = classes)))
    }
    if (method == "new_property") {
        args <- member_s7_arguments(expr, c("class", "getter", "setter", "validator", "default", "name"))
        if (is.null(args)) return(member_value())
        class <- if (is.null(args$class)) list(kind = "any") else member_s7_spec(infer(args$class), index, bindings, budget, resolve = FALSE)
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
        parent <- if (is.null(args$parent)) NULL else member_s7_spec(infer(args$parent), index, bindings, budget)
        if (!is.null(args$parent) && (is.null(parent) || identical(parent$kind, "external"))) return(member_value(reason = "unknown_s7_parent"))
        if (identical(parent$kind, "s4") && !isTRUE(modes$s4)) return(member_value(reason = "unsupported_s7_parent"))
        abstract <- if (is.null(args$abstract)) FALSE else infer(args$abstract)$literal
        if (!is.logical(abstract) || length(abstract) != 1L || is.na(abstract)) return(member_value(reason = "dynamic_s7_abstract"))
        package <- if (is.null(args$package)) index$package else if (is.character(args$package)) args$package
        own <- character()
        properties <- if (is.null(parent$properties)) list() else parent$properties
        if (!is.null(args$properties)) {
            value <- infer(args$properties)
            values <- value$elements
            if (!identical(value$type, "list") || length(values) > 256L) return(member_value(reason = "dynamic_s7_properties"))
            keys <- names(values)
            if (is.null(keys)) keys <- rep("", length(values))
            for (i in seq_along(values)) {
                property <- values[[i]]$s7_property
                if (is.null(property)) property <- list(class = member_s7_spec(values[[i]], index, bindings, budget, resolve = FALSE), getter = FALSE, setter = FALSE)
                name <- if (nzchar(keys[[i]])) keys[[i]] else property$name
                if (is.null(name) || !nzchar(name) || name %in% keys[seq_len(i - 1L)]) return(member_value(reason = "dynamic_s7_properties"))
                keys[[i]] <- name
                properties[name] <- list(property)
                own <- c(own, name)
            }
        }
        if (length(properties) > 256L) return(member_value(reason = "s7_property_limit"))
        custom <- !is.null(args$constructor) && !identical(infer(args$constructor)$type, "NULL")
        constructor <- if (custom) infer(args$constructor)$function_expr
        if (custom && is.null(constructor)) return(member_value(reason = "dynamic_s7_constructor"))
        if (!custom) {
            root <- is.null(parent) || identical(parent$name, "S7_object") || isTRUE(parent$abstract) || identical(parent$kind, "s4")
            forward <- !root && isTRUE(modes$forward) && ((isTRUE(parent$custom) && (identical(parent$kind, "class") || isTRUE(parent$s3))) || isTRUE(parent$external) ||
                    (!is.null(package) && !is.null(parent$package) && !identical(package, parent$package)))
            formals <- if (root) list() else if (forward) alist(... = ) else as.list(parent$constructor[[2L]])
            args_own <- if (root) names(properties) else if (isTRUE(modes$overrides)) own else setdiff(own, names(parent$properties))
            for (name in args_own) {
                property <- properties[[name]]
                if (property$getter && !property$setter) next
                default <- if (!is.null(property$default)) property$default else member_s7_default(property$class)
                formals[name] <- list(if (is.null(default) && is.null(property$class)) quote(expr = ) else default)
            }
            constructor <- member_s7_function(formals)
        }
        descriptor <- list(kind = "class", name = args$name, parent = parent, properties = properties,
            package = package, abstract = abstract,
            constructor = constructor, custom = custom, own = own, forward = if (!custom) forward else FALSE,
            modes = modes[c("class_ref", "overrides")])
        return(member_s7_generator(descriptor, bindings))
    }
    if (method == "prop") {
        args <- member_s7_arguments(expr, c("object", "name"))
        if (is.null(args)) return(member_value())
        return(member_slot(infer(args$object), member_name(args$name), index, bindings, budget))
    }
    if (method %in% c("set_props", "new_object")) {
        first <- if (method == "set_props") modes$object else modes$parent
        if (is.null(first)) first <- if (method == "set_props") "object" else ".parent"
        args <- member_s7_match(as.list(expr)[-1L], c(first, "...", if (method == "set_props") ".check"))
        if (is.null(args)) return(member_value(reason = "s7_argument_matching"))
        descriptor <- bindings$.__s7_constructing__
        value <- if (method == "set_props") infer(args$args[[first]]) else member_s7_shape(descriptor, bindings)
        if (!isTRUE(value$s7)) return(member_value())
        if (method == "new_object" && !is.null(args$args[[first]])) {
            parent <- infer(args$args[[first]])
            if (isTRUE(parent$s7)) {
                for (name in intersect(names(parent$slots), names(value$slots))) {
                    property <- descriptor$properties[[name]]
                    if (!property$getter && !property$setter) value$slots[name] <- parent$slots[name]
                }
            }
        }
        updates <- args$dots
        keys <- names(updates)
        if (is.null(keys)) keys <- rep("", length(updates))
        if (length(updates) == 1L && !nzchar(keys) && isTRUE(modes$lists)) {
            update <- infer(updates[[1L]])
            if (identical(update$type, "list") && (length(update$elements) == 0L ||
                        (!is.null(names(update$elements)) && all(nzchar(names(update$elements)))))) {
                updates <- update$elements
            } else {
                value$slots <- lapply(value$slots, function(slot) member_value(type = slot$type))
                return(value)
            }
        } else {
            if (any(!nzchar(keys))) return(member_value(reason = "unnamed_s7_properties"))
            updates <- lapply(updates, infer)
        }
        if (anyDuplicated(names(updates)) || any(!names(updates) %in% names(value$slot_types))) return(member_value(reason = "unknown_s7_property"))
        for (name in names(updates)) {
            property <- value$s7_descriptor$properties[[name]]
            if (!property$getter && !property$setter) value$slots[name] <- updates[name]
        }
        return(value)
    }
    NULL
}

member_s7_construct <- function(callee, actuals, index, bindings, budget) {
    descriptor <- callee$s7_generator
    nesting <- budget$s7_construct_depth
    if (is.null(nesting)) nesting <- 0L
    if (nesting >= 32L) return(member_value(reason = "s7_recursion"))
    budget$s7_construct_depth <- nesting + 1L
    on.exit(budget$s7_construct_depth <- nesting)
    if (isTRUE(descriptor$abstract)) return(member_value(reason = "abstract_s7_class"))
    value <- member_s7_shape(descriptor, callee$closure)
    matched <- member_s7_match(actuals, names(descriptor$constructor[[2L]]))
    if (is.null(matched)) return(member_value(reason = "s7_argument_matching"))
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
    provided <- names(matched$args)
    for (name in intersect(setdiff(names(descriptor$constructor[[2L]]), c(provided, "...")), names(descriptor$properties))) {
        property <- descriptor$properties[[name]]
        if (is.null(property$default)) next
        default <- descriptor$constructor[[2L]][[name]]
        if (!identical(default, quote(expr = ))) matched$args[name] <- list(
            member_value(binding_expr = default, binding_env = callee$closure))
    }
    if (!is.null(descriptor$parent) && !isTRUE(descriptor$parent$abstract) && descriptor$parent$kind == "class") {
        parent <- member_s7_generator(descriptor$parent, callee$closure)
        parent_args <- if (isTRUE(descriptor$forward)) matched$dots else
            matched$args[intersect(names(matched$args), names(descriptor$parent$constructor[[2L]]))]
        # Property overrides are forwarded to parents that can accept them.
        if (isTRUE(descriptor$forward)) {
            overrides <- intersect(names(matched$args), names(descriptor$parent$properties))
            parent_formals <- names(descriptor$parent$constructor[[2L]])
            if (!"..." %in% parent_formals) overrides <- intersect(overrides, parent_formals)
            parent_args <- c(matched$args[overrides], parent_args)
        }
        inherited <- member_s7_construct(parent, parent_args, index, bindings, budget)
        if (!isTRUE(inherited$s7)) return(inherited)
        for (name in intersect(names(inherited$slots), names(value$slots))) {
            property <- descriptor$properties[[name]]
            if (!property$getter && !property$setter &&
                    (!isTRUE(descriptor$modes$overrides) || !name %in% names(matched$args))) value$slots[name] <- inherited$slots[name]
        }
    }
    for (name in intersect(names(matched$args), names(value$slots))) {
        property <- descriptor$properties[[name]]
        if (!property$getter && !property$setter) value$slots[name] <- matched$args[name]
    }
    value
}

# Read only known descriptor attributes/list records, never S7 accessors.
member_s7_runtime_descriptor <- function(value, depth = 0L, modes = list(), budget = NULL) {
    if (is.null(budget)) {
        budget <- new.env(parent = emptyenv())
        budget$remaining <- 4096L
    }
    budget$remaining <- budget$remaining - 1L
    if (depth > 32L || budget$remaining < 0L) return(NULL)
    classes <- attr(value, "class", exact = TRUE)
    if ("S7_class" %in% classes && typeof(value) == "closure") {
        name <- attr(value, "name", exact = TRUE)
        properties <- attr(value, "properties", exact = TRUE)
        if (!is.character(name) || length(name) != 1L || !is.list(properties) || length(properties) > 256L) return(NULL)
        parent <- member_s7_runtime_descriptor(attr(value, "parent", exact = TRUE), depth + 1L, modes, budget)
        props <- lapply(properties, function(property) {
            if (typeof(property) != "list") return(list(class = NULL, getter = FALSE, setter = FALSE))
            property <- unclass(property)
            list(class = member_s7_runtime_descriptor(property$class, depth + 1L, modes, budget),
                getter = typeof(property$getter) == "closure", setter = typeof(property$setter) == "closure",
                default = if (!is.object(property$default) && !is.function(property$default) &&
                        !is.environment(property$default)) property$default)
        })
        return(list(kind = "class", name = name, parent = parent, properties = props,
                package = attr(value, "package", exact = TRUE), abstract = isTRUE(attr(value, "abstract", exact = TRUE)),
                constructor = member_s7_function(formals(value)),
                custom = !"S7_constructor" %in% attr(attr(value, "constructor", exact = TRUE), "class", exact = TRUE),
                forward = "..." %in% names(formals(value)), modes = modes[c("class_ref", "overrides")]))
    }
    if (isS4(value)) {
        descriptor <- member_s4_runtime_descriptor(value)
        if (!is.null(descriptor)) return(member_s7_s4_descriptor(descriptor))
    }
    if (typeof(value) != "list") return(NULL)
    value <- unclass(value)
    if ("S7_union" %in% classes) {
        if (!is.list(value$classes) || length(value$classes) > 256L) return(NULL)
        return(list(kind = "union", classes = lapply(value$classes, member_s7_runtime_descriptor, depth + 1L, modes, budget)))
    }
    if (any(c("S7_base_class", "S7_S3_class") %in% classes)) {
        constructor <- value$constructor
        if (typeof(constructor) != "closure") return(NULL)
        formals <- as.list(formals(constructor))
        default <- value$default
        if (is.null(default) && identical(body(constructor), quote(.data)) && identical(names(formals), ".data")) default <- formals[[1L]]
        # A closure-valued default cannot be serialized as executable code.
        if (is.null(default)) default <- as.name(".__unknown_s7_default__")
        if (is.object(default) || is.function(default) || is.environment(default)) default <- as.name(".__unknown_s7_default__")
        return(list(kind = "base", name = value$class, constructor = member_s7_function(formals),
                custom = !"S7_constructor" %in% attr(constructor, "class", exact = TRUE),
                s3 = "S7_S3_class" %in% classes, default = default))
    }
    if ("S7_external_class" %in% classes && is.character(value$package) && length(value$package) == 1L &&
            is.character(value$name) && length(value$name) == 1L) {
        return(list(kind = "external", name = value$name, package = value$package,
                version = if (is.character(value$version)) value$version))
    }
    if ("S7_any" %in% classes) return(list(kind = "any"))
    NULL
}

member_slot <- function(value, name, index, bindings, budget = NULL) {
    if (!isTRUE(value$s7)) return(member_s4_slot(value, name, index, bindings, budget))
    property <- member_lookup(value$slots, name)
    if (is.null(property)) return(member_value(reason = "unknown_s7_property"))
    spec <- property$s7_descriptor
    if (identical(spec$kind, "external")) {
        scope <- bindings
        scope$.__s7_package__ <- value$s7_descriptor$package
        descriptor <- member_s7_external(spec, index, scope, budget)
        if (!is.null(descriptor)) return(member_s7_shape(descriptor, bindings))
        return(member_value(type = spec$name, reason = "unresolved_s7_external"))
    }
    alias <- value$s7_descriptor$properties[[name]]$alias
    if (is.character(alias) && length(alias) == 1L && !identical(alias, name)) {
        property <- member_lookup(value$slots, alias)
        if (is.null(property)) return(member_value())
    }
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

# Normalize S4 class representations without initialize() or prototype access.
member_s7_s4_descriptor <- function(descriptor) {
    if (is.null(descriptor) || identical(descriptor$complete, FALSE)) return(NULL)
    properties <- lapply(descriptor$slots, function(type) {
        list(class = list(kind = "base", name = as.character(type)), getter = FALSE, setter = FALSE)
    })
    list(kind = "s4", name = descriptor$name, package = descriptor$package, slots = descriptor$slots,
        properties = properties, abstract = descriptor$virtual)
}

member_s7_spec <- function(value, index, bindings, budget, resolve = TRUE) {
    descriptor <- value$s7_descriptor
    if (identical(descriptor$kind, "external")) {
        if (!resolve) return(descriptor)
        resolved <- member_s7_external(descriptor, index, bindings, budget)
        if (!is.null(resolved)) {
            resolved$external <- TRUE
            resolved$package <- descriptor$package
            resolved$name <- descriptor$name
            return(resolved)
        }
        return(descriptor)
    }
    if (!is.null(descriptor)) return(descriptor)
    if (identical(value$type, "NULL")) return(list(kind = "base", name = "NULL", default = NULL))
    class <- value$s4_generator
    if (!is.null(class)) {
        scope <- bindings
        if (!is.null(class$recipe)) scope$.__s4_classes__[class$name] <- list(class$recipe)
        shape <- member_s4_shape(class$name, index, scope, class$package, budget = budget)
        descriptor <- member_s7_s4_descriptor(list(name = class$name, package = class$package,
                slots = shape$slot_types, virtual = FALSE, complete = !is.null(shape$slot_types)))
        for (name in names(descriptor$properties)) {
            type <- descriptor$properties[[name]]$class$name
            root <- member_lookup(index$namespace_roots, paste0("S7::class_", type))
            if (!is.null(root$s7_descriptor)) descriptor$properties[[name]]$class <- root$s7_descriptor
        }
        return(descriptor)
    }
    NULL
}

member_s7_external <- function(descriptor, index, bindings, budget) {
    if (is.null(budget)) budget <- new.env(parent = emptyenv())
    key <- paste(descriptor$package, descriptor$name, sep = "::")
    trail <- budget$s7_external
    if (key %in% trail || length(trail) >= 32L) return(NULL)
    budget$s7_external <- c(trail, key)
    on.exit(budget$s7_external <- trail)
    local <- identical(descriptor$package, index$package)
    same <- local || identical(descriptor$package, bindings$.__s7_package__)
    target <- if (local) index else member_lookup(index$namespace_indices, descriptor$package)
    if (is.null(target)) target <- member_lookup(index$s7_dependencies, descriptor$package)
    if (is.null(target) && same && !is.null(index$document_bindings)) target <- index
    if (is.null(target) || (!same && !descriptor$name %in% target$exports)) return(NULL)
    if (!is.null(descriptor$version)) {
        installed <- target$identity$version
        ok <- tryCatch(!is.null(installed) && utils::compareVersion(installed, descriptor$version) >= 0L,
            error = function(e) FALSE)
        if (!ok) return(NULL)
    }
    value <- member_lookup(target$package_roots, descriptor$name)
    if (is.null(value) && same) value <- member_infer(as.name(descriptor$name), target, bindings, budget = budget)
    if (identical(value$s7_descriptor$kind, "external")) return(member_s7_external(value$s7_descriptor, target, bindings, budget))
    if (identical(value$s7_descriptor$kind, "class")) value$s7_descriptor else NULL
}

# Decode current and legacy instance storage. Class references are read only
# through inert bindings; an active binding or promise is never forced.
member_s7_instance_descriptor <- function(value, modes = list()) {
    if (!"S7_object" %in% attr(value, "class", exact = TRUE)) return(NULL)
    class <- attr(value, "_S7_class", exact = TRUE)
    if (is.null(class)) class <- attr(value, "S7_class", exact = TRUE)
    if (is.environment(class)) {
        if (!"S7_class_ref" %in% attr(class, "class", exact = TRUE)) return(NULL)
        record <- member_binding(class, "class")
        if (!identical(record[[1L]], "value")) return(NULL)
        class <- record[[2L]]
    }
    descriptor <- member_s7_runtime_descriptor(class, modes = modes)
    if (identical(descriptor$kind, "class")) descriptor else NULL
}

# Capture already-loaded external class dependencies in the worker. Requests
# resolve these records without loading a package or calling an accessor.
member_s7_dependencies <- function(roots, package, modes) {
    dependencies <- list()
    queue <- lapply(roots, function(value) value$s7_descriptor)
    seen <- character()
    budget <- new.env(parent = emptyenv())
    budget$remaining <- 4096L
    references <- function(descriptor, depth = 0L) {
        budget$remaining <- budget$remaining - 1L
        if (is.null(descriptor) || depth > 32L || budget$remaining < 0L) return(list())
        if (identical(descriptor$kind, "external")) return(list(descriptor))
        out <- references(descriptor$parent, depth + 1L)
        for (class in c(descriptor$classes, lapply(descriptor$properties, function(property) property$class))) {
            out <- c(out, references(class, depth + 1L))
        }
        out
    }
    for (pass in seq_len(16L)) {
        refs <- unlist(lapply(queue, references), recursive = FALSE)
        next_queue <- list()
        for (ref in refs) {
            key <- paste(ref$package, ref$name, sep = "::")
            if (key %in% seen || length(seen) >= 256L || !ref$package %in% loadedNamespaces()) next
            seen <- c(seen, key)
            ns <- asNamespace(ref$package)
            exports <- getNamespaceExports(ns)
            if (!identical(package, ref$package) && !ref$name %in% exports) next
            record <- member_binding(ns, ref$name, materialize = TRUE)
            if (!identical(record[[1L]], "value")) next
            descriptor <- member_s7_runtime_descriptor(record[[2L]], modes = modes)
            if (!identical(descriptor$kind, "class")) next
            if (is.null(dependencies[[ref$package]])) {
                dependencies[[ref$package]] <- list(package = ref$package, exports = exports,
                    identity = list(version = as.character(getNamespaceVersion(ns))), package_roots = list())
            }
            dependencies[[ref$package]]$package_roots[ref$name] <- list(member_s7_generator(descriptor))
            next_queue[[length(next_queue) + 1L]] <- descriptor
        }
        if (!length(next_queue)) break
        queue <- next_queue
    }
    dependencies
}
