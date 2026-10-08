# S4 slot declarations are separate from dollar members. Read syntax and class
# metadata only: never call new(), initialize(), validity or slot accessors.
member_methods_intrinsics <- c("setClass", "setClassUnion", "representation", "prototype", "new", "slot")

member_s4_unshadowed <- function(name, index, bindings) {
    if (name %in% names(bindings)) return(FALSE)
    at <- bindings$.__member_position__
    history <- member_lookup(index$document_bindings, name)
    if (length(history) && (is.null(at) || any(vapply(history, function(item) {
        member_before(item$end, at)
    }, logical(1L))))) return(FALSE)
    if (name %in% names(index$definitions) && is.null(index$document_bindings)) return(FALSE)
    attached <- member_lookup(index$attached_roots, name)
    is.null(attached) || (!is.null(attached$metadata) && attached$metadata %in% c("base", "methods"))
}

member_s4_method <- function(expr, index, bindings) {
    if (!is.call(expr)) return(NULL)
    head <- expr[[1L]]
    if (member_head(head, "::")) {
        if (identical(member_name(head[[2L]]), "methods")) return(member_name(head[[3L]]))
        return(NULL)
    }
    name <- member_name(head)
    if (is.null(name) || !name %in% member_methods_intrinsics || !member_s4_unshadowed(name, index, bindings)) return(NULL)
    if (isTRUE(index$methods_attached) || name %in% index$intrinsics) name else NULL
}

member_s4_literal_call <- function(expr, name, index, bindings) {
    if (!is.call(expr)) return(FALSE)
    head <- expr[[1L]]
    if (member_head(head, "::")) {
        return(identical(member_name(head[[2L]]), "base") && identical(member_name(head[[3L]]), name))
    }
    member_head(expr, name) && member_s4_unshadowed(name, index, bindings)
}

member_s4_strings <- function(expr, index, bindings) {
    if (is.character(expr)) return(expr)
    if (member_s4_literal_call(expr, "c", index, bindings)) {
        args <- as.list(expr)[-1L]
        if (all(vapply(args, is.character, logical(1L)))) return(as.character(unlist(args)))
    }
    character()
}

member_s4_arguments <- function(expr, first = "Class") {
    args <- as.list(expr)[-1L]
    for (i in seq_along(args)) {
        if (identical(args[[i]], quote(expr = ))) args[[i]] <- as.name(".__unknown_s4_argument__")
    }
    if (is.null(names(args))) names(args) <- rep("", length(args))
    if (length(args) && (is.null(names(args)) || !nzchar(names(args)[[1L]]))) {
        names(args)[[1L]] <- first
    }
    args
}

member_s4_recipe <- function(expr, index, bindings) {
    if (member_head(expr, "<-") || member_head(expr, "=")) expr <- expr[[3L]]
    method <- member_s4_method(expr, index, bindings)
    if (is.null(method) || !method %in% c("setClass", "setClassUnion")) return(NULL)
    args <- member_s4_arguments(expr, if (method == "setClassUnion") "name" else "Class")
    key <- if (method == "setClassUnion") "name" else "Class"
    name <- args[[key]]
    if (!is.character(name) || length(name) != 1L || !nzchar(name)) return(NULL)
    if (method == "setClassUnion") {
        members <- if (!is.null(args$members)) args$members else if (length(args) >= 2L) args[[2L]]
        types <- member_s4_strings(members, index, bindings)
        return(list(name = name, union = as.list(types), virtual = TRUE, complete = length(types) <= 256L))
    }
    slots <- list()
    contains <- member_s4_strings(args$contains, index, bindings)
    complete <- is.null(args$contains) || length(contains) > 0L || identical(args$contains, character()) ||
        (member_s4_literal_call(args$contains, "c", index, bindings) && length(args$contains) == 1L)
    declaration <- function(node, legacy = FALSE) {
        if (is.null(node) || identical(node, character())) return()
        values <- if (is.character(node)) {
            as.list(node)
        } else if (member_s4_literal_call(node, "c", index, bindings) || member_s4_literal_call(node, "list", index, bindings) ||
                identical(member_s4_method(node, index, bindings), "representation")) {
            as.list(node)[-1L]
        } else {
            complete <<- FALSE
            return()
        }
        if (length(values) > 256L) {
            complete <<- FALSE
            return()
        }
        for (i in seq_along(values)) {
            key <- if (is.null(names(values))) "" else names(values)[[i]]
            if (identical(values[[i]], quote(expr = ))) {
                complete <<- FALSE
                next
            }
            type <- values[[i]]
            if (!is.character(type) || length(type) != 1L || !nzchar(type)) {
                complete <<- FALSE
            } else if (nzchar(key)) {
                slots[key] <<- list(type)
            } else if (legacy) {
                contains <<- union(contains, type)
            } else {
                complete <<- FALSE
            }
        }
    }
    representation <- args$representation
    if (is.null(representation) && length(args) >= 2L && !nzchar(names(args)[[2L]])) representation <- args[[2L]]
    declaration(representation, TRUE)
    declaration(args$slots)
    if (length(contains) > 32L) complete <- FALSE
    prototype <- args$prototype
    defaults <- list()
    if (member_s4_literal_call(prototype, "list", index, bindings) || identical(member_s4_method(prototype, index, bindings), "prototype")) {
        defaults <- as.list(prototype)[-1L]
    }
    list(name = name, slots = slots, contains = setdiff(contains, "VIRTUAL"),
        virtual = "VIRTUAL" %in% contains, defaults = defaults, complete = complete)
}

member_s4_effect <- function(expr, index, bindings) {
    recipe <- member_s4_recipe(expr, index, bindings)
    if (!is.null(recipe)) bindings$.__s4_classes__[recipe$name] <- list(recipe)
    bindings
}

member_s4_descriptor <- function(name, index, bindings, package = NULL) {
    if (!is.null(package)) {
        if (identical(package, index$package)) return(member_lookup(index$s4_classes, name))
        descriptor <- member_lookup(member_lookup(index$namespace_indices, package)$s4_classes, name)
        if (!is.null(descriptor)) return(descriptor)
        return(member_lookup(member_lookup(index$s4_dependencies, package), name))
    }
    local <- member_lookup(bindings$.__s4_classes__, name)
    if (!is.null(local)) return(local)
    at <- bindings$.__member_position__
    if (is.null(at)) at <- c(Inf, Inf)
    declarations <- member_lookup(index$document_s4, name)
    for (item in rev(declarations)) {
        if (!member_before(item$end, at)) next
        env <- bindings
        env$.__member_position__ <- item$start
        recipe <- member_s4_recipe(item$expr, index, env)
        if (!is.null(recipe)) return(recipe)
    }
    if (is.null(index$document_bindings)) return(member_lookup(index$s4_classes, name))
    member_lookup(index$attached_s4, name)
}

member_s4_shape <- function(name, index, bindings, package = NULL, trail = character(), budget = NULL) {
    if (!is.null(budget)) {
        budget$remaining <- budget$remaining - 1L
        if (budget$remaining < 0L || (!is.null(budget$deadline) && proc.time()[[3L]] > budget$deadline)) {
            budget$exhausted <- TRUE
            return(member_value(reason = "budget"))
        }
    }
    identity <- paste(package, name, sep = "::")
    if (length(trail) > 32L || identity %in% trail) return(member_value(reason = "s4_cycle"))
    descriptor <- member_s4_descriptor(name, index, bindings, package)
    if (is.null(descriptor) || identical(descriptor$complete, FALSE)) return(member_value(reason = "unknown_s4_class"))
    trail <- c(trail, identity)
    if (!is.null(descriptor$union)) {
        if (!length(descriptor$union)) return(member_value(reason = "unknown_s4_union"))
        return(Reduce(member_join, lapply(descriptor$union, function(type) {
            owner <- attr(type, "package", exact = TRUE)
            member_s4_shape(as.character(type), index, bindings, if (is.null(owner)) package else owner, trail, budget)
        })))
    }
    slots <- defaults <- list()
    for (parent in descriptor$contains) {
        # R's built-in storage classes contribute the data part of an S4
        # extension, rather than a user-defined slot inheritance recipe.
        if (parent %in% c("numeric", "integer", "double", "logical", "character", "complex", "raw", "list", "expression",
                "function", "language", "matrix", "array")) {
            slots[".Data"] <- list(parent)
            next
        }
        if (parent %in% c("environment", "externalptr")) {
            slots[".xData"] <- list(parent)
            next
        }
        inherited <- member_s4_shape(parent, index, bindings, package, trail, budget)
        if (is.null(inherited$slot_types)) return(member_value(reason = "unknown_s4_parent"))
        slots <- utils::modifyList(slots, inherited$slot_types)
        if (!is.null(inherited$slots)) defaults <- utils::modifyList(defaults, inherited$slots)
    }
    slots <- utils::modifyList(slots, descriptor$slots)
    if (length(slots) > 256L) return(member_value(reason = "s4_slot_limit"))
    for (slot in intersect(names(descriptor$defaults), names(slots))) {
        # Prototype declarations supply syntax, not evaluated default objects.
        if (identical(descriptor$defaults[[slot]], quote(expr = ))) next
        defaults[slot] <- list(member_value(binding_expr = descriptor$defaults[[slot]], binding_env = bindings))
    }
    owner <- if (is.null(descriptor$package)) package else descriptor$package
    member_value(type = name, s4_class = list(name = name, package = owner),
        slot_types = slots, slots = defaults)
}

member_s4_types <- function(types, package = NULL) {
    owners <- attr(types, "package", exact = TRUE)
    lapply(seq_along(types), function(i) {
        type <- types[[i]]
        owner <- if (length(owners) == length(types)) owners[[i]] else if (length(owners) == 1L) owners else package
        if (!is.null(owner) && nzchar(owner)) attr(type, "package") <- owner
        type
    })
}

member_s4_join_types <- function(a, b, owner_a = NULL, owner_b = NULL) {
    types <- unique(c(member_s4_types(a, owner_a), member_s4_types(b, owner_b)))
    out <- vapply(types, as.character, character(1L))
    owners <- vapply(types, function(type) {
        owner <- attr(type, "package", exact = TRUE)
        if (is.null(owner)) "" else owner
    }, character(1L))
    if (any(nzchar(owners))) attr(out, "package") <- owners
    out
}

member_s4_slot <- function(value, name, index, bindings, budget = NULL) {
    type <- member_lookup(value$slot_types, name)
    if (is.null(type)) return(member_value(reason = "unknown_s4_slot"))
    actual <- member_lookup(value$slots, name)
    if (!is.null(actual$binding_expr)) {
        if (is.null(budget)) {
            budget <- new.env(parent = emptyenv())
            budget$remaining <- 20000L
        }
        trail <- budget$s4_slots
        key <- paste(value$s4_class$package, value$s4_class$name, name, sep = "::")
        if (key %in% trail || length(trail) >= 64L) return(member_value(reason = "s4_cycle"))
        budget$s4_slots <- c(trail, key)
        on.exit(budget$s4_slots <- trail)
        actual <- member_infer(actual$binding_expr, index, actual$binding_env, budget = budget)
    }
    if (!is.null(actual) && (length(actual$type) || !is.null(actual$function_expr))) return(actual)
    shapes <- lapply(member_s4_types(type, value$s4_class$package), function(class) {
        package <- attr(class, "package", exact = TRUE)
        class <- as.character(class)
        types <- c(numeric = "double", integer = "integer", character = "character", logical = "logical",
            complex = "complex", raw = "raw", list = "list", environment = "environment", "function" = "function")
        if (class %in% names(types)) return(member_value(type = unname(types[[class]])))
        nested <- member_s4_shape(class, index, bindings, package, budget = budget)
        if (!is.null(nested$slot_types)) return(nested)
        member_value(type = if (class %in% names(types)) unname(types[[class]]) else if (class != "ANY") class)
    })
    Reduce(member_join, shapes)
}

member_s4_construct <- function(class, actuals, index, bindings, budget = NULL) {
    if (!is.null(class$recipe)) bindings$.__s4_classes__[class$name] <- list(class$recipe)
    descriptor <- member_s4_descriptor(class$name, index, bindings, class$package)
    if (isTRUE(descriptor$virtual)) return(member_value(reason = "virtual_s4_class"))
    value <- member_s4_shape(class$name, index, bindings, class$package, budget = budget)
    for (slot in intersect(names(actuals), names(value$slot_types))) value$slots[slot] <- list(actuals[[slot]])
    value
}

member_s4_runtime_descriptor <- function(value) {
    name <- attr(value, "className", exact = TRUE)
    slots <- attr(value, "slots", exact = TRUE)
    if (!is.character(name) || length(name) != 1L || !is.list(slots) || length(slots) > 256L) return(NULL)
    if ("ClassUnionRepresentation" %in% attr(value, "class", exact = TRUE)) {
        subclasses <- attr(value, "subclasses", exact = TRUE)
        if (length(subclasses) > 256L) return(NULL)
        types <- lapply(subclasses, function(extension) attr(extension, "subClass", exact = TRUE))
        if (!all(vapply(types, function(type) is.character(type) && length(type) == 1L, logical(1L)))) return(NULL)
        return(list(name = as.character(name), package = attr(name, "package", exact = TRUE), union = types, virtual = TRUE))
    }
    slots <- lapply(slots, function(type) {
        if (is.character(type) && length(type) == 1L) type else "ANY"
    })
    # Installed descriptors already include inherited slots. Their prototypes
    # and initialization methods are not read or invoked to guess slot values.
    list(name = as.character(name), package = attr(name, "package", exact = TRUE), slots = slots, contains = character(),
        virtual = isTRUE(attr(value, "virtual", exact = TRUE)), complete = TRUE)
}

member_s4_dependencies <- function(classes, references = list()) {
    out <- list()
    queue <- c(classes, list(list(slots = references)))
    seen <- character()
    for (pass in seq_len(16L)) {
        next_queue <- list()
        for (descriptor in queue) {
            for (type in c(descriptor$slots, descriptor$union)) {
                package <- attr(type, "package", exact = TRUE)
                if (!is.character(package) || length(package) != 1L || !package %in% loadedNamespaces()) next
                key <- paste(package, type, sep = "::")
                if (key %in% seen || length(seen) >= 256L) next
                seen <- c(seen, key)
                record <- member_binding(asNamespace(package), paste0(".__C__", type), materialize = TRUE)
                value <- record[[2L]]
                if (!identical(record[[1L]], "value") || !isS4(value)) next
                nested <- member_s4_runtime_descriptor(value)
                if (is.null(nested)) next
                out[[package]][nested$name] <- list(nested)
                next_queue[[length(next_queue) + 1L]] <- nested
            }
        }
        if (!length(next_queue)) break
        queue <- next_queue
    }
    out
}

member_s4_call <- function(expr, index, bindings, budget) {
    method <- member_s4_method(expr, index, bindings)
    if (is.null(method)) return(NULL)
    if (method == "setClass") {
        recipe <- member_s4_recipe(expr, index, bindings)
        return(if (is.null(recipe)) member_value() else member_value(
            type = "function", s4_generator = list(name = recipe$name, recipe = recipe)
        ))
    }
    if (method == "slot") {
        args <- member_s4_arguments(expr, "object")
        name <- if (!is.null(args$name)) args$name else if (length(args) >= 2L) args[[2L]]
        if (!is.character(name) || length(name) != 1L) return(member_value())
        value <- member_infer(args$object, index, bindings, budget = budget)
        return(member_s4_slot(value, name, index, bindings, budget))
    }
    if (method != "new") return(NULL)
    args <- member_s4_arguments(expr)
    class <- member_infer(args$Class, index, bindings, budget = budget)
    if (isTRUE(class$known_literal) && is.character(class$literal) && length(class$literal) == 1L) {
        target <- list(name = class$literal)
    } else {
        return(member_value(reason = "dynamic_s4_class"))
    }
    actuals <- lapply(args[names(args) != "Class"], function(arg) {
        member_infer(arg, index, bindings, budget = budget)
    })
    member_s4_construct(target, actuals, index, bindings, budget)
}
