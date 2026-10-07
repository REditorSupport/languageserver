# Package-independent extraction of declarative constructors and registry dispatch.
# Every name and relation below comes from syntax or inert namespace bindings.

member_data <- function(node, constants = list(), depth = 0L) {
    if (depth > 12L || missing(node)) {
        return(NULL)
    }
    if (is.atomic(node)) {
        return(node)
    }
    if (is.symbol(node)) {
        return(constants[[as.character(node)]])
    }
    if (!is.call(node)) {
        return(NULL)
    }
    head <- member_name(node[[1L]])
    if (is.null(head)) {
        return(NULL)
    }
    if (!head %in% c("c", "setdiff", "names")) {
        return(NULL)
    }
    args <- as.list(node)[-1L]
    values <- lapply(args, member_data, constants, depth + 1L)
    if (head == "c" && all(vapply(values, function(x) !is.null(x), logical(1L)))) {
        return(unlist(values))
    }
    if (head == "setdiff" && length(values) == 2L && all(vapply(values, function(x) !is.null(x), logical(1L)))) {
        return(setdiff(values[[1L]], values[[2L]]))
    }
    if (head == "names" && length(args) == 1L && is.symbol(args[[1L]])) {
        return(names(constants[[as.character(args[[1L]])]]))
    }
    NULL
}

member_constructor <- function(fn) {
    if (!member_head(fn, "function")) {
        return(NULL)
    }
    body <- fn[[3L]]
    if (!member_head(body, "{")) {
        return(NULL)
    }
    statements <- as.list(body)[-1L]
    last <- statements[[length(statements)]]
    if (member_head(last, "return") && length(last) == 2L) last <- last[[2L]]
    target <- member_name(last)
    if (is.null(target)) {
        return(NULL)
    }
    classes <- NULL
    fields <- list()
    aliases <- list()
    created <- FALSE
    for (node in statements) {
        if (member_head(node, "<-") && is.symbol(node[[2L]])) {
            aliases[as.character(node[[2L]])] <- list(node[[3L]])
            if (identical(member_name(node[[2L]]), target) && member_head(node[[3L]], "new.env")) created <- TRUE
        }
        if (member_head(node, "<-") && member_head(node[[2L]], "class") &&
            identical(member_name(node[[2L]][[2L]]), target)) {
            classes <- member_strings(node[[3L]])
            # Native-dependent subclasses precede a literal base-class suffix.
            # Retain that guaranteed suffix without inventing a subtype.
            if (!length(classes) && member_head(node[[3L]], "c")) {
                parts <- as.list(node[[3L]])[-1L]
                literal <- vapply(parts, is.character, logical(1L))
                last_dynamic <- max(c(0L, which(!literal)))
                if (last_dynamic < length(parts)) classes <- unlist(parts[seq.int(last_dynamic + 1L, length(parts))])
            }
        }
        if (member_head(node, "<-") && member_head(node[[2L]], "$") &&
            identical(member_name(node[[2L]][[2L]]), target)) {
            name <- member_name(node[[2L]][[3L]])
            rhs <- node[[3L]]
            if (is.symbol(rhs) && !is.null(aliases[[as.character(rhs)]])) rhs <- aliases[[as.character(rhs)]]
            fields[name] <- list(list(expr = rhs, active = FALSE))
        }
        if (member_head(node, "makeActiveBinding") && length(node) >= 4L &&
            identical(member_name(node[[4L]]), target)) {
            name <- member_strings(node[[2L]])
            if (length(name) == 1L) fields[name] <- list(list(expr = node[[3L]], active = TRUE))
        }
    }
    # Conditional fields and computed class vectors are not unconditional
    # constructor guarantees. Only literal top-level declarations qualify.
    if (!created || !length(classes)) {
        return(NULL)
    }
    list(type = classes[[1L]], classes = classes, fields = fields, body = body, target = target)
}


member_definition <- function(definitions, key) {
    seen <- character()
    while (is.symbol(member_lookup(definitions, key))) {
        if (key %in% seen) {
            return(NULL)
        }
        seen <- c(seen, key)
        key <- as.character(definitions[[key]])
    }
    member_lookup(definitions, key)
}

member_returned_function <- function(fn) {
    if (!member_head(fn, "function")) {
        return(NULL)
    }
    body <- fn[[3L]]
    statements <- if (member_head(body, "{")) as.list(body)[-1L] else list(body)
    if (!length(statements)) {
        return(NULL)
    }
    last <- utils::tail(statements, 1L)[[1L]]
    if (member_head(last, "return") && length(last) == 2L) last <- last[[2L]]
    if (member_head(last, "function")) {
        return(list(fn = last, prelude = statements[-length(statements)]))
    }
    if (!is.symbol(last)) {
        return(NULL)
    }
    target <- as.character(last)
    returned <- NULL
    copied <- NULL
    prelude <- list()
    for (node in statements[-length(statements)]) {
        if (member_head(node, "<-") && identical(member_name(node[[2L]]), target) &&
            member_head(node[[3L]], "function")) {
            returned <- node[[3L]]
        } else if (member_head(node, "<-") && member_head(node[[2L]], "formals") &&
            identical(member_name(node[[2L]][[2L]]), target) && member_head(node[[3L]], "formals")) {
            copied <- member_name(node[[3L]][[2L]])
        } else if (is.null(returned)) prelude[[length(prelude) + 1L]] <- node
    }
    if (is.null(returned)) {
        return(NULL)
    }
    list(fn = returned, prelude = prelude, copied = copied)
}

member_source_registries <- function(input) {
    definitions <- input$definitions
    registries <- list()
    for (key in names(definitions)) {
        if (member_head(definitions[[key]], "new.env")) registries[key] <- list(character())
    }
    for (key in names(definitions)) {
        parts <- strsplit(key, "$", fixed = TRUE)[[1L]]
        if (length(parts) == 2L && parts[[1L]] %in% names(registries)) {
            registries[[parts[[1L]]]][parts[[2L]]] <- key
        }
    }
    # Validate the source population recipe before interpreting a literal
    # prefix-to-environment map. An unrelated named list is not a registry.
    collectors <- character()
    for (key in names(definitions)) {
        fn <- definitions[[key]]
        if (!member_head(fn, "function") || length(fn[[2L]]) < 2L) next
        args <- names(fn[[2L]])
        enumerates <- strips <- assigns <- FALSE
        member_walk(fn[[3L]], function(node) {
            if (member_head(node, "ls") && identical(member_name(as.list(node)$pattern), args[[2L]])) enumerates <<- TRUE
            if (member_head(node, "sub") && length(node) >= 4L &&
                identical(member_name(node[[2L]]), args[[2L]]) && identical(node[[3L]], "")) {
                strips <<- TRUE
            }
            if (member_head(node, "assign") && identical(member_name(as.list(node)$envir), args[[1L]])) assigns <<- TRUE
        })
        if (enumerates && strips && assigns) collectors <- c(collectors, key)
    }
    maps <- character()
    for (code in input$expressions) {
        member_walk(code, function(node) {
            if (!is.call(node) || length(node) < 3L) {
                return()
            }
            key <- member_name(node[[1L]])
            if (is.null(key) || !key %in% collectors) {
                return()
            }
            destination <- node[[2L]]
            pattern <- node[[3L]]
            if (!member_head(destination, "[[") || !member_head(pattern, "sprintf") ||
                length(pattern) != 3L || !identical(pattern[[2L]], "^%s") ||
                !identical(destination[[3L]], pattern[[3L]])) {
                return()
            }
            map <- member_name(destination[[2L]])
            if (!is.null(map)) maps <<- union(maps, map)
        })
    }
    for (map in maps) {
        node <- definitions[[map]]
        if (!member_head(node, "list")) next
        entries <- as.list(node)[-1L]
        for (prefix in names(entries)) {
            registry <- member_name(entries[[prefix]])
            if (is.null(registry) || !registry %in% names(registries) || !nzchar(prefix)) next
            keys <- names(definitions)[startsWith(names(definitions), prefix)]
            registries[[registry]] <- c(
                registries[[registry]],
                stats::setNames(keys, substring(keys, nchar(prefix) + 1L))
            )
        }
    }
    registries
}

member_package_index <- function(input) {
    index <- member_generic_index("")
    index$package <- input$package
    index$definitions <- input$definitions
    index$locations <- input$locations
    index$exports <- input$exports
    index$registries <- if (!is.null(input$registries)) input$registries else member_source_registries(input)
    index$intrinsics <- if (!is.null(input$intrinsics)) input$intrinsics else character()
    index$nonreturning_functions <- intersect(c("stop", "abort"), c(member_base_intrinsics, index$intrinsics))
    index$package_roots <- list()
    index$constructor_inputs <- list()
    index$namespace_owners <- list()
    index$delegation <- list()
    index$method_receivers <- list()
    constants <- index$registries
    for (key in names(index$definitions)) {
        node <- index$definitions[[key]]
        literal <- member_data(node, constants)
        if (!is.null(literal)) constants[key] <- list(literal)
        # S7 class declarations expose literal properties without constructing
        # an instance. Accept only a verified imported descriptor constructor.
        if (member_head(node, "new_class") && "new_class" %in% index$intrinsics) {
            type <- member_strings(node[[2L]])
            properties <- as.list(node)$properties
            if (length(type) == 1L && member_head(properties, "list")) {
                fields <- names(as.list(properties)[-1L])
                index$constructor_types[key] <- list(type)
                index$classes[type] <- list(type)
                index$members[type] <- list(stats::setNames(rep(NA_character_, length(fields)), fields))
            }
        }
        ctor <- member_constructor(node)
        if (is.null(ctor)) next
        index$constructors[key] <- list(ctor)
        index$classes[ctor$type] <- list(ctor$classes)
        fields <- names(ctor$fields)
        index$members[ctor$type] <- list(stats::setNames(rep(NA_character_, length(fields)), fields))
        # Constructor-installed bound closures are inferred from the assignment
        # RHS, regardless of native generator, class or factory naming scheme.
        for (field in fields) {
            recipe <- ctor$fields[[field]]
            factory <- if (is.call(recipe$expr)) member_key(recipe$expr[[1L]]) else NULL
            returned <- member_returned_function(member_definition(index$definitions, factory))
            if (!is.null(returned) && is.null(returned$copied)) {
                index$native_factories <- unique(c(index$native_factories, factory))
                index$members[[ctor$type]][field] <- factory
            }
        }
    }
    for (key in names(input$descriptors)) {
        descriptor <- input$descriptors[[key]]
        index$constructor_types[key] <- list(descriptor$type)
        index$classes[descriptor$type] <- list(descriptor$type)
        old <- index$members[[descriptor$type]]
        fields <- setdiff(descriptor$fields, names(old))
        index$members[descriptor$type] <- list(c(old, stats::setNames(rep(NA_character_, length(fields)), fields)))
    }
    # UseMethod and standard S3 method IDs supply input-class constraints for
    # constructors. Multiple generics can share a class without a global wrap map.
    for (key in names(index$definitions)) {
        node <- index$definitions[[key]]
        if (!member_head(node, "function")) next
        generic <- NULL
        member_walk(node[[3L]], function(child) {
            if (member_head(child, "UseMethod")) generic <<- member_strings(child[[2L]])
        }, descend_functions = FALSE)
        if (length(generic) != 1L) next
        for (method in names(index$constructors)[startsWith(names(index$constructors), paste0(generic, "."))]) {
            formal <- names(index$definitions[[method]][[2L]])[[1L]]
            index$constructor_inputs[[method]][formal] <- list(member_value(type = substring(method, nchar(generic) + 2L)))
        }
    }
    for (registry in names(index$registries)) {
        index$members[registry] <- list(index$registries[[registry]])
        index$package_roots[registry] <- list(member_value(type = registry))
    }
    for (registry in names(index$registries)) {
        for (field in names(index$registries[[registry]])) {
            key <- index$registries[[registry]][[field]]
            if (key %in% names(index$package_roots)) {
                index$members[[registry]][field] <- NA_character_
                index$properties[[registry]][field] <- list(index$package_roots[[key]])
            }
        }
    }
    # Every ordinary environment surface is discovered, including static bundles
    # and exported roots. Active names are retained but getters are only analyzed.
    for (name in names(input$snapshots)) {
        snapshot <- input$snapshots[[name]]
        shape <- member_snapshot_shape(snapshot)
        shape$type <- name
        for (field in names(index$registries[[name]])) {
            shape$fields[field] <- list(member_value(function_key = index$registries[[name]][[field]]))
        }
        for (field in names(input$links[[name]])) shape$fields[field] <- list(member_value(type = input$links[[name]][[field]]))
        index$package_roots[name] <- list(shape)
        for (field in names(snapshot$fields)) {
            if (!field %in% names(index$members[[name]])) index$members[[name]][field] <- NA_character_
        }
    }
    # Registry lookup branches describe membership, precedence, lexical receiver
    # rebinding, returned method factories and delegation through closure factories.
    for (key in names(index$definitions)[startsWith(names(index$definitions), "$.")]) {
        fn <- index$definitions[[key]]
        if (!member_head(fn, "function")) next
        type <- substring(key, 3L)
        formals <- names(fn[[2L]])
        if (length(formals) < 2L) next
        locals <- constants
        member_walk(fn[[3L]], function(node) {
            if (member_head(node, "<-") && is.symbol(node[[2L]])) {
                value <- member_data(node[[3L]], locals)
                if (!is.null(value)) locals[as.character(node[[2L]])] <<- list(value)
            }
        }, descend_functions = FALSE)
        members <- character()
        inspect <- function(node) {
            if (!is.call(node)) {
                return(invisible(NULL))
            }
            if (member_head(node, "if")) {
                condition <- node[[2L]]
                allowed <- if (member_head(condition, "%in%") &&
                    identical(member_name(condition[[2L]]), formals[[2L]])) {
                    member_data(condition[[3L]], locals)
                } else {
                    NULL
                }
                branch <- node[[3L]]
                registry <- variable <- receiver_name <- NULL
                factory_call <- NULL
                bound_factory <- FALSE
                member_walk(branch, function(child) {
                    if (!is.call(child)) {
                        return()
                    }
                    if (member_head(child, "[[") && member_name(child[[2L]]) %in% names(index$registries) &&
                        identical(member_name(child[[3L]]), formals[[2L]])) {
                        registry <<- member_name(child[[2L]])
                    }
                    if (member_head(child, "<-") && member_head(child[[3L]], "[[")) variable <<- member_name(child[[2L]])
                    if (member_head(child, "<-") && identical(member_name(child[[3L]]), formals[[1L]])) receiver_name <<- member_name(child[[2L]])
                    if (is.call(child[[1L]]) && member_head(child[[1L]], "[[")) bound_factory <<- TRUE
                    returned <- member_returned_function(member_definition(index$definitions, member_key(child[[1L]])))
                    if (!is.null(returned) && !is.null(returned$copied)) factory_call <<- child
                }, descend_functions = FALSE)
                if (!is.null(registry) && !is.null(allowed)) {
                    values <- index$registries[[registry]][intersect(allowed, names(index$registries[[registry]]))]
                    for (name in names(values)) {
                        original_key <- values[[name]]
                        if (bound_factory) index$native_factories <- unique(c(index$native_factories, original_key))
                        if (!is.null(receiver_name)) index$method_receivers[[paste(type, original_key, sep = "|")]] <- receiver_name
                        if (!is.null(factory_call)) {
                            factory_key <- member_key(factory_call[[1L]])
                            wrapper <- member_definition(index$definitions, factory_key)
                            returned <- member_returned_function(wrapper)
                            original <- member_definition(index$definitions, original_key)
                            if (!member_head(original, "function")) next
                            derived <- paste0(".delegated:", type, ":", name)
                            index$definitions[derived] <- list(as.call(list(as.name("function"), original[[2L]], returned$fn[[3L]])))
                            args <- as.list(factory_call)[-1L]
                            # Bind syntax arguments to factory formals, substituting
                            # the dispatch receiver and the selected original method.
                            captures <- list()
                            for (i in seq_along(args)) {
                                formal <- if (!is.null(names(args)) && nzchar(names(args)[[i]])) names(args)[[i]] else names(wrapper[[2L]])[[i]]
                                captures[formal] <- list(args[[i]])
                            }
                            index$delegation[derived] <- list(list(
                                original = original_key,
                                captures = captures, prelude = returned$prelude,
                                receiver = formals[[1L]], variable = variable
                            ))
                            values[[name]] <- derived
                        }
                    }
                    members <<- c(members, values[!names(values) %in% names(members)])
                }
                if (length(node) == 4L) inspect(node[[4L]])
            } else if (member_head(node, "{")) for (child in as.list(node)[-1L]) inspect(child)
            invisible(NULL)
        }
        inspect(fn[[3L]])
        old <- index$members[[type]]
        index$members[type] <- list(c(members, old[!names(old) %in% names(members)]))
    }
    # Constructor constraints and registry-derived namespaces propagate through
    # a bounded fixed point; no constructor or active property is called.
    for (pass in seq_len(4L)) {
        for (key in names(index$constructors)) {
            ctor <- index$constructors[[key]]
            env <- index$package_roots
            for (name in names(index$constructor_inputs[[key]])) env[name] <- list(index$constructor_inputs[[key]][[name]])
            env[ctor$target] <- list(member_value(type = ctor$type))
            for (field in names(ctor$fields)) {
                if (!is.na(index$members[[ctor$type]][[field]])) next
                recipe <- ctor$fields[[field]]
                expr <- recipe$expr
                if (isTRUE(recipe$active) && member_head(expr, "function")) expr <- expr[[3L]]
                value <- member_infer(expr, index, env)
                if (length(value$type) == 1L && value$type != ".never") index$properties[[ctor$type]][field] <- list(value)
            }
            member_walk(ctor$body, function(node) {
                if (!member_head(node, "makeActiveBinding") || length(node) < 4L ||
                    !identical(member_name(node[[4L]]), ctor$target)) {
                    return()
                }
                member_walk(node[[3L]], function(child) {
                    if (!is.call(child) || !member_head(child[[1L]], "[[")) {
                        return()
                    }
                    registry <- member_name(child[[1L]][[2L]])
                    methods <- member_lookup(index$registries, registry)
                    if (is.null(methods) || length(child) != 2L ||
                        !identical(member_name(child[[2L]]), ctor$target)) {
                        return()
                    }
                    index$namespace_owners[[registry]] <- unique(c(index$namespace_owners[[registry]], ctor$type))
                    for (name in names(methods)) {
                        target <- member_lookup(index$constructors, methods[[name]])
                        if (is.null(target)) next
                        formal <- names(index$definitions[[methods[[name]]]][[2L]])[[1L]]
                        index$constructor_inputs[[methods[[name]]]][formal] <- list(member_value(type = ctor$type))
                        index$properties[[ctor$type]][name] <- list(member_value(type = target$type))
                        index$members[[ctor$type]][name] <- NA_character_
                    }
                })
            })
        }
        index$cache <- new.env(parent = emptyenv())
    }
    for (name in names(input$snapshots)) {
        for (field in names(input$snapshots[[name]]$fields)) {
            record <- input$snapshots[[name]]$fields[[field]]
            if (identical(record$kind, "active") && member_head(record$syntax, "function")) {
                env <- c(lapply(record$captures, member_literal), index$package_roots)
                index$properties[[name]][field] <- list(member_infer(record$syntax[[3L]], index, env))
            }
        }
    }
    for (key in names(index$definitions)) {
        fn <- index$definitions[[key]]
        if (!member_head(fn, "function")) next
        member_walk(fn[[3L]], function(node) {
            if (!member_head(node, "assign") || length(node) < 4L) {
                return()
            }
            args <- as.list(node)[-1L]
            registry <- member_name(if (!is.null(args$envir)) args$envir else args[[3L]])
            name_arg <- member_name(args[[1L]])
            value_arg <- member_name(args[[2L]])
            if (is.null(registry) || !registry %in% names(index$namespace_owners) ||
                !all(c(name_arg, value_arg) %in% names(fn[[2L]]))) {
                return()
            }
            index$registration_rules[key] <- list(list(
                formals = names(fn[[2L]]),
                name_arg = name_arg, value_arg = value_arg, owners = index$namespace_owners[[registry]]
            ))
        }, descend_functions = FALSE)
    }
    # Source-only active property declarations and literal registration loops.
    # These recipes are analyzed against lexical metadata, never executed.
    for (code in input$expressions) {
        member_walk(code, function(node) {
            if (!member_head(node, "lapply") || length(node) < 3L ||
                !member_head(node[[3L]], "function")) {
                return()
            }
            values <- member_data(node[[2L]], constants)
            if (!is.character(values) || length(values) > 256L) {
                return()
            }
            variable <- names(node[[3L]][[2L]])[[1L]]
            member_walk(node[[3L]][[3L]], function(child) {
                if (!member_head(child, "makeActiveBinding") || length(child) < 4L ||
                    !identical(member_name(child[[2L]]), variable)) {
                    return()
                }
                target <- member_name(child[[4L]])
                if (is.null(target) || !target %in% names(index$package_roots)) {
                    return()
                }
                getter <- child[[3L]]
                if (!member_head(getter, "function")) {
                    return()
                }
                for (name in values) {
                    env <- index$package_roots
                    env[variable] <- list(member_literal(name))
                    index$members[[target]][name] <- NA_character_
                    index$properties[[target]][name] <- list(member_infer(getter[[3L]], index, env))
                }
            })
        })
        for (node in code) {
            if (!member_head(node, "makeActiveBinding") || length(node) < 4L ||
                !member_head(node[[3L]], "function")) {
                next
            }
            name <- member_strings(node[[2L]])
            target <- member_name(node[[4L]])
            if (length(name) != 1L || is.null(target) || !target %in% names(index$package_roots)) next
            index$members[[target]][name] <- NA_character_
            index$properties[[target]][name] <- list(member_infer(node[[3L]][[3L]], index, index$package_roots))
        }
    }
    index$record_dispatch_classes <- unique(unlist(lapply(index$classes, function(x) utils::tail(x, 1L))))
    index$roots <- index$package_roots[intersect(names(index$package_roots), index$exports)]
    for (name in names(index$roots)) index$namespace_roots[paste(input$package, name, sep = "::")] <- list(index$roots[[name]])
    index$cache <- new.env(parent = emptyenv())
    index
}
