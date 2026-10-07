# Source-only counterpart of installed-package extraction. All facts below
# come from parsed declarations/bodies; no package/example expression is run.
# Source inference.R, inference-v2.R, and polars-adapter.R before this file.

static_data <- function(node, constants = list(), depth = 0L) {
    if (depth > 12L || missing(node)) return(NULL)
    if (is.atomic(node)) return(node)
    if (is.symbol(node)) return(constants[[as.character(node)]])
    if (!is.call(node)) return(NULL)
    head <- static_name(node[[1L]])
    if (is.null(head)) return(NULL)
    if (!head %in% c("c", "setdiff", "names")) return(NULL)
    args <- as.list(node)[-1L]
    values <- lapply(args, static_data, constants, depth + 1L)
    if (head == "c" && all(vapply(values, function(x) !is.null(x), logical(1L)))) return(unlist(values))
    if (head == "setdiff" && length(values) == 2L && all(vapply(values, function(x) !is.null(x), logical(1L)))) return(setdiff(values[[1L]], values[[2L]]))
    if (head == "names" && length(args) == 1L && is.symbol(args[[1L]])) return(names(constants[[as.character(args[[1L]])]]))
    NULL
}

static_constructor <- function(fn) {
    if (!static_head(fn, "function")) return(NULL)
    body <- fn[[3L]]
    if (!static_head(body, "{")) return(NULL)
    statements <- as.list(body)[-1L]
    last <- statements[[length(statements)]]
    if (static_head(last, "return") && length(last) == 2L) last <- last[[2L]]
    target <- static_name(last)
    if (is.null(target)) return(NULL)
    classes <- NULL
    fields <- list()
    aliases <- list()
    created <- FALSE
    for (node in statements) {
        if (static_head(node, "<-") && is.symbol(node[[2L]])) {
            aliases[as.character(node[[2L]])] <- list(node[[3L]])
            if (identical(static_name(node[[2L]]), target) && static_head(node[[3L]], "new.env")) created <- TRUE
        }
        if (static_head(node, "<-") && static_head(node[[2L]], "class") &&
            identical(static_name(node[[2L]][[2L]]), target)) {
            classes <- static_strings(node[[3L]])
            # Native-dependent subclasses precede a literal base-class suffix.
            # Retain that guaranteed suffix without inventing a subtype.
            if (!length(classes) && static_head(node[[3L]], "c")) {
                parts <- as.list(node[[3L]])[-1L]
                literal <- vapply(parts, is.character, logical(1L))
                last_dynamic <- max(c(0L, which(!literal)))
                if (last_dynamic < length(parts)) classes <- unlist(parts[seq.int(last_dynamic + 1L, length(parts))])
            }
        }
        if (static_head(node, "<-") && static_head(node[[2L]], "$") &&
            identical(static_name(node[[2L]][[2L]]), target)) {
            name <- static_name(node[[2L]][[3L]])
            rhs <- node[[3L]]
            if (is.symbol(rhs) && !is.null(aliases[[as.character(rhs)]])) rhs <- aliases[[as.character(rhs)]]
            fields[name] <- list(list(expr = rhs, active = FALSE))
        }
        if (static_head(node, "makeActiveBinding") && length(node) >= 4L &&
            identical(static_name(node[[4L]]), target)) {
            name <- static_strings(node[[2L]])
            if (length(name) == 1L) fields[name] <- list(list(expr = node[[3L]], active = TRUE))
        }
    }
    # Conditional fields and computed class vectors are not unconditional
    # constructor guarantees. Only literal top-level declarations qualify.
    if (!created || !length(classes)) return(NULL)
    list(type = classes[[1L]], classes = classes, fields = fields, body = body)
}

static_polars_index_v1 <- static_polars_index
static_polars_index <- function(source_root) {
    index <- static_polars_index_v1(source_root)
    index$cache <- new.env(parent = emptyenv())
    definitions <- index$definitions
    index$classes <- list()
    index$constructors <- list()
    index$constructor_types <- list()
    index$wrap_constructors <- list()
    index$native_factories <- character()
    index$registration_rules <- list()
    index$namespace_owners <- list()
    index$intrinsics <- character()
    constants <- index$registries
    for (key in names(definitions)) {
        value <- static_data(definitions[[key]], constants)
        if (!is.null(value)) constants[key] <- list(value)
        ctor <- static_constructor(definitions[[key]])
        if (!is.null(ctor)) {
            index$constructors[key] <- list(ctor)
            index$classes[ctor$type] <- list(ctor$classes)
            if (startsWith(key, "wrap.polars::")) index$wrap_constructors[substring(key, 6L)] <- list(key)
            fields <- names(ctor$fields)
            old <- index$members[[ctor$type]]
            extra <- setdiff(fields, names(old))
            index$members[ctor$type] <- list(c(old, setNames(rep(NA_character_, length(extra)), extra)))
        }
        fn <- definitions[[key]]
        if (static_head(fn, "function")) {
            body <- fn[[3L]]
            last <- if (static_head(body, "{")) body[[length(body)]] else body
            if (static_head(last, "function") && any(startsWith(key,
                paste0(unlist(index$native_member_prefixes), "_")))) index$native_factories <- c(index$native_factories, key)
        }
    }
    # Literal environment registries are roots/properties, not functions.
    for (registry in names(index$registries)) {
        definition <- definitions[[registry]]
        if (static_head(definition, "new.env")) {
            index$members[registry] <- list(index$registries[[registry]])
            index$roots[registry] <- list(static_value(type = registry))
            index$namespace_roots[paste0("polars::", registry)] <- list(static_value(type = registry))
        }
    }
    # Package internals remain lexical bindings during body analysis. Only
    # exported environment roots are introduced by library() in document code.
    exports <- character()
    for (node in parse(file.path(source_root, "NAMESPACE"))) {
        if (static_head(node, "export")) exports <- c(exports, vapply(as.list(node)[-1L], static_name, character(1L)))
    }
    index$package_roots <- index$roots
    index$exports <- exports
    index$roots <- index$roots[intersect(names(index$roots), exports)]
    namespace_keys <- paste0("polars::", exports)
    index$namespace_roots <- index$namespace_roots[intersect(names(index$namespace_roots), namespace_keys)]
    index$properties$pl$api <- static_value(type = "pl__api")
    index$members$pl["api"] <- NA_character_

    # Derive dispatch membership/exclusions from the name expressions and the
    # registry lookups in each branch. A delegated closure gets a derived
    # function summary of the adapter factory's returned body.
    for (key in names(definitions)[startsWith(names(definitions), "$.")]) {
        type <- substring(key, 3L)
        fn <- definitions[[key]]
        if (!static_head(fn, "function")) next
        body <- fn[[3L]]
        locals <- constants
        static_walk(body, function(node) {
            if (static_head(node, "<-") && is.symbol(node[[2L]])) {
                value <- static_data(node[[3L]], locals)
                if (!is.null(value)) locals[as.character(node[[2L]])] <<- list(value)
            }
        }, descend_functions = FALSE)
        members <- character()
        inspect_branch <- function(node) {
            if (!is.call(node)) return(invisible(NULL))
            if (static_head(node, "if")) {
                condition <- node[[2L]]
                allowed <- if (static_head(condition, "%in%")) static_data(condition[[3L]], locals) else NULL
                branch <- node[[3L]]
                registry <- NULL
                factory <- NULL
                static_walk(branch, function(child) {
                    if (static_head(child, "[[") && static_name(child[[2L]]) %in% names(index$registries)) registry <<- static_name(child[[2L]])
                    if (static_head(child, "expr_wrap_function_factory")) factory <<- child
                }, descend_functions = FALSE)
                if (!is.null(registry) && !is.null(allowed)) {
                    values <- index$registries[[registry]][intersect(allowed, names(index$registries[[registry]]))]
                    if (!is.null(factory)) {
                        wrapper <- definitions[[static_name(factory[[1L]])]]
                        returned <- NULL
                        static_walk(wrapper, function(child) {
                            if (static_head(child, "<-") && static_head(child[[3L]], "function")) returned <<- child[[3L]]
                        }, descend_functions = TRUE)
                        if (!is.null(returned)) {
                            for (name in names(values)) {
                                derived <- paste0(".delegated:", type, ":", name)
                                original <- definitions[[values[[name]]]]
                                visited <- character()
                                while (is.symbol(original) && !as.character(original) %in% visited) {
                                    visited <- c(visited, as.character(original))
                                    original <- definitions[[as.character(original)]]
                                }
                                if (!static_head(original, "function")) next
                                generated <- as.call(list(as.name("function"), original[[2L]], returned[[3L]]))
                                index$definitions[derived] <- list(generated)
                                index$delegation[derived] <- list(list(original = values[[name]], factory = factory, registry = registry))
                                values[[name]] <- derived
                            }
                        }
                    }
                    members <<- c(members, values[!names(values) %in% names(members)])
                }
                if (length(node) == 4L) inspect_branch(node[[4L]])
                return(invisible(NULL))
            }
            if (static_head(node, "{")) for (child in as.list(node)[-1L]) inspect_branch(child)
            invisible(NULL)
        }
        inspect_branch(body)
        if (length(members)) {
            old <- index$members[[type]]
            properties <- old[is.na(old)]
            index$members[type] <- list(c(members, properties[!names(properties) %in% names(members)]))
        }
    }
    # Raw parameter-field propagation and namespace getters follow the actual
    # constructor bodies. Iterate small monotone metadata additions to resolve
    # dependencies between public/raw wrappers and namespace constructors.
    for (pass in seq_len(3L)) {
        for (key in names(index$constructors)) {
            ctor <- index$constructors[[key]]
            if (startsWith(key, ".savvy_wrap_")) next
            env <- index$package_roots
            if (startsWith(key, "wrap.polars::")) env$x <- static_value(type = substring(key, 6L))
            if (startsWith(key, "namespace_")) {
                registry <- if (startsWith(key, "namespace_expr_")) "polars_expr" else "polars_series"
                env$x <- static_value(type = registry)
            }
            env$self <- static_value(type = ctor$type)
            for (field in names(ctor$fields)) {
                recipe <- ctor$fields[[field]]
                expr <- recipe$expr
                if (isTRUE(recipe$active) && static_head(expr, "function")) expr <- expr[[3L]]
                value <- static_infer(expr, index, env)
                if (length(value$type) == 1L && value$type != ".never") {
                    index$properties[[ctor$type]][field] <- list(value)
                    if (startsWith(value$type, "polars::")) index$raw_fields[[ctor$type]][field] <- value$type
                } else if (!is.null(value$function_key)) index$members[[ctor$type]][field] <- value$function_key
            }
            # Loop-installed namespace bindings: read names from the actual
            # constructor's referenced registry, then derive constructor output.
            static_walk(ctor$body, function(node) {
                if (!static_head(node, "makeActiveBinding") || length(node) < 4L) return()
                fn <- node[[3L]]
                static_walk(fn, function(child) {
                    if (!static_head(child, "[[")) return()
                    registry <- static_name(child[[2L]])
                    methods <- static_lookup(index$registries, registry)
                    if (is.null(methods)) return()
                    for (name in names(methods)) {
                        target <- static_lookup(index$constructors, methods[[name]])
                        if (!is.null(target)) {
                            index$namespace_owners[[registry]] <- unique(c(index$namespace_owners[[registry]], ctor$type))
                            index$properties[[ctor$type]][name] <- list(static_value(type = target$type))
                            index$members[[ctor$type]][name] <- NA_character_
                        }
                    }
                })
            })
        }
        index$cache <- new.env(parent = emptyenv())
    }
    # Derive literal registration effects from registry setters and from the
    # constructors consuming that registry. The core has no named Polars hook.
    for (key in names(definitions)) {
        fn <- definitions[[key]]
        if (!static_head(fn, "function")) next
        static_walk(fn[[3L]], function(node) {
            if (!static_head(node, "assign") || length(node) < 4L) return()
            args <- as.list(node)[-1L]
            registry <- if (!is.null(args$envir)) static_name(args$envir) else if (length(args) >= 3L) static_name(args[[3L]]) else NULL
            if (is.null(registry) || !registry %in% names(index$namespace_owners)) return()
            name_arg <- static_name(args[[1L]])
            value_arg <- static_name(args[[2L]])
            if (!all(c(name_arg, value_arg) %in% names(fn[[2L]]))) return()
            index$registration_rules[key] <- list(list(formals = names(fn[[2L]]),
                name_arg = name_arg, value_arg = value_arg, owners = index$namespace_owners[[registry]]))
        }, descend_functions = FALSE)
    }
    # Extract active constants from literal-name lapply/for registration loops,
    # retaining getter syntax. The same names come directly from runtime ls().
    for (path in sort(list.files(file.path(source_root, "R"), "\\.[Rr]$", full.names = TRUE))) {
        code <- parse(path)
        static_walk(code, function(node) {
            if (!static_head(node, "lapply") || length(node) < 3L || !static_head(node[[3L]], "function")) return()
            values <- static_data(node[[2L]], constants)
            if (!is.character(values)) return()
            fn <- node[[3L]]
            variable <- names(fn[[2L]])[[1L]]
            static_walk(fn[[3L]], function(child) {
                if (!static_head(child, "makeActiveBinding") || !identical(static_name(child[[2L]]), variable)) return()
                target <- static_name(child[[4L]])
                if (!target %in% names(index$members)) return()
                getter <- child[[3L]]
                for (name in values) {
                    env <- index$package_roots
                    env[variable] <- list(static_literal(name))
                    value <- static_infer(getter[[3L]], index, env)
                    index$members[[target]][name] <- NA_character_
                    index$properties[[target]][name] <- list(value)
                }
            })
        })
    }
    # S7 literal descriptors supply class/property metadata without constructing
    # an instance. Analyze the constructor separately in formal implementation.
    for (key in names(definitions)) {
        node <- definitions[[key]]
        if (!static_head(node, "new_class")) next
        type <- static_strings(node[[2L]])
        properties <- as.list(node)[["properties"]]
        if (length(type) == 1L && static_head(properties, "list")) {
            names <- names(as.list(properties)[-1L])
            index$constructor_types[key] <- list(type)
            index$classes[type] <- list(type)
            index$members[type] <- list(setNames(rep(NA_character_, length(names)), names))
        }
    }
    index$cache <- new.env(parent = emptyenv())
    index
}
