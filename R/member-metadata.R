# Metadata preparation runs in package workers. Request handlers only read
# serializable snapshots; no runtime object is retained as a completion hook.

member_base_intrinsics <- c(
    "list", "new.env", "invisible", "identity", "local", "withAutoprint",
    "head", "tail", "tryCatch", "structure", "data.frame", "length", "startsWith",
    "c", ":", "seq", "seq_len", "seq_along", "rep", "rep.int", "as.character",
    "character", "as.integer", "integer", "as.numeric", "as.double", "numeric",
    "double", "as.logical", "logical", "as.raw", "raw", "charToRaw", "as.Date",
    "as.POSIXct", "factor", "paste", "paste0", "sprintf", "names", "is.null",
    "missing", "isTRUE", "isFALSE", "inherits", "is.character", "is.numeric",
    "is.integer", "is.logical", "is.list", "lapply", "Reduce", "UseMethod",
    "NextMethod", "stop", "switch", "+", "-", "*", "/", "^", "%%", "%/%", "&",
    "|", "!", "==", "!=", ">", "<", ">=", "<=", "&&", "||"
)

member_binding <- function(env, name, materialize = FALSE) {
    .Call("member_binding_c", env, name, materialize, PACKAGE = "languageserver")
}

member_function_syntax <- function(fn) {
    as.call(list(as.name("function"), formals(fn), body(fn)))
}

member_list_storage <- function(value) {
    storage <- unclass(value)
    nms <- attr(value, "names", exact = TRUE)
    # Dimensions supply implicit matrix/array classes even after unclass().
    # Keep only names so lapply() cannot dispatch a coercion method.
    attributes(storage) <- if (is.null(nms)) NULL else list(names = nms)
    storage
}

member_snapshot <- function(env, max_depth = 4L, max_bindings = 5000L, s7_modes = list(),
    lookup_scope = parent.frame()) {
    seen <- list()
    remaining <- max_bindings
    method_scopes <- list()
    scope <- lookup_scope
    while (is.environment(scope) && !identical(scope, emptyenv())) {
        method_scopes[[length(method_scopes) + 1L]] <- list(env = scope, names = ls(scope, all.names = TRUE))
        scope <- parent.env(scope)
    }
    # $ is an internal S3 generic whose registered methods live in base.
    # Inspect binding kinds rather than calling getS3method(), which can force
    # deferred method bindings or invoke active bindings during lookup.
    table <- member_binding(asNamespace("base"), ".__S3MethodsTable__.")
    registered <- if (identical(table[[1L]], "value") && is.environment(table[[2L]])) table[[2L]] else NULL
    if (!is.null(registered)) {
        method_scopes[[length(method_scopes) + 1L]] <- list(
            env = registered, names = ls(registered, all.names = TRUE)
        )
    }
    dollar_methods <- new.env(parent = emptyenv())
    default_dollar <- function(current) {
        if (isS4(current) || is.null(registered)) return(FALSE)
        classes <- attr(current, "class", exact = TRUE)
        if (!is.null(classes) && (!is.character(classes) || anyNA(classes))) return(FALSE)
        if (length(classes)) classes <- c(classes, "default")
        for (class in classes) {
            method <- paste0("$.", class)
            present <- get0(method, dollar_methods, inherits = FALSE)
            if (is.null(present)) {
                present <- FALSE
                for (scope in method_scopes) {
                    if (!method %in% scope$names) next
                    record <- member_binding(scope$env, method)
                    if (!identical(record[[1L]], "value") || is.function(record[[2L]])) {
                        present <- TRUE
                        break
                    }
                }
                assign(method, present, dollar_methods)
            }
            if (present) return(FALSE)
        }
        TRUE
    }
    captures <- function(fn) {
        scope <- environment(fn)
        if (is.null(scope) || isNamespace(scope) || identical(scope, globalenv()) ||
            identical(scope, baseenv())) {
            return(list())
        }
        nms <- ls(scope, all.names = TRUE)
        if (length(nms) > 100L) {
            return(list())
        }
        values <- list()
        for (name in nms) {
            record <- member_binding(scope, name)
            x <- record[[2L]]
            if (identical(record[[1L]], "value") && !is.object(x) &&
                is.atomic(x) && length(x) <= 100L) {
                values[name] <- list(x)
            }
        }
        values
    }
    r6_cache <- new.env(parent = emptyenv())
    r6_cache$receivers <- r6_cache$scopes <- list()
    r6_method_scopes <- function(receiver) {
        cached <- which(vapply(r6_cache$receivers, identical, logical(1L), receiver))
        if (length(cached)) return(r6_cache$scopes[[cached[[1L]]]])
        record <- member_binding(receiver, ".__enclos_env__")
        queue <- if (identical(record[[1L]], "value") && is.environment(record[[2L]])) list(record[[2L]]) else list()
        scopes <- list()
        checked <- 0L
        next_scope <- 1L
        while (next_scope <= length(queue) && length(scopes) < 32L) {
            scope <- queue[[next_scope]]
            next_scope <- next_scope + 1L
            if (any(vapply(scopes, identical, logical(1L), scope))) next
            self <- member_binding(scope, "self")
            if (!identical(self[[1L]], "value") || !identical(self[[2L]], receiver)) next
            scopes[[length(scopes) + 1L]] <- scope
            if (checked >= 5000L) next
            super <- member_binding(scope, "super")
            if (!identical(super[[1L]], "value") || !is.environment(super[[2L]])) next
            for (name in utils::head(ls(super[[2L]], all.names = TRUE), 5000L - checked)) {
                checked <- checked + 1L
                method <- member_binding(super[[2L]], name)
                if (identical(method[[1L]], "value") && typeof(method[[2L]]) == "closure") {
                    enclosing <- environment(method[[2L]])
                    if (!any(vapply(queue, identical, logical(1L), enclosing))) {
                        queue[[length(queue) + 1L]] <- enclosing
                    }
                }
            }
        }
        key <- length(r6_cache$receivers) + 1L
        r6_cache$receivers[key] <- list(receiver)
        r6_cache$scopes[key] <- list(scopes)
        scopes
    }
    visit <- function(current, depth, r6_receiver = NULL) {
        classes <- attr(current, "class", exact = TRUE)
        r6_instance <- "R6" %in% classes && !"R6ClassGenerator" %in% classes
        if (r6_instance) r6_receiver <- current
        kind <- if (r6_instance) "r6_instance" else "environment"
        old <- which(vapply(seen, identical, logical(1L), current))
        if (length(old)) {
            return(list(kind = "reference", id = old[[1L]]))
        }
        id <- length(seen) + 1L
        seen[[id]] <<- current
        if (depth > max_depth) {
            return(list(kind = kind, id = id, complete = FALSE))
        }
        nms <- ls(current, all.names = TRUE)
        if (length(nms) > remaining) {
            return(list(kind = kind, id = id, complete = FALSE))
        }
        remaining <<- remaining - length(nms)
        fields <- stats::setNames(lapply(nms, function(name) {
            record <- member_binding(current, name)
            value <- record[[2L]]
            field <- if (identical(record[[1L]], "active")) {
                list(
                    kind = "active", syntax = if (typeof(value) == "closure") member_function_syntax(value),
                    captures = if (typeof(value) == "closure") captures(value) else list()
                )
            } else if (identical(record[[1L]], "value")) {
                receiver <- if (typeof(value) == "closure" ||
                        (is.environment(value) && name %in% c(".__enclos_env__", "private", "super"))) r6_receiver
                describe(value, depth + 1L, receiver)
            } else {
                list(kind = "unknown", reason = "promise_not_forced")
            }
            if (!is.null(r6_receiver)) field$locked <- bindingIsLocked(name, current)
            field
        }), nms)
        list(
            kind = kind, id = id, complete = TRUE, classes = classes, fields = fields,
            default_dollar = default_dollar(current),
            r6_locked = if (!is.null(r6_receiver)) environmentIsLocked(current) else NULL
        )
    }
    describe <- function(value, depth, r6_receiver = NULL) {
        if (missing(value) || identical(value, quote(expr = ))) return(list(kind = "unknown", reason = "missing_value"))
        descriptor <- member_s7_instance_descriptor(value, s7_modes)
        if (!is.null(descriptor)) {
            if (depth > max_depth) return(list(kind = "unknown", reason = "snapshot_depth"))
            slots <- list()
            for (name in names(descriptor$properties)) {
                property <- descriptor$properties[[name]]
                if (property$getter || property$setter) next
                # Reserved base attributes use underscore storage in S7 1.0.
                storage <- if (!is.null(attr(value, "_S7_class", exact = TRUE)) && name %in% c(
                    "names", "dim", "dimnames", "class", "tsp", "comment", "row.names"
                )) paste0("_", name) else name
                stored <- attr(value, storage, exact = TRUE)
                if (!is.null(stored)) slots[name] <- list(describe(stored, depth + 1L))
            }
            return(list(kind = "s7_instance", descriptor = descriptor, slots = slots))
        }
        descriptor <- member_s7_runtime_descriptor(value, modes = s7_modes)
        if (!is.null(descriptor)) return(list(kind = "s7", descriptor = descriptor))
        snapshot <- member_s4_instance_snapshot(value, describe, depth, max_depth, lookup_scope,
            reserve = function(n) {
                if (n > remaining) return(FALSE)
                remaining <<- remaining - n
                TRUE
            })
        if (!is.null(snapshot)) return(snapshot)
        if (typeof(value) == "closure") {
            syntax <- member_function_syntax(value)
            scope <- environment(value)
            self <- if (!is.null(r6_receiver) && is.environment(scope)) member_binding(scope, "self")
            bound <- identical(self[[1L]], "value") && identical(self[[2L]], r6_receiver) &&
                any(vapply(r6_method_scopes(r6_receiver), identical, logical(1L), scope))
            contexts <- list()
            if (bound) {
                for (name in intersect(c("private", "super"), member_syntax_names(syntax))) {
                    record <- member_binding(scope, name)
                    if (identical(record[[1L]], "value")) {
                        contexts[name] <- list(describe(record[[2L]], depth + 1L, r6_receiver))
                    }
                }
            }
            list(kind = "function", syntax = syntax, captures = captures(value),
                r6_bound = bound, r6_contexts = contexts,
                r6_classes = if (bound) attr(r6_receiver, "class", exact = TRUE))
        } else if (is.environment(value)) {
            # Namespace registries can refer back to whole package namespaces.
            # Apply the root exclusion recursively rather than copying those
            # large scopes through every private registry reference.
            if (isNamespace(value) || identical(value, globalenv()) || identical(value, baseenv())) {
                return(list(kind = "unknown", reason = "namespace_environment"))
            }
            visit(value, depth, r6_receiver)
        } else if (!is.object(value) && is.atomic(value) && length(value) <= 100L) {
            list(kind = "literal", value = value)
        } else if (typeof(value) %in% c("list", "pairlist") && !isS4(value) && depth <= max_depth) {
            storage <- member_list_storage(value)
            if (length(storage) > remaining) return(list(kind = "unknown", reason = "snapshot_budget"))
            remaining <<- remaining - length(storage)
            list(
                kind = "list", type = typeof(value), elements = lapply(storage, describe, depth + 1L),
                classes = attr(value, "class", exact = TRUE), default_dollar = default_dollar(value)
            )
        } else {
            list(kind = "unknown", reason = "unsupported_storage")
        }
    }
    describe(env, 0L)
}

member_snapshot_shape <- function(snapshot, package = "") {
    records <- list()
    collect <- function(record) {
        if (record$kind %in% c("environment", "r6_instance")) {
            records[[as.character(record$id)]] <<- record
            for (field in record$fields) collect(field)
        } else if (identical(record$kind, "list")) {
            for (field in record$elements) collect(field)
        } else if (record$kind %in% c("s7_instance", "s4_instance")) {
            for (field in record$slots) collect(field)
        } else if (identical(record$kind, "function")) {
            for (context in record$r6_contexts) collect(context)
        }
    }
    collect(snapshot)
    describe_shape <- function(record, trail = integer()) {
        if (identical(record$kind, "reference")) {
            if (record$id %in% trail) {
                return(member_value(reason = "cycle"))
            }
            return(decode(records[[as.character(record$id)]], trail))
        }
        if (identical(record$kind, "function")) {
            value <- member_value(
                type = "function", function_expr = record$syntax, closure = lapply(record$captures, member_literal)
            )
            if (isTRUE(record$r6_bound)) {
                classes <- setdiff(record$r6_classes, "R6")
                class <- list(name = if (length(classes)) classes[[1L]] else NULL, package = package)
                value$receiver_name <- "self"
                value$r6_static_bound <- TRUE
                private <- member_lookup(record$r6_contexts, "private")
                if (identical(private$kind, "reference") && identical(private$id, utils::tail(trail, 1L))) {
                    value$r6_private_self <- TRUE
                }
                value$closure$self <- NULL
                for (name in names(record$r6_contexts)) {
                    stored <- record$r6_contexts[[name]]
                    context <- decode(stored, trail)
                    if (identical(stored$kind, "reference")) stored <- records[[as.character(stored$id)]]
                    context$r6_class <- class
                    context$r6_role <- name
                    context$r6_locked <- isTRUE(stored$r6_locked)
                    fields <- stored$fields
                    context$r6_bindings <- stats::setNames(vapply(fields, function(field) {
                        if (identical(field$kind, "active")) {
                            "active"
                        } else if (isTRUE(field$locked)) {
                            if (isTRUE(field$r6_bound)) "method" else "locked"
                        } else {
                            "data"
                        }
                    }, character(1L)), names(fields))
                    value$closure[name] <- list(context)
                }
            }
            return(value)
        }
        if (identical(record$kind, "literal")) {
            return(member_literal(record$value))
        }
        if (record$kind %in% c("environment", "r6_instance")) {
            if (record$id %in% trail || !isTRUE(record$complete)) {
                return(member_value(reason = "cycle_or_budget"))
            }
            value <- member_value(
                type = "environment", classes = record$classes,
                fields = lapply(record$fields, decode, c(trail, record$id)),
                default_dollar = record$default_dollar
            )
            return(if (identical(record$kind, "r6_instance")) member_r6_instance_shape(value, record, package) else value)
        }
        if (identical(record$kind, "s4_instance")) return(member_s4_snapshot_shape(record, decode, trail))
        if (identical(record$kind, "s7_instance")) {
            value <- member_s7_shape(record$descriptor)
            for (name in names(record$slots)) value$slots[name] <- list(decode(record$slots[[name]], trail))
            return(value)
        }
        if (identical(record$kind, "s7")) {
            return(if (identical(record$descriptor$kind, "class")) member_s7_generator(record$descriptor) else
                    member_s7_value(type = "S7_descriptor", s7_descriptor = record$descriptor))
        }
        if (identical(record$kind, "list")) {
            values <- lapply(record$elements, decode, trail)
            fields <- if (is.null(names(values))) list() else values[!is.na(names(values)) & nzchar(names(values))]
            named <- length(values) && !is.null(names(values)) && !anyNA(names(values)) && all(nzchar(names(values)))
            return(member_value(
                type = if (is.null(record$type)) "list" else record$type, fields = fields,
                elements = if (named) NULL else values,
                classes = record$classes, default_dollar = record$default_dollar,
                named_elements = isTRUE(named)
            ))
        }
        member_value(reason = if (identical(record$kind, "active")) "getter_not_invoked" else record$reason)
    }
    decode <- function(record, trail = integer()) {
        value <- describe_shape(record, trail)
        if (nzchar(package)) {
            if (!is.null(value$function_expr)) value$metadata <- package
            if (!is.null(value$fields) || !is.null(value$slot_types)) value$member_package <- package
        }
        # Namespace values can repeat hundreds of small structures. Optional
        # NULL fields and duplicate named elements need no serialized storage.
        value[!vapply(value, is.null, logical(1L))]
    }
    decode(snapshot)
}

member_source_input <- function(root) {
    definitions <- locations <- expressions <- list()
    for (path in sort(list.files(file.path(root, "R"), "\\.[Rr]$", full.names = TRUE))) {
        code <- parse(path, keep.source = FALSE)
        expressions[[length(expressions) + 1L]] <- code
        for (expr in code) {
            if (!member_head(expr, "<-") || length(expr) != 3L) next
            key <- member_key(expr[[2L]])
            if (is.null(key)) next
            definitions[key] <- list(expr[[3L]])
            locations[key] <- list(basename(path))
        }
    }
    exports <- intrinsics <- character()
    providers <- list(rlang = c(
        "list2", "arg_match0", "arg_match", "try_fetch",
        "is_character", "is_list", "is_bool", "abort"
    ), S7 = member_s7_intrinsics, methods = member_methods_intrinsics)
    for (expr in parse(file.path(root, "NAMESPACE"))) {
        if (member_head(expr, "import") || member_head(expr, "importFrom")) {
            provider <- member_name(expr[[2L]])
            allowed <- member_lookup(providers, provider)
            if (member_head(expr, "importFrom")) {
                allowed <- intersect(
                    allowed,
                    vapply(as.list(expr)[-c(1L, 2L)], member_name, character(1L))
                )
            }
            intrinsics <- union(intrinsics, setdiff(allowed, names(definitions)))
        }
        if (member_head(expr, "export")) {
            exports <- c(
                exports,
                vapply(as.list(expr)[-1L], member_name, character(1L))
            )
        }
    }
    list(
        definitions = definitions, locations = locations, expressions = expressions,
        exports = exports, intrinsics = intrinsics, package = read.dcf(file.path(root, "DESCRIPTION"))[[1L, "Package"]]
    )
}

member_namespace_input <- function(
    ns, package = unname(getNamespaceName(ns)),
    exports = getNamespaceExports(ns)
) {
    nms <- ls(ns, all.names = TRUE)
    if (length(nms) > 10000L) stop("Namespace exceeds metadata budget")
    definitions <- snapshots <- registries <- links <- descriptors <- s4_classes <- s4_roots <- s4_objects <- list()
    s7_roots <- list()
    s7_modes <- member_s7_capabilities()
    functions <- values <- list()
    for (name in nms) {
        record <- member_binding(ns, name, materialize = TRUE)
        value <- record[[2L]]
        if (!identical(record[[1L]], "value")) next
        descriptor <- member_s7_runtime_descriptor(value, modes = s7_modes)
        if (!is.null(descriptor)) {
            s7_roots[name] <- list(if (identical(descriptor$kind, "class")) {
                member_s7_generator(descriptor)
            } else {
                member_s7_value(type = "S7_descriptor", s7_descriptor = descriptor)
            })
        } else if ("S7_object" %in% attr(value, "class", exact = TRUE)) {
            descriptor <- member_s7_instance_descriptor(value, s7_modes)
            if (!is.null(descriptor)) {
                holder <- new.env(parent = emptyenv())
                holder$object <- value
                s7_roots[name] <- list(member_snapshot_shape(member_snapshot(
                    holder, s7_modes = s7_modes, lookup_scope = ns
                ))$fields$object)
            }
        }
        if (isS4(value) && any(c("classRepresentation", "ClassUnionRepresentation") %in%
                    attr(value, "class", exact = TRUE))) {
            descriptor <- member_s4_runtime_descriptor(value)
            if (!is.null(descriptor)) s4_classes[descriptor$name] <- list(descriptor)
            next
        }
        s4_instance <- isS4(value) && !"classGeneratorFunction" %in% attr(value, "class", exact = TRUE) &&
            is.null(s7_roots[[name]])
        if (s4_instance) {
            classes <- attr(value, "class", exact = TRUE)
            if (is.character(classes) && length(classes) == 1L) {
                owner <- attr(classes, "package", exact = TRUE)
                s4_objects[name] <- list(list(name = as.character(classes), package = if (is.null(owner)) package else owner))
            }
        }
        if (s4_instance) values[name] <- list(value)
        if (typeof(value) == "closure") {
            if ("classGeneratorFunction" %in% attr(value, "class", exact = TRUE)) {
                class <- attr(value, "className", exact = TRUE)
                if (is.character(class) && length(class) == 1L) {
                    owner <- attr(class, "package", exact = TRUE)
                    s4_roots[name] <- list(member_value(type = "function", s4_generator = list(
                        name = as.character(class), package = if (is.null(owner)) package else owner
                    )))
                }
            }
            if ("S7_class" %in% attr(value, "class", exact = TRUE)) {
                type <- attr(value, "name", exact = TRUE)
                properties <- attr(value, "properties", exact = TRUE)
                if (is.character(type) && length(type) == 1L && is.list(properties)) {
                    descriptors[name] <- list(list(type = type, fields = names(properties)))
                }
            }
            definitions[name] <- list(member_function_syntax(value))
            functions[name] <- list(value)
        } else if (is.environment(value) || typeof(value) %in% c("list", "pairlist") || isS4(value)) {
            values[name] <- list(value)
        } else if (!is.object(value) && is.atomic(value) && length(value) <= 256L) {
            definitions[name] <- list(value)
        }
    }
    # Match bindings by identity, not a method-name prefix. Retain namespace
    # function IDs for documentation; anonymous registry closures get path IDs.
    function_names <- names(functions)
    remaining_bindings <- 50000L
    for (name in names(values)) {
        value <- values[[name]]
        if (is.environment(value) && (isNamespace(value) || identical(value, ns) || identical(value, globalenv()) ||
                    identical(value, baseenv()))) {
            next
        }
        storage <- if (is.environment(value)) value else if (!isS4(value)) member_list_storage(value) else NULL
        size <- if (is.environment(value)) {
            length(ls(value, all.names = TRUE))
        } else if (isS4(value)) {
            length(attributes(value))
        } else {
            length(storage)
        }
        if (size > remaining_bindings) {
            if (is.environment(value)) stop("Namespace exceeds registry budget")
            snapshots[name] <- list(list(kind = "unknown", reason = "snapshot_budget"))
            registries[name] <- list(character())
            next
        }
        remaining_bindings <- remaining_bindings - size
        snapshots[name] <- list(member_snapshot(value,
                max_bindings = remaining_bindings + size, s7_modes = s7_modes, lookup_scope = ns))
        definitions[name] <- list(if (is.environment(value)) quote(new.env(parent = emptyenv())) else quote(list()))
        methods <- character()
        fields <- if (snapshots[[name]]$kind %in% c("environment", "r6_instance")) {
            snapshots[[name]]$fields
        } else if (identical(snapshots[[name]]$kind, "list")) {
            snapshots[[name]]$elements
        } else {
            list()
        }
        for (field in names(fields)[!is.na(names(fields)) & nzchar(names(fields))]) {
            if (identical(fields[[field]]$reason, "missing_value")) next
            record <- if (is.environment(value)) member_binding(value, field) else list("value", storage[[field]])
            fn <- record[[2L]]
            if (identical(record[[1L]], "value") && (is.environment(fn) || typeof(fn) == "list")) {
                matches <- which(vapply(values, identical, logical(1L), fn))
                if (length(matches)) links[[name]][field] <- names(values)[[matches[[1L]]]]
            }
            if (!identical(record[[1L]], "value") || typeof(fn) != "closure") next
            matches <- which(vapply(functions, identical, logical(1L), fn))
            key <- if (length(matches)) function_names[[matches[[1L]]]] else paste(name, field, sep = "$")
            definitions[key] <- list(member_function_syntax(fn))
            # A static factory may also be addressed by its environment path.
            definitions[paste(name, field, sep = "$")] <- list(as.name(key))
            if (identical(key, paste(name, field, sep = "$"))) definitions[key] <- list(member_function_syntax(fn))
            methods[field] <- key
        }
        registries[name] <- list(methods)
    }
    # Imported intrinsic summaries are allowed only for the actual provider's
    # binding, not arbitrary functions with a matching local name.
    intrinsics <- character()
    imports <- parent.env(ns)
    providers <- list(rlang = c(
        "list2", "arg_match0", "arg_match", "try_fetch",
        "is_character", "is_list", "is_bool", "abort"
    ), S7 = member_s7_intrinsics, methods = member_methods_intrinsics)
    for (provider in names(providers)) {
        if (!provider %in% loadedNamespaces()) next
        for (name in providers[[provider]]) {
            if (name %in% names(definitions)) next
            record <- member_binding(imports, name, materialize = TRUE)
            source <- member_binding(asNamespace(provider), name, materialize = TRUE)
            if (identical(record[[1L]], "value") && identical(source[[1L]], "value") &&
                identical(record[[2L]], source[[2L]])) {
                intrinsics <- c(intrinsics, name)
            }
        }
    }
    list(
        definitions = definitions, locations = list(), expressions = list(),
        exports = exports, registries = registries,
        snapshots = snapshots, links = links, descriptors = descriptors, package = package, intrinsics = intrinsics,
        s4_classes = s4_classes, s4_roots = s4_roots, s4_objects = s4_objects,
        s7_roots = s7_roots, s7_capabilities = s7_modes,
        s7_dependencies = member_s7_dependencies(s7_roots, package, s7_modes),
        s4_dependencies = member_s4_dependencies(s4_classes, lapply(c(
            lapply(s4_roots, function(value) value$s4_generator), s4_objects
        ), function(class) structure(class$name, package = class$package)))
    )
}

member_index_freeze <- function(index) {
    value <- as.list(index)
    value$cache <- NULL
    # Prepare a cheap request-time hint in the metadata worker. Check syntax
    # and already inferred roots, including runtime R6 generators.
    value$class_scope <- isTRUE(value$r6_attached) ||
        any(vapply(value$definitions, function(expr) "R6Class" %in% member_syntax_names(expr), logical(1L))) ||
        any(vapply(value$package_roots, function(root) !is.null(root$r6_class), logical(1L)))
    value$symbol_scope <- value$class_scope ||
        any(vapply(value$definitions, function(expr) any(member_syntax_names(expr) %in% member_s7_intrinsics), logical(1L))) ||
        any(vapply(value$package_roots, function(root) !is.null(root$s7_generator), logical(1L)))
    value$generation <- digest::digest(value, algo = "sha256")
    value$schema <- 1L
    if (as.numeric(utils::object.size(value)) > 32 * 1024^2) stop("Namespace exceeds snapshot budget")
    value
}

member_index_thaw <- function(snapshot) {
    if (!is.list(snapshot) || !identical(snapshot$schema, 1L)) {
        return(NULL)
    }
    index <- list2env(snapshot, parent = emptyenv())
    if (!is.null(index$return_definitions) &&
            !identical(index$return_definitions, digest::digest(index$definitions, algo = "xxhash64"))) {
        index$method_results <- list()
    }
    index$cache <- new.env(parent = emptyenv())
    index
}

member_prepare_package <- function(package, lib_paths = .libPaths()) {
    .libPaths(lib_paths)
    ns <- asNamespace(package)
    input <- member_namespace_input(ns)
    index <- member_package_index(input)
    for (name in names(index$package_roots)) {
        record <- member_binding(ns, name)
        value <- record[[2L]]
        if (identical(record[[1L]], "value") && is.environment(value) &&
            "R6ClassGenerator" %in% attr(value, "class", exact = TRUE)) {
            index$package_roots[name] <- list(member_r6_runtime_shape(value, index))
        }
    }
    index$roots <- index$package_roots[intersect(index$exports, names(index$package_roots))]
    index$r6_attached <- identical(package, "R6")
    for (name in names(index$roots)) index$namespace_roots[paste(package, name, sep = "::")] <- list(index$roots[[name]])
    index$identity <- list(
        package = package, path = find.package(package),
        version = as.character(utils::packageVersion(package)), r = as.character(getRversion())
    )
    member_index_freeze(index)
}

member_resolve_packages <- function(pkgs, lib_paths, prepare = TRUE, namespace_packages = character(), prepare_s3 = TRUE) {
    .libPaths(lib_paths)
    packages <- resolve_attached_packages(pkgs)
    snapshots <- list()
    if (isTRUE(prepare)) {
        for (package in unique(c(setdiff(packages, startup_packages), namespace_packages))) {
            snapshots[package] <- list(tryCatch(member_prepare_package(package, lib_paths),
                error = function(e) {
                    list(
                        schema = 1L, roots = list(), namespace_roots = list(),
                        package = package, generation = "", error = conditionMessage(e)
                    )
                }
            ))
        }
    }
    list(
        packages = packages, members = snapshots,
        s3 = if (isTRUE(prepare_s3)) s3_prepare_packages(unique(c(packages, namespace_packages))) else NULL,
        requested = list(packages = normalize_package_request(pkgs),
            namespace_packages = normalize_package_request(unique(namespace_packages)))
    )
}
