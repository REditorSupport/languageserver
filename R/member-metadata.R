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

member_snapshot <- function(env, max_depth = 4L, max_bindings = 5000L, s7_modes = list()) {
    seen <- list()
    remaining <- max_bindings
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
    visit <- function(current, depth) {
        old <- which(vapply(seen, identical, logical(1L), current))
        if (length(old)) {
            return(list(kind = "reference", id = old[[1L]]))
        }
        id <- length(seen) + 1L
        seen[[id]] <<- current
        if (depth > max_depth) {
            return(list(kind = "environment", id = id, complete = FALSE))
        }
        nms <- ls(current, all.names = TRUE)
        if (length(nms) > remaining) {
            return(list(kind = "environment", id = id, complete = FALSE))
        }
        remaining <<- remaining - length(nms)
        fields <- stats::setNames(lapply(nms, function(name) {
            record <- member_binding(current, name)
            value <- record[[2L]]
            if (identical(record[[1L]], "active")) {
                list(
                    kind = "active", syntax = if (typeof(value) == "closure") member_function_syntax(value),
                    captures = if (typeof(value) == "closure") captures(value) else list()
                )
            } else if (identical(record[[1L]], "value")) {
                describe(value, depth + 1L)
            } else {
                list(kind = "unknown", reason = "promise_not_forced")
            }
        }), nms)
        list(
            kind = "environment", id = id, complete = TRUE,
            classes = attr(current, "class", exact = TRUE), fields = fields
        )
    }
    describe <- function(value, depth) {
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
        if (typeof(value) == "closure") {
            list(kind = "function", syntax = member_function_syntax(value), captures = captures(value))
        } else if (is.environment(value)) {
            visit(value, depth)
        } else if (!is.object(value) && is.atomic(value) && length(value) <= 100L) {
            list(kind = "literal", value = value)
        } else if (typeof(value) == "list" && !is.object(value) &&
            depth <= max_depth && length(value) <= remaining) {
            remaining <<- remaining - length(value)
            list(kind = "list", elements = lapply(value, describe, depth + 1L))
        } else {
            list(kind = "unknown", reason = "unsupported_storage")
        }
    }
    visit(env, 0L)
}

member_snapshot_shape <- function(snapshot) {
    records <- list()
    collect <- function(record) {
        if (identical(record$kind, "environment")) {
            records[[as.character(record$id)]] <<- record
            for (field in record$fields) collect(field)
        } else if (identical(record$kind, "list")) {
            for (field in record$elements) collect(field)
        } else if (identical(record$kind, "s7_instance")) {
            for (field in record$slots) collect(field)
        }
    }
    collect(snapshot)
    decode <- function(record, trail = integer()) {
        if (identical(record$kind, "reference")) {
            if (record$id %in% trail) {
                return(member_value(reason = "cycle"))
            }
            return(decode(records[[as.character(record$id)]], trail))
        }
        if (identical(record$kind, "function")) {
            return(member_value(
                type = "function", function_expr = record$syntax, closure = lapply(record$captures, member_literal)
            ))
        }
        if (identical(record$kind, "literal")) {
            return(member_literal(record$value))
        }
        if (identical(record$kind, "environment")) {
            if (record$id %in% trail || !isTRUE(record$complete)) {
                return(member_value(reason = "cycle_or_budget"))
            }
            return(member_value(
                type = "environment", classes = record$classes,
                fields = lapply(record$fields, decode, c(trail, record$id))
            ))
        }
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
            fields <- if (is.null(names(values))) list() else values[nzchar(names(values))]
            return(member_value(type = "list", fields = fields, elements = values))
        }
        member_value(reason = if (identical(record$kind, "active")) "getter_not_invoked" else record$reason)
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
                s7_roots[name] <- list(member_snapshot_shape(member_snapshot(holder, s7_modes = s7_modes))$fields$object)
            }
        }
        if (isS4(value) && any(c("classRepresentation", "ClassUnionRepresentation") %in%
                    attr(value, "class", exact = TRUE))) {
            descriptor <- member_s4_runtime_descriptor(value)
            if (!is.null(descriptor)) s4_classes[descriptor$name] <- list(descriptor)
            next
        }
        if (isS4(value) && typeof(value) != "closure" && is.null(s7_roots[[name]])) {
            classes <- attr(value, "class", exact = TRUE)
            if (is.character(classes) && length(classes) == 1L) {
                owner <- attr(classes, "package", exact = TRUE)
                s4_objects[name] <- list(list(name = as.character(classes), package = if (is.null(owner)) package else owner))
            }
        }
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
        } else if (is.environment(value)) {
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
        if (isNamespace(value) || identical(value, ns) || identical(value, globalenv()) ||
            identical(value, baseenv())) {
            next
        }
        size <- length(ls(value, all.names = TRUE))
        if (size > remaining_bindings) stop("Namespace exceeds registry budget")
        remaining_bindings <- remaining_bindings - size
        snapshots[name] <- list(member_snapshot(value, max_bindings = remaining_bindings + size, s7_modes = s7_modes))
        definitions[name] <- list(quote(new.env(parent = emptyenv())))
        methods <- character()
        for (field in names(snapshots[[name]]$fields)) {
            record <- member_binding(value, field)
            fn <- record[[2L]]
            if (identical(record[[1L]], "value") && is.environment(fn)) {
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
    for (name in intersect(index$exports, names(index$roots))) {
        record <- member_binding(ns, name)
        value <- record[[2L]]
        if (identical(record[[1L]], "value") && is.environment(value) &&
            "R6ClassGenerator" %in% attr(value, "class", exact = TRUE)) {
            index$roots[name] <- list(member_r6_runtime_shape(value, index))
        }
    }
    index$r6_attached <- identical(package, "R6")
    for (name in names(index$roots)) index$namespace_roots[paste(package, name, sep = "::")] <- list(index$roots[[name]])
    index$identity <- list(
        package = package, path = find.package(package),
        version = as.character(utils::packageVersion(package)), r = as.character(getRversion())
    )
    member_index_freeze(index)
}

member_resolve_packages <- function(pkgs, lib_paths, prepare = TRUE) {
    .libPaths(lib_paths)
    packages <- resolve_attached_packages(pkgs)
    snapshots <- list()
    if (isTRUE(prepare)) {
        for (package in setdiff(packages, startup_packages)) {
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
        requested = normalize_package_request(pkgs)
    )
}
