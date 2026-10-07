# Read binding metadata, not the value produced by a getter or promise.
# Research helper: requires rlang for promise inspection and R's
# activeBindingFunction API for active-binding syntax. The target package is
# never loaded here; rlang supplies the binding-kind inspection primitive.

inspection_function_syntax <- function(fn) {
    if (typeof(fn) != "closure") return(NULL)
    as.call(list(as.name("function"), formals(fn), body(fn)))
}

inspection_snapshot <- function(env, max_depth = 3L, max_bindings = 5000L) {
    stopifnot(is.environment(env), requireNamespace("rlang", quietly = TRUE))
    seen <- list()
    remaining <- max_bindings
    lexical_literals <- function(fn) {
        scope <- environment(fn)
        if (is.null(scope) || isNamespace(scope) || identical(scope, globalenv()) || identical(scope, baseenv())) return(list())
        nms <- ls(scope, all.names = TRUE)
        if (length(nms) > 100L) return(list())
        out <- list()
        for (name in nms) {
            if (bindingIsActive(name, scope) || isTRUE(rlang::env_binding_are_lazy(scope, name)[[1L]])) next
            value <- get(name, scope, inherits = FALSE)
            if (!is.object(value) && is.atomic(value) && length(value) <= 100L) out[name] <- list(value)
        }
        out
    }
    visit <- function(current, depth) {
        old <- which(vapply(seen, identical, logical(1L), current))
        if (length(old)) return(list(kind = "reference", id = old[[1L]]))
        seen[[length(seen) + 1L]] <<- current
        id <- length(seen)
        nms <- ls(current, all.names = TRUE)
        if (depth > max_depth || length(nms) > remaining) {
            return(list(kind = "environment", id = id, complete = FALSE,
                names = nms, reason = "budget"))
        }
        remaining <<- remaining - length(nms)
        fields <- setNames(vector("list", length(nms)), nms)
        for (name in nms) {
            if (bindingIsActive(name, current)) {
                getter <- if (exists("activeBindingFunction", baseenv(), inherits = FALSE)) {
                    activeBindingFunction(name, current)
                } else NULL
                fields[[name]] <- list(kind = "active", syntax =
                    if (is.null(getter)) NULL else inspection_function_syntax(getter),
                    lexical_literals = if (is.null(getter)) list() else lexical_literals(getter))
            } else if (isTRUE(rlang::env_binding_are_lazy(current, name)[[1L]])) {
                fields[[name]] <- list(kind = "deferred", reason = "promise_not_forced")
            } else {
                # Only ordinary bindings reach get(); it has no S3 dispatch.
                value <- get(name, current, inherits = FALSE)
                fields[[name]] <- describe(value, depth + 1L)
            }
        }
        list(kind = "environment", id = id, complete = TRUE,
            classes = attr(current, "class", exact = TRUE), fields = fields)
    }
    describe <- function(value, depth) {
        if (typeof(value) == "closure") {
            list(kind = "function", syntax = inspection_function_syntax(value),
                lexical_literals = lexical_literals(value))
        } else if (is.environment(value)) {
            visit(value, depth)
        } else if (!is.object(value) && is.atomic(value) && length(value) <= 100L) {
            list(kind = "literal", value = value)
        } else if (typeof(value) == "list" && !is.object(value) && depth <= max_depth && length(value) <= remaining) {
            remaining <<- remaining - length(value)
            list(kind = "list", elements = lapply(value, describe, depth + 1L))
        } else {
            list(kind = "unknown", storage_type = typeof(value))
        }
    }
    # Only syntax/literals/IDs are retained; no user closure environment or
    # native pointer is serialized as metadata.
    visit(env, 0L)
}

# Compose a binding snapshot with the generic shape engine. Active getters are
# exposed as names with Unknown values; their bodies remain available to a
# specialized extractor. No promise, getter, initializer or method is invoked.
inspection_shape_index <- function(snapshot) {
    index <- static_generic_index("")
    records <- list()
    collect <- function(record) {
        if (identical(record$kind, "environment")) {
            records[[as.character(record$id)]] <<- record
            for (field in record$fields) collect(field)
        } else if (identical(record$kind, "list")) for (field in record$elements) collect(field)
    }
    collect(snapshot)
    decode <- function(record, trail = integer()) {
        if (identical(record$kind, "reference")) {
            if (record$id %in% trail) return(static_value(reason = "cycle"))
            return(decode(records[[as.character(record$id)]], trail))
        }
        if (identical(record$kind, "function")) {
            lexical <- lapply(record$lexical_literals, static_literal)
            return(static_value(function_expr = record$syntax, closure = lexical))
        }
        if (identical(record$kind, "literal")) return(static_literal(record$value))
        if (identical(record$kind, "environment")) {
            if (record$id %in% trail || !isTRUE(record$complete)) return(static_value(reason = "cycle_or_budget"))
            return(static_value(type = "environment", fields = lapply(record$fields, decode, c(trail, record$id)), classes = record$classes))
        }
        if (identical(record$kind, "list")) {
            values <- lapply(record$elements, decode, trail)
            fields <- values[nzchar(names(values))]
            return(static_value(type = "list", fields = fields, elements = values))
        }
        static_value(reason = if (identical(record$kind, "active")) "getter_not_invoked" else "deferred_or_unsupported")
    }
    root <- decode(snapshot)
    index$roots <- root$fields
    index
}

inspection_fingerprint <- function(snapshot) {
    digest::digest(snapshot, algo = "sha256", serialize = TRUE)
}

# Optional package-data metadata input. Read a trusted installed serialized
# database directly, not data() scripts or an attached lazy promise. Refuse
# environment references and large entries. This does not evaluate data code.
inspection_data_shapes <- function(package, max_entry_bytes = 100000L) {
    root <- system.file("data", package = package)
    map_path <- file.path(root, "Rdata.rdx")
    if (!nzchar(root) || !file.exists(map_path)) return(list())
    map <- readRDS(map_path)
    if (length(map$references)) return(list())
    shapes <- list()
    fetch <- get("lazyLoadDBfetch", baseenv())
    for (name in names(map$variables)) {
        key <- map$variables[[name]]
        if (length(key) != 2L || key[[2L]] > max_entry_bytes) next
        value <- fetch(key, file.path(root, "Rdata.rdb"), map$compressed,
            function(...) stop("Referenced environment is not plain package data"))
        # Do not call class(), names() or [[ methods on deserialized objects.
        classes <- attr(value, "class", exact = TRUE)
        type <- if (length(classes)) classes[[1L]] else typeof(value)
        fields <- NULL
        if (typeof(value) == "list" && (is.null(classes) || "data.frame" %in% classes)) {
            ordinary <- unclass(value)
            names <- attr(ordinary, "names", exact = TRUE)
            fields <- setNames(lapply(seq_along(ordinary), function(i) {
                x <- ordinary[[i]]
                cls <- attr(x, "class", exact = TRUE)
                static_value(type = if (length(cls)) cls[[1L]] else typeof(x), classes = cls)
            }), names)
        }
        shapes[name] <- list(static_value(type = type, classes = classes, fields = fields))
    }
    shapes
}
