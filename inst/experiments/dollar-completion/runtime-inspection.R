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
                    if (is.null(getter)) NULL else inspection_function_syntax(getter))
            } else if (isTRUE(rlang::env_binding_are_lazy(current, name)[[1L]])) {
                fields[[name]] <- list(kind = "deferred", reason = "promise_not_forced")
            } else {
                # Only ordinary bindings reach get(); it has no S3 dispatch.
                value <- get(name, current, inherits = FALSE)
                fields[[name]] <- if (typeof(value) == "closure") {
                    list(kind = "function", syntax = inspection_function_syntax(value))
                } else if (is.environment(value)) {
                    visit(value, depth + 1L)
                } else if (!is.object(value) && is.atomic(value) && length(value) <= 100L) {
                    list(kind = "literal", value = value)
                } else {
                    list(kind = "unknown", storage_type = typeof(value))
                }
            }
        }
        list(kind = "environment", id = id, complete = TRUE,
            classes = attr(current, "class", exact = TRUE), fields = fields)
    }
    # Only syntax/literals/IDs are retained; no user closure environment or
    # native pointer is serialized as metadata.
    visit(env, 0L)
}

inspection_fingerprint <- function(snapshot) {
    digest::digest(snapshot, algo = "sha256", serialize = TRUE)
}
