# Syntax summaries include defaults, which all.names() omits on pairlists.
# The native walk deduplicates symbols without allocating per-node R lists.
member_syntax_names <- function(expr) {
    .Call("member_syntax_names_c", expr, PACKAGE = "languageserver")
}

member_symbol_required <- function(workspace, document) {
    data <- document$parse_data
    if (!identical(document$version, data$version) || isTRUE(data$parse_error)) return(TRUE)
    if (!identical(data$member_data$symbol_scope, FALSE)) return(TRUE)
    member_package_scope_required(workspace, data$member_data, "symbol_scope")
}

member_ordinary_request <- function(workspace, document) {
    isTRUE(document$member_ordinary_edit) && !isTRUE(document$parse_data$parse_error) &&
        identical(document$parse_data$member_data$class_scope, FALSE) &&
        identical(document$parse_data$member_data$symbol_scope, FALSE) &&
        !member_package_scope_required(workspace, document$parse_data$member_data, "symbol_scope")
}

member_package_scope_required <- function(workspace, data, capability) {
    metadata <- workspace$member_metadata
    if (is.null(metadata)) return(FALSE)
    packages <- unique(c(data$packages, vapply(data$imports, function(item) {
        if (length(item$expr) < 2L) return("")
        name <- member_name(item$expr[[2L]])
        if (is.null(name)) "" else name
    }, character(1L))))
    for (package in intersect(packages, metadata$keys())) {
        candidate <- if (is.function(metadata$catalog)) metadata$catalog(package) else metadata$get(package)
        if (!identical(candidate[[capability]], FALSE)) return(TRUE)
    }
    FALSE
}

# Histories are ordered by their end position. Earlier assignments are looked
# up repeatedly while following aliases and resolving shadowed intrinsics.
member_history_position <- function(history, at) {
    lo <- 1L
    hi <- length(history)
    while (lo <= hi) {
        middle <- as.integer(floor((lo + hi) / 2L))
        if (member_before(history[[middle]]$end, at)) lo <- middle + 1L else hi <- middle - 1L
    }
    hi
}

# Lazy local values keep the lexical state from before the assignment. The
# memo belongs to this recovered scope, never to a package-wide summary.
member_local_binding <- function(expr, bindings, referenced = member_syntax_names(expr), lookup = NULL) {
    # Keep only lexical dependencies. Copying every earlier local into every
    # deferred binding makes a flat method quadratic in retained state.
    internal <- c(".__member_position__", ".__member_properties__", ".__s4_classes__",
        ".__s7_package__", ".__s7_constructing__")
    captured <- if (is.null(lookup)) {
        bindings[intersect(names(bindings), c(referenced, internal))]
    } else {
        # The mutable lookup stays in the scope walker. Copy its current
        # values so later assignments cannot change this lexical snapshot.
        keys <- unique(c(referenced, internal))
        values <- mget(keys, lookup, inherits = FALSE, ifnotfound = list(NULL))
        values[!vapply(values, is.null, logical(1L))]
    }
    list(binding_expr = expr, binding_env = captured,
        binding_cache = new.env(hash = FALSE, parent = emptyenv()))
}

member_local_value <- function(value, index, budget, depth = 0L, trail = character(),
    receiver = NULL, context = NULL) {
    if (is.null(value$binding_expr)) return(value)
    cache <- value$binding_cache
    if (!is.null(cache) && exists("value", cache, inherits = FALSE)) return(cache$value)
    result <- member_infer(value$binding_expr, index, value$binding_env, receiver,
        depth = depth + 1L, trail = trail, budget = budget, context = context)
    if (!is.null(cache) && !isTRUE(budget$exhausted) && !isTRUE(budget$transient)) cache$value <- result
    result
}

# Ordinary completion needs the binding kind and R6 identity, not every list
# element. A proven base list cannot be a function or R6 instance, even when
# its arguments contain constructors. Keep the full deferred value for member
# access and never store this outline in its inference memo.
member_completion_value <- function(value, index, budget) {
    if (!is.null(value$binding_expr) && !is.null(value$binding_cache) && !isTRUE(budget$exhausted)) {
        expr <- value$binding_expr
        if (!is.call(expr) && !is.symbol(expr) && !is.object(expr)) return(member_literal(expr))
        if (member_head(expr, "list") && !"list" %in% names(value$binding_env) &&
                is.null(member_lookup(index$definitions, "list")) &&
                !length(member_lookup(index$document_bindings, "list"))) {
            return(member_value(type = "list"))
        }
    }
    member_local_value(value, index, budget)
}

member_receiver_cacheable <- function(value, index, budget) {
    if (isTRUE(budget$exhausted) || !length(value$type)) return(FALSE)
    if (!isTRUE(budget$transient)) return(TRUE)
    # Recursive argument analysis must not preserve partial instance fields.
    # A native-backed class-only result has a complete declared member surface
    # independent of argument values; retain just that proven surface.
    length(value$type) == 1L && is.null(value$reason) && is.null(value$fields) &&
        is.null(value$slots) && is.null(value$function_expr) && is.null(value$closure) &&
        is.null(value$binding_expr) && is.null(value$elements) && is.null(value$element_shape) &&
        is.null(value$result_shape) && is.null(value$r6_class) && !isTRUE(value$open) &&
        (any(index$members[[value$type]] %in% index$native_factories, na.rm = TRUE) ||
            any(vapply(index$properties[[value$type]], function(property) {
                length(property$type) == 1L &&
                    any(index$members[[property$type]] %in% index$native_factories, na.rm = TRUE)
            }, logical(1L))))
}
