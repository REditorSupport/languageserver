# Static receiver analysis. All inputs are syntax or inert metadata.
# No document expression, method, getter, or default is evaluated.

member_head <- function(x, name) {
    !missing(x) && is.call(x) && is.symbol(x[[1L]]) && identical(as.character(x[[1L]]), name)
}

member_name <- function(x) {
    if (missing(x)) {
        return(NULL)
    }
    if (is.symbol(x)) {
        return(as.character(x))
    }
    if (is.character(x) && length(x) == 1L) {
        return(x)
    }
    NULL
}

member_key <- function(x) {
    if (is.symbol(x)) {
        return(as.character(x))
    }
    if (member_head(x, "$") && length(x) == 3L) {
        lhs <- member_key(x[[2L]])
        rhs <- member_name(x[[3L]])
        if (!is.null(lhs) && !is.null(rhs)) {
            return(paste(lhs, rhs, sep = "$"))
        }
    }
    NULL
}

member_walk <- function(x, visit, descend_functions = TRUE) {
    if (missing(x)) {
        return(invisible(NULL))
    }
    visit(x)
    if (is.call(x) || is.expression(x)) {
        if (!descend_functions && member_head(x, "function")) {
            return(invisible(NULL))
        }
        for (child in as.list(x)) member_walk(child, visit, descend_functions)
    }
    invisible(NULL)
}

member_strings <- function(x) {
    if (is.character(x)) {
        return(x)
    }
    if (member_head(x, "c")) {
        args <- as.list(x)[-1L]
        if (all(vapply(args, is.character, logical(1L)))) {
            return(unlist(args))
        }
    }
    character()
}

member_lookup <- function(x, key) {
    if (is.null(x) || !is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
        return(NULL)
    }
    # Lists and environments return NULL for absent names. Avoid scanning
    # large namespace maps twice for each syntax node.
    if (is.list(x) || is.environment(x)) return(x[[key]])
    if (!key %in% names(x)) return(NULL)
    x[[key]]
}

member_namespace_index <- function(index, package) {
    value <- member_lookup(index$namespace_indices, package)
    if (is.null(value) && identical(package, index$package)) return(index)
    if (isTRUE(value$.metadata_lazy)) {
        value <- index$namespace_metadata$get(package)
        index$namespace_indices[package] <- list(value)
    }
    value
}

# Member tables belong to the receiver's package, while lexical shadowing and
# attached namespaces still come from the document containing the request.
member_package_context <- function(index, package) {
    if (is.null(package) || identical(package, index$package)) return(index)
    owner <- member_namespace_index(index, package)
    if (is.null(owner)) return(NULL)
    context <- list2env(as.list(owner), parent = emptyenv())
    for (name in c("namespace_indices", "namespace_metadata", "document_bindings", "document_s4",
            "attached_roots", "attached_s4", "methods_attached")) context[[name]] <- index[[name]]
    context
}

member_subset_method <- function(value, operator, index, bindings, budget, depth, trail) {
    owner <- member_package_context(index, value$member_package)
    if (is.null(owner)) return(NULL)
    if (identical(value$reason, "ambiguous_class")) return(member_value(reason = "unresolved_subset_dispatch"))
    if (!length(value$classes) && !length(member_lookup(owner$classes, value$type))) return(NULL)
    classes <- member_classes(value, owner)
    if (!length(classes)) return(NULL)
    registries <- c(list(owner$subset_registry), lapply(index$namespace_indices, function(candidate) candidate$subset_registry))
    for (key in paste0(operator, ".", c(classes, "default"))) {
        history <- member_lookup(index$document_bindings, key)
        at <- bindings$.__member_position__
        if (key %in% names(bindings) || (length(history) && (is.null(at) || member_history_position(history, at)))) {
            return(member_infer(as.name(key), index, bindings, depth = depth + 1L, trail = trail, budget = budget))
        }
        registered <- unique(unlist(lapply(registries, function(registry) member_lookup(registry, key)), use.names = FALSE))
        definition <- member_definition(owner$definitions, key)
        if (length(registered)) {
            # Registrations can come from another namespace, or replace a
            # method after this receiver's package snapshot was prepared.
            if (identical(registered, "base")) return(NULL)
            expected <- if (member_head(definition, "function")) {
                paste0("namespace:", owner$package, ":", digest::digest(definition, algo = "xxhash64"))
            }
            if (length(registered) != 1L || is.null(expected) || !identical(registered, expected)) {
                return(member_value(reason = "unresolved_subset_dispatch"))
            }
            return(member_value(function_key = key, metadata = owner$package))
        }
        if (!is.null(definition)) return(member_value(function_key = key, metadata = owner$package))
    }
    if (!is.null(value$member_package) && !isTRUE(owner$subset_registry_complete)) {
        return(member_value(reason = "unresolved_subset_dispatch"))
    }
    NULL
}

member_value <- function(
  type = NULL, function_key = NULL, receiver = NULL,
  fields = NULL, function_expr = NULL, closure = NULL, receiver_name = NULL,
  receiver_value = NULL, literal = NULL, known_literal = FALSE,
  elements = NULL, classes = NULL, reason = NULL, element_shape = NULL,
  open = FALSE, result_shape = NULL, binding_expr = NULL, binding_env = NULL,
  metadata = NULL, slots = NULL, slot_types = NULL, s4_class = NULL,
  s4_generator = NULL, r6_private = NULL, r6_super = NULL, r6_self = NULL,
  r6_class = NULL, r6_role = NULL, default_dollar = FALSE, named_elements = FALSE,
  member_package = NULL, column_names_only = FALSE, possible_returns = NULL
) {
    value <- list(
        type = type, function_key = function_key, receiver = receiver,
        fields = fields, function_expr = function_expr, closure = closure,
        receiver_name = receiver_name, receiver_value = receiver_value,
        literal = literal, known_literal = known_literal, elements = elements,
        classes = classes, reason = reason, element_shape = element_shape,
        open = open, result_shape = result_shape,
        binding_expr = binding_expr, binding_env = binding_env, metadata = metadata,
        slots = slots, slot_types = slot_types, s4_class = s4_class,
        s4_generator = s4_generator, r6_private = r6_private,
        r6_super = r6_super, r6_self = r6_self,
        r6_class = r6_class, r6_role = r6_role
    )
    if (isTRUE(default_dollar)) value$default_dollar <- TRUE
    if (isTRUE(named_elements)) value$named_elements <- TRUE
    if (!is.null(member_package)) value$member_package <- member_package
    if (isTRUE(column_names_only)) value$column_names_only <- TRUE
    if (length(possible_returns)) value$possible_returns <- possible_returns
    value
}

member_literal <- function(x) {
    type <- if (is.null(x)) "NULL" else typeof(x)
    member_value(type = type, literal = x, known_literal = TRUE)
}

member_join <- function(a, b) {
    if (identical(a$type, ".never")) {
        return(b)
    }
    if (identical(b$type, ".never")) {
        return(a)
    }
    # Compacted snapshots and source values can omit different optional NULLs.
    if (identical(a, b) || (isTRUE(a$known_literal) && isTRUE(b$known_literal) &&
                identical(a[!vapply(a, is.null, logical(1L))], b[!vapply(b, is.null, logical(1L))]))) {
        return(a)
    }
    if (!length(a$type) || !length(b$type)) {
        return(member_value(reason = "unknown_branch"))
    }
    fields <- NULL
    if (!is.null(a$fields) && !is.null(b$fields)) {
        common <- intersect(names(a$fields), names(b$fields))
        fields <- stats::setNames(lapply(common, function(n) member_join(a$fields[[n]], b$fields[[n]])), common)
    }
    slots <- NULL
    slot_types <- NULL
    if (!is.null(a$slot_types) && !is.null(b$slot_types)) {
        common <- intersect(names(a$slot_types), names(b$slot_types))
        slot_types <- stats::setNames(lapply(common, function(n) {
            member_s4_join_types(a$slot_types[[n]], b$slot_types[[n]], a$s4_class$package, b$s4_class$package)
        }), common)
        slots <- stats::setNames(lapply(common, function(n) {
            member_join(member_lookup(a$slots, n), member_lookup(b$slots, n))
        }), common)
    }
    value <- member_value(
        type = sort(unique(c(a$type, b$type))), fields = fields,
        open = isTRUE(a$open) || isTRUE(b$open) || !identical(names(a$fields), names(b$fields)) ||
            !is.null(a$elements) || !is.null(b$elements),
        slots = slots, slot_types = slot_types,
        s4_class = if (identical(a$s4_class, b$s4_class)) a$s4_class else NULL,
        element_shape = if (!is.null(a$element_shape) && !is.null(b$element_shape)) member_join(a$element_shape, b$element_shape) else NULL,
        classes = if (identical(a$classes, b$classes)) a$classes else if (
            isTRUE(a$default_dollar) || isTRUE(b$default_dollar)
        ) unique(c(a$classes, b$classes)) else NULL,
        default_dollar = isTRUE(a$default_dollar) && isTRUE(b$default_dollar),
        named_elements = isTRUE(a$named_elements) && isTRUE(b$named_elements) && identical(names(a$fields), names(b$fields)),
        member_package = if (identical(a$member_package, b$member_package)) a$member_package else NULL,
        column_names_only = isTRUE(a$column_names_only) || isTRUE(b$column_names_only)
    )
    if (!identical(a$classes, b$classes) && (length(a$classes) || length(b$classes))) value$reason <- "ambiguous_class"
    if (!identical(a$member_package, b$member_package) &&
            !is.null(a$member_package) && !is.null(b$member_package) && !is.null(fields)) {
        # Object names are local to each namespace; a joined receiver can only
        # retain its common structural fields, not either package's registry.
        value$type <- if (all(c(a$type, b$type) %in% c("list", "pairlist")) ||
                (isTRUE(a$named_elements) && isTRUE(b$named_elements))) "list" else "environment"
    }
    if (isTRUE(a$s7) && isTRUE(b$s7)) value$s7 <- TRUE
    if (identical(a$r6_class, b$r6_class)) value$r6_class <- a$r6_class
    if (identical(a$r6_role, b$r6_role)) value$r6_role <- a$r6_role
    if (identical(a$r6_owner, b$r6_owner)) value$r6_owner <- a$r6_owner
    if (identical(a$r6_member_kind, b$r6_member_kind)) value$r6_member_kind <- a$r6_member_kind
    if (identical(a$r6_locked, b$r6_locked)) value$r6_locked <- a$r6_locked
    if (!is.null(a$r6_bindings) || !is.null(b$r6_bindings)) {
        bindings <- c(a$r6_bindings, b$r6_bindings)
        # A binding must be writable on every branch to infer replacement.
        bindings <- c(bindings[bindings %in% c("method", "active", "locked")], bindings)
        value$r6_bindings <- bindings[!duplicated(names(bindings))]
    }
    if (identical(a$r6_identity, b$r6_identity)) value$r6_identity <- a$r6_identity
    value
}

member_classes <- function(value, index) {
    if (length(value$type) != 1L) {
        return(character())
    }
    if (length(value$classes)) {
        return(value$classes)
    }
    classes <- member_lookup(index$classes, value$type)
    if (is.null(classes)) value$type else classes
}

# A worker snapshot can verify ordinary environment access. A method declared
# in the document or current lexical scope invalidates that package-time fact.
member_default_dollar <- function(value, index, bindings) {
    if (!isTRUE(value$default_dollar)) return(FALSE)
    if (!length(value$classes)) return(TRUE)
    methods <- paste0("$.", c(value$classes, "default"))
    if (any(methods %in% names(bindings))) return(FALSE)
    for (method in methods) {
        if (!is.null(member_lookup(index$definitions, method))) return(FALSE)
        history <- member_lookup(index$document_bindings, method)
        at <- bindings$.__member_position__
        if (length(history) && (is.null(at) || member_history_position(history, at))) return(FALSE)
    }
    TRUE
}

member_elements <- function(value) {
    if (!is.null(value$elements)) return(value$elements)
    if (isTRUE(value$named_elements) && !isTRUE(value$open)) return(value$fields)
    NULL
}

member_dataframe_columns <- function(value) {
    "data.frame" %in% value$classes && !is.null(value$fields)
}

member_members <- function(value, index, bindings = list(), accessor = "$") {
    if (!length(value$type) || any(value$type %in% c(".never", ".missing"))) {
        return(character())
    }
    if (identical(accessor, "@")) {
        slots <- names(value$slot_types)
        return(stats::setNames(rep(NA_character_, length(slots)), slots))
    }
    index <- member_package_context(index, value$member_package)
    if (is.null(index)) return(character())
    columns <- member_dataframe_columns(value)
    if (!columns && isTRUE(value$default_dollar) && !member_default_dollar(value, index, bindings)) return(character())
    if (!isTRUE(value$default_dollar) && length(value$classes) &&
            !all(value$classes %in% c("data.frame", "list", "environment", "R6")) &&
            !any(value$classes %in% names(index$members)) && !columns) {
        return(character())
    }
    sets <- lapply(value$type, function(type) {
        out <- member_lookup(index$members, type)
        if (is.null(out)) character() else out
    })
    common <- Reduce(intersect, lapply(sets, names))
    out <- sets[[1L]][common]
    dynamic <- member_lookup(bindings, ".__member_properties__")
    if (length(value$type) == 1L) {
        extra <- setdiff(names(member_lookup(dynamic, value$type)), names(out))
        out <- c(out, stats::setNames(rep(NA_character_, length(extra)), extra))
    }
    if (!is.null(value$fields)) {
        extra <- setdiff(names(value$fields), names(out))
        out <- c(out, stats::setNames(rep(NA_character_, length(extra)), extra))
    }
    out
}

# Possible receivers keep their inferred type unresolved. All providers use
# the same intersection and reject conflicting member implementations.
member_possible_receiver <- function(value, index, bindings = list(), accessor = "$") {
    if (length(value$type) || !length(value$possible_returns)) return(NULL)
    candidates <- value$possible_returns
    sets <- lapply(candidates, function(candidate) member_members(candidate, index, bindings, accessor))
    common <- Reduce(intersect, lapply(sets, names))
    owners <- vapply(candidates, function(candidate) {
        if (is.null(candidate$member_package)) "" else candidate$member_package
    }, character(1L))
    ambiguous <- common[vapply(common, function(name) {
        if (length(unique(owners)) > 1L || !all(vapply(sets, function(set) identical(set[[name]], sets[[1L]][[name]]), logical(1L)))) {
            return(TRUE)
        }
        if (all(vapply(sets, function(set) is.na(set[[name]]), logical(1L)))) {
            fields <- lapply(candidates, function(candidate) {
                if (identical(accessor, "@")) member_slot(candidate, name, index, bindings) else
                    member_access(candidate, name, index, bindings)
            })
            return(!all(vapply(fields, identical, logical(1L), fields[[1L]])))
        }
        FALSE
    }, logical(1L))]
    list(value = candidates[[1L]], members = common, ambiguous = ambiguous, sets = sets)
}

member_possible_result <- function(values) {
    candidates <- list()
    for (value in values) {
        if (!length(value$type) && length(value$possible_returns)) {
            candidates <- c(candidates, value$possible_returns)
        } else if ((length(value$type) && !any(value$type %in% c(".never", ".missing"))) ||
                !is.null(value$function_key) || !is.null(value$function_expr) || !is.null(value$result_shape)) {
            candidates <- c(candidates, list(value))
        }
    }
    candidates <- candidates[!duplicated(candidates)]
    member_value(reason = "possible_result", possible_returns = candidates)
}

# A bounded structural cache key omits irrelevant literals/large closure maps.
# Function identity and the metadata generation isolate package summaries.
member_shape_key <- function(value, depth = 0L) {
    if (is.null(value)) {
        return(NULL)
    }
    if (depth > 4L) {
        return("...")
    }
    list(
        type = value$type, classes = value$classes, default_dollar = value$default_dollar,
        named_elements = value$named_elements, member_package = value$member_package,
        column_names_only = isTRUE(value$column_names_only),
        class_ambiguous = identical(value$reason, "ambiguous_class"),
        literal = if (isTRUE(value$known_literal) &&
            length(value$literal) <= 8L) {
            value$literal
        } else {
            NULL
        },
        known = value$known_literal, function_key = value$function_key,
        fields = lapply(value$fields, member_shape_key, depth + 1L),
        slots = lapply(value$slots, member_shape_key, depth + 1L),
        slot_types = value$slot_types, s4_class = value$s4_class,
        s4_generator = value$s4_generator,
        s7 = value$s7, s7_descriptor = value$s7_descriptor,
        r6_private = member_shape_key(value$r6_private, depth + 1L),
        r6_super = member_shape_key(value$r6_super, depth + 1L),
        r6_self = member_shape_key(value$r6_self, depth + 1L),
        r6_locked = value$r6_locked, r6_bindings = value$r6_bindings,
        r6_identity = value$r6_identity, r6_class = value$r6_class,
        r6_role = value$r6_role,
        r6_owner = value$r6_owner, r6_member_kind = value$r6_member_kind,
        r6_static_bound = isTRUE(value$r6_static_bound),
        r6_private_self = isTRUE(value$r6_private_self),
        elements = lapply(value$elements, member_shape_key, depth + 1L),
        element_shape = if (!is.null(value$element_shape)) member_shape_key(value$element_shape, depth + 1L) else NULL
    )
}

member_cacheable <- function(value, depth = 0L) {
    if (is.null(value)) {
        return(TRUE)
    }
    if (depth > 4L || isTRUE(value$open) || length(value$possible_returns) || !is.null(value$function_expr) || !is.null(value$closure) ||
        !is.null(value$binding_expr)) {
        return(FALSE)
    }
    if (value$known_literal && length(value$literal) > 8L) {
        return(FALSE)
    }
    all(vapply(
        c(value$fields, value$slots, value$elements, value[c("r6_private", "r6_super", "r6_self")],
            if (!is.null(value$element_shape)) list(value$element_shape)),
        member_cacheable, logical(1L), depth + 1L
    ))
}

member_assigned_names <- function(node) {
    out <- character()
    member_walk(node, function(x) {
        if (member_head(x, "<-") || member_head(x, "=") || member_head(x, ":=")) {
            target <- x[[2L]]
            while (is.call(target) && length(target) > 1L) target <- target[[2L]]
            name <- member_name(target)
            if (!is.null(name)) out <<- c(out, name)
        }
    }, descend_functions = FALSE)
    unique(out)
}

# Retain lexical inputs used by the function (including nested closures and
# defaults), rather than copying an entire instance into every generated method.
member_function_closure <- function(expr, bindings) {
    if (!length(bindings)) return(list())
    internal <- intersect(names(bindings), c(".__member_position__", ".__member_properties__",
            ".__s4_classes__", ".__s7_package__", ".__s7_constructing__"))
    if (length(internal) == length(bindings)) return(bindings)
    referenced <- setdiff(member_syntax_names(expr), names(expr[[2L]]))
    bindings[intersect(names(bindings), c(referenced, internal))]
}

# Select a member from an already inferred receiver. This does not traverse
# syntax, so a known declaration remains usable after inference hits a limit.
member_slot <- function(value, name, index, bindings, budget = NULL) {
    if (isTRUE(value$s7)) member_s7_slot(value, name, index, bindings, budget) else
        member_s4_slot(value, name, index, bindings, budget)
}

member_access <- function(lhs, name, index, bindings) {
    unknown <- member_value(reason = "unsupported")
    if (is.null(name)) return(unknown)
    possible <- member_possible_receiver(lhs, index, bindings)
    if (!is.null(possible)) {
        if (!name %in% possible$members) return(unknown)
        return(member_possible_result(lapply(lhs$possible_returns, function(candidate) {
            member_access(candidate, name, index, bindings)
        })))
    }
    if (isTRUE(lhs$column_names_only) || identical(lhs$reason, "unresolved_dollar_dispatch")) {
        return(member_value(reason = "unresolved_dollar_dispatch"))
    }
    index <- member_package_context(index, lhs$member_package)
    if (is.null(index)) return(unknown)
    if (isTRUE(lhs$default_dollar) && !member_default_dollar(lhs, index, bindings)) {
        return(member_value(reason = "unresolved_dollar_dispatch"))
    }
    dispatch_classes <- c(
        names(index$members), names(index$classes),
        unlist(lapply(index$namespace_indices, function(x) names(x$classes)))
    )
    if (!is.null(lhs$fields) && any(lhs$classes %in% index$record_dispatch_classes)) {
        dispatch_classes <- c(dispatch_classes, lhs$classes)
    }
    if (!isTRUE(lhs$default_dollar) && length(lhs$classes) &&
            !all(lhs$classes %in% c("list", "environment", "data.frame", "R6")) &&
            !any(lhs$classes %in% dispatch_classes)) {
        return(member_value(reason = "unresolved_dollar_dispatch"))
    }
    if (is.null(name)) {
        return(unknown)
    }
    dynamic <- member_lookup(member_lookup(member_lookup(bindings, ".__member_properties__"), lhs$type), name)
    if (!is.null(dynamic)) {
        return(dynamic)
    }
    value <- member_lookup(lhs$fields, name)
    if (!is.null(value)) {
        value <- member_r6_member_info(value, lhs)
        if (!is.null(value$receiver_name)) {
            value$receiver_value <- if (is.null(lhs$r6_self)) lhs else lhs$r6_self
            if (isTRUE(value$r6_private_self)) {
                value$closure$private <- lhs
            } else if (!isTRUE(value$r6_static_bound) && !is.null(value$receiver_value$r6_private)) {
                value$closure$private <- value$receiver_value$r6_private
            }
            if (!is.null(value$closure$private)) {
                value$closure$private$r6_self <- value$receiver_value
            }
            if (!is.null(value$closure$super)) value$closure$super$r6_self <- value$receiver_value
        }
        if (!is.null(value$function_key)) value$receiver_value <- lhs
        if (length(value$type) || !is.null(value$function_expr) || !is.null(value$function_key) ||
                !is.null(value$r6_owner)) {
            return(value)
        }
    }
    if (length(lhs$type) > 1L) {
        return(Reduce(member_join, lapply(lhs$type, function(type) member_access(member_value(type = type), name, index, bindings))))
    }
    property <- member_lookup(member_lookup(index$properties, lhs$type), name)
    if (!is.null(property)) {
        value <- if (is.character(property)) member_value(type = property) else property
        if (is.null(value$member_package) && length(value$type) == 1L && value$type %in% names(index$members)) {
            value$member_package <- index$package
        }
        return(value)
    }
    raw <- member_lookup(member_lookup(index$raw_fields, lhs$type), name)
    if (!is.null(raw)) {
        return(member_value(type = raw, member_package = index$package))
    }
    members <- member_lookup(index$members, lhs$type)
    if (name %in% names(members) && !is.na(members[[name]])) {
        return(member_value(function_key = members[[name]], receiver_value = lhs, metadata = lhs$member_package))
    }
    unknown
}

member_infer <- function(
  expr, index, bindings = list(), receiver = NULL,
  depth = 0L, trail = character(), budget = NULL, context = NULL
) {
    unknown <- member_value(reason = "unsupported")
    if (missing(expr)) {
        return(member_value(type = ".missing"))
    }
    if (is.null(budget)) {
        budget <- new.env(parent = emptyenv())
        budget$remaining <- 20000L
        budget$exhausted <- FALSE
        budget$transient <- FALSE
    }
    budget$remaining <- budget$remaining - 1L
    # Package installation byte-compiles this engine. In development sessions,
    # start timing after R's first-call JIT compilation, before traversing ASTs.
    if (!is.null(budget$time_limit) && is.null(budget$deadline)) {
        budget$deadline <- proc.time()[[3L]] + budget$time_limit
        budget$next_time_check <- budget$remaining - 128L
    }
    # Reading the clock for every AST node can consume most of the request's
    # time limit itself. Check periodically; node and depth bounds still apply
    # on every visit, and an externally supplied deadline is checked first.
    timed_out <- FALSE
    if (!is.null(budget$deadline) && (is.null(budget$next_time_check) ||
                budget$remaining <= budget$next_time_check)) {
        budget$next_time_check <- budget$remaining - 128L
        timed_out <- member_budget_expired(budget, proc.time())
    }
    depth_limit <- if (is.null(budget$depth_limit)) 64L else budget$depth_limit
    if (isTRUE(budget$exhausted) || depth > depth_limit || budget$remaining < 0L || timed_out) {
        budget$exhausted <- TRUE
        if (timed_out) budget$timed_out <- TRUE
        return(member_value(reason = "budget"))
    }
    infer <- function(x, env = bindings, ctx = context) {
        # Only the initializer's outer body returns its instance effects.
        # Nested expressions and called functions keep ordinary return values.
        if (isTRUE(ctx$r6_initialize)) ctx$r6_initialize <- FALSE
        member_infer(x, index, env, receiver, depth + 1L, trail, budget, ctx)
    }
    intrinsic <- function(name) {
        at <- bindings$.__member_position__
        history <- member_lookup(index$document_bindings, name)
        shadowed <- length(history) && (is.null(at) || member_history_position(history, at) > 0L)
        !name %in% names(bindings) &&
            !shadowed &&
            ((is.null(member_lookup(index$definitions, name)) && name %in% member_base_intrinsics) ||
                    (name %in% index$intrinsics && (is.null(index$document_bindings) || !is.null(context$key))))
    }
    package_env <- if (!is.null(index$package_roots)) index$package_roots else index$roots
    join_env <- function(a, b) {
        if (identical(a, b)) return(a)
        keys <- union(names(a), names(b))
        xs <- a[match(keys, names(a))]
        ys <- b[match(keys, names(b))]
        stats::setNames(lapply(seq_along(keys), function(i) {
            x <- xs[[i]]
            y <- ys[[i]]
            if (identical(x, y)) return(x)
            if (!is.list(x) || !is.list(y)) unknown else member_join(x, y)
        }), keys)
    }
    join_effects <- function(a, b) {
        if (is.null(a)) return(b)
        if (is.null(b)) return(a)
        member_join(a, b)
    }
    update_object <- function(env, name, object) {
        identity <- object$r6_identity
        if (!is.null(identity)) {
            for (alias in names(env)) {
                if (is.list(env[[alias]]) && identical(env[[alias]]$r6_identity, identity)) {
                    env[alias] <- list(object)
                }
            }
        }
        env[name] <- list(object)
        env
    }
    # Flow records keep early returns separate from fall-through values.
    flow <- function(node, env) {
        env <- member_s4_effect(node, index, env)
        if (member_head(node, "{")) {
            state <- list(
                value = member_literal(NULL), env = env,
                returns = member_value(type = ".never"), falls = TRUE
            )
            for (child in as.list(node)[-1L]) {
                if (!state$falls) break
                next_state <- flow(child, state$env)
                state$returns <- member_join(state$returns, next_state$returns)
                if (isTRUE(context$r6_initialize)) {
                    state$r6_returns <- join_effects(state$r6_returns, next_state$r6_returns)
                }
                state$value <- next_state$value
                state$env <- next_state$env
                state$falls <- next_state$falls
            }
            return(state)
        }
        if (member_head(node, "return")) {
            if (!isTRUE(context$r6_initialize)) {
                result <- if (length(node) > 1L) infer(node[[2L]], env) else member_literal(NULL)
                return(list(value = result, env = env, returns = result, falls = FALSE))
            }
            state <- if (length(node) > 1L) flow(node[[2L]], env) else
                list(value = member_literal(NULL), env = env, falls = TRUE)
            returns <- if (is.null(state$returns)) member_value(type = ".never") else state$returns
            result <- if (state$falls) member_join(returns, state$value) else returns
            effects <- state$r6_returns
            if (state$falls) effects <- join_effects(effects, state$env$.__r6_instance__)
            return(list(value = result, env = state$env, returns = result, falls = FALSE, r6_returns = effects))
        }
        if (member_head(node, "if")) {
            cond <- infer(node[[2L]], env)
            yes <- !isTRUE(cond$known_literal) || !isFALSE(cond$literal)
            no <- !isTRUE(cond$known_literal) || !identical(cond$literal, TRUE)
            a <- if (yes) flow(node[[3L]], env) else NULL
            b <- if (no) {
                if (length(node) == 4L) {
                    flow(node[[4L]], env)
                } else {
                    list(
                        value = member_literal(NULL), env = env,
                        returns = member_value(type = ".never"), falls = TRUE
                    )
                }
            } else {
                NULL
            }
            if (is.null(a)) {
                return(b)
            }
            if (is.null(b)) {
                return(a)
            }
            falls <- a$falls || b$falls
            out_env <- if (!a$falls) b$env else if (!b$falls) a$env else join_env(a$env, b$env)
            value <- if (!a$falls) b$value else if (!b$falls) a$value else member_join(a$value, b$value)
            return(list(
                value = value, env = out_env,
                returns = member_join(a$returns, b$returns), falls = falls,
                r6_returns = if (isTRUE(context$r6_initialize)) join_effects(a$r6_returns, b$r6_returns)
            ))
        }
        if (member_head(node, "for") || member_head(node, "while")) {
            # A literal named collection copied field-for-field has a finite
            # surface. Other loops retain the conservative invalidation below.
            if (member_head(node, "for") && member_head(node[[3L]], "names") && length(node[[3L]]) == 2L &&
                    intrinsic("names") && !"names" %in% names(env)) {
                body <- node[[4L]]
                if (member_head(body, "{") && length(body) == 2L) body <- body[[2L]]
                variable <- member_name(node[[2L]])
                if (member_head(body, "<-") && member_head(body[[2L]], "[[") &&
                        member_head(body[[3L]], "[[") && length(body[[2L]]) == 3L && length(body[[3L]]) == 3L &&
                        identical(member_name(body[[2L]][[3L]]), variable) &&
                        identical(member_name(body[[3L]][[3L]]), variable) &&
                        identical(body[[3L]][[2L]], node[[3L]][[2L]])) {
                    target <- member_name(body[[2L]][[2L]])
                    object <- member_lookup(env, target)
                    source <- infer(node[[3L]][[2L]], env)
                    if (identical(object$type, "environment") && identical(source$type, "list") &&
                            !isTRUE(source$open) && !is.null(source$fields) && length(source$fields) <= 256L) {
                        for (name in unique(names(source$fields))) {
                            if (!member_r6_writable(object, name)) next
                            value <- source$fields[[name]]
                            if (!is.null(value$function_expr) && target %in% names(value$closure)) {
                                value$closure[target] <- NULL
                                value$receiver_name <- target
                            }
                            object$fields[name] <- list(value)
                        }
                        env <- update_object(env, target, object)
                        env[variable] <- list(member_value(type = "character"))
                        return(list(value = member_literal(NULL), env = env,
                                returns = member_value(type = ".never"), falls = TRUE))
                    }
                }
            }
            # Preserve bindings not assigned by the loop. Never assume one
            # iteration represents all iterations of an unknown loop.
            writes <- member_assigned_names(node)
            if (member_head(node, "for")) writes <- c(writes, member_name(node[[2L]]))
            for (name in writes) env[name] <- list(unknown)
            has_return <- FALSE
            member_walk(node, function(x) {
                if (member_head(x, "return")) has_return <<- TRUE
            }, descend_functions = FALSE)
            return(list(
                value = member_literal(NULL), env = env,
                returns = if (has_return) unknown else member_value(type = ".never"), falls = TRUE
            ))
        }
        if (member_head(node, ":=")) {
            bound <- member_s7_bind(node, index, env)
            if (!is.null(bound)) node <- bound
        }
        if (member_head(node, "<-") || member_head(node, "=")) {
            lhs <- node[[2L]]
            value <- infer(node[[3L]], env)
            if (is.symbol(lhs)) {
                env[as.character(lhs)] <- list(value)
            } else if (member_head(lhs, "$") || member_head(lhs, "[[") || member_head(lhs, "@")) {
                name <- member_name(lhs[[2L]])
                field <- member_name(lhs[[3L]])
                object <- member_lookup(env, name)
                surface <- if (member_head(lhs, "@")) "slots" else "fields"
                if (!is.null(object[[surface]]) && !is.null(field)) {
                    if (!member_r6_writable(object, field)) {
                        return(list(value = value, env = env, returns = member_value(type = ".never"), falls = TRUE))
                    }
                    if (!is.null(value$function_expr) && name %in% names(value$closure)) {
                        value$closure[name] <- NULL
                        value$receiver_name <- name
                    }
                    object[[surface]][field] <- list(value)
                    env <- update_object(env, name, object)
                } else if (!is.null(name)) {
                    env[name] <- list(unknown)
                }
            } else if (member_head(lhs, "class")) {
                name <- member_name(lhs[[2L]])
                object <- member_lookup(env, name)
                classes <- member_strings(node[[3L]])
                if (!is.null(object) && length(classes)) {
                    object$type <- classes[[1L]]
                    object$classes <- classes
                    object$default_dollar <- FALSE
                    env[name] <- list(object)
                } else if (!is.null(name)) env[name] <- list(unknown)
            }
            return(list(value = value, env = env, returns = member_value(type = ".never"), falls = TRUE))
        }
        # Arbitrary reflection can mutate local bindings. Do not retain types
        # through it. Reviewed package metadata wrappers have their own rules.
        if (member_head(node, "assign") || member_head(node, "eval") || member_head(node, "source")) {
            env <- lapply(env, function(x) unknown)
        }
        value <- infer(node, env)
        list(
            value = value, env = env, returns = member_value(type = ".never"),
            falls = !identical(value$type, ".never")
        )
    }
    analyze <- function(body, env) {
        state <- flow(body, env)
        if (isTRUE(context$r6_initialize)) {
            return(join_effects(state$r6_returns, if (state$falls) state$env$.__r6_instance__ else NULL))
        }
        if (state$falls) member_join(state$returns, state$value) else state$returns
    }
    bind_arguments <- function(fn, actuals, env, defaults = TRUE) {
        formals <- as.list(fn[[2L]])
        keys <- names(formals)
        # An open forwarded dots list can fill any formal. Preparation must
        # explore unknown values instead of treating it as one supplied item.
        if (isTRUE(attr(actuals, "member_open_dots"))) {
            for (name in keys) env[name] <- list(unknown)
            if ("..." %in% keys) env["..."] <- list(member_value(type = "list", open = TRUE))
            return(env)
        }
        dots <- match("...", keys)
        before_dots <- if (is.na(dots)) keys else keys[seq_len(dots - 1L)]
        supplied <- names(actuals)
        if (is.null(supplied)) supplied <- rep("", length(actuals))
        matched <- rep(FALSE, length(actuals))
        used <- character()
        for (i in which(nzchar(supplied))) {
            name <- supplied[[i]]
            candidates <- if (name %in% keys) name else before_dots[startsWith(before_dots, name)]
            if (length(candidates) == 1L && candidates != "...") {
                if (candidates %in% used) {
                    return(NULL)
                }
                env[candidates] <- list(actuals[[i]])
                used <- c(used, candidates)
                matched[[i]] <- TRUE
            } else if (length(candidates) > 1L) {
                return(NULL)
            }
        }
        available <- setdiff(before_dots, used)
        for (i in which(!matched & !nzchar(supplied))) {
            if (!length(available)) break
            name <- available[[1L]]
            available <- available[-1L]
            env[name] <- list(actuals[[i]])
            used <- c(used, name)
            matched[[i]] <- TRUE
        }
        if (any(!matched) && is.na(dots)) {
            return(NULL)
        }
        if (!is.na(dots)) env["..."] <- list(member_value(type = "list", elements = actuals[!matched]))
        for (name in setdiff(keys, c(used, "..."))) {
            env[name] <- list(if (!defaults) {
                unknown
            } else if (identical(formals[[name]], quote(expr = ))) {
                member_value(type = ".missing")
            } else {
                infer(formals[[name]], env)
            })
        }
        env
    }
    apply_function <- function(key = NULL, self = NULL, actuals = list(),
        fn = NULL, lexical = package_env, dispatch = NULL, r6_initialize = FALSE) {
        if (!is.null(key)) {
            aliases <- character()
            while (is.symbol(member_lookup(index$definitions, key))) {
                if (key %in% c(aliases, trail)) {
                    return(member_value(reason = "alias_cycle"))
                }
                aliases <- c(aliases, key)
                key <- as.character(index$definitions[[key]])
            }
            if (key %in% trail) {
                budget$transient <- TRUE
                return(member_value(reason = "recursion"))
            }
            definition <- member_lookup(index$definitions, key)
            fn <- definition
        }
        shape <- member_lookup(index$constructor_types, key)
        if (!is.null(shape)) {
            return(member_value(type = shape, member_package = index$package))
        }
        if (!member_head(fn, "function")) {
            return(unknown)
        }
        # S3 dispatch invokes methods directly, without the ordinary callee
        # shortcut below. Reuse a proven return shape after argument matching.
        summary_key <- if (is.null(self)) key else paste(self$type, key, sep = "|")
        summary <- member_lookup(index$method_results, summary_key)
        if (is.null(summary)) summary <- member_lookup(index$method_results, key)
        if (!is.null(summary) && !isTRUE(attr(actuals, "member_open_dots"))) {
            if (is.null(bind_arguments(fn, actuals, list(), defaults = FALSE))) {
                return(member_value(reason = "argument_matching"))
            }
            return(summary)
        }
        # Package syntax and captures are immutable. A cached invocation has
        # already passed matching; look it up before defaults and delegation.
        cache_values <- c(list(self), actuals)
        cache_key <- if (!is.null(key) && !isTRUE(attr(actuals, "member_open_dots")) &&
                all(vapply(cache_values, member_cacheable, logical(1L)))) {
            digest::digest(list(key, lapply(cache_values, member_shape_key), dispatch), algo = "xxhash64")
        } else {
            NULL
        }
        if (!is.null(cache_key) && exists(cache_key, index$cache, inherits = FALSE)) {
            return(get(cache_key, index$cache, inherits = FALSE))
        }
        # A native instance binding exposes the inner method's formals. The
        # outer factory takes the pointer and is never called for inference.
        body <- fn[[3L]]
        if (!is.null(self) && !is.null(key) && key %in% index$native_factories) {
            returned <- member_returned_function(fn)
            if (is.null(returned)) {
                return(unknown)
            }
            fn <- returned$fn
        }
        env <- bind_arguments(fn, actuals, lexical)
        if (is.null(env)) {
            return(member_value(reason = "argument_matching"))
        }
        if (!is.null(self)) {
            bound <- member_lookup(index$method_receivers, paste(self$type, key, sep = "|"))
            if (is.null(bound)) bound <- "self"
            env[bound] <- list(self)
        }
        delegated <- member_lookup(index$delegation, key)
        if (!is.null(delegated) && !is.null(self)) {
            scope <- package_env
            scope[delegated$receiver] <- list(self)
            if (!is.null(delegated$variable)) scope[delegated$variable] <- list(member_value(function_key = delegated$original))
            for (name in names(delegated$captures)) scope[name] <- list(infer(delegated$captures[[name]], scope))
            for (node in delegated$prelude) {
                if (member_head(node, "<-") && is.symbol(node[[2L]])) {
                    scope[as.character(node[[2L]])] <- list(infer(node[[3L]], scope))
                }
            }
            env <- utils::modifyList(scope, env)
            for (name in setdiff(names(scope), names(fn[[2L]]))) env[name] <- list(scope[[name]])
        }
        # Only package summaries are cached. Local closures can share syntax
        # while capturing different values and must keep their lexical identity.
        ctor <- member_lookup(index$constructors, key)
        summary_index <- index
        if (!is.null(key) && !is.null(index$document_bindings)) {
            summary_index <- list2env(as.list(index), parent = emptyenv())
            summary_index$document_bindings <- NULL
            summary_index$attached_roots <- NULL
        }
        if (!is.null(ctor)) {
            fields <- list()
            for (name in names(ctor$fields)) {
                recipe <- ctor$fields[[name]]
                factory <- member_lookup(index$members[[ctor$type]], name)
                if (!is.null(factory) && !is.na(factory) && factory %in% index$native_factories) {
                    value <- member_value(function_key = factory, metadata = index$package)
                } else if (isTRUE(recipe$active)) {
                    body <- recipe$expr
                    if (member_head(body, "function")) body <- body[[3L]]
                    value <- member_infer(body, summary_index, c(env, stats::setNames(list(member_value(type = ctor$type)), ctor$target)),
                        depth = depth + 1L, trail = c(trail, key), budget = budget
                    )
                } else {
                    value <- member_infer(recipe$expr, summary_index, env,
                        depth = depth + 1L, trail = c(trail, key), budget = budget
                    )
                }
                fields[name] <- list(value)
            }
            result <- member_value(type = ctor$type, classes = ctor$classes, fields = fields,
                member_package = index$package)
        } else {
            body <- fn[[3L]]
            ctx <- list(key = key, actuals = actuals, formals = names(fn[[2L]]), dispatch = dispatch,
                r6_initialize = r6_initialize,
                namespace_code = !is.null(index$package) && is.null(index$document_bindings))
            result <- member_infer(body, summary_index, env,
                depth = depth + 1L,
                trail = if (is.null(key)) trail else c(trail, key), budget = budget, context = ctx
            )
        }
        if (!is.null(cache_key) && !budget$exhausted && !isTRUE(budget$transient) && is.null(result$function_expr)) {
            size <- as.numeric(utils::object.size(result))
            if (size <= 1024^2) {
                if (is.null(index$cache$.bytes)) index$cache$.bytes <- 0
                if (length(index$cache) >= 512L || index$cache$.bytes + size > 4 * 1024^2) {
                    rm(list = setdiff(ls(index$cache, all.names = TRUE), ".reserve"), envir = index$cache)
                    index$cache$.bytes <- 0
                }
                if (!is.function(index$cache$.reserve) || index$cache$.reserve(size)) {
                    assign(cache_key, result, index$cache)
                    index$cache$.bytes <- index$cache$.bytes + size
                }
            }
        }
        result
    }
    if (is.symbol(expr)) {
        key <- as.character(expr)
        if (key %in% names(bindings)) {
            value <- bindings[[key]]
            if (!is.null(value$binding_expr)) {
                if (!is.null(value$binding_cache)) {
                    return(member_local_value(value, index, budget, depth, trail, receiver, context))
                }
                return(infer(value$binding_expr, value$binding_env))
            }
            return(value)
        }
        if (key %in% names(index$document_bindings) || key %in% names(index$attached_roots)) {
            value <- member_resolve_document(key, index, bindings, budget, depth, trail)
            if (!is.null(value)) {
                return(value)
            }
        }
        if (identical(key, "self") && !is.null(receiver)) {
            return(member_value(type = receiver))
        }
        if (!is.null(member_lookup(index$definitions, key)) && (is.null(index$document_bindings) ||
                    !is.null(context$key))) {
            return(member_value(function_key = key))
        }
        # Imported roots must be supplied by the document/package lexical
        # context. Never resurrect an unattached or shadowed root by name.
        if (key %in% c("NA", "NA_real_", "NA_integer_", "NA_character_", "TRUE", "FALSE", "NULL")) {
            return(switch(key,
                `TRUE` = member_literal(TRUE),
                `FALSE` = member_literal(FALSE),
                `NULL` = member_literal(NULL),
                unknown
            ))
        }
        return(unknown)
    }
    if (!is.call(expr)) {
        return(member_literal(expr))
    }
    head <- member_name(expr[[1L]])
    if (is.null(head)) head <- ""
    if (member_head(expr, "(")) {
        return(infer(expr[[2L]]))
    }
    if (head %in% c("{", "if", "return")) {
        return(analyze(expr, bindings))
    }
    if (head == "switch" && intrinsic(head)) {
        selector <- infer(expr[[2L]])
        alternatives <- as.list(expr)[-c(1L, 2L)]
        keys <- names(alternatives)
        position <- NULL
        if (selector$known_literal && length(selector$literal) == 1L) {
            if (is.character(selector$literal)) {
                position <- match(selector$literal, keys)
            } else if (is.numeric(selector$literal)) position <- as.integer(selector$literal)
            if (length(position) == 1L && !is.na(position) && position >= 1L && position <= length(alternatives)) {
                while (position < length(alternatives) && identical(alternatives[[position]], quote(expr = ))) position <- position + 1L
                return(infer(alternatives[[position]]))
            }
            # An unnamed arm is the default for a character selector.
            defaults <- if (is.null(keys)) integer() else which(!nzchar(keys))
            return(if (length(defaults) == 1L) infer(alternatives[[defaults]]) else member_literal(NULL))
        }
        results <- lapply(seq_along(alternatives), function(i) {
            if (identical(alternatives[[i]], quote(expr = ))) member_value(type = ".never") else infer(alternatives[[i]])
        })
        return(if (length(results)) Reduce(member_join, results) else member_literal(NULL))
    }
    if (member_head(expr, "function")) {
        package <- if (isTRUE(context$namespace_code)) index$package else NULL
        return(member_value(function_expr = expr, closure = member_function_closure(expr, bindings), metadata = package))
    }
    if (member_is_r6_call(expr, index, bindings)) {
        return(member_r6_shape(expr, index, bindings, budget, depth, trail))
    }
    s4 <- member_s4_call(expr, index, bindings, budget)
    if (!is.null(s4)) return(s4)
    if (head %in% member_s7_intrinsics || (member_head(expr[[1L]], "::") &&
                identical(member_name(expr[[1L]][[2L]]), "S7"))) {
        s7 <- member_s7_call(expr, index, bindings, budget)
        if (!is.null(s7)) return(s7)
    }
    if (member_head(expr, "::") || member_head(expr, ":::")) {
        package <- member_name(expr[[2L]])
        name <- member_name(expr[[3L]])
        private <- member_head(expr, ":::")
        root <- if (!private) member_lookup(index$namespace_roots, paste(package, name, sep = "::"))
        if (!is.null(root)) {
            return(root)
        }
        package_index <- member_namespace_index(index, package)
        if (private) {
            root <- member_lookup(package_index$package_roots, name)
            if (is.null(root) && identical(package, index$package)) root <- member_lookup(index$package_roots, name)
            if (!is.null(root)) return(root)
        }
        if (!is.null(package_index) && (private || name %in% package_index$exports) &&
            name %in% names(package_index$definitions)) {
            return(member_value(
                function_key = name, metadata = package
            ))
        }
        if (identical(package, index$package) && name %in% names(index$definitions) &&
                (private || is.null(index$exports) || name %in% index$exports)) {
            return(member_value(function_key = name))
        }
        return(unknown)
    }
    if (member_head(expr, "$")) {
        lhs <- infer(expr[[2L]])
        out <- member_access(lhs, member_name(expr[[3L]]), index, bindings)
        key <- member_key(expr)
        if (!length(out$type) && is.null(out$function_key) && !is.null(key) &&
            key %in% names(index$definitions) && !member_name(expr[[2L]]) %in% names(bindings)) {
            return(member_value(function_key = key))
        }
        return(out)
    }
    if (member_head(expr, "@")) {
        return(member_slot(infer(expr[[2L]]), member_name(expr[[3L]]), index, bindings, budget))
    }
    if (head %in% c("[[", "[")) {
        lhs <- infer(expr[[2L]])
        elements <- member_elements(lhs)
        selector <- if (length(expr) >= 3L && !identical(expr[[3L]], quote(expr = ))) infer(expr[[3L]]) else unknown
        method <- member_subset_method(lhs, head, index, bindings, budget, depth, trail)
        if (!is.null(method)) {
            env <- bindings
            env$.__member_subset_method__ <- method
            env$.__member_subset_receiver__ <- lhs
            call <- as.call(c(list(as.name(".__member_subset_method__"), as.name(".__member_subset_receiver__")),
                    as.list(expr)[-c(1L, 2L)]))
            return(member_infer(call,
                    index, env, depth = depth + 1L, trail = trail, budget = budget))
        }
        if (isTRUE(selector$known_literal) && length(selector$literal) == 1L) {
            name <- selector$literal
            if (is.character(name)) {
                return(member_access(lhs, name, index, bindings))
            }
            if (head == "[[" && !is.null(elements) && is.numeric(name) &&
                    name >= 1L && name <= length(elements)) {
                return(elements[[name]])
            }
        }
        if (head == "[" && length(lhs$type) == 1L && lhs$type %in% c("double", "integer", "character", "logical", "list", "data.frame")) {
            return(lhs)
        }
        if (!is.null(elements) && length(elements) && head == "[[") {
            return(Reduce(member_join, elements))
        }
        if (!is.null(lhs$element_shape) && head == "[[") {
            return(lhs$element_shape)
        }
        # Registered [ methods are analyzed with their receiver argument.
        classes <- member_classes(lhs, index)
        keys <- paste0(head, ".", classes)
        key <- keys[keys %in% names(index$definitions)]
        if (length(key)) {
            return(apply_function(key[[1L]], actuals = list(lhs, selector)))
        }
        return(unknown)
    }
    if (head %in% c("UseMethod", "NextMethod") && intrinsic(head) && !is.null(context)) {
        if (head == "UseMethod") {
            generic <- member_strings(expr[[2L]])
            input <- if (length(expr) > 2L) {
                infer(expr[[3L]])
            } else {
                member_lookup(bindings, context$formals[[1L]])
            }
            if (length(input$type) > 1L && length(generic) == 1L) {
                return(Reduce(member_join, lapply(input$type, function(type) {
                    classes <- member_classes(member_value(type = type), index)
                    keys <- c(paste(generic, classes, sep = "."), paste0(generic, ".default"))
                    keys <- keys[keys %in% names(index$definitions)]
                    if (!length(keys)) {
                        return(unknown)
                    }
                    actuals <- context$actuals
                    actuals[[1L]] <- member_value(type = type)
                    apply_function(keys[[1L]], actuals = actuals, dispatch = keys[-1L])
                })))
            }
            classes <- member_classes(input, index)
            if (!length(classes) || !length(generic)) {
                return(member_value(reason = "open_dispatch", possible_returns = member_lookup(index$generic_returns, generic)))
            }
            keys <- c(paste(generic, classes, sep = "."), paste0(generic, ".default"))
            keys <- keys[keys %in% names(index$definitions)]
        } else {
            keys <- context$dispatch
            if (!length(keys)) {
                return(unknown)
            }
        }
        if (!length(keys)) {
            return(unknown)
        }
        return(apply_function(keys[[1L]], actuals = context$actuals, dispatch = keys[-1L]))
    }
    if (head %in% index$nonreturning_functions && intrinsic(head)) {
        return(member_value(type = ".never"))
    }
    args <- as.list(expr)[-1L]
    callee <- NULL
    if (!intrinsic(head)) {
        callee <- infer(expr[[1L]])
        if (!is.null(callee$function_key) && is.null(callee$metadata)) {
            self <- callee$receiver_value
            key <- if (is.null(self)) callee$function_key else paste(self$type, callee$function_key, sep = "|")
            summary <- member_lookup(index$method_results, key)
            if (is.null(summary)) summary <- member_lookup(index$method_results, callee$function_key)
            if (!is.null(summary) && !any(vapply(args, identical, logical(1L), as.name("...")))) {
                fn <- member_definition(index$definitions, callee$function_key)
                if (member_head(fn, "function")) {
                    actuals <- lapply(args, function(arg) unknown)
                    if (is.null(bind_arguments(fn, actuals, list(), defaults = FALSE))) {
                        return(member_value(reason = "argument_matching"))
                    }
                    return(summary)
                }
            }
        }
    }
    if (head == "missing" && intrinsic(head) && length(args) == 1L &&
        identical(args[[1L]], as.name("..."))) {
        dots <- member_lookup(bindings, "...")
        if (!is.null(dots$elements)) {
            return(member_literal(length(dots$elements) == 0L))
        }
    }
    actuals <- lapply(args, function(arg) {
        if (identical(arg, quote(expr = ))) member_value(type = ".missing") else infer(arg)
    })
    # Expand syntactic dots from a known argument list without evaluating it.
    expanded <- list()
    open_dots <- FALSE
    for (i in seq_along(args)) {
        if (identical(args[[i]], as.name("...")) && !is.null(actuals[[i]]$elements)) {
            expanded <- c(expanded, actuals[[i]]$elements)
        } else {
            if (identical(args[[i]], as.name("...")) && isTRUE(actuals[[i]]$open)) open_dots <- TRUE
            expanded <- c(expanded, actuals[i])
        }
    }
    actuals <- expanded
    if (open_dots) attr(actuals, "member_open_dots") <- TRUE
    if (head == "|" && intrinsic(head) && length(actuals) == 2L && isTRUE(member_s7_modes(index)$union)) {
        classes <- lapply(actuals, function(value) member_s7_spec(value, index, bindings, budget, resolve = FALSE))
        if (any(vapply(actuals, function(value) !is.null(value$s7_descriptor), logical(1L))) &&
                !any(vapply(classes, is.null, logical(1L)))) {
            return(member_s7_value(type = "S7_union", s7_descriptor = list(kind = "union", classes = classes)))
        }
    }
    arg <- function(i) if (length(actuals) >= i) actuals[[i]] else unknown
    if (!is.null(head) && intrinsic(head)) {
        # Forwarded open dots have unknown cardinality and argument types.
        # Do not inspect the list placeholder as if it were one real argument.
        if (open_dots) {
            return(if (head %in% c("list", "list2")) {
                member_value(type = "list", open = TRUE)
            } else if (head == "missing") {
                member_value(type = "logical")
            } else {
                unknown
            })
        }
        if (head %in% c("list", "list2")) {
            if (length(actuals) > 256L) {
                return(member_value(type = "list", open = TRUE))
            }
            nms <- names(actuals)
            fields <- if (is.null(nms)) list() else actuals[nzchar(nms)]
            return(member_value(type = "list", fields = fields, elements = actuals))
        }
        if (head == "new.env") {
            return(member_value(type = "environment", fields = list()))
        }
        if (head %in% c("invisible", "identity", "arg_match0", "arg_match", "set_props", "try_fetch", "local", "withAutoprint")) {
            return(arg(1L))
        }
        if (head %in% c("head", "tail") && length(arg(1L)$type) == 1L &&
            arg(1L)$type %in% c("data.frame", "integer", "double", "character", "logical", "list")) {
            return(arg(1L))
        }
        if (head == "tryCatch") {
            result <- arg(1L)
            for (name in intersect(names(actuals), c("error", "warning", "interrupt"))) {
                handler <- actuals[[name]]
                branch <- if (!is.null(handler$function_expr)) {
                    apply_function(
                        fn = handler$function_expr,
                        actuals = list(unknown), lexical = handler$closure
                    )
                } else {
                    unknown
                }
                result <- member_join(result, branch)
            }
            return(result)
        }
        if (head %in% c("&&", "||") && isTRUE(arg(1L)$known_literal)) {
            if (head == "&&" && isFALSE(arg(1L)$literal)) {
                return(member_literal(FALSE))
            }
            if (head == "||" && identical(arg(1L)$literal, TRUE)) {
                return(member_literal(TRUE))
            }
        }
        if (head == "structure") {
            out <- arg(1L)
            classes <- actuals[["class"]]
            if (!is.null(classes)) {
                class_expr <- member_lookup(args, "class")
                empty_character <- FALSE
                if (is.call(class_expr) && length(class_expr) == 1L) {
                    empty_character <- member_head(class_expr, "character") && intrinsic("character")
                    if (member_head(class_expr[[1L]], "::")) {
                        empty_character <- identical(member_name(class_expr[[1L]][[2L]]), "base") &&
                            identical(member_name(class_expr[[1L]][[3L]]), "character")
                    }
                }
                if (empty_character) classes <- member_literal(character())
                if (!isTRUE(classes$known_literal)) return(unknown)
                if (is.null(classes$literal) || (is.character(classes$literal) && !length(classes$literal))) {
                    out$classes <- NULL
                    out$default_dollar <- NULL
                    out$column_names_only <- NULL
                    return(out)
                }
                if (!is.character(classes$literal) || anyNA(classes$literal)) return(unknown)
                out$type <- classes$literal[[1L]]
                out$classes <- classes$literal
                out$default_dollar <- FALSE
                if (!"data.frame" %in% out$classes) out$column_names_only <- NULL
            }
            return(out)
        }
        if (head == "data.frame") {
            return(member_value(type = "data.frame", classes = "data.frame", fields = actuals[nzchar(names(actuals))]))
        }
        if (head == "length") {
            x <- arg(1L)
            elements <- member_elements(x)
            if (!is.null(elements)) {
                return(member_literal(length(elements)))
            }
            if (x$known_literal) {
                return(member_literal(length(x$literal)))
            }
            return(member_value(type = "integer"))
        }
        if (head == "startsWith" && arg(1L)$known_literal && arg(2L)$known_literal) {
            return(member_literal(startsWith(arg(1L)$literal, arg(2L)$literal)))
        }
        if (head %in% c("c", ":", "seq", "seq_len", "seq_along", "rep", "rep.int")) {
            types <- unique(unlist(lapply(actuals, function(x) x$type)))
            if (any(!vapply(actuals, function(x) length(x$type) > 0L, logical(1L)))) {
                return(unknown)
            }
            type <- if ("character" %in% types) "character" else if (head %in% c(":", "seq_len", "seq_along")) "integer" else if ("double" %in% types) "double" else if ("integer" %in% types) "integer" else if ("logical" %in% types) "logical" else "list"
            if (head == "c" && all(vapply(actuals, function(x) isTRUE(x$known_literal), logical(1L)))) {
                # Concatenate literal data only, never eval the original call.
                return(member_literal(unlist(lapply(actuals, function(x) x$literal), recursive = FALSE)))
            }
            return(member_value(type = type))
        }
        simple_types <- c(
            as.character = "character", character = "character", as.integer = "integer",
            integer = "integer", as.numeric = "double", as.double = "double", numeric = "double",
            double = "double", as.logical = "logical", logical = "logical", as.raw = "raw", raw = "raw",
            charToRaw = "raw", as.Date = "Date", as.POSIXct = "POSIXct", factor = "factor",
            paste = "character", paste0 = "character", sprintf = "character", names = "character"
        )
        if (head %in% names(simple_types)) {
            return(member_value(type = unname(simple_types[[head]])))
        }
        if (head %in% c("is.null", "missing", "isTRUE", "isFALSE", "inherits", "is.character", "is.numeric", "is.integer", "is.logical", "is.list", "is_character", "is_list", "is_bool")) {
            x <- arg(1L)
            if (!length(x$type)) {
                return(member_value(type = "logical"))
            }
            if (length(x$type) != 1L) {
                return(member_value(type = "logical"))
            }
            yes <- switch(head,
                is.null = identical(x$type, "NULL"),
                missing = identical(x$type, ".missing") ||
                    (identical(member_name(args[[1L]]), "...") && identical(x$type, "list") &&
                        !is.null(x$elements) && length(x$elements) == 0L),
                isTRUE = if (x$known_literal) identical(x$literal, TRUE) else NA,
                isFALSE = if (x$known_literal) isFALSE(x$literal) else NA,
                inherits = if (isTRUE(arg(2L)$known_literal)) arg(2L)$literal %in% member_classes(x, index) else NA,
                is.character = identical(x$type, "character"),
                is.numeric = x$type %in% c("double", "integer"),
                is.integer = identical(x$type, "integer"),
                is.logical = identical(x$type, "logical"),
                is.list = identical(x$type, "list"),
                is_character = identical(x$type, "character"),
                is_list = identical(x$type, "list"),
                is_bool = if (x$known_literal) is.logical(x$literal) && length(x$literal) == 1L && !is.na(x$literal) else NA
            )
            return(if (length(yes) == 1L && !is.na(yes)) member_literal(yes) else member_value(type = "logical"))
        }
        if (head == "lapply") {
            xs <- member_elements(arg(1L))
            fn <- arg(2L)
            if (is.null(xs)) {
                input <- if (is.null(arg(1L)$element_shape)) unknown else arg(1L)$element_shape
                value <- if (!is.null(fn$function_expr)) {
                    apply_function(actuals = list(input), fn = fn$function_expr, lexical = fn$closure)
                } else {
                    apply_function(fn$function_key, actuals = list(input))
                }
                return(member_value(type = "list", element_shape = value))
            }
            if (length(xs) > 32L) {
                return(member_value(type = "list"))
            }
            elements <- lapply(xs, function(x) {
                if (!is.null(fn$function_expr)) {
                    apply_function(actuals = list(x), fn = fn$function_expr, lexical = fn$closure)
                } else {
                    apply_function(fn$function_key, actuals = list(x))
                }
            })
            return(member_value(type = "list", elements = elements))
        }
        if (head == "Reduce") {
            fn <- arg(1L)
            xs <- member_elements(arg(2L))
            init <- member_lookup(actuals, "init")
            if (is.null(init) && length(actuals) >= 3L) init <- arg(3L)
            if (is.null(xs)) {
                if (is.null(init)) {
                    return(unknown)
                }
                input <- if (is.null(arg(2L)$element_shape)) unknown else arg(2L)$element_shape
                next_shape <- if (!is.null(fn$function_expr)) {
                    apply_function(
                        fn = fn$function_expr,
                        actuals = list(init, input), lexical = fn$closure
                    )
                } else {
                    unknown
                }
                # Prove a shape-preserving transfer for any number of inputs.
                if (identical(member_shape_key(init), member_shape_key(next_shape))) {
                    return(init)
                }
                return(unknown)
            }
            if (length(xs) > 32L) {
                return(unknown)
            }
            if (is.null(init)) {
                if (!length(xs)) {
                    return(member_literal(NULL))
                }
                init <- xs[[1L]]
                xs <- xs[-1L]
            }
            for (input in xs) {
                init <- if (!is.null(fn$function_expr)) {
                    apply_function(
                        fn = fn$function_expr,
                        actuals = list(init, input), lexical = fn$closure
                    )
                } else {
                    apply_function(fn$function_key, actuals = list(init, input))
                }
            }
            return(init)
        }
    }
    operators <- c("+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "==", "!=", ">", "<", ">=", "<=", "&&", "||")
    if (head %in% operators && intrinsic(head)) {
        keys <- lapply(actuals[seq_len(min(2L, length(actuals)))], function(value) {
            candidates <- paste(head, member_classes(value, index), sep = ".")
            candidates <- candidates[candidates %in% names(index$definitions)]
            if (length(candidates)) candidates[[1L]] else character()
        })
        keys <- unique(unlist(keys))
        if (length(keys) == 1L) {
            return(apply_function(keys[[1L]], actuals = actuals))
        }
        if (length(keys) > 1L) {
            return(member_value(reason = "conflicting_dispatch"))
        }
        if (head %in% c("==", "!=", ">", "<", ">=", "<=") && length(actuals) == 2L &&
            all(vapply(actuals, function(x) x$known_literal && length(x$literal) == 1L && is.atomic(x$literal), logical(1L)))) {
            x <- arg(1L)$literal
            y <- arg(2L)$literal
            return(member_literal(switch(head,
                `==` = x == y,
                `!=` = x != y,
                `>` = x > y,
                `<` = x < y,
                `>=` = x >= y,
                `<=` = x <= y
            )))
        }
        if (head %in% c("!", "&&", "||") && all(vapply(actuals, function(x) x$known_literal && is.logical(x$literal) && length(x$literal) == 1L, logical(1L)))) {
            x <- arg(1L)$literal
            y <- arg(2L)$literal
            return(member_literal(switch(head,
                `!` = !x,
                `&&` = x && y,
                `||` = x || y
            )))
        }
        if (all(vapply(actuals, function(x) length(x$type) == 1L && x$type %in% c("integer", "double", "logical", "character"), logical(1L)))) {
            return(member_value(type = if (head %in% c("+", "-", "*", "/", "^", "%%", "%/%")) "double" else "logical"))
        }
        return(unknown)
    }
    if (is.null(callee)) callee <- infer(expr[[1L]])
    if (length(callee$possible_returns)) {
        if (open_dots) return(unknown)
        # Infer arguments once, then analyse each known callable against its
        # own receiver. Results remain possibilities through further chains.
        env <- bindings
        for (i in seq_along(actuals)) env[paste0(".__possible_arg", i)] <- list(actuals[[i]])
        call <- as.call(c(list(as.name(".__possible_callee__")),
            stats::setNames(lapply(seq_along(actuals), function(i) as.name(paste0(".__possible_arg", i))), names(actuals))))
        values <- lapply(callee$possible_returns, function(candidate) {
            env$.__possible_callee__ <- candidate
            member_infer(call, index, env, depth = depth + 1L, trail = trail, budget = budget)
        })
        return(member_possible_result(values))
    }
    if (identical(callee$metadata, "S7") && callee$function_key %in% member_s7_intrinsics) {
        return(member_s7_call(expr, index, bindings, budget, callee$function_key))
    }
    if (!is.null(callee$s7_generator)) {
        return(member_s7_construct(callee, actuals, index, bindings, budget))
    }
    if (!is.null(callee$s4_generator)) {
        return(member_s4_construct(callee$s4_generator, actuals, index, bindings, budget))
    }
    if (!is.null(callee$result_shape)) {
        return(member_r6_construct(callee, actuals, index, budget, depth, trail))
    }
    if (!is.null(callee$metadata)) {
        if (open_dots) return(unknown)
        package_index <- member_namespace_index(index, callee$metadata)
        if (is.null(package_index)) {
            return(unknown)
        }
        descriptor <- s3_generic_descriptor(member_definition(package_index$definitions, callee$function_key))
        if (!is.null(descriptor) && length(package_index$generic_returns[[descriptor$name]])) {
            methods <- union(names(bindings), names(index$document_bindings))
            methods <- methods[startsWith(methods, paste0(descriptor$name, "."))]
            at <- bindings$.__member_position__
            overridden <- any(methods %in% names(bindings)) || any(vapply(methods, function(method) {
                history <- member_lookup(index$document_bindings, method)
                length(history) && (is.null(at) || member_history_position(history, at) > 0L)
            }, logical(1L)))
            if (overridden) {
                package_index <- list2env(as.list(package_index), parent = emptyenv())
                package_index$generic_returns[[descriptor$name]] <- NULL
                package_index$cache <- new.env(parent = emptyenv())
            }
        }
        env <- package_index$package_roots
        callee$metadata <- NULL
        env$.__member_callee__ <- callee
        call <- as.call(c(
            list(as.name(".__member_callee__")),
            stats::setNames(lapply(seq_along(actuals), function(i) as.name(paste0(".__member_arg", i))), names(actuals))
        ))
        for (i in seq_along(actuals)) env[paste0(".__member_arg", i)] <- list(actuals[[i]])
        return(member_infer(call, package_index, env, depth = depth + 1L, budget = budget))
    }
    if (!is.null(callee$function_expr)) {
        env <- callee$closure
        if (!is.null(callee$receiver_name)) env[callee$receiver_name] <- list(callee$receiver_value)
        return(apply_function(actuals = actuals, fn = callee$function_expr, lexical = env,
                r6_initialize = isTRUE(callee$r6_initialize)))
    }
    key <- callee$function_key
    # Follow named aliases with the original abstract arguments intact.
    aliases <- character()
    while (!is.null(key) && is.symbol(member_lookup(index$definitions, key))) {
        if (key %in% aliases) {
            return(member_value(reason = "alias_cycle"))
        }
        aliases <- c(aliases, key)
        key <- as.character(index$definitions[[key]])
    }
    apply_function(key, callee$receiver_value, actuals)
}

# Apply only statically described registration effects to a document context.
# The registration call itself is never evaluated. Unknown name/factory stops.
member_registration_effect <- function(expr, index, bindings, budget = NULL) {
    if (!is.call(expr)) {
        return(bindings)
    }
    callee <- member_infer(expr[[1L]], index, bindings, budget = budget)
    rule <- member_lookup(index$registration_rules, callee$function_key)
    if (is.null(rule)) {
        return(bindings)
    }
    args <- as.list(expr)[-1L]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    values <- list()
    positional <- 1L
    for (i in seq_along(args)) {
        name <- nms[[i]]
        if (!nzchar(name)) {
            if (positional > length(rule$formals)) {
                return(bindings)
            }
            name <- rule$formals[[positional]]
            positional <- positional + 1L
        }
        values[name] <- list(member_infer(args[[i]], index, bindings, budget = budget))
    }
    name <- values[[rule$name_arg]]
    factory <- values[[rule$value_arg]]
    if (!isTRUE(name$known_literal) || !is.character(name$literal) || length(name$literal) != 1L || is.null(factory)) {
        return(bindings)
    }
    for (type in rule$owners) {
        env <- bindings
        env$.__factory__ <- factory
        env$.__input__ <- member_value(type = type)
        value <- member_infer(quote(.__factory__(.__input__)), index, env, budget = budget)
        if (length(value$type)) bindings$.__member_properties__[[type]][name$literal] <- list(value)
    }
    bindings
}

member_generic_index <- function(code) {
    index <- new.env(parent = emptyenv())
    index$definitions <- list()
    for (expr in str2expression(code)) {
        if (member_head(expr, "<-") && is.symbol(expr[[2L]])) {
            index$definitions[as.character(expr[[2L]])] <- list(expr[[3L]])
        }
    }
    index$generic <- TRUE
    index$package <- NULL
    index$cache <- new.env(parent = emptyenv())
    index$roots <- list()
    index$namespace_roots <- list()
    index$members <- list()
    index$properties <- list()
    index$raw_fields <- list()
    index$nonreturning_functions <- "stop"
    index$classes <- list()
    index$intrinsics <- character()
    index$constructors <- list()
    index$constructor_types <- list()
    index$native_factories <- character()
    index$registration_rules <- list()
    index$s4_classes <- list()
    index$methods_attached <- TRUE
    index
}
