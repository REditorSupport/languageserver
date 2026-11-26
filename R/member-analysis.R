# Syntax summaries include defaults, which all.names() omits on pairlists.
# The native walk deduplicates symbols without allocating per-node R lists.
member_syntax_names <- function(expr) {
    .Call("member_syntax_names_c", expr, PACKAGE = "languageserver")
}

member_symbol_required <- function(workspace, document) {
    data <- document$parse_data
    if (!identical(document$version, data$version) || isTRUE(data$parse_error)) return(TRUE)
    if (!isFALSE(data$member_data$symbol_scope)) return(TRUE)
    member_package_scope_required(workspace, data$member_data, "symbol_scope")
}

member_ordinary_request <- function(workspace, document) {
    isTRUE(document$member_ordinary_edit) && !isTRUE(document$parse_data$parse_error) &&
        isFALSE(document$parse_data$member_data$class_scope) &&
        isFALSE(document$parse_data$member_data$symbol_scope) &&
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
        if (!isFALSE(candidate[[capability]])) return(TRUE)
    }
    FALSE
}

# A simple name-to-name assignment can share an environment. Keep these
# groups syntactic and only propagate a write when inference proves that
# the receiver is an environment. Lists retain their value at assignment.
member_alias_assignment <- function(groups, name, expr) {
    source <- if (is.symbol(expr)) as.character(expr)
    if (identical(source, name)) return(groups)
    if (name %in% names(groups)) {
        previous <- groups[[name]]
        groups <- groups[names(groups) != name]
        if (sum(groups == previous) == 1L) groups <- groups[groups != previous]
    }
    if (length(source) && nzchar(source)) {
        if (!source %in% names(groups)) groups[source] <- max(0L, groups) + 1L
        groups[name] <- groups[[source]]
    }
    groups
}

member_aliases <- function(groups, name) {
    if (!name %in% names(groups)) return(character())
    names(groups)[groups == groups[[name]] & names(groups) != name]
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

# Writes to a field, setter or alias change the receiver, not the source
# declaration of its name. Conditional assignments may replace that binding.
member_history_owner <- function(history, at, uri) {
    position <- member_history_position(history, at)
    if (!position) return(NULL)
    first <- history[[position]]$first_write
    if (!is.null(first)) {
        if (first <= 1L || any(vapply(history[seq.int(first, position)],
                    function(entry) identical(entry$expr, quote(.__unknown_branch_write__)), logical(1L)))) return(NULL)
        position <- first - 1L
    }
    source <- history[[position]]$declaration$uri
    if (is.null(source)) uri else source
}

# Reconstruct one visible receiver history from its own base binding and the
# writes since it joined a possible environment alias group. An ordinary list
# will ignore the indirect writes when its value is resolved.
member_history_at <- function(history, at) {
    if (!isTRUE(attr(history, "member_aliases", exact = TRUE))) return(history)
    position <- member_history_position(history, at)
    group <- if (position) history[[position]]$aliases
    if (is.null(group)) return(history)
    updates <- group$group$items
    last <- member_history_position(updates, at)
    if (last <= group$skip) return(history)
    anchor <- group$anchor
    first <- history[[anchor]]$first_write
    if (is.null(first)) first <- anchor + 1L
    history <- c(history[seq_len(anchor)], updates[seq.int(group$skip + 1L, last)])
    history[[length(history)]]$first_write <- first
    history
}

# Incremental source fingerprints are prepared with the parse, not rebuilt by
# providers. Each source segment is hashed once. Invalid virtual source ranges
# disable reuse rather than making recovery fail.
member_source_prefix <- function(content, start, end, prefix) {
    if (is.null(prefix) || length(end) != 2L || anyNA(end) || !member_before(start, end) ||
            end[[1L]] < 0L || end[[1L]] >= length(content) || end[[2L]] < 0L ||
            is.na(content[[end[[1L]] + 1L]]) ||
            end[[2L]] > nchar(content[[end[[1L]] + 1L]])) return(NULL)
    lines <- content[seq.int(start[[1L]] + 1L, end[[1L]] + 1L)]
    if (anyNA(lines)) return(NULL)
    lines[[length(lines)]] <- substr(lines[[length(lines)]], 1L, end[[2L]])
    lines[[1L]] <- substring(lines[[1L]], start[[2L]] + 1L)
    digest::digest(list(prefix, start, end, lines), algo = "xxhash64")
}

member_package_context_required <- function(document, workspace) {
    data <- document$parse_data$member_data
    if (!isTRUE(workspace$index$enabled)) return(FALSE)
    isTRUE(data$package_classes) || (!is.null(data$document_package) &&
            (isTRUE(data$has_type_hints) || workspace$index$package_has_type_hints(document$uri)))
}

member_document_cache_revision <- function(document, metadata, source_revision = NULL, workspace = NULL) {
    if (isTRUE(attr(document$parse_data$member_data$type_hints, "has_inheritance", exact = TRUE))) {
        # A donor can occur after the use or in another source file. A
        # fingerprint of statements before an assignment cannot cover either.
        source_revision <- list(source_revision, document$parse_data$member_data$content_key,
            workspace$s3_revision, workspace$index$revision,
            if (is.function(workspace$documents$size)) workspace$documents$size())
    } else if (member_package_context_required(document, workspace)) {
        source_revision <- list(source_revision, workspace$s3_revision, workspace$index$revision)
    }
    if (isTRUE(document$member_s3_dispatch)) {
        source_revision <- list(source_revision, workspace$s3_revision, workspace$index$revision,
            if (!is.null(workspace$documents)) workspace$documents$size(),
            s3_namespace_generation(workspace, document$uri))
    }
    revision <- if (is.null(metadata)) 0L else if (is.function(metadata$revision)) metadata$revision() else NULL
    identity <- list(metadata = if (is.function(metadata$identity)) metadata$identity() else metadata,
        revision = revision, sources = source_revision, roxygen = roxygen_type_hints_enabled())
    document$member_binding_cache <- analysis_cache(document, "member-bindings", identity, max_bytes = 4 * 1024^2)
    if (!identical(document$member_revision, identity)) {
        document$member_receivers$clear()
        document$member_revision <- identity
    }
    revision
}

# Cross-edit summaries must not capture lexical positions, source closures or
# mutable object identities. Package function keys are tied to the metadata
# revision; uncertain candidates have the exact same source/context key.
member_binding_cacheable <- function(value, depth = 0L) {
    if (is.null(value)) return(TRUE)
    if (depth > 8L || !is.null(value$origin) || !is.null(value$function_expr) || !is.null(value$closure) ||
            !is.null(value$binding_cache) || !is.null(value$binding_expr) || !is.null(value$binding_env) || !is.null(value$return_evidence) ||
            !is.null(value$r6_class) || !is.null(value$r6_identity) || !is.null(value$r6_role) ||
            !is.null(value$s7_descriptor) || !is.null(value$s7_generator) || !is.null(value$s4_generator) || !is.null(value$s4_contains) ||
            (!is.null(value$function_key) && is.null(value$metadata)) ||
            (isTRUE(value$known_literal) && (!is.atomic(value$literal) || is.object(value$literal)))) return(FALSE)
    children <- c(value$fields, value$slots, value$elements, value$possible_returns,
        value[c("receiver_value", "element_shape", "result_shape", "r6_private", "r6_super", "r6_self")])
    all(vapply(children, member_binding_cacheable, logical(1L), depth + 1L))
}

# Lazy local values keep the lexical state from before the assignment. The
# memo belongs to this recovered scope, never to a package-wide summary.
member_local_binding <- function(expr, bindings, referenced = member_syntax_names(expr), lookup = NULL) {
    # Keep only lexical dependencies. Copying every earlier local into every
    # deferred binding makes a flat method quadratic in retained state.
    internal <- c("$<-", "[[<-", "@<-", member_s7_setters, ".__member_position__", ".__member_properties__", ".__s4_classes__",
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
    if (is.null(value$binding_expr) && is.null(value$hint_types) && is.null(value$binding_choices)) return(value)
    cache <- value$binding_cache
    if (!is.null(cache) && exists("value", cache, inherits = FALSE) &&
            (!isTRUE(budget$track_returns) || isTRUE(cache$tracked) ||
                    !isTRUE(cache$value$reason %in% c("unknown_branch", "unknown_union_result")))) return(cache$value)
    result <- if (!is.null(value$binding_choices)) {
        limit <- if (is.null(budget$depth_limit)) 64L else budget$depth_limit
        budget$remaining <- budget$remaining - 1L
        if (budget$remaining < 0L || depth >= limit || isTRUE(budget$exhausted)) {
            budget$exhausted <- TRUE
            return(member_value(reason = "budget"))
        }
        choices <- lapply(value$binding_choices, member_local_value, index = index, budget = budget,
            depth = depth + 1L, trail = trail, receiver = receiver, context = context)
        if (isTRUE(budget$track_returns)) member_join_return_evidence(choices[[1L]], choices[[2L]]) else member_join(choices[[1L]], choices[[2L]])
    } else if (is.null(value$hint_types)) {
        env <- value$binding_env
        if (!is.null(cache$defaults)) {
            if (isTRUE(cache$resolving_default)) return(member_value(reason = "recursive_default"))
            cache$resolving_default <- TRUE
            on.exit(cache$resolving_default <- NULL, add = TRUE)
            env[names(cache$defaults)] <- cache$defaults
        }
        member_infer(value$binding_expr, index, env, receiver,
            depth = depth + 1L, trail = trail, budget = budget, context = context)
    } else if (!is.null(value$hint_input)) {
        input <- member_local_value(value$hint_input, index, budget, depth, trail, receiver, context)
        member_refine_call_hint(input, value$hint_types, index, value$binding_env, budget)
    } else {
        hint <- member_parameter_hint_value(value$hint_types, index, value$binding_env, budget)
        if (is.null(hint)) member_value(reason = "formal") else hint
    }
    if (!is.null(cache) && !isTRUE(budget$exhausted) && !isTRUE(budget$transient)) {
        cache$value <- result
        if (isTRUE(budget$track_returns)) cache$tracked <- TRUE
    }
    result
}

# Inheritance can involve a different file and different lexical class names.
# Keep each formal lazy; no document/workspace handle is retained by a closure.
member_parameter_hints <- function(hints, formals) {
    params <- hints$params
    if (!isTRUE(attr(hints, "roxygen_inherits", exact = TRUE))) return(params)
    missing <- setdiff(formals, c("...", names(params), attr(hints, "roxygen_documented", exact = TRUE)))
    if (length(missing)) {
        params[missing] <- lapply(missing, function(name) list(formal = name, declaration = hints))
    }
    params
}

member_parameter_hint_value <- function(types, index, lexical, budget) {
    if (!is.list(types)) return(member_hinted_value(types, index, lexical, budget))
    workspace <- index$dispatch_workspace
    if (is.null(workspace)) workspace <- budget$inherit_workspace
    if (is.null(workspace) || isTRUE(budget$exhausted)) return(NULL)
    hints <- types$declaration
    document <- index$dispatch_document
    uri <- attr(hints, "roxygen_uri", exact = TRUE)
    if (is.null(uri)) uri <- document$uri
    location <- attr(hints, "roxygen_key", exact = TRUE)
    if (!length(uri) || !length(location)) return(NULL)
    if (is.null(workspace$s3_revision) && is.function(workspace$documents$size) && workspace$documents$size() > 1L) {
        budget$unversioned_inherited_params <- TRUE
    }
    # Resolve the documentation once per request, even when several arguments
    # read its hints. Receiver shapes themselves depend on the current budget
    # and the requested source origin, and stay on the individual bindings.
    memo <- budget$inherited_params
    if (is.null(memo)) budget$inherited_params <- memo <- new.env(parent = emptyenv())
    key <- paste(uri, location, sep = "\n")
    if (!exists(key, memo, inherits = FALSE)) {
        assign(key, roxygen_effective_params(hints, workspace, document, hint_wait = budget$hint_wait), memo)
    }
    hint <- memo[[key]][[types$formal]]
    if (is.null(hint)) return(NULL)
    if (is.character(hint)) return(member_hinted_value(hint, index, lexical, budget))
    member_inherited_hint_value(hint, workspace, budget)
}

# A caller may never read an unknown argument. Capture its documented type
# in the defining scope without spending the request budget to resolve it.
member_hinted_argument <- function(value, types, lexical) {
    if (!length(types) || !member_needs_call_hint(value)) return(value)
    list(hint_types = types, hint_input = value, binding_env = lexical, binding_cache = new.env(parent = emptyenv()))
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
    if (isTRUE(budget$exhausted) || (!length(value$type) && !length(value$possible_returns))) return(FALSE)
    if (!isTRUE(budget$transient)) return(TRUE)
    if (!length(value$type)) {
        return(all(vapply(value$possible_returns, member_receiver_cacheable, logical(1L), index, budget)))
    }
    # Alias histories can start in a generic document context. The declared
    # native surface belongs to the receiver's namespace, regardless of which
    # package was selected while recovering the cursor.
    owner <- member_package_context(index, value$member_package)
    if (is.null(owner)) return(FALSE)
    # Recursive argument analysis must not preserve partial instance fields.
    # A native-backed class-only result has a complete declared member surface
    # independent of argument values; retain just that proven surface.
    length(value$type) == 1L && is.null(value$reason) && is.null(value$fields) &&
        is.null(value$slots) && is.null(value$function_expr) && is.null(value$closure) &&
        is.null(value$binding_expr) && is.null(value$elements) && is.null(value$element_shape) &&
        is.null(value$result_shape) && is.null(value$r6_class) && !isTRUE(value$open) &&
        (any(owner$members[[value$type]] %in% owner$native_factories, na.rm = TRUE) ||
            any(vapply(owner$properties[[value$type]], function(property) {
                length(property$type) == 1L &&
                    any(owner$members[[property$type]] %in% owner$native_factories, na.rm = TRUE)
            }, logical(1L))))
}

member_receiver_cache_value <- function(value, index, budget) {
    # Source dispatch depends on registrations in other open documents.
    # Unversioned donors also provide only request-local evidence.
    if (!is.null(budget$member_s3_context) || isTRUE(budget$unversioned_s3_sources) ||
            isTRUE(budget$unversioned_inherited_params)) return(NULL)
    if (member_receiver_cacheable(value, index, budget)) return(value)
    if (isTRUE(budget$exhausted) || !isTRUE(budget$transient) ||
            length(value$type) || !length(value$possible_returns)) return(NULL)
    # A possible native result may carry argument-dependent instance fields.
    # Its declared surface is still useful, but those partial fields cannot be
    # retained. The existing native-surface check verifies every projection.
    candidates <- lapply(value$possible_returns, function(candidate) {
        candidate$fields <- NULL
        if (member_receiver_cacheable(candidate, index, budget)) candidate else NULL
    })
    if (any(vapply(candidates, is.null, logical(1L)))) return(NULL)
    value$possible_returns <- candidates
    value
}
