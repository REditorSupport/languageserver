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
    if (is.null(x) || is.null(key) || length(key) != 1L || is.na(key) || !key %in% names(x)) {
        return(NULL)
    }
    x[[key]]
}

member_value <- function(
  type = NULL, function_key = NULL, receiver = NULL,
  fields = NULL, function_expr = NULL, closure = NULL, receiver_name = NULL,
  receiver_value = NULL, literal = NULL, known_literal = FALSE,
  elements = NULL, classes = NULL, reason = NULL, element_shape = NULL,
  open = FALSE, result_shape = NULL, binding_expr = NULL, binding_env = NULL,
  metadata = NULL, slots = NULL, slot_types = NULL, s4_class = NULL,
  s4_generator = NULL, r6_private = NULL, r6_super = NULL, r6_self = NULL
) {
    list(
        type = type, function_key = function_key, receiver = receiver,
        fields = fields, function_expr = function_expr, closure = closure,
        receiver_name = receiver_name, receiver_value = receiver_value,
        literal = literal, known_literal = known_literal, elements = elements,
        classes = classes, reason = reason, element_shape = element_shape,
        open = open, result_shape = result_shape,
        binding_expr = binding_expr, binding_env = binding_env, metadata = metadata,
        slots = slots, slot_types = slot_types, s4_class = s4_class,
        s4_generator = s4_generator, r6_private = r6_private,
        r6_super = r6_super, r6_self = r6_self
    )
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
    if (identical(a, b)) {
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
    member_value(
        type = sort(unique(c(a$type, b$type))), fields = fields,
        slots = slots, slot_types = slot_types,
        s4_class = if (identical(a$s4_class, b$s4_class)) a$s4_class else NULL,
        element_shape = if (!is.null(a$element_shape) && !is.null(b$element_shape)) member_join(a$element_shape, b$element_shape) else NULL,
        classes = if (identical(a$classes, b$classes)) a$classes else NULL
    )
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

member_members <- function(value, index, bindings = list(), accessor = "$") {
    if (!length(value$type) || any(value$type %in% c(".never", ".missing"))) {
        return(character())
    }
    if (identical(accessor, "@")) {
        slots <- names(value$slot_types)
        return(stats::setNames(rep(NA_character_, length(slots)), slots))
    }
    if (length(value$classes) && !all(value$classes %in% c("data.frame", "list", "environment", "R6")) &&
        !any(value$classes %in% names(index$members))) {
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
        type = value$type, classes = value$classes, literal = if (isTRUE(value$known_literal) &&
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
        r6_private = member_shape_key(value$r6_private, depth + 1L),
        r6_super = member_shape_key(value$r6_super, depth + 1L),
        r6_self = member_shape_key(value$r6_self, depth + 1L),
        elements = lapply(value$elements, member_shape_key, depth + 1L),
        element_shape = if (!is.null(value$element_shape)) member_shape_key(value$element_shape, depth + 1L) else NULL
    )
}

member_cacheable <- function(value, depth = 0L) {
    if (is.null(value)) {
        return(TRUE)
    }
    if (depth > 4L || !is.null(value$function_expr) || !is.null(value$closure) ||
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
        if (member_head(x, "<-") || member_head(x, "=")) {
            target <- x[[2L]]
            while (is.call(target) && length(target) > 1L) target <- target[[2L]]
            name <- member_name(target)
            if (!is.null(name)) out <<- c(out, name)
        }
    }, descend_functions = FALSE)
    unique(out)
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
    }
    if (depth > 64L || budget$remaining < 0L ||
        (!is.null(budget$deadline) && proc.time()[[3L]] > budget$deadline)) {
        budget$exhausted <- TRUE
        return(member_value(reason = "budget"))
    }
    infer <- function(x, env = bindings, ctx = context) {
        member_infer(x, index, env, receiver, depth + 1L, trail, budget, ctx)
    }
    intrinsic <- function(name) {
        at <- bindings$.__member_position__
        history <- member_lookup(index$document_bindings, name)
        shadowed <- length(history) && (is.null(at) || any(vapply(
            history,
            function(item) member_before(item$end, at), logical(1L)
        )))
        !name %in% names(bindings) &&
            !shadowed &&
            ((!name %in% names(index$definitions) && name %in% member_base_intrinsics) ||
                (name %in% index$intrinsics && (is.null(index$document_bindings) || !is.null(context$key))))
    }
    package_env <- if (!is.null(index$package_roots)) index$package_roots else index$roots
    join_env <- function(a, b) {
        keys <- union(names(a), names(b))
        stats::setNames(lapply(keys, function(key) {
            x <- member_lookup(a, key)
            y <- member_lookup(b, key)
            if (is.null(x) || is.null(y)) unknown else member_join(x, y)
        }), keys)
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
                state$value <- next_state$value
                state$env <- next_state$env
                state$falls <- next_state$falls
            }
            return(state)
        }
        if (member_head(node, "return")) {
            result <- if (length(node) > 1L) infer(node[[2L]], env) else member_literal(NULL)
            return(list(value = result, env = env, returns = result, falls = FALSE))
        }
        if (member_head(node, "if")) {
            cond <- infer(node[[2L]], env)
            yes <- !isTRUE(cond$known_literal) || !identical(cond$literal, FALSE)
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
                returns = member_join(a$returns, b$returns), falls = falls
            ))
        }
        if (member_head(node, "for") || member_head(node, "while")) {
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
                    if (!is.null(value$function_expr)) {
                        value$closure[name] <- NULL
                        value$receiver_name <- name
                    }
                    object[[surface]][field] <- list(value)
                    env[name] <- list(object)
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
        if (state$falls) member_join(state$returns, state$value) else state$returns
    }
    bind_arguments <- function(fn, actuals, env) {
        formals <- as.list(fn[[2L]])
        keys <- names(formals)
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
            env[name] <- list(if (identical(formals[[name]], quote(expr = ))) {
                member_value(type = ".missing")
            } else {
                infer(formals[[name]], env)
            })
        }
        env
    }
    apply_function <- function(key = NULL, self = NULL, actuals = list(),
                               fn = NULL, lexical = package_env, dispatch = NULL) {
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
            return(member_value(type = shape))
        }
        if (!member_head(fn, "function")) {
            return(unknown)
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
        cache_values <- env[intersect(names(env), c(names(fn[[2L]]), "self"))]
        cache_key <- if (!is.null(key) && all(vapply(cache_values, member_cacheable, logical(1L)))) {
            digest::digest(list(
                key,
                member_shape_key(self), lapply(env[intersect(names(env), c(names(fn[[2L]]), "self"))], member_shape_key),
                dispatch
            ), algo = "xxhash64")
        } else {
            NULL
        }
        if (!is.null(cache_key) && exists(cache_key, index$cache, inherits = FALSE)) {
            return(get(cache_key, index$cache, inherits = FALSE))
        }
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
                    value <- member_value(function_key = factory)
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
            result <- member_value(type = ctor$type, classes = ctor$classes, fields = fields)
        } else {
            body <- fn[[3L]]
            ctx <- list(key = key, actuals = actuals, formals = names(fn[[2L]]), dispatch = dispatch)
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
                    rm(list = ls(index$cache, all.names = TRUE), envir = index$cache)
                    index$cache$.bytes <- 0
                }
                assign(cache_key, result, index$cache)
                index$cache$.bytes <- index$cache$.bytes + size
            }
        }
        result
    }
    member <- function(lhs, name) {
        dispatch_classes <- c(
            names(index$members), names(index$classes),
            unlist(lapply(index$namespace_indices, function(x) names(x$classes)))
        )
        if (!is.null(lhs$fields) && any(lhs$classes %in% index$record_dispatch_classes)) {
            dispatch_classes <- c(dispatch_classes, lhs$classes)
        }
        if (length(lhs$classes) && !all(lhs$classes %in% c("list", "environment", "data.frame", "R6")) &&
            !any(lhs$classes %in% dispatch_classes)) {
            return(member_value(reason = "unresolved_dollar_dispatch"))
        }
        if (is.null(name)) {
            return(unknown)
        }
        if (length(lhs$type) > 1L) {
            return(Reduce(member_join, lapply(lhs$type, function(type) member(member_value(type = type), name))))
        }
        dynamic <- member_lookup(member_lookup(member_lookup(bindings, ".__member_properties__"), lhs$type), name)
        if (!is.null(dynamic)) {
            return(dynamic)
        }
        value <- member_lookup(lhs$fields, name)
        if (!is.null(value)) {
            if (!is.null(value$receiver_name)) {
                value$receiver_value <- if (is.null(lhs$r6_self)) lhs else lhs$r6_self
                if (!is.null(value$receiver_value$r6_private)) {
                    value$closure$private <- value$receiver_value$r6_private
                    value$closure$private$r6_self <- value$receiver_value
                }
                if (!is.null(value$closure$super)) value$closure$super$r6_self <- value$receiver_value
            }
            if (!is.null(value$function_key)) value$receiver_value <- lhs
            if (length(value$type) || !is.null(value$function_expr) || !is.null(value$function_key)) {
                return(value)
            }
        }
        property <- member_lookup(member_lookup(index$properties, lhs$type), name)
        if (!is.null(property)) {
            return(if (is.character(property)) member_value(type = property) else property)
        }
        raw <- member_lookup(member_lookup(index$raw_fields, lhs$type), name)
        if (!is.null(raw)) {
            return(member_value(type = raw))
        }
        members <- member_lookup(index$members, lhs$type)
        if (name %in% names(members) && !is.na(members[[name]])) {
            return(member_value(function_key = members[[name]], receiver_value = lhs))
        }
        unknown
    }
    if (is.symbol(expr)) {
        key <- as.character(expr)
        if (key %in% names(bindings)) {
            value <- bindings[[key]]
            if (!is.null(value$binding_expr)) {
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
        if (key %in% names(index$definitions) && (is.null(index$document_bindings) ||
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
        return(member_value(function_expr = expr, closure = bindings))
    }
    if (member_is_r6_call(expr, index, bindings)) {
        return(member_r6_shape(expr, index, bindings, budget))
    }
    s4 <- member_s4_call(expr, index, bindings, budget)
    if (!is.null(s4)) return(s4)
    if (member_head(expr, "::")) {
        package <- member_name(expr[[2L]])
        name <- member_name(expr[[3L]])
        root <- member_lookup(index$namespace_roots, paste(package, name, sep = "::"))
        if (!is.null(root)) {
            return(root)
        }
        package_index <- member_lookup(index$namespace_indices, package)
        if (!is.null(package_index) && name %in% package_index$exports &&
            name %in% names(package_index$definitions)) {
            return(member_value(
                function_key = name, metadata = package
            ))
        }
        if (identical(package, index$package) && name %in% names(index$definitions) &&
            (is.null(index$exports) || name %in% index$exports)) {
            return(member_value(function_key = name))
        }
        return(unknown)
    }
    if (member_head(expr, "$")) {
        lhs <- infer(expr[[2L]])
        out <- member(lhs, member_name(expr[[3L]]))
        key <- member_key(expr)
        if (!length(out$type) && is.null(out$function_key) && !is.null(key) &&
            key %in% names(index$definitions) && !member_name(expr[[2L]]) %in% names(bindings)) {
            return(member_value(function_key = key))
        }
        return(out)
    }
    if (member_head(expr, "@")) {
        return(member_s4_slot(infer(expr[[2L]]), member_name(expr[[3L]]), index, bindings, budget))
    }
    if (head %in% c("[[", "[")) {
        lhs <- infer(expr[[2L]])
        selector <- if (length(expr) >= 3L && !identical(expr[[3L]], quote(expr = ))) infer(expr[[3L]]) else unknown
        if (isTRUE(selector$known_literal) && length(selector$literal) == 1L) {
            name <- selector$literal
            if (is.character(name)) {
                return(member(lhs, name))
            }
            if (head == "[[" && !is.null(lhs$elements) && is.numeric(name) &&
                name >= 1L && name <= length(lhs$elements)) {
                return(lhs$elements[[name]])
            }
        }
        if (head == "[" && length(lhs$type) == 1L && lhs$type %in% c("double", "integer", "character", "logical", "list", "data.frame")) {
            return(lhs)
        }
        if (!is.null(lhs$elements) && length(lhs$elements) && head == "[[") {
            return(Reduce(member_join, lhs$elements))
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
                return(member_value(reason = "open_dispatch"))
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
    for (i in seq_along(args)) {
        if (identical(args[[i]], as.name("...")) && !is.null(actuals[[i]]$elements)) {
            expanded <- c(expanded, actuals[[i]]$elements)
        } else {
            expanded <- c(expanded, actuals[i])
        }
    }
    actuals <- expanded
    arg <- function(i) if (length(actuals) >= i) actuals[[i]] else unknown
    if (!is.null(head) && intrinsic(head)) {
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
            if (head == "&&" && identical(arg(1L)$literal, FALSE)) {
                return(member_literal(FALSE))
            }
            if (head == "||" && identical(arg(1L)$literal, TRUE)) {
                return(member_literal(TRUE))
            }
        }
        if (head == "structure") {
            out <- arg(1L)
            classes <- actuals[["class"]]
            if (!is.null(classes) && classes$known_literal) {
                out$type <- classes$literal[[1L]]
                out$classes <- classes$literal
            }
            return(out)
        }
        if (head == "data.frame") {
            return(member_value(type = "data.frame", classes = "data.frame", fields = actuals[nzchar(names(actuals))]))
        }
        if (head == "length") {
            x <- arg(1L)
            if (!is.null(x$elements)) {
                return(member_literal(length(x$elements)))
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
                isFALSE = if (x$known_literal) identical(x$literal, FALSE) else NA,
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
            xs <- arg(1L)$elements
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
            xs <- arg(2L)$elements
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
    callee <- infer(expr[[1L]])
    if (!is.null(callee$s4_generator)) {
        return(member_s4_construct(callee$s4_generator, actuals, index, bindings, budget))
    }
    if (!is.null(callee$result_shape)) {
        return(callee$result_shape)
    }
    if (!is.null(callee$metadata)) {
        package_index <- member_lookup(index$namespace_indices, callee$metadata)
        if (is.null(package_index)) {
            return(unknown)
        }
        env <- package_index$package_roots
        env$.__member_callee__ <- member_value(function_key = callee$function_key)
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
        return(apply_function(actuals = actuals, fn = callee$function_expr, lexical = env))
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
member_registration_effect <- function(expr, index, bindings) {
    if (!is.call(expr)) {
        return(bindings)
    }
    callee <- member_infer(expr[[1L]], index, bindings)
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
        values[name] <- list(member_infer(args[[i]], index, bindings))
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
        value <- member_infer(quote(.__factory__(.__input__)), index, env)
        if (length(value$type)) bindings$.__member_properties__[[type]][name$literal] <- list(value)
    }
    bindings
}

member_generic_index <- function(code) {
    index <- new.env(parent = emptyenv())
    index$definitions <- list()
    for (expr in parse(text = code, keep.source = FALSE)) {
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
