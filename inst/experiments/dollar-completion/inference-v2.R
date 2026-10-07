# Extended abstract interpreter. Source inference.R first for syntax helpers.
# This replaces the prototype inference functions, not any production provider.
# Every transfer operates on ASTs and metadata; it never evaluates input code.

static_lookup <- function(x, key) {
    if (is.null(x) || is.null(key) || length(key) != 1L || is.na(key) || !key %in% names(x)) return(NULL)
    x[[key]]
}

static_value <- function(type = NULL, function_key = NULL, receiver = NULL,
    fields = NULL, function_expr = NULL, closure = NULL, receiver_name = NULL,
    receiver_value = NULL, literal = NULL, known_literal = FALSE,
    elements = NULL, classes = NULL, reason = NULL, element_shape = NULL) {
    list(type = type, function_key = function_key, receiver = receiver,
        fields = fields, function_expr = function_expr, closure = closure,
        receiver_name = receiver_name, receiver_value = receiver_value,
        literal = literal, known_literal = known_literal, elements = elements,
        classes = classes, reason = reason, element_shape = element_shape)
}

static_literal <- function(x) {
    type <- if (is.null(x)) "NULL" else typeof(x)
    static_value(type = type, literal = x, known_literal = TRUE)
}

static_join <- function(a, b) {
    if (identical(a$type, ".never")) return(b)
    if (identical(b$type, ".never")) return(a)
    if (identical(a, b)) return(a)
    if (!length(a$type) || !length(b$type)) return(static_value(reason = "unknown_branch"))
    fields <- NULL
    if (!is.null(a$fields) && !is.null(b$fields)) {
        common <- intersect(names(a$fields), names(b$fields))
        fields <- setNames(lapply(common, function(n) static_join(a$fields[[n]], b$fields[[n]])), common)
    }
    static_value(type = sort(unique(c(a$type, b$type))), fields = fields,
        element_shape = if (!is.null(a$element_shape) && !is.null(b$element_shape)) static_join(a$element_shape, b$element_shape) else NULL,
        classes = if (identical(a$classes, b$classes)) a$classes else NULL)
}

static_classes <- function(value, index) {
    if (length(value$type) != 1L) return(character())
    if (length(value$classes)) return(value$classes)
    classes <- static_lookup(index$classes, value$type)
    if (is.null(classes)) value$type else classes
}

static_members <- function(value, index, bindings = list()) {
    if (!length(value$type) || any(value$type %in% c(".never", ".missing"))) return(character())
    sets <- lapply(value$type, function(type) {
        out <- static_lookup(index$members, type)
        if (is.null(out)) character() else out
    })
    common <- Reduce(intersect, lapply(sets, names))
    out <- sets[[1L]][common]
    dynamic <- static_lookup(bindings, ".__member_properties__")
    if (length(value$type) == 1L) {
        extra <- setdiff(names(static_lookup(dynamic, value$type)), names(out))
        out <- c(out, setNames(rep(NA_character_, length(extra)), extra))
    }
    if (!is.null(value$fields)) {
        extra <- setdiff(names(value$fields), names(out))
        out <- c(out, setNames(rep(NA_character_, length(extra)), extra))
    }
    out
}

# A bounded structural cache key omits irrelevant literals/large closure maps.
# Function identity and the metadata generation isolate package summaries.
static_shape_key <- function(value, depth = 0L) {
    if (is.null(value)) return(NULL)
    if (depth > 4L) return("...")
    list(type = value$type, classes = value$classes, literal = if (isTRUE(value$known_literal) &&
        length(value$literal) <= 8L) value$literal else NULL,
        known = value$known_literal, function_key = value$function_key,
        fields = lapply(value$fields, static_shape_key, depth + 1L),
        elements = lapply(value$elements, static_shape_key, depth + 1L),
        element_shape = if (!is.null(value$element_shape)) static_shape_key(value$element_shape, depth + 1L) else NULL)
}

static_cacheable <- function(value, depth = 0L) {
    if (is.null(value)) return(TRUE)
    if (depth > 4L || !is.null(value$function_expr) || !is.null(value$closure)) return(FALSE)
    if (value$known_literal && length(value$literal) > 8L) return(FALSE)
    all(vapply(c(value$fields, value$elements, if (!is.null(value$element_shape)) list(value$element_shape)),
        static_cacheable, logical(1L), depth + 1L))
}

static_assigned_names <- function(node) {
    out <- character()
    static_walk(node, function(x) {
        if (static_head(x, "<-") || static_head(x, "=")) {
            target <- x[[2L]]
            while (is.call(target) && length(target) > 1L) target <- target[[2L]]
            name <- static_name(target)
            if (!is.null(name)) out <<- c(out, name)
        }
    }, descend_functions = FALSE)
    unique(out)
}

static_infer <- function(expr, index, bindings = list(), receiver = NULL,
    depth = 0L, trail = character(), budget = NULL, context = NULL) {
    unknown <- static_value(reason = "unsupported")
    if (missing(expr)) return(static_value(type = ".missing"))
    if (is.null(budget)) {
        budget <- new.env(parent = emptyenv())
        budget$remaining <- 20000L
        budget$exhausted <- FALSE
        budget$transient <- FALSE
    }
    budget$remaining <- budget$remaining - 1L
    if (depth > 80L || budget$remaining < 0L) {
        budget$exhausted <- TRUE
        return(static_value(reason = "budget"))
    }
    infer <- function(x, env = bindings, ctx = context) {
        static_infer(x, index, env, receiver, depth + 1L, trail, budget, ctx)
    }
    intrinsic <- function(name) !name %in% names(bindings) &&
        !name %in% names(index$definitions) || name %in% index$intrinsics &&
        !name %in% names(bindings)
    package_env <- if (!is.null(index$package_roots)) index$package_roots else index$roots
    join_env <- function(a, b) {
        keys <- union(names(a), names(b))
        setNames(lapply(keys, function(key) {
            x <- static_lookup(a, key)
            y <- static_lookup(b, key)
            if (is.null(x) || is.null(y)) unknown else static_join(x, y)
        }), keys)
    }
    # Flow records keep early returns separate from fall-through values.
    flow <- function(node, env) {
        if (static_head(node, "{")) {
            state <- list(value = static_literal(NULL), env = env,
                returns = static_value(type = ".never"), falls = TRUE)
            for (child in as.list(node)[-1L]) {
                if (!state$falls) break
                next_state <- flow(child, state$env)
                state$returns <- static_join(state$returns, next_state$returns)
                state$value <- next_state$value
                state$env <- next_state$env
                state$falls <- next_state$falls
            }
            return(state)
        }
        if (static_head(node, "return")) {
            result <- if (length(node) > 1L) infer(node[[2L]], env) else static_literal(NULL)
            return(list(value = result, env = env, returns = result, falls = FALSE))
        }
        if (static_head(node, "if")) {
            cond <- infer(node[[2L]], env)
            yes <- !isTRUE(cond$known_literal) || !identical(cond$literal, FALSE)
            no <- !isTRUE(cond$known_literal) || !identical(cond$literal, TRUE)
            a <- if (yes) flow(node[[3L]], env) else NULL
            b <- if (no) {
                if (length(node) == 4L) flow(node[[4L]], env) else
                    list(value = static_literal(NULL), env = env,
                        returns = static_value(type = ".never"), falls = TRUE)
            } else NULL
            if (is.null(a)) return(b)
            if (is.null(b)) return(a)
            falls <- a$falls || b$falls
            out_env <- if (!a$falls) b$env else if (!b$falls) a$env else join_env(a$env, b$env)
            value <- if (!a$falls) b$value else if (!b$falls) a$value else static_join(a$value, b$value)
            return(list(value = value, env = out_env,
                returns = static_join(a$returns, b$returns), falls = falls))
        }
        if (static_head(node, "for") || static_head(node, "while")) {
            # Preserve bindings not assigned by the loop. Never assume one
            # iteration represents all iterations of an unknown loop.
            writes <- static_assigned_names(node)
            for (name in writes) env[name] <- list(unknown)
            has_return <- FALSE
            static_walk(node, function(x) {
                if (static_head(x, "return")) has_return <<- TRUE
            }, descend_functions = FALSE)
            return(list(value = static_literal(NULL), env = env,
                returns = if (has_return) unknown else static_value(type = ".never"), falls = TRUE))
        }
        if (static_head(node, "<-") || static_head(node, "=")) {
            lhs <- node[[2L]]
            value <- infer(node[[3L]], env)
            if (is.symbol(lhs)) {
                env[as.character(lhs)] <- list(value)
            } else if (static_head(lhs, "$") || static_head(lhs, "[[")) {
                name <- static_name(lhs[[2L]])
                field <- static_name(lhs[[3L]])
                object <- static_lookup(env, name)
                if (!is.null(object$fields) && !is.null(field)) {
                    if (!is.null(value$function_expr)) {
                        value$closure[name] <- NULL
                        value$receiver_name <- name
                    }
                    object$fields[field] <- list(value)
                    env[name] <- list(object)
                } else if (!is.null(name)) env[name] <- list(unknown)
            } else if (static_head(lhs, "class")) {
                name <- static_name(lhs[[2L]])
                object <- static_lookup(env, name)
                classes <- static_strings(node[[3L]])
                if (!is.null(object) && length(classes)) {
                    object$type <- classes[[1L]]
                    object$classes <- classes
                    env[name] <- list(object)
                } else if (!is.null(name)) env[name] <- list(unknown)
            }
            return(list(value = value, env = env, returns = static_value(type = ".never"), falls = TRUE))
        }
        # Arbitrary reflection can mutate local bindings. Do not retain types
        # through it. Reviewed package metadata wrappers have their own rules.
        if (static_head(node, "assign") || static_head(node, "eval") || static_head(node, "source")) {
            env <- lapply(env, function(x) unknown)
        }
        value <- infer(node, env)
        list(value = value, env = env, returns = static_value(type = ".never"),
            falls = !identical(value$type, ".never"))
    }
    analyze <- function(body, env) {
        state <- flow(body, env)
        if (state$falls) static_join(state$returns, state$value) else state$returns
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
                if (candidates %in% used) return(NULL)
                env[candidates] <- list(actuals[[i]])
                used <- c(used, candidates)
                matched[[i]] <- TRUE
            } else if (length(candidates) > 1L) return(NULL)
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
        if (any(!matched) && is.na(dots)) return(NULL)
        if (!is.na(dots)) env["..."] <- list(static_value(type = "list", elements = actuals[!matched]))
        for (name in setdiff(keys, c(used, "..."))) {
            env[name] <- list(if (identical(formals[[name]], quote(expr = )))
                static_value(type = ".missing") else infer(formals[[name]], env))
        }
        env
    }
    apply_function <- function(key = NULL, self = NULL, actuals = list(),
        fn = NULL, lexical = package_env, dispatch = NULL) {
        if (!is.null(key)) {
            aliases <- character()
            while (is.symbol(static_lookup(index$definitions, key))) {
                if (key %in% c(aliases, trail)) return(static_value(reason = "alias_cycle"))
                aliases <- c(aliases, key)
                key <- as.character(index$definitions[[key]])
            }
            if (key %in% trail) {
                budget$transient <- TRUE
                return(static_value(reason = "recursion"))
            }
            definition <- static_lookup(index$definitions, key)
            fn <- definition
        }
        if (!static_head(fn, "function")) {
            shape <- static_lookup(index$constructor_types, key)
            return(if (is.null(shape)) unknown else static_value(type = shape))
        }
        # A native instance binding exposes the inner method's formals. The
        # outer factory takes the pointer and is never called for inference.
        body <- fn[[3L]]
        if (!is.null(key) && key %in% index$native_factories) {
            inner <- if (static_head(body, "{")) body[[length(body)]] else body
            fn <- as.call(list(as.name("function"), inner[[2L]], inner[[3L]]))
        }
        env <- bind_arguments(fn, actuals, lexical)
        if (is.null(env)) return(static_value(reason = "argument_matching"))
        if (!is.null(self)) env["self"] <- list(self)
        delegated <- static_lookup(index$delegation, key)
        if (!is.null(delegated) && !is.null(self)) {
            env["_s"] <- list(member(self, "_s"))
        }
        # Only package summaries are cached. Local closures can share syntax
        # while capturing different values and must keep their lexical identity.
        cache_values <- env[intersect(names(env), c(names(fn[[2L]]), "self"))]
        cache_key <- if (!is.null(key) && all(vapply(cache_values, static_cacheable, logical(1L)))) digest::digest(list(key,
            static_shape_key(self), lapply(env[intersect(names(env), c(names(fn[[2L]]), "self"))], static_shape_key),
            dispatch), algo = "xxhash64") else NULL
        if (!is.null(cache_key) && exists(cache_key, index$cache, inherits = FALSE)) {
            return(get(cache_key, index$cache, inherits = FALSE))
        }
        ctor <- static_lookup(index$constructors, key)
        if (!is.null(ctor)) {
            fields <- list()
            for (name in names(ctor$fields)) {
                recipe <- ctor$fields[[name]]
                if (isTRUE(recipe$active)) {
                    body <- recipe$expr
                    if (static_head(body, "function")) body <- body[[3L]]
                    value <- static_infer(body, index, c(env, list(self = static_value(type = ctor$type))),
                        depth = depth + 1L, trail = c(trail, key), budget = budget)
                } else value <- static_infer(recipe$expr, index, env,
                    depth = depth + 1L, trail = c(trail, key), budget = budget)
                fields[name] <- list(value)
            }
            result <- static_value(type = ctor$type, classes = ctor$classes, fields = fields)
        } else {
            body <- fn[[3L]]
            ctx <- list(key = key, actuals = actuals, formals = names(fn[[2L]]), dispatch = dispatch)
            result <- static_infer(body, index, env, depth = depth + 1L,
                trail = if (is.null(key)) trail else c(trail, key), budget = budget, context = ctx)
        }
        if (!is.null(cache_key) && !budget$exhausted && !isTRUE(budget$transient) && is.null(result$function_expr)) {
            assign(cache_key, result, index$cache)
        }
        result
    }
    member <- function(lhs, name) {
        if (is.null(name)) return(unknown)
        if (length(lhs$type) > 1L) {
            return(Reduce(static_join, lapply(lhs$type, function(type) member(static_value(type = type), name))))
        }
        dynamic <- static_lookup(static_lookup(static_lookup(bindings, ".__member_properties__"), lhs$type), name)
        if (!is.null(dynamic)) return(dynamic)
        prefix <- static_lookup(index$native_member_prefixes, lhs$type)
        if (!is.null(prefix)) {
            key <- paste(prefix, name, sep = "_")
            if (key %in% names(index$definitions)) return(static_value(function_key = key, receiver_value = lhs))
        }
        value <- static_lookup(lhs$fields, name)
        if (!is.null(value)) {
            if (!is.null(value$receiver_name)) value$receiver_value <- lhs
            if (!is.null(value$function_key)) value$receiver_value <- lhs
            if (length(value$type) || !is.null(value$function_expr) || !is.null(value$function_key)) return(value)
        }
        property <- static_lookup(static_lookup(index$properties, lhs$type), name)
        if (!is.null(property)) return(if (is.character(property)) static_value(type = property) else property)
        raw <- static_lookup(static_lookup(index$raw_fields, lhs$type), name)
        if (!is.null(raw)) return(static_value(type = raw))
        members <- static_lookup(index$members, lhs$type)
        if (name %in% names(members) && !is.na(members[[name]])) {
            return(static_value(function_key = members[[name]], receiver_value = lhs))
        }
        prefix <- static_lookup(index$native_member_prefixes, lhs$type)
        if (!is.null(prefix)) {
            key <- paste(prefix, name, sep = "_")
            if (key %in% names(index$definitions)) return(static_value(function_key = key, receiver_value = lhs))
        }
        unknown
    }
    if (is.symbol(expr)) {
        key <- as.character(expr)
        if (key %in% names(bindings)) return(bindings[[key]])
        if (identical(key, "self") && !is.null(receiver)) return(static_value(type = receiver))
        if (key %in% names(index$definitions)) return(static_value(function_key = key))
        # Imported roots must be supplied by the document/package lexical
        # context. Never resurrect an unattached or shadowed root by name.
        if (key %in% c("NA", "NA_real_", "NA_integer_", "NA_character_", "TRUE", "FALSE", "NULL")) {
            return(switch(key, `TRUE` = static_literal(TRUE), `FALSE` = static_literal(FALSE), `NULL` = static_literal(NULL), unknown))
        }
        return(unknown)
    }
    if (!is.call(expr)) return(static_literal(expr))
    head <- static_name(expr[[1L]])
    if (is.null(head)) head <- ""
    if (static_head(expr, "(")) return(infer(expr[[2L]]))
    if (head %in% c("{", "if", "return")) return(analyze(expr, bindings))
    if (head == "switch" && intrinsic(head)) {
        selector <- infer(expr[[2L]])
        alternatives <- as.list(expr)[-c(1L, 2L)]
        keys <- names(alternatives)
        position <- NULL
        if (selector$known_literal && length(selector$literal) == 1L) {
            if (is.character(selector$literal)) position <- match(selector$literal, keys)
            else if (is.numeric(selector$literal)) position <- as.integer(selector$literal)
            if (length(position) == 1L && !is.na(position) && position >= 1L && position <= length(alternatives)) {
                while (position < length(alternatives) && identical(alternatives[[position]], quote(expr = ))) position <- position + 1L
                return(infer(alternatives[[position]]))
            }
            # An unnamed arm is the default for a character selector.
            defaults <- if (is.null(keys)) integer() else which(!nzchar(keys))
            return(if (length(defaults) == 1L) infer(alternatives[[defaults]]) else static_literal(NULL))
        }
        results <- lapply(seq_along(alternatives), function(i) {
            if (identical(alternatives[[i]], quote(expr = ))) static_value(type = ".never") else infer(alternatives[[i]])
        })
        return(if (length(results)) Reduce(static_join, results) else static_literal(NULL))
    }
    if (static_head(expr, "function")) return(static_value(function_expr = expr, closure = bindings))
    if (static_head(expr, "::")) {
        package <- static_name(expr[[2L]])
        name <- static_name(expr[[3L]])
        root <- static_lookup(index$namespace_roots, paste(package, name, sep = "::"))
        if (!is.null(root)) return(root)
        if (identical(package, index$package) && name %in% names(index$definitions) &&
            (is.null(index$exports) || name %in% index$exports)) return(static_value(function_key = name))
        return(unknown)
    }
    if (static_head(expr, "$")) {
        lhs <- infer(expr[[2L]])
        out <- member(lhs, static_name(expr[[3L]]))
        key <- static_key(expr)
        if (!length(out$type) && is.null(out$function_key) && !is.null(key) &&
            key %in% names(index$definitions) && !static_name(expr[[2L]]) %in% names(bindings)) {
            return(static_value(function_key = key))
        }
        return(out)
    }
    if (head %in% c("[[", "[")) {
        lhs <- infer(expr[[2L]])
        selector <- if (length(expr) >= 3L && !identical(expr[[3L]], quote(expr = ))) infer(expr[[3L]]) else unknown
        if (isTRUE(selector$known_literal) && length(selector$literal) == 1L) {
            name <- selector$literal
            if (is.character(name)) return(member(lhs, name))
            if (head == "[[" && !is.null(lhs$elements) && is.numeric(name) &&
                name >= 1L && name <= length(lhs$elements)) return(lhs$elements[[name]])
        }
        if (head == "[" && length(lhs$type) == 1L && lhs$type %in% c("double", "integer", "character", "logical", "list", "data.frame")) return(lhs)
        if (!is.null(lhs$elements) && length(lhs$elements) && head == "[[") return(Reduce(static_join, lhs$elements))
        if (!is.null(lhs$element_shape) && head == "[[") return(lhs$element_shape)
        # Registered [ methods are analyzed with their receiver argument.
        classes <- static_classes(lhs, index)
        keys <- paste0(head, ".", classes)
        key <- keys[keys %in% names(index$definitions)]
        if (length(key)) return(apply_function(key[[1L]], actuals = list(lhs, selector)))
        return(unknown)
    }
    if (head %in% c("UseMethod", "NextMethod") && intrinsic(head) && !is.null(context)) {
        if (head == "UseMethod") {
            generic <- static_strings(expr[[2L]])
            input <- if (length(expr) > 2L) infer(expr[[3L]]) else
                static_lookup(bindings, context$formals[[1L]])
            classes <- static_classes(input, index)
            if (!length(classes) || !length(generic)) return(static_value(reason = "open_dispatch"))
            keys <- c(paste(generic, classes, sep = "."), paste0(generic, ".default"))
            keys <- keys[keys %in% names(index$definitions)]
        } else {
            keys <- context$dispatch
            if (!length(keys)) return(unknown)
        }
        if (!length(keys)) return(unknown)
        return(apply_function(keys[[1L]], actuals = context$actuals, dispatch = keys[-1L]))
    }
    # Do not infer arguments to native wrappers or error functions: their
    # summaries are independent of argument values and never call native code.
    if (head %in% names(index$native_types) && !head %in% names(bindings)) return(static_value(type = index$native_types[[head]]))
    if (head %in% index$nonreturning_functions && intrinsic(head)) return(static_value(type = ".never"))
    if (head %in% index$wrapper_functions && !head %in% names(bindings)) {
        value <- infer(expr[[2L]])
        if (!length(value$type)) return(unknown)
        return(Reduce(static_join, lapply(value$type, function(type) {
            ctor <- static_lookup(index$wrap_constructors, type)
            if (!is.null(ctor)) return(apply_function(ctor, actuals = list(static_value(type = type))))
            target <- static_lookup(index$wrapped_types, type)
            if (!is.null(target)) return(static_value(type = target))
            if (type %in% names(index$members) || type %in% c(".never", "NULL", "list", "double", "integer", "character", "logical", "raw")) {
                result <- value
                result$type <- type
                return(result)
            }
            unknown
        })))
    }
    args <- as.list(expr)[-1L]
    actuals <- lapply(args, function(arg) {
        if (identical(arg, quote(expr = ))) static_value(type = ".missing") else infer(arg)
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
            nms <- names(actuals)
            fields <- if (is.null(nms)) list() else actuals[nzchar(nms)]
            return(static_value(type = "list", fields = fields, elements = actuals))
        }
        if (head == "new.env") return(static_value(type = "environment", fields = list()))
        if (head %in% c("invisible", "identity", "arg_match0", "arg_match", "set_props", "try_fetch", "local", "withAutoprint")) return(arg(1L))
        if (head %in% c("head", "tail") && length(arg(1L)$type) == 1L &&
            arg(1L)$type %in% c("data.frame", "integer", "double", "character", "logical", "list")) return(arg(1L))
        if (head == "tryCatch") {
            result <- arg(1L)
            for (name in intersect(names(actuals), c("error", "warning", "interrupt"))) {
                handler <- actuals[[name]]
                branch <- if (!is.null(handler$function_expr)) apply_function(fn = handler$function_expr,
                    actuals = list(unknown), lexical = handler$closure) else unknown
                result <- static_join(result, branch)
            }
            return(result)
        }
        if (head %in% c("&&", "||") && isTRUE(arg(1L)$known_literal)) {
            if (head == "&&" && identical(arg(1L)$literal, FALSE)) return(static_literal(FALSE))
            if (head == "||" && identical(arg(1L)$literal, TRUE)) return(static_literal(TRUE))
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
        if (head == "data.frame") return(static_value(type = "data.frame", classes = "data.frame", fields = actuals[nzchar(names(actuals))]))
        if (head == "length") {
            x <- arg(1L)
            if (!is.null(x$elements)) return(static_literal(length(x$elements)))
            if (x$known_literal) return(static_literal(length(x$literal)))
            return(static_value(type = "integer"))
        }
        if (head == "startsWith" && arg(1L)$known_literal && arg(2L)$known_literal) {
            return(static_literal(startsWith(arg(1L)$literal, arg(2L)$literal)))
        }
        if (head %in% c("c", ":", "seq", "seq_len", "seq_along", "rep", "rep.int")) {
            types <- unique(unlist(lapply(actuals, function(x) x$type)))
            if (any(!vapply(actuals, function(x) length(x$type) > 0L, logical(1L)))) return(unknown)
            type <- if ("character" %in% types) "character" else if (head %in% c(":", "seq_len", "seq_along")) "integer" else
                if ("double" %in% types) "double" else if ("integer" %in% types) "integer" else if ("logical" %in% types) "logical" else "list"
            if (head == "c" && all(vapply(actuals, function(x) isTRUE(x$known_literal), logical(1L)))) {
                # Concatenate literal data only, never eval the original call.
                return(static_literal(unlist(lapply(actuals, function(x) x$literal), recursive = FALSE)))
            }
            return(static_value(type = type))
        }
        simple_types <- c(as.character = "character", character = "character", as.integer = "integer",
            integer = "integer", as.numeric = "double", as.double = "double", numeric = "double",
            double = "double", as.logical = "logical", logical = "logical", as.raw = "raw", raw = "raw",
            charToRaw = "raw", as.Date = "Date", as.POSIXct = "POSIXct", factor = "factor",
            paste = "character", paste0 = "character", sprintf = "character", names = "character")
        if (head %in% names(simple_types)) return(static_value(type = unname(simple_types[[head]])))
        if (head %in% c("is.null", "missing", "isTRUE", "isFALSE", "inherits", "is.character", "is.numeric", "is.integer", "is.logical", "is.list", "is_character", "is_list", "is_bool")) {
            x <- arg(1L)
            if (!length(x$type)) return(static_value(type = "logical"))
            if (length(x$type) != 1L) return(static_value(type = "logical"))
            yes <- switch(head, is.null = identical(x$type, "NULL"), missing = identical(x$type, ".missing"),
                isTRUE = if (x$known_literal) identical(x$literal, TRUE) else NA,
                isFALSE = if (x$known_literal) identical(x$literal, FALSE) else NA,
                inherits = if (isTRUE(arg(2L)$known_literal)) arg(2L)$literal %in% static_classes(x, index) else NA,
                is.character = identical(x$type, "character"), is.numeric = x$type %in% c("double", "integer"),
                is.integer = identical(x$type, "integer"), is.logical = identical(x$type, "logical"), is.list = identical(x$type, "list"),
                is_character = identical(x$type, "character"), is_list = identical(x$type, "list"),
                is_bool = if (x$known_literal) is.logical(x$literal) && length(x$literal) == 1L && !is.na(x$literal) else NA)
            return(if (length(yes) == 1L && !is.na(yes)) static_literal(yes) else static_value(type = "logical"))
        }
        if (head %in% c("lapply", "Map")) {
            xs <- arg(1L)$elements
            fn <- arg(2L)
            if (is.null(xs)) {
                input <- if (is.null(arg(1L)$element_shape)) unknown else arg(1L)$element_shape
                value <- if (!is.null(fn$function_expr)) apply_function(actuals = list(input), fn = fn$function_expr, lexical = fn$closure) else
                    apply_function(fn$function_key, actuals = list(input))
                return(static_value(type = "list", element_shape = value))
            }
            if (length(xs) > 32L) return(static_value(type = "list"))
            elements <- lapply(xs, function(x) {
                if (!is.null(fn$function_expr)) apply_function(actuals = list(x), fn = fn$function_expr, lexical = fn$closure) else
                    apply_function(fn$function_key, actuals = list(x))
            })
            return(static_value(type = "list", elements = elements))
        }
        if (head == "Reduce") {
            fn <- arg(1L)
            xs <- arg(2L)$elements
            init <- static_lookup(actuals, "init")
            if (is.null(init) && length(actuals) >= 3L) init <- arg(3L)
            if (is.null(xs)) {
                if (is.null(init)) return(unknown)
                input <- if (is.null(arg(2L)$element_shape)) unknown else arg(2L)$element_shape
                next_shape <- if (!is.null(fn$function_expr)) apply_function(fn = fn$function_expr,
                    actuals = list(init, input), lexical = fn$closure) else unknown
                # Prove a shape-preserving transfer for any number of inputs.
                if (identical(static_shape_key(init), static_shape_key(next_shape))) return(init)
                return(unknown)
            }
            if (length(xs) > 32L) return(unknown)
            if (is.null(init)) {
                if (!length(xs)) return(static_literal(NULL))
                init <- xs[[1L]]
                xs <- xs[-1L]
            }
            for (input in xs) {
                init <- if (!is.null(fn$function_expr)) apply_function(fn = fn$function_expr,
                    actuals = list(init, input), lexical = fn$closure) else apply_function(fn$function_key, actuals = list(init, input))
            }
            return(init)
        }
    }
    operators <- c("+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "==", "!=", ">", "<", ">=", "<=", "&&", "||")
    if (head %in% operators && intrinsic(head)) {
        keys <- lapply(actuals[seq_len(min(2L, length(actuals)))], function(value) {
            candidates <- paste(head, static_classes(value, index), sep = ".")
            candidates <- candidates[candidates %in% names(index$definitions)]
            if (length(candidates)) candidates[[1L]] else character()
        })
        keys <- unique(unlist(keys))
        if (length(keys) == 1L) return(apply_function(keys[[1L]], actuals = actuals))
        if (length(keys) > 1L) return(static_value(reason = "conflicting_dispatch"))
        if (head %in% c("==", "!=", ">", "<", ">=", "<=") && length(actuals) == 2L &&
            all(vapply(actuals, function(x) x$known_literal && length(x$literal) == 1L && is.atomic(x$literal), logical(1L)))) {
            x <- arg(1L)$literal
            y <- arg(2L)$literal
            return(static_literal(switch(head, `==` = x == y, `!=` = x != y, `>` = x > y,
                `<` = x < y, `>=` = x >= y, `<=` = x <= y)))
        }
        if (head %in% c("!", "&&", "||") && all(vapply(actuals, function(x) x$known_literal && is.logical(x$literal) && length(x$literal) == 1L, logical(1L)))) {
            x <- arg(1L)$literal
            y <- arg(2L)$literal
            return(static_literal(switch(head, `!` = !x, `&&` = x && y, `||` = x || y)))
        }
        if (all(vapply(actuals, function(x) length(x$type) == 1L && x$type %in% c("integer", "double", "logical", "character"), logical(1L)))) {
            return(static_value(type = if (head %in% c("+", "-", "*", "/", "^", "%%", "%/%")) "double" else "logical"))
        }
        return(unknown)
    }
    callee <- infer(expr[[1L]])
    if (!is.null(callee$function_expr)) {
        env <- callee$closure
        if (!is.null(callee$receiver_name)) env[callee$receiver_name] <- list(callee$receiver_value)
        return(apply_function(actuals = actuals, fn = callee$function_expr, lexical = env))
    }
    key <- callee$function_key
    # Follow named aliases with the original abstract arguments intact.
    aliases <- character()
    while (!is.null(key) && is.symbol(static_lookup(index$definitions, key))) {
        if (key %in% aliases) return(static_value(reason = "alias_cycle"))
        aliases <- c(aliases, key)
        key <- as.character(index$definitions[[key]])
    }
    apply_function(key, callee$receiver_value, actuals)
}

# Keep the proven cursor recovery; use the common-member policy for unions.
static_complete_v1 <- static_complete
static_complete <- function(code, index, attached = FALSE, closers = "") {
    result <- static_complete_v1(code, index, attached, closers)
    if (is.null(result)) return(NULL)
    # v1 calls the overridden inference; replace candidate extraction to include
    # class members alongside instance fields and guaranteed union members.
    sentinel <- ".__static_member_completion__"
    suffix <- regexpr("\\$[[:alnum:]_.]*$", code)
    parsed <- tryCatch(parse(text = paste0(substr(code, 1L, suffix[[1L]]), sentinel, closers)), error = function(e) NULL)
    env <- if (attached) index$roots else list()
    for (expr in parsed) {
        found <- FALSE
        static_walk(expr, function(node) {
            if (static_head(node, "$") && identical(static_name(node[[3L]]), sentinel)) found <<- TRUE
        })
        if (found) break
        if (static_head(expr, "library") && identical(static_name(expr[[2L]]), index$package)) env <- index$roots
        if (static_head(expr, "<-") && is.symbol(expr[[2L]])) env[as.character(expr[[2L]])] <- list(static_infer(expr[[3L]], index, env))
        env <- static_registration_effect(expr, index, env)
    }
    value <- static_infer(result$receiver, index, env)
    members <- static_members(value, index, env)
    labels <- as.character(names(members))
    result$labels <- sort(unique(labels[!startsWith(labels, "_") & !startsWith(labels, ".") &
        startsWith(tolower(labels), tolower(result$token))]))
    result$functions <- members[result$labels]
    result
}

# Apply only statically described registration effects to a document context.
# The registration call itself is never evaluated. Unknown name/factory stops.
static_registration_effect <- function(expr, index, bindings) {
    if (!is.call(expr)) return(bindings)
    callee <- static_infer(expr[[1L]], index, bindings)
    rule <- static_lookup(index$registration_rules, callee$function_key)
    if (is.null(rule)) return(bindings)
    args <- as.list(expr)[-1L]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    values <- list()
    positional <- 1L
    for (i in seq_along(args)) {
        name <- nms[[i]]
        if (!nzchar(name)) {
            if (positional > length(rule$formals)) return(bindings)
            name <- rule$formals[[positional]]
            positional <- positional + 1L
        }
        values[name] <- list(static_infer(args[[i]], index, bindings))
    }
    name <- values[[rule$name_arg]]
    factory <- values[[rule$value_arg]]
    if (!isTRUE(name$known_literal) || !is.character(name$literal) || length(name$literal) != 1L || is.null(factory)) return(bindings)
    for (type in rule$owners) {
        env <- bindings
        env$.__factory__ <- factory
        env$.__input__ <- static_value(type = type)
        value <- static_infer(quote(.__factory__(.__input__)), index, env)
        if (length(value$type)) bindings$.__member_properties__[[type]][name$literal] <- list(value)
    }
    bindings
}
