# Package-independent research prototype; not registered as a provider.
# Package and document expressions are parsed, never sourced or evaluated.

static_head <- function(x, name) {
    !missing(x) && is.call(x) && is.symbol(x[[1L]]) && identical(as.character(x[[1L]]), name)
}

static_name <- function(x) {
    if (missing(x)) return(NULL)
    if (is.symbol(x)) return(as.character(x))
    if (is.character(x) && length(x) == 1L) return(x)
    NULL
}

static_key <- function(x) {
    if (is.symbol(x)) return(as.character(x))
    if (static_head(x, "$") && length(x) == 3L) {
        lhs <- static_key(x[[2L]])
        rhs <- static_name(x[[3L]])
        if (!is.null(lhs) && !is.null(rhs)) return(paste(lhs, rhs, sep = "$"))
    }
    NULL
}

static_walk <- function(x, visit, descend_functions = TRUE) {
    if (missing(x)) return(invisible(NULL))
    visit(x)
    if (is.call(x) || is.expression(x)) {
        if (!descend_functions && static_head(x, "function")) return(invisible(NULL))
        for (child in as.list(x)) static_walk(child, visit, descend_functions)
    }
    invisible(NULL)
}

static_strings <- function(x) {
    if (is.character(x)) return(x)
    if (static_head(x, "c")) {
        args <- as.list(x)[-1L]
        if (all(vapply(args, is.character, logical(1L)))) return(unlist(args))
    }
    character()
}

static_value <- function(type = NULL, function_key = NULL, receiver = NULL,
    fields = NULL, function_expr = NULL, closure = NULL,
    receiver_name = NULL, receiver_value = NULL) {
    list(type = type, function_key = function_key, receiver = receiver,
        fields = fields, function_expr = function_expr, closure = closure,
        receiver_name = receiver_name, receiver_value = receiver_value)
}

static_lookup <- function(x, key) {
    if (is.null(key) || length(key) != 1L || is.na(key)) return(NULL)
    x[[key]]
}

# Unknown absorbs known types: a branch with an unknown result cannot prove a
# return type. Named classes may form a union; members of unions are currently
# withheld until all alternatives can be resolved by the metadata adapter.
static_join <- function(a, b) {
    if (identical(a$type, ".never")) return(b)
    if (identical(b$type, ".never")) return(a)
    if (identical(a, b)) return(a)
    if (length(a$type) && length(b$type) && is.null(a$fields) && is.null(b$fields)) {
        return(static_value(type = sort(unique(c(a$type, b$type)))))
    }
    static_value()
}

static_infer <- function(expr, index, bindings = list(), receiver = NULL,
    depth = 0L, trail = character(), budget = NULL) {
    unknown <- static_value()
    if (missing(expr)) return(unknown)
    if (is.null(budget)) {
        budget <- new.env(parent = emptyenv())
        budget$remaining <- 2000L
    }
    budget$remaining <- budget$remaining - 1L
    if (depth > 48L || budget$remaining < 0L) return(unknown)
    infer <- function(x, env = bindings) {
        static_infer(x, index, env, receiver, depth + 1L, trail, budget)
    }
    summarize <- function(key, self = NULL, aliases = character()) {
        if (is.null(key) || is.na(key) || key %in% c(trail, aliases)) return(unknown)
        cache_key <- paste(key, self, sep = "|")
        if (exists(cache_key, index$cache, inherits = FALSE)) {
            return(get(cache_key, index$cache, inherits = FALSE))
        }
        fn <- index$definitions[[key]]
        if (!static_head(fn, "function")) {
            if (is.symbol(fn)) return(summarize(as.character(fn), self, c(aliases, key)))
            return(unknown)
        }
        # Non-tail returns need control-flow analysis. Reject these functions
        # rather than ignoring paths or searching for any constructor in a body.
        has_return <- FALSE
        static_walk(fn[[3L]], function(node) {
            if (static_head(node, "return")) has_return <<- TRUE
        }, descend_functions = FALSE)
        if (has_return) return(unknown)
        # Generated native instance methods are factories returning a closure.
        body <- fn[[3L]]
        if (static_head(body, "{")) {
            last <- body[[length(body)]]
            if (static_head(last, "function")) body <- last[[3L]]
        }
        result <- static_infer(body, index, index$roots, self,
            depth + 1L, c(trail, key), budget)
        # Only completed summaries can be shared between independent requests.
        if (budget$remaining >= 0L) assign(cache_key, result, index$cache)
        result
    }

    invoke_shape_function <- function(callee, args) {
        fn <- callee$function_expr
        env <- callee$closure
        if (is.null(env)) env <- list()
        if (!is.null(callee$receiver_value)) {
            name <- if (is.null(callee$receiver_name)) "self" else callee$receiver_name
            env[name] <- list(callee$receiver_value)
        }
        # Basic exact named/positional matching. Defaults, dots, partial
        # matching and argument-dependent dispatch are left for formal work.
        formals <- names(fn[[2L]])
        arg_names <- names(args)
        if (is.null(arg_names)) arg_names <- rep("", length(args))
        positional <- 1L
        for (i in seq_along(args)) {
            name <- arg_names[[i]]
            if (!nzchar(name)) {
                if (positional > length(formals)) next
                name <- formals[[positional]]
                positional <- positional + 1L
            }
            if (name %in% formals && name != "...") env[name] <- list(infer(args[[i]]))
        }
        has_return <- FALSE
        static_walk(fn[[3L]], function(node) {
            if (static_head(node, "return")) has_return <<- TRUE
        }, descend_functions = FALSE)
        if (has_return) return(unknown)
        static_infer(fn[[3L]], index, env, NULL, depth + 1L, trail, budget)
    }

    if (is.symbol(expr)) {
        key <- as.character(expr)
        if (key %in% names(bindings)) return(bindings[[key]])
        if (identical(key, "self") && !is.null(receiver)) return(static_value(type = receiver))
        if (key %in% names(index$definitions)) {
            fn <- index$definitions[[key]]
            # Source-defined ordinary functions use the same shape engine.
            if (isTRUE(index$generic) && static_head(fn, "function")) {
                return(static_value(function_expr = fn, closure = bindings))
            }
            return(static_value(function_key = key))
        }
        return(unknown)
    }
    if (!is.call(expr)) return(unknown)
    head <- static_name(expr[[1L]])
    if (!is.null(head) && head %in% index$nonreturning_functions &&
            !head %in% names(bindings) && !head %in% names(index$definitions)) {
        return(static_value(type = ".never"))
    }
    if (static_head(expr, "function")) {
        return(static_value(function_expr = expr, closure = bindings))
    }
    if (static_head(expr, "new.env")) return(static_value(type = "environment", fields = list()))
    if (static_head(expr, "list")) {
        args <- as.list(expr)[-1L]
        named <- names(args)
        if (length(args) && (is.null(named) || any(!nzchar(named)))) return(unknown)
        return(static_value(type = "list", fields = lapply(args, infer)))
    }
    if (static_head(expr, "(")) return(infer(expr[[2L]]))
    if (static_head(expr, "{")) {
        env <- bindings
        result <- unknown
        for (node in as.list(expr)[-1L]) {
            if (static_head(node, "<-") && is.symbol(node[[2L]])) {
                result <- infer(node[[3L]], env)
                env[as.character(node[[2L]])] <- list(result)
            } else if (static_head(node, "<-") && static_head(node[[2L]], "$")) {
                name <- static_name(node[[2L]][[2L]])
                field <- static_name(node[[2L]][[3L]])
                object <- static_lookup(env, name)
                if (!is.null(object$fields) && !is.null(field)) {
                    result <- infer(node[[3L]], env)
                    if (!is.null(result$function_expr)) {
                        # Bind a method's receiver when extracting the method;
                        # avoid cyclic R structures while building a factory.
                        result$closure[name] <- NULL
                        result$receiver_name <- name
                    }
                    object$fields[field] <- list(result)
                    env[name] <- list(object)
                } else {
                    result <- unknown
                }
            } else {
                result <- infer(node, env)
                # Branch/loop assignments and dynamic writes are not modeled.
                # Do not retain earlier local types across these statements.
                if (static_head(node, "if") || static_head(node, "for") ||
                        static_head(node, "while") || static_head(node, "assign")) {
                    env <- lapply(env, function(x) unknown)
                }
            }
        }
        return(result)
    }
    if (static_head(expr, "if")) {
        if (length(expr) != 4L) return(unknown)
        return(static_join(infer(expr[[3L]]), infer(expr[[4L]])))
    }
    if (static_head(expr, "::")) {
        key <- paste(static_name(expr[[2L]]), static_name(expr[[3L]]), sep = "::")
        value <- static_lookup(index$namespace_roots, key)
        return(if (is.null(value)) unknown else value)
    }
    if (static_head(expr, "$")) {
        lhs <- infer(expr[[2L]])
        member <- static_name(expr[[3L]])
        if (is.null(member)) return(unknown)
        if (!is.null(lhs$fields)) {
            value <- static_lookup(lhs$fields, member)
            if (is.null(value)) return(unknown)
            if (!is.null(value$receiver_name)) value$receiver_value <- lhs
            return(value)
        }
        properties <- static_lookup(index$properties, lhs$type)
        if (member %in% names(properties)) {
            return(static_value(type = properties[[member]]))
        }
        raw <- static_lookup(index$raw_fields, lhs$type)
        if (!is.null(raw) && member %in% names(raw)) return(static_value(type = raw[[member]]))
        members <- static_lookup(index$members, lhs$type)
        if (!is.null(members) && member %in% names(members)) {
            if (!is.na(members[[member]])) {
                return(static_value(function_key = members[[member]], receiver = lhs$type))
            }
        }
        # Native wrapper factories and bundle constructors have static names.
        key <- static_key(expr)
        if (!is.null(key) && key %in% names(index$definitions)) return(static_value(function_key = key))
        prefix <- static_lookup(index$native_member_prefixes, lhs$type)
        if (!is.null(prefix)) {
            key <- paste(prefix, member, sep = "_")
            if (key %in% names(index$definitions)) return(static_value(function_key = key))
        }
        return(unknown)
    }
    if (!is.null(head) && head %in% index$wrapper_functions && length(expr) >= 2L) {
        value <- infer(expr[[2L]])
        if (!length(value$type)) return(unknown)
        wrapped <- vapply(value$type, function(type) {
            target <- static_lookup(index$wrapped_types, type)
            if (!is.null(target)) return(target)
            # wrap.default is the identity for an already public object.
            if (type %in% unlist(index$wrapped_types) || type == ".never") return(type)
            NA_character_
        }, character(1L))
        return(if (anyNA(wrapped)) unknown else static_value(type = sort(unique(wrapped))))
    }
    if (!is.null(head) && head %in% names(index$native_types)) {
        return(static_value(type = index$native_types[[head]]))
    }
    callee <- infer(expr[[1L]])
    if (!is.null(callee$function_expr)) {
        return(invoke_shape_function(callee, as.list(expr)[-1L]))
    }
    summarize(callee$function_key, callee$receiver)
}

static_complete <- function(code, index, attached = FALSE, closers = "") {
    # The host supplies delimiter recovery from its existing bracket scanner.
    # Replace only the member being edited, then find its receiver in the AST.
    sentinel <- ".__static_member_completion__"
    if (grepl(sentinel, code, fixed = TRUE)) return(NULL)
    suffix <- regexpr("\\$[[:alnum:]_.]*$", code)
    if (suffix[[1L]] < 0L) return(NULL)
    token <- substring(code, suffix[[1L]] + 1L)
    patched <- paste0(substr(code, 1L, suffix[[1L]]), sentinel, closers)
    parsed <- tryCatch(parse(text = patched, keep.source = FALSE), error = function(e) NULL)
    if (is.null(parsed)) return(NULL)
    bindings <- if (attached) index$roots else list()
    target <- NULL
    for (expr in parsed) {
        static_walk(expr, function(node) {
            if (static_head(node, "$") && identical(static_name(node[[3L]]), sentinel)) {
                target <<- node[[2L]]
            }
        })
        if (!is.null(target)) break
        if (static_head(expr, "library") &&
                identical(static_name(expr[[2L]]), index$package)) bindings <- index$roots
        if (static_head(expr, "<-") && is.symbol(expr[[2L]])) {
            # Store Unknown explicitly so shadowing cannot resurrect pl.
            bindings[as.character(expr[[2L]])] <- list(static_infer(expr[[3L]], index, bindings))
        }
    }
    value <- static_infer(target, index, bindings)
    members <- static_lookup(index$members, value$type)
    if (!is.null(value$fields)) {
        members <- setNames(rep(NA_character_, length(value$fields)), names(value$fields))
    }
    labels <- as.character(names(members))
    labels <- sort(unique(labels[!startsWith(labels, "_") &
        !startsWith(labels, ".") & startsWith(tolower(labels), tolower(token))]))
    list(type = value$type, labels = labels, token = token,
        functions = members[labels], receiver = target)
}

# Ordinary source-defined functions, named lists, and environment factories
# share static_infer()/static_complete() with the r-polars adapter.
static_generic_index <- function(code) {
    index <- new.env(parent = emptyenv())
    index$definitions <- list()
    for (expr in parse(text = code, keep.source = FALSE)) {
        if (static_head(expr, "<-") && is.symbol(expr[[2L]])) {
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
    index$wrapped_types <- list()
    index$native_types <- list()
    index$native_member_prefixes <- list()
    index$wrapper_functions <- character()
    index$nonreturning_functions <- "stop"
    index
}
