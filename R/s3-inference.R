s3_constructor_classes <- list(
    "base::factor" = "factor",
    "base::ordered" = c("ordered", "factor"),
    "base::as.Date" = "Date",
    "base::as.POSIXct" = c("POSIXct", "POSIXt"),
    "base::as.POSIXlt" = c("POSIXlt", "POSIXt"),
    "base::as.difftime" = "difftime",
    "stats::ts" = "ts",
    "base::table" = "table",
    "base::as.table" = "table",
    "base::as.data.frame" = "data.frame",
    "base::as.numeric" = c("double", "numeric"),
    "base::as.double" = c("double", "numeric"),
    "base::as.integer" = c("integer", "numeric"),
    "base::as.character" = "character",
    "base::as.logical" = "logical",
    "base::as.raw" = "raw",
    "base::seq_len" = c("integer", "numeric"),
    "base::seq_along" = c("integer", "numeric"),
    "tibble::tibble" = c("tbl_df", "tbl", "data.frame"),
    "tibble::as_tibble" = c("tbl_df", "tbl", "data.frame"),
    "data.table::data.table" = c("data.table", "data.frame"),
    "data.table::as.data.table" = c("data.table", "data.frame"),
    "base::read.csv" = "data.frame",
    "utils::read.csv" = "data.frame"
)

s3_model_classes <- list(
    "stats::aov" = c("aov", "lm"),
    "stats::loess" = "loess",
    "stats::smooth.spline" = "smooth.spline",
    "stats::nls" = "nls",
    "stats::prcomp" = "prcomp",
    "stats::princomp" = "princomp",
    "stats::hclust" = "hclust",
    "stats::kmeans" = "kmeans",
    "stats::t.test" = "htest",
    "stats::wilcox.test" = "htest",
    "stats::cor.test" = "htest",
    "stats::chisq.test" = "htest",
    "MASS::rlm" = c("rlm", "lm"),
    "MASS::glm.nb" = c("negbin", "glm", "lm"),
    "MASS::polr" = "polr",
    "survival::coxph" = "coxph",
    "survival::survfit" = "survfit",
    "rpart::rpart" = "rpart",
    "nlme::lme" = "lme"
)

# Class-only S3 analysis shares syntax, argument and source-index helpers with
# member inference. Values are inert; document code and defaults are not run.
s3_unknown <- function(reason = "unsupported") list(classes = NULL, type = NULL, reason = reason)

s3_binding_cacheable <- function(value, depth = 0L) {
    if (is.null(value) || depth > 12L || isTRUE(value$reason %in% c("budget", "binding_cycle")) ||
            !is.null(value$fn) || !is.null(value$closure) || !is.null(value$info) ||
            (isTRUE(value$known) && (!is.atomic(value$literal) || is.object(value$literal)))) return(FALSE)
    all(vapply(value$fields, s3_binding_cacheable, logical(1L), depth + 1L))
}
s3_value <- function(
    classes, type = NULL, literal = NULL, known = FALSE, fields = NULL,
    fn = NULL, closure = NULL, origin = NULL, function_name = NULL, info = NULL, dimensions = NULL,
    explicit_class = NULL
) {
    if (missing(explicit_class) && !is.null(classes) && !is.null(type)) {
        explicit_class <- !identical(classes, s3_implicit_classes(type, dimensions))
    }
    list(
        classes = classes, type = type, literal = literal, known = known, fields = fields,
        fn = fn, closure = closure, origin = origin, function_name = function_name,
        info = info, dimensions = dimensions, explicit_class = explicit_class
    )
}
`%s3_or%` <- function(a, b) if (is.null(a)) b else a
# Branches overlay the enclosing local frame. Reflection can invalidate all
# bindings in those overlays, but must not write through a function closure.
s3_invalidate_locals <- function(scope, reason, symbols = NULL) {
    frame <- scope
    repeat {
        keys <- if (is.null(symbols)) {
            setdiff(ls(frame, all.names = TRUE), c(".registrations", ".dynamic_registration"))
        } else {
            symbols[vapply(symbols, exists, logical(1L), envir = frame, inherits = FALSE)]
        }
        for (key in keys) assign(key, s3_unknown(reason), scope)
        if (!isTRUE(attr(frame, "s3_branch", exact = TRUE))) break
        frame <- parent.env(frame)
    }
}
s3_call_name <- function(expr) {
    if (is.symbol(expr)) {
        return(list(name = as.character(expr), package = NULL))
    }
    if (member_head(expr, "::") || member_head(expr, ":::")) {
        return(list(
            name = member_name(expr[[3L]]), package = member_name(expr[[2L]]),
            internal = member_head(expr, ":::")
        ))
    }
    NULL
}
s3_implicit_classes <- function(type, dimensions = NULL) {
    storage <- switch(type,
        double = c("double", "numeric"),
        integer = c("integer", "numeric"),
        type
    )
    if (!is.null(dimensions)) c(if (length(dimensions) == 2L) c("matrix", "array") else "array", storage) else storage
}

# Several ordinary constructors return a fixed, ordered S3 class vector.
# Index those vectors by the class and its owner once, including glm's
# standard subclass of lm.
s3_hint_ancestry <- local({
    known <- c(s3_constructor_classes, s3_model_classes, list("stats::glm" = c("glm", "lm")))
    keys <- vapply(known, `[[`, "", 1L)
    qualified <- paste(sub("::.*", "", names(known)), keys, sep = "::")
    paths <- c(stats::setNames(known, qualified), stats::setNames(known, keys))
    function(name, package) paths[[if (is.null(package)) name else paste(package, name, sep = "::")]]
})

# Keep each possible receiver's dispatch order separate. A union of classes
# is not an S3 inheritance chain.
s3_type_hint <- function(types, context, at = context$at, scope = NULL, source = NULL) {
    if (!roxygen_type_hints_enabled()) return(s3_unknown("unknown_type_hint"))
    document <- context$document
    if (!length(types) || length(types) > max_type_hint_alternatives) return(s3_unknown("unknown_type_hint"))
    if (is.null(source)) source <- context$hint_source
    if (!is.null(source$uri) && !identical(source$uri, document$uri) && !is.null(context$workspace$documents)) {
        owner <- context$workspace$documents$get(source$uri, NULL)
        if (!is.null(owner) && identical(owner$version, owner$parse_data$version)) {
            document <- owner
            at <- source$start
        }
    }
    if (!is.null(context$hint_wait) && s3_hint_worker_waiting(context$workspace, document, context$hint_wait$manager, types)) {
        context$hint_wait$owners[[document$uri]] <- union(context$hint_wait$owners[[document$uri]], types)
    }
    index <- member_context_index(context$workspace, document$uri, document, at,
        referenced = member_type_dependencies(types))
    budget <- member_request_budget()
    bindings <- list(.__member_position__ = at)
    paths <- lapply(types, function(type) {
        local <- if (is.environment(scope) && !grepl("::", type, fixed = TRUE)) get0(type, scope, inherits = TRUE)
        if (!is.null(local)) {
            generator <- member_local_value(local$hint_generator, index, budget)
            if (!identical(generator$r6_role, "generator")) return(NULL)
            return(member_hint_dispatch_classes(generator$fields$new$result_shape, index))
        }
        value <- member_hinted_value(type, index, bindings, budget)
        member_hint_dispatch_classes(value, index)
    })
    if (isTRUE(budget$exhausted)) context$exhausted <- TRUE
    paths <- unique(unname(paths))
    if (!length(paths) || any(!lengths(paths))) return(s3_unknown("unknown_type_hint"))
    if (length(paths) == 1L) return(s3_value(paths[[1L]]))
    value <- s3_unknown("union_type_hint")
    value$class_options <- paths
    value
}

# Only an argument whose class is queried needs the member index. Capture
# its lexical position before another source binding temporarily changes it.
s3_bind_hinted_formal <- function(name, types, scope, context) {
    force(types)
    at <- context$at
    enclosing <- parent.env(scope)
    delayedAssign(name, s3_type_hint(types, context, at, enclosing), assign.env = scope)
}

# R memoizes the argument promise. Multiple unknown exits can share a
# single lookup, and proven exits never request a class catalog.
s3_lazy_return_hint <- function(value) function() value

s3_apply_type_hint <- function(value, hint) {
    if (length(value$classes) || length(value$class_options) || !is.null(value$fn) ||
            isTRUE(value$reason %in% c("budget", "ambiguous_classes", "conditional_return", "argument_match", "missing")) || is.null(hint)) {
        return(value)
    }
    if (is.function(hint)) hint <- hint()
    if (isTRUE(value$returned)) hint$returned <- TRUE
    hint
}

s3_class_join <- function(a, b) {
    if (identical(a, b)) {
        return(a)
    }
    if (!is.list(a) || !is.list(b)) {
        return(s3_unknown("ambiguous_binding"))
    }
    if (is.null(a$classes) || is.null(b$classes) || !identical(a$classes, b$classes)) {
        out <- s3_unknown("ambiguous_classes")
        choices <- function(value) {
            if (length(value$classes)) list(value$classes) else value$class_options
        }
        left <- choices(a)
        right <- choices(b)
        if (length(left) && length(right)) {
            paths <- unique(c(left, right))
            if (length(paths) <= max_type_hint_alternatives) out$class_options <- paths
        }
        return(out)
    }
    s3_value(a$classes, type = if (identical(a$type, b$type)) a$type else NULL,
        dimensions = if (identical(a$dimensions, b$dimensions)) a$dimensions else NULL,
        explicit_class = if (identical(a$explicit_class, b$explicit_class)) a$explicit_class else NULL)
}
s3_match_values <- function(formals, actuals, env, infer, parameter_hint = NULL) {
    keys <- names(formals)
    matched <- s3_match_arguments(formals, actuals)
    if (is.null(matched)) return(NULL)
    dot <- match("...", keys)
    bind <- function(name, value) {
        if (is.null(parameter_hint)) {
            assign(name, value, env)
        } else {
            # Capture the current argument. Only a read of this formal should
            # consult its documented class or wait for package metadata.
            force(name)
            force(value)
            delayedAssign(name, parameter_hint(name, value), assign.env = env)
        }
    }
    ordinary <- which(keys[matched] != "...")
    for (i in ordinary) bind(keys[[matched[[i]]]], actuals[[i]])
    supplied <- ordinary[!vapply(actuals[ordinary], function(x) identical(x$reason, "missing"), logical(1L))]
    used <- keys[matched[supplied]]
    if (!is.na(dot)) assign("...", s3_value("list", type = "list", fields = actuals[matched == dot]), env)
    defaults <- new.env(parent = emptyenv())
    defaults$pending <- setdiff(keys, c(used, "..."))
    for (k in defaults$pending) assign(k, s3_unknown("missing"), env)
    resolve_default <- function(name, trail = character()) {
        if (!name %in% defaults$pending || name %in% trail) return(invisible(NULL))
        expression <- formals[name]
        if (!identical(expression[[1L]], quote(expr = ))) {
            dependencies <- intersect(member_syntax_names(expression[[1L]]), defaults$pending)
            for (dependency in dependencies) resolve_default(dependency, c(trail, name))
            bind(name, infer(expression[[1L]], env))
        }
        defaults$pending <- setdiff(defaults$pending, name)
        invisible(NULL)
    }
    for (k in defaults$pending) resolve_default(k)
    env
}
s3_analyzer <- function(context, env = new.env(parent = emptyenv())) {
    if (!exists(".registrations", env, inherits = FALSE)) assign(".registrations", list(), env)

    get_value <- function(name, scope, source_only = FALSE) s3_get_value(name, scope, context, source_only)
    namespace_function <- function(name, package = NULL) s3_namespace_function(name, package, context)
    package_object <- function(name, package = NULL) s3_package_object(name, package, context)
    lookup_method <- function(generic, classes, scope) s3_lookup_method(generic, classes, scope, context, "source")
    reflective_calls <- c("<<-", "->>", "eval", "evalq", "assign", "source", "sys.source")
    context$nodes <- 0L
    infer <- function(expr, scope = env, depth = 0L, return_hint = NULL) {
        context$nodes <- context$nodes + 1L
        if (context$nodes > 20000L || depth > 64L || proc.time()[[3L]] > context$deadline) {
            context$exhausted <- TRUE
            return(s3_unknown("budget"))
        }
        recur <- function(x, e = scope, hint = NULL) infer(x, e, depth + 1L, hint)
        if (identical(expr, quote(expr = ))) {
            return(s3_unknown("missing"))
        }
        if (is.null(expr) || is.atomic(expr)) {
            return(s3_value(.class2(expr), typeof(expr), expr, TRUE))
        }
        if (is.symbol(expr)) {
            v <- get_value(as.character(expr), scope)
            if (!is.null(v)) {
                return(v)
            }
            return(package_object(as.character(expr)))
        }
        if (!is.call(expr)) {
            return(s3_unknown())
        }
        head <- s3_call_name(expr[[1]])
        op <- if (is.symbol(expr[[1]])) as.character(expr[[1]]) else ""
        if (!is.null(context$effect_probe) && isTRUE(head$name %in% reflective_calls)) {
            context$effect_probe$reached <- TRUE
            return(s3_unknown("factory_effect"))
        }
        if (op == "function") {
            return(s3_value(NULL, fn = expr, closure = scope))
        }
        if (identical(head$name, "R6Class") && !identical(scope, context$root) &&
                (isTRUE(context$document$parse_data$member_data$has_type_hints) || !is.null(context$hint_source)) &&
                (identical(head$package, "R6") || (is.null(head$package) && identical(namespace_function(head$name)$owner, "R6") &&
                            is.null(get_value("R6Class", scope, source_only = TRUE))))) {
            # Resolve its public instance only if a later hint names this
            # generator. Capture earlier locals at the declaration just as the
            # member analyzer does, including a shadowed or reassigned parent.
            names <- member_syntax_names(expr)
            locals <- mget(names, scope, inherits = TRUE, ifnotfound = list(NULL))
            bindings <- lapply(Filter(Negate(is.null), locals), function(value) {
                if (!is.null(value$hint_generator)) return(value$hint_generator)
                if (isTRUE(value$known)) member_literal(value$literal) else member_value(reason = "unknown_local")
            })
            bindings$.__member_position__ <- context$hint_source$start %s3_or% context$at
            value <- s3_unknown("r6_generator")
            value$hint_generator <- member_local_binding(expr, bindings, names)
            return(value)
        }
        if (op == "{") {
            out <- s3_value("NULL", "NULL", NULL, TRUE)
            for (i in seq_along(expr)[-1L]) {
                out <- recur(expr[[i]], hint = if (i == length(expr)) return_hint)
                if (isTRUE(out$returned)) {
                    return(out)
                }
            }
            return(out)
        }
        if (op == "return") {
            out <- if (length(expr) > 1) recur(expr[[2]], hint = return_hint) else s3_value("NULL", "NULL", NULL, TRUE)
            out$returned <- TRUE
            return(out)
        }
        if (op %in% c("<-", "=")) {
            v <- recur(expr[[3]])
            lhs <- expr[[2]]
            if (is.symbol(lhs)) {
                assign(as.character(lhs), v, scope)
            } else if (is.call(lhs) && identical(lhs[[1]], as.name("class")) && is.symbol(lhs[[2]])) {
                if (!is.null(get_value("class<-", scope, source_only = TRUE))) {
                    assign(as.character(lhs[[2]]), s3_unknown("shadowed_class_assignment"), scope)
                    return(v)
                }
                old <- get_value(as.character(lhs[[2]]), scope)
                if (isTRUE(v$known) && is.character(v$literal) && length(v$literal)) {
                    old$classes <- v$literal
                    old$class_options <- NULL
                    old$explicit_class <- TRUE
                    old$known <- FALSE
                    assign(as.character(lhs[[2]]), old, scope)
                } else if (isTRUE(v$known) && is.null(v$literal) && !is.null(old$type)) {
                    old$classes <- s3_implicit_classes(old$type, old$dimensions)
                    old$class_options <- NULL
                    old$explicit_class <- FALSE
                    assign(as.character(lhs[[2]]), old, scope)
                } else {
                    assign(as.character(lhs[[2]]), s3_unknown("dynamic_class"), scope)
                }
            } else if (is.call(lhs) && length(lhs) > 1L && is.symbol(lhs[[2]])) {
                assign(as.character(lhs[[2]]), s3_unknown("unmodeled_mutation"), scope)
            }
            return(v)
        }
        if (op == "if") {
            cond <- recur(expr[[2]])
            if (isTRUE(cond$known) && identical(cond$literal, TRUE)) {
                return(recur(expr[[3]], hint = return_hint))
            }
            if (isTRUE(cond$known) && isFALSE(cond$literal)) {
                return(if (length(expr) > 3L) recur(expr[[4]], hint = return_hint) else s3_value("NULL", "NULL"))
            }
            # Each branch records its own writes. Inheriting the original
            # scope keeps unrelated arguments lazy and out of the join.
            a_scope <- new.env(parent = scope)
            b_scope <- new.env(parent = scope)
            attr(a_scope, "s3_branch") <- attr(b_scope, "s3_branch") <- TRUE
            a <- recur(expr[[3]], a_scope, return_hint)
            b <- if (length(expr) > 3L) recur(expr[[4]], b_scope, return_hint) else s3_value("NULL", "NULL")
            if (!is.null(return_hint)) {
                a <- s3_apply_type_hint(a, return_hint)
                b <- s3_apply_type_hint(b, return_hint)
            }
            for (k in union(ls(a_scope, all.names = TRUE), ls(b_scope, all.names = TRUE))) {
                assign(
                    k,
                    s3_class_join(get_value(k, a_scope), get_value(k, b_scope)), scope
                )
            }
            out <- if (xor(isTRUE(a$returned), isTRUE(b$returned))) {
                s3_unknown("conditional_return")
            } else {
                s3_class_join(a, b)
            }
            if (isTRUE(a$returned) || isTRUE(b$returned)) out$returned <- TRUE
            return(out)
        }
        if (op %in% c("for", "while", "repeat")) {
            for (k in member_assigned_names(expr)) assign(k, s3_unknown("loop_mutation"), scope)
            return(s3_value("NULL", "NULL"))
        }
        if (op %in% c("::", ":::")) {
            ref <- s3_call_name(expr)
            fn <- namespace_function(ref$name, ref$package)
            if (!is.null(fn)) {
                return(s3_value(NULL,
                        fn = as.call(list(as.name("function"), fn$formals, NULL)), origin = fn$owner,
                        function_name = sub("^[^:]*::", "", fn$summary %s3_or% ref$name), info = fn
                    ))
            }
            return(package_object(ref$name, ref$package))
        }
        if (op %in% c("|>", "%>%", "%T>%", "%<>%")) {
            if (!is.null(get_value(op, scope, source_only = TRUE))) {
                return(s3_unknown("shadowed_pipe"))
            }
            lhs <- expr[[2]]
            rhs <- expr[[3]]
            if (is.symbol(rhs)) rhs <- as.call(list(rhs))
            rhs <- s3_pipe_call(lhs, rhs)
            if (is.null(rhs)) {
                return(s3_unknown("pipe"))
            }
            out <- recur(rhs)
            if (op == "%T>%") out <- recur(lhs)
            if (op == "%<>%" && is.symbol(lhs)) assign(as.character(lhs), out, scope)
            return(out)
        }
        if (op == "$") {
            x <- recur(expr[[2]])
            method <- s3_lookup_method("$", x$classes, scope, context, "base")
            if (!is.null(method) && (!is.null(method$fn) || !method$owner %in% c("base", "tibble"))) {
                return(s3_unknown("custom_field_dispatch"))
            }
            return(x$fields[[as.character(expr[[3]])]] %s3_or% s3_unknown("field"))
        }
        if (op %in% c("[", "[[")) {
            x <- recur(expr[[2]])
            args <- as.list(expr)[-c(1, 2)]
            if (is.null(x$classes)) {
                return(s3_unknown("subset_receiver"))
            }
            # Analyze a local custom subset method, rather than assuming preservation.
            if (length(x$classes) && !all(x$classes %in% c(
                "integer", "double", "numeric", "character", "logical",
                "complex", "raw", "list", "data.frame", "tbl_df", "tbl", "factor", "ordered"
            ))) {
                method <- lookup_method(op, x$classes, scope)
                if (!is.null(method$fn)) {
                    return(apply_local(method$fn, c(list(x), lapply(args, recur)), method$closure, depth + 1L, method$source$implementation))
                }
                return(s3_unknown("custom_subset"))
            }
            if (op == "[[" && length(args) == 1L) {
                sel <- recur(args[[1]])
                if (isTRUE(sel$known) && length(sel$literal) == 1L && !is.null(x$fields)) {
                    return(x$fields[[sel$literal]] %s3_or% s3_unknown("subset_field"))
                }
                return(s3_unknown("element"))
            }
            if ("data.frame" %in% x$classes) {
                if ("tbl_df" %in% x$classes) {
                    return(x)
                }
                if (length(args) == 1L) {
                    return(x)
                }
                drop <- args[["drop"]]
                sel <- if (length(args) > 1L && !identical(
                    args[[2]],
                    quote(expr = )
                )) {
                    recur(args[[2]])
                } else {
                    s3_unknown()
                }
                if (isFALSE(drop)) {
                    return(x)
                }
                if (isTRUE(sel$known) && length(sel$literal) > 1L) {
                    return(x)
                }
                if (isTRUE(sel$known) && length(sel$literal) == 1L && !is.null(x$fields)) {
                    return(x$fields[[sel$literal]] %s3_or% s3_unknown("column"))
                }
                return(s3_unknown("drop_dependent"))
            }
            if (op == "[") {
                return(s3_value(x$classes, x$type))
            }
            return(s3_unknown("element"))
        }
        if (op %in% c("library", "require")) {
            context$imports <- unique(c(context$imports, member_name(expr[[2]])))
            return(s3_value("logical", "logical"))
        }
        if (op == "data") {
            return(s3_value("NULL", "NULL"))
        }
        if (!is.null(head) && head$name == "registerS3method") {
            raw <- as.list(expr)[-1L]
            matched <- s3_match_arguments(as.pairlist(alist(genname = , class = , method = , envir = )), raw)
            trusted <- if (is.null(head$package)) {
                is.null(get_value("registerS3method", scope, source_only = TRUE))
            } else {
                head$package == "base"
            }
            if (trusted && !is.null(matched) && length(raw) == 3L && all(1:3 %in% matched)) {
                raw <- raw[match(1:3, matched)]
            } else {
                raw <- list()
            }
            if (length(raw) == 3L && is.character(raw[[1L]]) && length(raw[[1L]]) == 1L &&
                    is.character(raw[[2L]]) && length(raw[[2L]]) == 1L) {
                fn <- recur(raw[[3L]])
                site <- context$registration_site
                if (!is.null(site) && identical(site$call, expr)) {
                    fn$registration <- list(uri = site$uri, at = site$at, expr = raw[[3L]])
                }
                generic <- get_value(raw[[1L]], scope)
                owner <- if (!is.null(generic$info)) generic$info$owner else if (!is.null(generic$fn)) "source"
                if (is.null(owner)) {
                    assign(".dynamic_registration", TRUE, scope)
                    return(s3_value("NULL", "NULL"))
                }
                reg <- get_value(".registrations", scope)
                key <- paste(raw[[1L]], raw[[2L]], sep = ".")
                reg[[paste(owner, key, sep = "::")]] <- fn
                assign(".registrations", reg, scope)
            } else {
                assign(".dynamic_registration", TRUE, scope)
            }
            return(s3_value("NULL", "NULL"))
        }
        if (op %in% c("assign", "eval", "source", "sys.source")) {
            s3_invalidate_locals(scope, "reflection")
            return(s3_unknown("reflection"))
        }
        local <- if (!is.null(head) && is.null(head$package)) get_value(head$name, scope, source_only = TRUE) else NULL
        if (!is.null(local) && is.null(local$fn)) {
            return(s3_unknown("shadowed_callable"))
        }
        ns <- if (is.null(local) && !is.null(head)) namespace_function(head$name, head$package) else NULL
        if (!is.null(local$info)) {
            ns <- local$info
            local <- NULL
        }
        name <- if (!is.null(head)) head$name else ""
        package <- if (!is.null(local)) NULL else if (!is.null(ns$summary)) ns$owner else NULL
        if (!is.null(ns$summary)) name <- sub("^[^:]*::", "", ns$summary)
        raw <- as.list(expr)[-1]
        actuals <- lapply(raw, recur)
        if (any(vapply(raw, function(x) is.symbol(x) && identical(member_name(x), "..."), logical(1L)))) {
            expanded <- list()
            for (i in seq_along(raw)) {
                if (is.symbol(raw[[i]]) && identical(member_name(raw[[i]]), "...")) {
                    dots <- get_value("...", scope)
                    if (is.null(dots$fields)) {
                        return(s3_unknown("open_dots"))
                    }
                    expanded <- c(expanded, dots$fields)
                } else {
                    expanded <- c(expanded, actuals[i])
                }
            }
            actuals <- expanded
        }
        if (!is.null(ns$summary)) {
            matched_constructor <- s3_constructor_arguments(ns, raw, actuals, infer, scope)
            if (is.null(matched_constructor)) {
                return(s3_unknown("constructor_arguments"))
            }
            raw <- matched_constructor$raw
            actuals <- matched_constructor$values
        }
        if (!is.null(local$fn)) {
            if (!is.null(local$info$summary)) {
                out <- s3_constructor_summary(local$function_name, local$origin, raw, actuals, scope, recur, context)
                if (!is.null(out)) {
                    return(out)
                }
            }
            if (!is.null(local$info)) {
                return(s3_unknown("opaque_package_function"))
            }
            return(apply_local(local$fn, actuals, local$closure, depth + 1L, local$source$implementation))
        }
        if (identical(package, "base") && name == "structure") {
            out <- actuals[[1]]
            cls <- actuals[["class"]]
            if (!is.null(cls)) {
                if (!isTRUE(cls$known) || !is.character(cls$literal) || !length(cls$literal)) {
                    return(s3_unknown("dynamic_class"))
                }
                out$classes <- cls$literal
                out$class_options <- NULL
                out$explicit_class <- TRUE
                out$known <- FALSE
            }
            # Dimension attributes can change implicit dispatch even without class.
            if (is.null(cls) && !is.null(actuals[["dim"]])) {
                return(s3_unknown("dimension_attribute"))
            }
            return(out)
        }
        if (identical(package, "base") && name == "list") {
            return(s3_value("list", "list", fields = actuals))
        }
        if (identical(package, "base") && name == "data.frame") {
            fields <- actuals[!names(actuals) %in% c(
                "row.names", "check.rows", "check.names", "fix.empty.names",
                "stringsAsFactors"
            )]
            if (any(vapply(fields, function(x) {
                is.null(x$classes) || x$type %in% c("list") ||
                    any(x$classes %in% c("matrix", "array", "data.frame"))
            }, logical(1L)))) {
                fields <- NULL
            }
            strings <- actuals[["stringsAsFactors"]]
            if (!is.null(fields)) {
                for (field in names(fields)) {
                    if (identical(fields[[field]]$type, "character")) {
                        if (!is.null(strings) && isTRUE(strings$known) && identical(strings$literal, TRUE)) {
                            fields[[field]] <- s3_value("factor", "integer")
                        } else if (!is.null(strings) && !isTRUE(strings$known)) {
                            fields[[field]] <- s3_unknown("unknown_strings_as_factors")
                        }
                    }
                }
            }
            return(s3_value("data.frame", "list", fields = fields))
        }
        if (identical(package, "base") && name == "c") {
            if (all(vapply(actuals, function(x) isTRUE(x$known) && is.null(attr(x$literal, "class")), logical(1)))) {
                lit <- do.call(base::c, lapply(actuals, `[[`, "literal"))
                return(s3_value(.class2(lit), typeof(lit), lit, TRUE))
            }
            return(s3_unknown("c_dispatch"))
        }
        if (identical(package, "base") && name == ":") {
            if (all(vapply(actuals, function(x) isTRUE(x$known), logical(1)))) {
                a <- actuals[[1]]$literal
                b <- actuals[[2]]$literal
                if (length(a) != 1L || length(b) != 1L || !is.finite(a) || !is.finite(b)) {
                    return(s3_unknown("range"))
                }
                if (abs(a - b) > 256L) {
                    return(s3_value(if (a == floor(a) && abs(a) <= .Machine$integer.max &&
                            abs(b) <= .Machine$integer.max) {
                        c("integer", "numeric")
                    } else {
                        c("double", "numeric")
                    }))
                }
                lit <- a:b
                return(s3_value(.class2(lit), typeof(lit), lit, TRUE))
            }
            return(s3_unknown("range_type"))
        }
        if (identical(package, "base") && name %in% c("identity", "invisible")) {
            return(actuals[[1]])
        }
        if (op %in% c("+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!=")) {
            if (all(vapply(actuals, function(x) {
                isTRUE(x$known) && is.numeric(x$literal) &&
                    length(x$literal) == 1L
            }, logical(1)))) {
                a <- actuals[[1]]$literal
                b <- if (length(actuals) > 1L) actuals[[2]]$literal else NULL
                lit <- switch(op,
                    "+" = if (is.null(b)) a else a + b,
                    "-" = if (is.null(b)) -a else a - b,
                    "*" = a * b,
                    "/" = a / b,
                    ">" = a > b,
                    "<" = a < b,
                    ">=" = a >= b,
                    "<=" = a <= b,
                    "==" = a == b,
                    "!=" = a != b
                )
                return(s3_value(.class2(lit), typeof(lit), lit, TRUE))
            }
            return(s3_unknown("arithmetic_dispatch"))
        }
        out <- s3_constructor_summary(name, package, raw, actuals, scope, recur, context)
        if (!is.null(out)) {
            return(out)
        }
        # Unknown calls can change referenced objects; discard their class evidence.
        symbols <- unique(unlist(lapply(raw, member_syntax_names), use.names = FALSE))
        if (length(symbols)) s3_invalidate_locals(scope, "opaque_call_effect", symbols)
        s3_unknown(paste0("no_summary:", package, "::", name))
    }
    apply_local <- function(fn, actuals, closure, depth, source = NULL) {
        hints <- roxygen_inference_hints(fn)
        if (!is.null(source$uri) && !is.null(attr(fn, "member_type_names", exact = TRUE))) {
            previous_source <- context$hint_source
            context$hint_source <- source
            on.exit(context$hint_source <- previous_source, add = TRUE)
        }
        opaque <- any(member_syntax_names(fn[[3L]]) %in% reflective_calls)
        if (opaque && !length(hints$returns) && is.null(context$effect_probe)) {
            return(s3_unknown("factory_effect"))
        }
        scope <- new.env(parent = if (is.environment(closure)) closure else env)
        parameter_hint <- if (length(hints$params)) {
            hint_at <- context$at
            hint_source <- source %s3_or% context$hint_source
            function(name, value) {
                types <- hints$params[[name]]
                if (!length(types)) return(value)
                s3_apply_type_hint(value, s3_type_hint(types, context, at = hint_at, scope = closure, source = hint_source))
            }
        }
        scope <- s3_match_values(fn[[2]], actuals, scope, function(x, e) infer(x, e, depth + 1L), parameter_hint)
        if (is.null(scope)) {
            return(s3_unknown("argument_match"))
        }
        return_hint <- if (length(hints$returns)) s3_lazy_return_hint(s3_type_hint(hints$returns, context, scope = closure, source = source))
        if (opaque) {
            # Only a reached reflective call invalidates the return evidence.
            # The interpreter already prunes known branches and early exits.
            previous_probe <- context$effect_probe
            probe <- new.env(parent = emptyenv())
            context$effect_probe <- probe
            on.exit({
                context$effect_probe <- previous_probe
                if (!is.null(previous_probe) && isTRUE(probe$reached)) previous_probe$reached <- TRUE
            }, add = TRUE)
        }
        descriptor <- s3_generic_descriptor(fn)
        if (!is.null(get_value("UseMethod", scope, source_only = TRUE))) descriptor <- NULL
        if (!is.null(descriptor)) {
            input <- get_value(descriptor$formal, scope)
            method <- s3_receiver_method(descriptor$name, input, scope, context)
            if (is.null(method$fn)) {
                return(s3_apply_type_hint(s3_unknown("method_body"), return_hint))
            }
            out <- apply_local(method$fn, actuals, method$closure, depth + 1L, method$source$implementation)
        } else {
            out <- infer(fn[[3]], scope, depth + 1L, return_hint)
        }
        if (opaque && isTRUE(probe$reached)) return(s3_apply_type_hint(s3_unknown("factory_effect"), return_hint))
        if (!is.null(return_hint)) out <- s3_apply_type_hint(out, return_hint)
        out$returned <- NULL
        out
    }
    context$infer <- infer
    list(infer = infer, apply = apply_local, scope = env)
}
s3_constructor_summary <- function(name, package, raw, actuals, env, recur, context) {
    if (is.null(package)) {
        return(NULL)
    }
    x <- if (length(actuals)) actuals[[1]] else s3_unknown()
    generics <- c(
        "as.numeric", "as.double", "as.integer", "as.character", "as.logical", "as.raw", "as.data.frame",
        "as.Date", "as.POSIXct", "as.POSIXlt",
        "as_tibble", "as.data.table", "as.table", "prcomp", "princomp", "t.test", "wilcox.test", "cor.test", "rep", "mutate", "select", "filter", "arrange", "group_by", "rowwise", "summarise", "left_join"
    )
    if (name %in% generics) {
        if (isTRUE(context$incomplete)) {
            return(s3_unknown("incomplete_constructor_metadata"))
        }
        if (is.null(x$classes)) {
            return(s3_unknown("unknown_constructor_dispatch"))
        }
        dispatch_name <- if (name == "as.numeric") "as.double" else name
        method <- s3_lookup_method(dispatch_name, x$classes, env, context, package)
        if (!is.null(method) && !is.null(method$owner) && method$owner != package) {
            return(s3_unknown("foreign_constructor_method"))
        }
        for (cl in c(x$classes, "default")) {
            override <- s3_get_value(paste(dispatch_name, cl, sep = "."), env, context, source_only = TRUE)
            if (!is.null(override$fn) || !is.null(s3_get_value(".registrations", env, context,
                        source_only = TRUE
                    )[[paste0(package, "::", dispatch_name, ".", cl)]])) {
                return(s3_unknown("constructor_method_override"))
            }
        }
    }
    if (package == "stats" && name %in% c("lm", "aov")) {
        mode <- actuals[["method"]]
        if (!is.null(mode) && isTRUE(mode$known) && identical(mode$literal, "model.frame")) {
            return(s3_value("data.frame", "list"))
        }
        formula <- raw[[1]]
        if ("Error" %in% member_syntax_names(formula)) {
            return(s3_unknown("aov_strata"))
        }
        if (!is.call(formula) || !identical(formula[[1]], as.name("~")) || length(formula) < 3L) {
            return(s3_unknown("unknown_model_formula"))
        }
        response <- formula[[2]]
        if ((member_head(response, "cbind") || member_head(response, "I")) &&
                !is.null(s3_get_value(member_name(response[[1L]]), env, context, source_only = TRUE))) {
            return(s3_unknown("shadowed_formula_helper"))
        }
        if (is.call(response) && identical(response[[1]], as.name("cbind"))) {
            if (name == "lm") {
                return(s3_value(c("mlm", "lm"), "list"))
            }
            return(s3_unknown("multivariate_aov"))
        }
        if (is.symbol(response)) {
            data <- actuals[["data"]]
            column <- data$fields[[as.character(response)]] %s3_or% s3_get_value(as.character(response), env, context)
            if (is.null(column$classes) || any(column$classes %in% c("matrix", "array")) || is.null(column$type)) {
                return(s3_unknown("unknown_response_dimension"))
            }
        } else if (!(is.call(response) && identical(response[[1]], as.name("I")) && is.call(response[[2]]) &&
                    as.character(response[[2]][[1]]) %in% c("==", "!=", "<", ">", "<=", ">="))) {
            return(s3_unknown("unknown_response_dimension"))
        }
    }
    if (package == "stats" && name == "ts") {
        class <- actuals[["class"]]
        if (!is.null(class)) {
            if (isTRUE(class$known) && is.character(class$literal) && length(class$literal)) {
                return(s3_value(class$literal, explicit_class = TRUE))
            }
            return(s3_unknown("dynamic_ts_class"))
        }
        if (!isFALSE(x$explicit_class)) return(s3_unknown("unknown_ts_dimension"))
        if ("matrix" %in% x$classes) {
            columns <- if (length(x$dimensions) == 2L) x$dimensions[[2L]] else NA_real_
            if (is.na(columns) || !is.finite(columns)) return(s3_unknown("unknown_ts_dimension"))
            classes <- if (columns > 1L) c("mts", "ts", "matrix", "array") else "ts"
            return(s3_value(classes, type = x$type, dimensions = x$dimensions, explicit_class = TRUE))
        }
        if (!is.null(x$dimensions)) return(s3_unknown("unknown_ts_dimension"))
    }
    if (package == "base" && name == "array") {
        dims <- actuals[["dim"]]
        if (!isTRUE(dims$known) || is.null(x$classes)) {
            return(s3_unknown("unknown_array_dimension"))
        }
        if (is.null(x$type) || identical(x$type, "NULL") || !isFALSE(x$explicit_class) ||
                !identical(x$classes, s3_implicit_classes(x$type, x$dimensions))) {
            return(s3_unknown("array_coercion"))
        }
        return(s3_value(s3_implicit_classes(x$type, dims$literal), type = x$type, dimensions = dims$literal))
    }
    if (package == "dplyr" && name == "summarise" && "rowwise_df" %in% x$classes) {
        return(s3_unknown("rowwise_groups"))
    }
    if (package == "survival" && name %in% c("survfit", "coxph")) {
        formula <- raw[[1]]
        response <- if (is.call(formula) && length(formula) > 2L) formula[[2]] else NULL
        args <- if (is.call(response)) as.list(response)[-1] else list()
        event <- if (length(args) >= 2L) args[[length(args)]] else NULL
        column <- if (is.symbol(event)) actuals[["data"]]$fields[[as.character(event)]] else NULL
        if (is.null(column$classes) || "factor" %in% column$classes) {
            return(s3_unknown("survival_response_type"))
        }
    }
    fixed <- s3_constructor_classes
    key <- paste(package, name, sep = "::")
    if (key %in% names(fixed)) {
        classes <- fixed[[key]]
        if (key == "base::factor") {
            ordered <- actuals[["ordered"]]
            if (!is.null(ordered) && !isTRUE(ordered$known)) {
                return(s3_unknown("unknown_ordered_factor"))
            }
            if ((!is.null(ordered) && identical(ordered$literal, TRUE)) || (is.null(ordered) &&
                        "ordered" %in% actuals[[1L]]$classes)) {
                classes <- c("ordered", "factor")
            }
        }
        type <- if ("data.frame" %in% classes) {
            "list"
        } else {
            switch(classes[[1L]],
                factor = "integer",
                ordered = "integer",
                Date = "double",
                POSIXct = "double",
                POSIXlt = "list",
                difftime = "double",
                table = "integer",
                double = "double",
                integer = "integer",
                character = "character",
                logical = "logical",
                raw = "raw",
                NULL
            )
        }
        fields <- if (key %in% c("tibble::tibble", "data.table::data.table")) actuals[nzchar(names(actuals))]
        return(s3_value(classes, type = type, fields = fields))
    }
    if (key %in% c("base::matrix", "base::array")) {
        x <- actuals[[1L]]
        if (is.null(x$type) || identical(x$type, "NULL") || !isFALSE(x$explicit_class) ||
                !identical(x$classes, s3_implicit_classes(x$type, x$dimensions))) {
            return(s3_unknown("matrix_coercion"))
        }
        dimensions <- if (name == "matrix") s3_matrix_dimensions(actuals, x) else actuals[["dim"]]$literal
        if (is.null(dimensions)) {
            return(s3_unknown("array_dimension"))
        }
        return(s3_value(s3_implicit_classes(x$type, dimensions), type = x$type, dimensions = dimensions))
    }
    if (key == "base::as.table" && "table" %in% actuals[[1L]]$classes) {
        return(actuals[[1L]])
    }
    if (key == "base::rep") {
        x <- actuals[[1]]
        if (!is.null(x$classes) && all(x$classes %in% c("integer", "double", "numeric", "character", "logical"))) {
            return(s3_value(x$classes, x$type))
        }
        return(s3_unknown("rep_dispatch"))
    }
    if (key == "base::unclass") {
        x <- actuals[[1]]
        if (!is.null(x$type)) {
            return(s3_value(s3_implicit_classes(x$type, x$dimensions), x$type, dimensions = x$dimensions))
        }
        return(s3_unknown("unknown_storage"))
    }
    if (identical(package, "stats") && name %in% c("lm", "glm")) {
        mode <- actuals[["method"]]
        if (!is.null(mode) && (!isTRUE(mode$known) || !is.character(mode$literal))) {
            return(s3_unknown("dynamic_model_method"))
        }
        if (!is.null(mode) && identical(mode$literal, "model.frame")) {
            return(s3_value("data.frame", "list"))
        }
        if (!is.null(mode) && !mode$literal %in% c("qr", "glm.fit")) {
            return(s3_unknown("custom_model_method"))
        }
        cls <- if (name == "glm") c("glm", "lm") else "lm"
        form <- raw[[1]]
        if (name == "lm" && is.call(form) && identical(form[[1]], as.name("~")) && is.call(form[[2]]) &&
                identical(form[[2]][[1]], as.name("cbind"))) {
            cls <- c("mlm", "lm")
        }
        return(s3_value(cls, "list"))
    }
    models <- s3_model_classes
    if (key %in% names(models)) {
        return(s3_value(models[[key]], "list"))
    }
    if (identical(package, "dplyr")) {
        x <- if (length(actuals)) actuals[[1]] else s3_unknown()
        allowed <- list("data.frame", c("tbl_df", "tbl", "data.frame"), c(
            "grouped_df", "tbl_df", "tbl",
            "data.frame"
        ), c("rowwise_df", "tbl_df", "tbl", "data.frame"))
        if (name == "group_by") {
            return(if (any(vapply(
                allowed, function(cls) identical(cls, x$classes),
                logical(1)
            ))) {
                s3_value(c("grouped_df", "tbl_df", "tbl", "data.frame"), "list")
            } else {
                s3_unknown("custom_group_dispatch")
            })
        }
        if (name == "rowwise") {
            return(s3_value(c("rowwise_df", "tbl_df", "tbl", "data.frame"), "list"))
        }
        if (name %in% c("mutate", "select", "filter", "arrange", "bind_rows", "left_join") && any(vapply(
            allowed,
            function(cls) identical(cls, x$classes), logical(1)
        ))) {
            return(s3_value(x$classes, "list"))
        }
        if (name == "summarise" && any(vapply(allowed, function(cls) identical(cls, x$classes), logical(1)))) {
            group <- actuals[[".groups"]]
            if ("grouped_df" %in% x$classes && is.null(group)) {
                return(s3_unknown("groups_default"))
            }
            if (!is.null(group) && isTRUE(group$known) && identical(group$literal, "keep")) {
                return(s3_value(x$classes, "list"))
            }
            if (!is.null(group) && isTRUE(group$known) && identical(group$literal, "drop")) {
                return(s3_value(setdiff(x$classes, "grouped_df"), "list"))
            }
            if (!"grouped_df" %in% x$classes) {
                return(s3_value(x$classes, "list"))
            }
        }
    }
    if (identical(package, "data.table") && name %in% c("setDT", "setDF") && is.symbol(raw[[1]])) {
        cls <- if (name == "setDT") c("data.table", "data.frame") else "data.frame"
        out <- s3_value(cls, "list")
        assign(as.character(raw[[1]]), out, env)
        return(out)
    }
    NULL
}

s3_constructor_arguments <- function(info, raw, actuals, infer, scope) {
    formals <- info$formals
    if (!length(formals)) {
        return(list(raw = raw, values = actuals))
    }
    matched <- s3_match_arguments(formals, raw)
    if (is.null(matched)) {
        return(NULL)
    }
    formal_names <- names(formals)
    supplied <- names(raw) %s3_or% rep("", length(raw))
    names <- supplied
    named <- formal_names[matched] != "..."
    names[named] <- formal_names[matched[named]]
    order <- order(matched, seq_along(matched))
    raw <- raw[order]
    actuals <- actuals[order]
    names(raw) <- names(actuals) <- names[order]
    first <- formal_names[[1L]]
    if (first != "..." && !first %in% names(actuals) && !identical(formals[[1L]], quote(expr = ))) {
        raw <- c(stats::setNames(list(formals[[1L]]), first), raw)
        actuals <- c(stats::setNames(list(infer(formals[[1L]], scope)), first), actuals)
    }
    list(raw = raw, values = actuals)
}

s3_matrix_dimensions <- function(actuals, data) {
    size <- function(value) {
        literal <- value$literal
        if (!isTRUE(value$known) || !is.numeric(literal) || length(literal) != 1L ||
                !is.finite(literal) || literal < 1L || literal != floor(literal)) return(NA_real_)
        literal
    }
    rows <- size(actuals[["nrow"]])
    columns <- size(actuals[["ncol"]])
    data_length <- if (isTRUE(data$known)) length(data$literal) else NA_real_
    if (is.null(actuals[["nrow"]]) && is.null(actuals[["ncol"]])) rows <- 1L
    if (!is.na(data_length)) {
        if (is.null(actuals[["nrow"]]) && !is.na(columns)) rows <- ceiling(data_length / columns)
        if (is.null(actuals[["ncol"]]) && !is.na(rows)) columns <- ceiling(data_length / rows)
    }
    c(rows, columns)
}
