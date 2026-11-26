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
s3_copy_scope <- function(env) list2env(as.list(env, all.names = TRUE), parent = parent.env(env))
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
s3_class_join <- function(a, b) {
    if (identical(a, b)) {
        return(a)
    }
    if (!is.list(a) || !is.list(b)) {
        return(s3_unknown("ambiguous_binding"))
    }
    if (is.null(a$classes) || is.null(b$classes) || !identical(a$classes, b$classes)) {
        return(s3_unknown("ambiguous_classes"))
    }
    s3_value(a$classes, type = if (identical(a$type, b$type)) a$type else NULL,
        dimensions = if (identical(a$dimensions, b$dimensions)) a$dimensions else NULL,
        explicit_class = if (identical(a$explicit_class, b$explicit_class)) a$explicit_class else NULL)
}
s3_match_values <- function(formals, actuals, env, infer) {
    keys <- names(formals)
    matched <- s3_match_arguments(formals, actuals)
    if (is.null(matched)) return(NULL)
    dot <- match("...", keys)
    ordinary <- which(keys[matched] != "...")
    for (i in ordinary) assign(keys[[matched[[i]]]], actuals[[i]], env)
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
            assign(name, infer(expression[[1L]], env), env)
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
    context$nodes <- 0L
    infer <- function(expr, scope = env, depth = 0L) {
        context$nodes <- context$nodes + 1L
        if (context$nodes > 20000L || depth > 64L || proc.time()[[3L]] > context$deadline) {
            context$exhausted <- TRUE
            return(s3_unknown("budget"))
        }
        recur <- function(x, e = scope) infer(x, e, depth + 1L)
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
        if (op == "function") {
            return(s3_value(NULL, fn = expr, closure = scope))
        }
        if (op == "{") {
            out <- s3_value("NULL", "NULL", NULL, TRUE)
            for (child in as.list(expr)[-1]) {
                out <- recur(child)
                if (isTRUE(out$returned)) {
                    return(out)
                }
            }
            return(out)
        }
        if (op == "return") {
            out <- if (length(expr) > 1) recur(expr[[2]]) else s3_value("NULL", "NULL", NULL, TRUE)
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
                    old$explicit_class <- TRUE
                    old$known <- FALSE
                    assign(as.character(lhs[[2]]), old, scope)
                } else if (isTRUE(v$known) && is.null(v$literal) && !is.null(old$type)) {
                    old$classes <- s3_implicit_classes(old$type, old$dimensions)
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
                return(recur(expr[[3]]))
            }
            if (isTRUE(cond$known) && isFALSE(cond$literal)) {
                return(if (length(expr) > 3L) recur(expr[[4]]) else s3_value("NULL", "NULL"))
            }
            a_scope <- s3_copy_scope(scope)
            b_scope <- s3_copy_scope(scope)
            a <- recur(expr[[3]], a_scope)
            b <- if (length(expr) > 3L) recur(expr[[4]], b_scope) else s3_value("NULL", "NULL")
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
                    return(apply_local(method$fn, c(list(x), lapply(args, recur)), method$closure, depth + 1L))
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
                reg <- get_value(".registrations", scope)
                reg[[paste(raw[[1L]], raw[[2L]], sep = ".")]] <- fn
                assign(".registrations", reg, scope)
            } else {
                assign(".dynamic_registration", TRUE, scope)
            }
            return(s3_value("NULL", "NULL"))
        }
        if (op %in% c("assign", "eval", "source", "sys.source")) {
            for (k in setdiff(ls(scope, all.names = TRUE), c(".registrations", ".dynamic_registration"))) {
                assign(k, s3_unknown("reflection"), scope)
            }
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
            return(apply_local(local$fn, actuals, local$closure, depth + 1L))
        }
        if (identical(package, "base") && name == "structure") {
            out <- actuals[[1]]
            cls <- actuals[["class"]]
            if (!is.null(cls)) {
                if (!isTRUE(cls$known) || !is.character(cls$literal) || !length(cls$literal)) {
                    return(s3_unknown("dynamic_class"))
                }
                out$classes <- cls$literal
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
        for (symbol in unique(unlist(lapply(raw, member_syntax_names), use.names = FALSE))) {
            if (exists(symbol,
                    scope,
                    inherits = FALSE
                )) {
                assign(symbol, s3_unknown("opaque_call_effect"), scope)
            }
        }
        s3_unknown(paste0("no_summary:", package, "::", name))
    }
    apply_local <- function(fn, actuals, closure, depth) {
        if (any(member_syntax_names(fn[[3L]]) %in% c("<<-", "->>", "eval", "evalq", "assign", "source", "sys.source"))) {
            return(s3_unknown("factory_effect"))
        }
        scope <- new.env(parent = if (is.environment(closure)) closure else env)
        scope <- s3_match_values(fn[[2]], actuals, scope, function(x, e) infer(x, e, depth + 1L))
        if (is.null(scope)) {
            return(s3_unknown("argument_match"))
        }
        descriptor <- s3_generic_descriptor(fn)
        if (!is.null(get_value("UseMethod", scope, source_only = TRUE))) descriptor <- NULL
        if (!is.null(descriptor)) {
            input <- get_value(descriptor$formal, scope)
            method <- lookup_method(descriptor$name, input$classes, scope)
            if (is.null(method$fn)) {
                return(s3_unknown("method_body"))
            }
            return(apply_local(method$fn, actuals, method$closure, depth + 1L))
        }
        out <- infer(fn[[3]], scope, depth + 1L)
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
                    )[[paste(dispatch_name, cl, sep = ".")]])) {
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
