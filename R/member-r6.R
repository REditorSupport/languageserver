# Recognize declarative R6 definitions without creating an instance or running
# initialize/active functions. Only explicit package calls or verified imports
# qualify; a document function called R6Class has ordinary function semantics.
member_is_r6_call <- function(expr, index, bindings) {
    if (!is.call(expr)) {
        return(FALSE)
    }
    head <- expr[[1L]]
    if (member_head(head, "::")) {
        return(identical(member_name(head[[2L]]), "R6") &&
                identical(member_name(head[[3L]]), "R6Class"))
    }
    identical(member_name(head), "R6Class") && !"R6Class" %in% names(bindings) &&
        !"R6Class" %in% names(index$document_bindings) &&
        isTRUE(index$r6_attached)
}

member_r6_shape <- function(expr, index, bindings, budget, depth = 0L, trail = character()) {
    infer <- function(expr) {
        member_infer(expr, index, bindings,
            budget = budget, depth = depth + 1L, trail = trail)
    }
    args <- as.list(expr)[-1L]
    classname <- args$classname
    if (is.null(classname) && length(args) && (is.null(names(args)) || !nzchar(names(args)[[1L]]))) {
        classname <- args[[1L]]
    }
    if (!is.character(classname) && !is.null(classname)) {
        classname <- infer(classname)$literal
    }
    class <- list(name = if (is.character(classname) && length(classname) == 1L) classname else NULL,
        package = if (is.null(index$document_bindings)) index$package else "")
    active <- args$active
    inherited <- member_value()
    if (!is.null(args$inherit)) {
        generator <- infer(args$inherit)
        inherited <- generator$fields$new$result_shape
    }
    # A method does not store its `private` and `super` contexts. Each level
    # of the inheritance chain is kept once (`r6_layers`), and the contexts of
    # a method are bound from its receiver when it is accessed.
    layers <- member_r6_all_layers(inherited)
    level <- length(layers) + 1L
    fields <- if (is.null(inherited$fields)) list() else inherited$fields
    bindings_public <- inherited$r6_bindings
    bindings_private <- inherited$r6_private$r6_bindings
    # Private fields are available inside methods, outside the public surface.
    private <- if (is.null(inherited$r6_private$fields)) list() else inherited$r6_private$fields
    declared <- logical()
    declared_private <- character()
    for (section in c("private", "public")) {
        node <- args[[section]]
        if (!member_head(node, "list")) next
        values <- as.list(node)[-1L]
        for (name in names(values)) {
            value <- infer(values[[name]])
            value$r6_owner <- class
            value$r6_member_kind <- paste(section,
                if (!is.null(value$function_expr) || "function" %in% value$type) "method" else "field")
            if (!is.null(value$function_expr)) {
                value$r6_owner_level <- level
                value$receiver_name <- "self"
                value$closure$self <- NULL
                if (section == "public") value$closure$private <- NULL
                value$closure$super <- NULL
                if ("super" %in% member_syntax_names(value$function_expr)) value$r6_uses_super <- TRUE
            }
            if (section == "public") {
                fields[name] <- list(value)
                bindings_public[name] <- if (is.null(value$function_expr)) "data" else "method"
                if (!is.na(name) && nzchar(name)) declared[name] <- member_r6_is_method(value)
            } else {
                private[name] <- list(value)
                bindings_private[name] <- if (is.null(value$function_expr)) "data" else "method"
                if (!is.na(name) && nzchar(name)) declared_private <- union(declared_private, name)
            }
        }
    }
    if (member_head(active, "list")) {
        for (name in names(as.list(active)[-1L])) {
            value <- member_value(reason = "active_property")
            value$r6_owner <- class
            value$r6_member_kind <- "active binding"
            fields[name] <- list(value)
            bindings_public[name] <- "active"
            if (!is.na(name) && nzchar(name)) declared[name] <- TRUE
        }
    }
    # R6 locks instance environments before initialize() runs. Only an explicit
    # FALSE can allow new bindings; an unknown setting remains conservative.
    locked <- is.null(args$lock_objects) ||
        !isFALSE(infer(args$lock_objects)$literal)
    instance <- member_value(type = "environment", fields = fields, open = TRUE,
        r6_private = member_r6_private_value(private, class, locked, bindings_private),
        r6_class = class, r6_role = "instance")
    instance$r6_level <- level
    if (length(layers)) instance$r6_layers <- layers
    if (length(declared)) instance$r6_declared <- declared
    if (length(declared_private)) instance$r6_declared_private <- declared_private
    instance$r6_locked <- locked
    instance$r6_bindings <- bindings_public
    instance$r6_cloneable <- !isFALSE(args$cloneable) &&
        !isFALSE(inherited$r6_cloneable)
    # new() forwards its arguments to the public (possibly inherited)
    # initialize method. Preserve defaults as syntax, without evaluating them.
    unknown_parent <- !is.null(args$inherit) && !identical(args$inherit, quote(NULL)) &&
        is.null(inherited$r6_class)
    initialize <- fields$initialize
    has_initializer <- !is.null(initialize) &&
        (!length(initialize$type) || "function" %in% initialize$type)
    constructor <- if (has_initializer || unknown_parent) quote(function(...) NULL) else quote(function() NULL)
    if (member_head(fields$initialize$function_expr, "function")) {
        constructor[[2L]] <- fields$initialize$function_expr[[2L]]
    }
    generator <- member_value(type = "environment", fields = list(new = member_value(
        type = "function", function_expr = constructor, result_shape = instance,
        r6_class = class, r6_role = "constructor"
    )), r6_class = class, r6_role = "generator")
    generator$fields$new$r6_initialize <- fields$initialize
    generator
}

member_r6_is_method <- function(value) {
    !is.null(value$function_expr) || identical(value$reason, "active_property")
}

member_r6_private_value <- function(fields, class, locked, bindings) {
    value <- member_value(type = "environment", fields = fields, r6_class = class, r6_role = "private")
    value$r6_locked <- locked
    value$r6_bindings <- bindings
    value
}

member_r6_private_layers <- function(private, layers) {
    if (is.null(private)) return(NULL)
    levels <- unlist(lapply(private$fields, function(value) value$r6_owner_level), use.names = FALSE)
    if (length(levels) && length(layers)) {
        private$r6_layers <- layers[seq_len(min(length(layers), max(levels)))]
    } else {
        private$r6_layers <- NULL
    }
    private
}

member_r6_attach_stored_contexts <- function(context, contexts, visited = NULL) {
    if (is.null(context) || identical(context$reason, "cycle")) return(context)
    if (is.null(contexts)) {
        context$r6_contexts <- context$r6_visited <- NULL
        return(context)
    }
    kept <- character()
    queue <- unique(unlist(lapply(context$fields, function(field) unname(field$r6_context_ids)), use.names = FALSE))
    next_idx <- 1L
    while (next_idx <= length(queue)) {
        key <- queue[[next_idx]]
        next_idx <- next_idx + 1L
        if (is.null(key) || !nzchar(key) || key %in% kept || is.null(contexts[[key]])) next
        kept <- c(kept, key)
        for (field in contexts[[key]]$fields) {
            if (length(field$r6_context_ids)) queue <- c(queue, unname(field$r6_context_ids))
        }
    }
    if (length(kept)) {
        context$r6_contexts <- contexts[kept]
        vis <- intersect(visited, kept)
        context$r6_visited <- if (length(vis)) vis else NULL
    } else {
        context$r6_contexts <- context$r6_visited <- NULL
    }
    context
}

# What one class adds to its inheritance chain: the public methods and active
# properties it declares (a public data name is kept as NULL, because it hides
# an ancestor method of that name from `super`) and every private member it
# declares. Read from declarations only, so initialize() effects on an
# instance do not change it.
member_r6_layer <- function(instance) {
    public <- list()
    for (name in names(instance$r6_declared)) {
        if (is.na(name) || !nzchar(name)) next
        public[name] <- list(if (instance$r6_declared[[name]]) instance$fields[[name]])
    }
    private <- instance$r6_private$fields
    private_names <- names(private)
    own <- if (is.null(private_names)) {
        logical(length(private))
    } else {
        !is.na(private_names) & nzchar(private_names) & (private_names %in% instance$r6_declared_private)
    }
    list(public = public, private = if (is.null(private)) list() else private[own],
        r6_class = instance$r6_class, r6_locked = instance$r6_locked)
}

# Every level of a class, root first. An instance keeps its ancestors in
# `r6_layers` and its own level in its fields; a joined receiver has no
# single own level and carries all of them.
member_r6_all_layers <- function(instance) {
    if (is.null(instance$r6_level)) return(instance$r6_layers)
    c(instance$r6_layers, list(member_r6_layer(instance)))
}

# The `super` environment of a method declared at `level`: the methods and
# active properties visible one level below, private methods included, and no
# data fields. Built on demand, so no method stores it.
member_r6_super <- function(layers, level) {
    layers <- layers[seq_len(max(min(length(layers), level - 1L), 0L))]
    fields <- class <- NULL
    if (length(layers)) {
        public <- private <- list()
        for (layer in layers) {
            for (name in names(layer$public)) if (!is.na(name) && nzchar(name)) public[name] <- layer$public[name]
            for (name in names(layer$private)) if (!is.na(name) && nzchar(name)) private[name] <- layer$private[name]
        }
        fields <- Filter(member_r6_is_method, c(public, private))
        class <- layers[[length(layers)]]$r6_class
    }
    super <- member_value(type = "environment", fields = fields, r6_class = class, r6_role = "super")
    sup_levels <- unlist(lapply(fields, function(value) value$r6_owner_level), use.names = FALSE)
    if (length(sup_levels) && length(layers)) {
        super$r6_layers <- layers[seq_len(min(length(layers), max(sup_levels)))]
    }
    super
}

# The private table of the class that declares `level`, as that class's
# R6Class() call described it. A method called on one known instance uses the
# instance's own table instead (see member_access()); this one serves the
# receivers that have none: a join of branches, the `private` and `super`
# bindings seen while initialize() runs, and non-portable bare names.
member_r6_declared_private <- function(source, level) {
    layers <- source$r6_layers
    if (level <= length(layers)) {
        fields <- list()
        bindings <- NULL
        for (layer in layers[seq_len(level)]) {
            for (name in names(layer$private)) {
                if (is.na(name) || !nzchar(name)) next
                fields[name] <- layer$private[name]
                bindings[name] <- if (is.null(layer$private[[name]]$function_expr)) "data" else "method"
            }
        }
        private <- member_r6_private_value(fields, layers[[level]]$r6_class, layers[[level]]$r6_locked, bindings)
    } else if (level == length(layers) + 1L && identical(source$r6_role, "instance")) {
        private <- source$r6_private
    } else {
        private <- NULL
    }
    if (is.null(private)) return(NULL)
    if (any(vapply(private$fields, function(value) !is.null(value$function_expr), logical(1L)))) {
        if (level > 1L && length(layers)) private$r6_layers <- layers[seq_len(min(length(layers), level - 1L))]
        private$r6_declared_level <- level
    }
    private
}

# Bind the contexts of a declared method referencing bare names in a
# non-portable class body. The level fields are dropped: a bound method keeps
# its contexts wherever it is stored afterwards.
member_r6_bind <- function(value, source) {
    level <- value$r6_owner_level
    if (is.null(level) || is.null(value$function_expr)) return(value)
    value$closure$private <- member_r6_declared_private(source, level)
    if (isTRUE(value$r6_uses_super) || level > 1L) value$closure$super <- member_r6_super(source$r6_layers, level)
    value$r6_owner_level <- value$r6_uses_super <- NULL
    value
}

# The levels a value can bind method contexts from, root first.
member_r6_levels <- function(value) {
    if (identical(value$r6_role, "instance")) return(member_r6_all_layers(value))
    if (identical(value$r6_role, "private") && is.null(value$r6_layers) &&
            is.null(value$r6_declared_level) && !is.null(value$r6_self)) {
        # An instance's own private table, reached through that instance.
        return(member_r6_all_layers(value$r6_self))
    }
    value$r6_layers
}

# Two branches share a method only if it would run with the same contexts on
# both. Methods used to embed their contexts, so the comparison of the method
# values covered this; now the levels (or stored-instance context entries) are
# compared. The joined receiver keeps the levels and reachable contexts of the
# first branch, which are the ones every shared method agrees on.
member_r6_comparator <- function(a, b, depth = 0L) {
    layers <- member_r6_levels(a)
    other <- member_r6_levels(b)
    all_same <- identical(layers, other)
    ca_map <- if (is.null(a$r6_contexts)) list() else a$r6_contexts
    cb_map <- if (is.null(b$r6_contexts)) list() else b$r6_contexts
    contexts_identical <- identical(a$r6_contexts, b$r6_contexts) &&
        identical(a$r6_visited, b$r6_visited)
    known <- list()
    known_super <- list()
    tables_a <- NULL
    tables_b <- NULL
    build_tables <- function(src_layers) {
        n <- length(src_layers)
        supers <- vector("list", n + 1L)
        supers[[1L]] <- list(r6_class = NULL, fields = NULL)
        privs <- vector("list", n)
        pub <- prv <- list()
        prv_bnd <- NULL
        for (k in seq_len(n)) {
            layer <- src_layers[[k]]
            for (nm in names(layer$public)) {
                if (!is.na(nm) && nzchar(nm)) pub[nm] <- layer$public[nm]
            }
            for (nm in names(layer$private)) {
                if (!is.na(nm) && nzchar(nm)) {
                    val <- layer$private[[nm]]
                    prv[nm] <- list(val)
                    prv_bnd[nm] <- if (is.null(val$function_expr)) "data" else "method"
                }
            }
            privs[[k]] <- list(fields = prv, r6_class = layer$r6_class,
                r6_locked = layer$r6_locked, r6_bindings = prv_bnd)
            supers[[k + 1L]] <- list(r6_class = layer$r6_class,
                fields = Filter(member_r6_is_method, c(pub, prv)))
        }
        list(supers = supers, privs = privs)
    }
    ensure_tables <- function() {
        if (is.null(tables_a)) {
            tables_a <<- build_tables(layers)
            tables_b <<- build_tables(other)
        }
    }
    private_table <- function(src, tabs, level) {
        if (identical(src$r6_role, "private") && identical(src$r6_declared_level, level)) {
            return(src[c("fields", "r6_class", "r6_locked", "r6_bindings")])
        }
        if (level >= 1L && level <= length(tabs$privs)) return(tabs$privs[[level]])
        if (level == length(tabs$privs) + 1L && identical(src$r6_role, "instance") && !is.null(src$r6_private)) {
            return(src$r6_private[c("fields", "r6_class", "r6_locked", "r6_bindings")])
        }
        list(fields = NULL, r6_class = NULL, r6_locked = NULL, r6_bindings = NULL)
    }
    strip_level <- function(fn) {
        fn$r6_owner_level <- NULL
        fn
    }
    cut_shape <- function(ctx) {
        list(r6_class = ctx$r6_class, r6_role = ctx$r6_role,
            r6_locked = ctx$r6_locked, r6_bindings = ctx$r6_bindings)
    }
    strip_context_ids <- function(fn) {
        fn$r6_context_ids <- NULL
        fn
    }
    stored_method_equiv <- function(ma, mb, owner_a = a, owner_b = b) {
        if (!member_r6_value_equiv(strip_context_ids(ma), strip_context_ids(mb), depth + 1L) ||
                !identical(names(ma$r6_context_ids), names(mb$r6_context_ids))) {
            return(FALSE)
        }
        fwd <- character()
        rev <- character()
        ctx_eq <- function(ka, kb, vis_a, vis_b) {
            ca <- member_lookup(ca_map, ka)
            cb <- member_lookup(cb_map, kb)
            if (is.null(ca) || is.null(cb)) return(identical(ca, cb))
            cut_a <- ka %in% vis_a || identical(ca$reason, "cycle")
            cut_b <- kb %in% vis_b || identical(cb$reason, "cycle")
            if (cut_a != cut_b) return(FALSE)
            if (cut_a) return(identical(cut_shape(ca), cut_shape(cb)))
            mapped <- if (ka %in% names(fwd)) fwd[[ka]] else NULL
            if (!is.null(mapped)) return(identical(mapped, kb))
            if (kb %in% names(rev)) return(FALSE)
            meta <- setdiff(union(names(ca), names(cb)), c("fields", "r6_contexts", "r6_visited"))
            if (!identical(ca[meta], cb[meta]) ||
                    is.null(ca$fields) != is.null(cb$fields) ||
                    length(ca$fields) != length(cb$fields) ||
                    !identical(names(ca$fields), names(cb$fields))) {
                return(FALSE)
            }
            fwd[[ka]] <<- kb
            rev[[kb]] <<- ka
            next_vis_a <- c(vis_a, ka)
            next_vis_b <- c(vis_b, kb)
            for (i in seq_along(ca$fields)) {
                fa <- ca$fields[[i]]
                fb <- cb$fields[[i]]
                if (!is.null(fa$r6_context_ids) || !is.null(fb$r6_context_ids)) {
                    if (!member_r6_value_equiv(strip_context_ids(fa), strip_context_ids(fb), depth + 1L) ||
                            !identical(names(fa$r6_context_ids), names(fb$r6_context_ids))) {
                        return(FALSE)
                    }
                    for (r2 in names(fa$r6_context_ids)) {
                        if (!ctx_eq(fa$r6_context_ids[[r2]], fb$r6_context_ids[[r2]], next_vis_a, next_vis_b)) {
                            return(FALSE)
                        }
                    }
                } else if (!member_r6_value_equiv(fa, fb, depth + 1L)) {
                    return(FALSE)
                }
            }
            TRUE
        }
        for (role in names(ma$r6_context_ids)) {
            ka <- ma$r6_context_ids[[role]]
            kb <- mb$r6_context_ids[[role]]
            if (identical(role, "private") && isTRUE(ma$r6_private_self)) {
                if (!identical(cut_shape(owner_a), cut_shape(owner_b))) return(FALSE)
            } else if (!ctx_eq(ka, kb, owner_a$r6_visited, owner_b$r6_visited)) {
                return(FALSE)
            }
        }
        TRUE
    }
    field_equiv <- function(fa, fb, self_la, self_lb, owner_a = a, owner_b = b) {
        if (!is.null(fa$r6_owner_level) || !is.null(fb$r6_owner_level)) {
            if (is.null(fa$r6_owner_level) || is.null(fb$r6_owner_level)) return(FALSE)
            sa <- strip_level(fa)
            sb <- strip_level(fb)
            if (!identical(sa, sb) && !member_r6_value_equiv(sa, sb, depth + 1L)) {
                return(FALSE)
            }
            own_a <- identical(fa$r6_owner_level, self_la)
            own_b <- identical(fb$r6_owner_level, self_lb)
            if (!identical(own_a, own_b)) return(FALSE)
            if (own_a) return(same_super(fa$r6_owner_level, fb$r6_owner_level))
            return(same(fa$r6_owner_level, fb$r6_owner_level))
        }
        if (length(fa$r6_context_ids) || length(fb$r6_context_ids)) {
            return((contexts_identical && identical(fa, fb)) ||
                    stored_method_equiv(fa, fb, owner_a, owner_b))
        }
        member_r6_value_equiv(fa, fb, depth + 1L)
    }
    same_fields <- function(fa_map, fb_map, self_la, self_lb, owner_a = a, owner_b = b) {
        n_fields <- length(fa_map)
        if (is.null(fa_map) != is.null(fb_map) || n_fields != length(fb_map) ||
                !identical(names(fa_map), names(fb_map))) {
            return(FALSE)
        }
        if (!n_fields) return(TRUE)
        is_method <- logical(n_fields)
        for (i in seq_len(n_fields)) {
            fa <- fa_map[[i]]
            fb <- fb_map[[i]]
            if (!is.null(fa$r6_owner_level) || !is.null(fb$r6_owner_level) ||
                    length(fa$r6_context_ids) || length(fb$r6_context_ids)) {
                is_method[[i]] <- TRUE
            } else if (!member_r6_value_equiv(fa, fb, depth + 1L)) {
                return(FALSE)
            }
        }
        for (i in which(is_method)) {
            if (!field_equiv(fa_map[[i]], fb_map[[i]], self_la, self_lb, owner_a, owner_b)) {
                return(FALSE)
            }
        }
        TRUE
    }
    same_super <- function(la, lb) {
        if (all_same && identical(la, lb)) return(TRUE)
        if (la < 1L || lb < 1L || la - 1L > length(layers) || lb - 1L > length(other)) return(FALSE)
        key <- paste(la, lb, sep = ":")
        if (is.null(known_super[[key]])) {
            ensure_tables()
            super_a <- tables_a$supers[[la]]
            super_b <- tables_b$supers[[lb]]
            known_super[[key]] <<- identical(super_a$r6_class, super_b$r6_class) &&
                same_fields(super_a$fields, super_b$fields, 0L, 0L)
        }
        known_super[[key]]
    }
    same <- function(la, lb) {
        if (all_same && identical(la, lb)) return(TRUE)
        if (la < 1L || lb < 1L || la > length(layers) + 1L || lb > length(other) + 1L) return(FALSE)
        key <- paste(la, lb, sep = ":")
        if (is.null(known[[key]])) {
            ensure_tables()
            priv_a <- private_table(a, tables_a, la)
            priv_b <- private_table(b, tables_b, lb)
            meta <- c("r6_class", "r6_locked", "r6_bindings")
            known[[key]] <<- same_super(la, lb) &&
                identical(priv_a[meta], priv_b[meta]) &&
                same_fields(priv_a$fields, priv_b$fields, la, lb)
        }
        known[[key]]
    }
    list(
        layers = layers, other = other, ca_map = ca_map, contexts_identical = contexts_identical,
        strip_level = strip_level, strip_context_ids = strip_context_ids,
        stored_method_equiv = stored_method_equiv, field_equiv = field_equiv,
        same_fields = same_fields, same_super = same_super, same = same
    )
}

member_r6_equiv_skip <- c(
    "r6_level", "r6_layers", "r6_declared", "r6_declared_private", "r6_declared_level",
    "r6_owner_level", "r6_uses_super", "r6_context_ids", "r6_contexts", "r6_visited",
    "fields", "slots", "elements", "possible_returns", "closure", "binding_env",
    "receiver_value", "element_shape", "result_shape", "r6_private", "r6_super",
    "r6_self", "r6_initialize"
)

member_r6_internal_env_keys <- c(
    ".__member_position__", ".__member_properties__",
    ".__s4_classes__", ".__s7_package__", ".__s7_constructing__"
)

# Bounded structural equivalence modulo internal by-reference R6 bookkeeping
# (`r6_level`, `r6_layers`, `r6_owner_level`, `r6_declared_level`, `r6_contexts`,
# `r6_context_ids`, `r6_visited`), matching `identical()` before R6 scopes were
# moved out of method closures.
member_r6_value_equiv <- function(a, b, depth = 0L, cmp = NULL, seen = NULL) {
    if (identical(a, b)) return(TRUE)
    if (depth > 8L || !is.list(a) || !is.list(b)) return(FALSE)
    if (!is.null(a$r6_owner_level) || !is.null(b$r6_owner_level) ||
            length(a$r6_context_ids) || length(b$r6_context_ids) ||
            !identical(a$type, b$type) || !identical(a$r6_role, b$r6_role) ||
            !identical(a$r6_class, b$r6_class) || !identical(a$function_expr, b$function_expr) ||
            !identical(a$known_literal, b$known_literal) || !identical(a$literal, b$literal)) {
        return(FALSE)
    }
    if (is.null(seen)) {
        seen <- new.env(parent = emptyenv())
        seen$a <- list()
        seen$b <- list()
        seen$cmps <- list()
    } else {
        for (idx in seq_along(seen$a)) {
            if (identical(a, seen$a[[idx]]) && identical(b, seen$b[[idx]])) return(TRUE)
        }
    }
    reuse_cmp <- function(va, vb, next_depth) {
        if (!is.null(va$r6_contexts) || !is.null(vb$r6_contexts)) {
            return(member_r6_comparator(va, vb, next_depth))
        }
        la <- member_r6_levels(va)
        lb <- member_r6_levels(vb)
        na <- length(la)
        nb <- length(lb)
        is_prefix <- function(full, sub, len) {
            if (len > length(full)) return(FALSE)
            for (k in seq_len(len)) {
                if (!identical(full[[k]], sub[[k]])) return(FALSE)
            }
            TRUE
        }
        for (prev in seen$cmps) {
            if (is_prefix(prev$layers, la, na) && is_prefix(prev$other, lb, nb)) {
                return(prev)
            }
        }
        out <- member_r6_comparator(va, vb, next_depth)
        if (na || nb) seen$cmps[[length(seen$cmps) + 1L]] <- out
        out
    }
    record_true <- function() {
        n <- length(seen$a) + 1L
        seen$a[[n]] <- a
        seen$b[[n]] <- b
        TRUE
    }
    meta_equiv <- function(va, vb) {
        if (is.null(va) || is.null(vb)) return(identical(va, vb))
        if (!is.list(va) || !is.list(vb)) return(FALSE)
        ka <- setdiff(names(va), member_r6_equiv_skip)
        kb <- setdiff(names(vb), member_r6_equiv_skip)
        identical(va[ka], vb[kb])
    }
    if (!meta_equiv(a, b)) return(FALSE)
    map_equiv <- function(ma, mb, env_map = FALSE) {
        if (identical(ma, mb)) return(TRUE)
        if (is.null(ma) || is.null(mb) || length(ma) != length(mb) ||
                !identical(names(ma), names(mb))) {
            return(FALSE)
        }
        nms <- names(ma)
        for (i in seq_along(ma)) {
            if (env_map && !is.null(nms) && nms[[i]] %in% member_r6_internal_env_keys) {
                if (!identical(ma[[i]], mb[[i]])) return(FALSE)
            } else if (!member_r6_value_equiv(ma[[i]], mb[[i]], depth + 1L, NULL, seen)) {
                return(FALSE)
            }
        }
        TRUE
    }
    cmp_res <- if (!is.null(a$result_shape) && !is.null(b$result_shape) &&
            !identical(a$result_shape, b$result_shape)) {
        reuse_cmp(a$result_shape, b$result_shape, depth + 1L)
    } else {
        NULL
    }
    if (!member_r6_value_equiv(a$receiver_value, b$receiver_value, depth + 1L, NULL, seen) ||
            !member_r6_value_equiv(a$element_shape, b$element_shape, depth + 1L, NULL, seen) ||
            !member_r6_value_equiv(a$result_shape, b$result_shape, depth + 1L, cmp_res, seen) ||
            !member_r6_value_equiv(a$r6_self, b$r6_self, depth + 1L, NULL, seen)) {
        return(FALSE)
    }
    for (slot in c("slots", "elements", "possible_returns")) {
        if (!map_equiv(a[[slot]], b[[slot]])) return(FALSE)
    }
    for (slot in c("closure", "binding_env")) {
        if (!map_equiv(a[[slot]], b[[slot]], env_map = TRUE)) return(FALSE)
    }
    if (identical(a$r6_role, "instance")) {
        if (is.null(a$r6_level) != is.null(b$r6_level)) return(FALSE)
        if (!meta_equiv(a$r6_private, b$r6_private) ||
                !meta_equiv(a$r6_super, b$r6_super)) {
            return(FALSE)
        }
        if (is.null(cmp)) {
            cmp <- reuse_cmp(a, b, depth)
        } else if (is.null(a$r6_contexts) && is.null(b$r6_contexts) &&
                (length(cmp$layers) || length(cmp$other))) {
            seen$cmps[[length(seen$cmps) + 1L]] <- cmp
        }
        if (!is.null(a$r6_private)) {
            if (!member_r6_value_equiv(a$r6_private$r6_self, b$r6_private$r6_self, depth + 1L, NULL, seen) ||
                    !cmp$same_fields(a$r6_private$fields, b$r6_private$fields, 0L, 0L, a$r6_private, b$r6_private)) {
                return(FALSE)
            }
        }
        if (!is.null(a$r6_super)) {
            if (!member_r6_value_equiv(a$r6_super$r6_self, b$r6_super$r6_self, depth + 1L, NULL, seen) ||
                    !cmp$same_fields(a$r6_super$fields, b$r6_super$fields, 0L, 0L, a$r6_super, b$r6_super)) {
                return(FALSE)
            }
        }
        if (!cmp$same_fields(a$fields, b$fields, 0L, 0L, a, b)) return(FALSE)
        if (!is.null(a$r6_level) && !cmp$same(a$r6_level, b$r6_level)) return(FALSE)
        return(record_true())
    }
    if (any(a$r6_role %in% c("private", "super"))) {
        if (!member_r6_value_equiv(a$r6_private, b$r6_private, depth + 1L, NULL, seen) ||
                !member_r6_value_equiv(a$r6_super, b$r6_super, depth + 1L, NULL, seen)) {
            return(FALSE)
        }
        if (is.null(cmp)) cmp <- reuse_cmp(a, b, depth)
        self_la <- if (is.null(a$r6_declared_level)) 0L else a$r6_declared_level
        self_lb <- if (is.null(b$r6_declared_level)) 0L else b$r6_declared_level
        if (!cmp$same_fields(a$fields, b$fields, self_la, self_lb, a, b)) return(FALSE)
        return(record_true())
    }
    if (!member_r6_value_equiv(a$r6_private, b$r6_private, depth + 1L, NULL, seen) ||
            !member_r6_value_equiv(a$r6_super, b$r6_super, depth + 1L, NULL, seen)) {
        return(FALSE)
    }
    if (!identical(a$r6_initialize, b$r6_initialize)) {
        if (!is.list(a$r6_initialize) || !is.list(b$r6_initialize) ||
                is.null(a$result_shape) || is.null(b$result_shape)) {
            return(FALSE)
        }
        if (is.null(cmp_res)) cmp_res <- member_r6_comparator(a$result_shape, b$result_shape, depth + 1L)
        if (!cmp_res$field_equiv(a$r6_initialize, b$r6_initialize, 0L, 0L)) return(FALSE)
    }
    if (!map_equiv(a$fields, b$fields)) return(FALSE)
    record_true()
}

member_r6_join <- function(value, a, b, cmp = NULL) {
    if (is.null(cmp)) cmp <- member_r6_comparator(a, b, 0L)
    kept_levels <- integer()
    for (name in names(value$fields)) {
        if (is.na(name) || !nzchar(name)) next
        ma <- a$fields[[name]]
        mb <- b$fields[[name]]
        if (is.null(ma$function_expr) || is.null(mb$function_expr)) next
        if (!is.null(ma$r6_owner_level) || !is.null(mb$r6_owner_level)) {
            decl_a <- identical(a$r6_declared_level, ma$r6_owner_level)
            decl_b <- identical(b$r6_declared_level, mb$r6_owner_level)
            if (!is.null(ma$r6_owner_level) && !is.null(mb$r6_owner_level) &&
                    identical(decl_a, decl_b) &&
                    member_r6_value_equiv(cmp$strip_level(ma), cmp$strip_level(mb), 1L) &&
                    if (decl_a) cmp$same_super(ma$r6_owner_level, mb$r6_owner_level) else
                        cmp$same(ma$r6_owner_level, mb$r6_owner_level)) {
                value$fields[name] <- list(ma)
                kept_levels <- c(kept_levels, ma$r6_owner_level)
            } else if (!is.null(value$fields[[name]]$function_expr)) {
                # Join as two different functions.
                da <- cmp$strip_level(ma)
                db <- cmp$strip_level(mb)
                db$function_expr <- NULL
                value$fields[name] <- list(member_join(da, db))
            }
        } else if (length(ma$r6_context_ids) || length(mb$r6_context_ids)) {
            shared <- (cmp$contexts_identical && identical(ma, mb)) ||
                cmp$stored_method_equiv(ma, mb)
            if (shared) {
                value$fields[name] <- list(ma)
            } else {
                da <- cmp$strip_context_ids(ma)
                db <- cmp$strip_context_ids(mb)
                db$function_expr <- NULL
                value$fields[name] <- list(member_join(da, db))
            }
        }
    }
    if (!is.null(a$r6_declared_level) && !is.null(b$r6_declared_level) &&
            any(kept_levels == a$r6_declared_level)) {
        value$r6_declared_level <- a$r6_declared_level
    }
    if (length(kept_levels) && length(cmp$layers)) {
        needed <- if (is.null(value$r6_declared_level)) {
            kept_levels
        } else {
            ifelse(kept_levels == value$r6_declared_level, kept_levels - 1L, kept_levels)
        }
        top <- max(needed)
        if (top >= 1L) value$r6_layers <- cmp$layers[seq_len(min(length(cmp$layers), top))]
    }
    if (length(cmp$ca_map)) {
        value <- member_r6_attach_stored_contexts(value, cmp$ca_map, a$r6_visited)
    }
    value
}

# Members inferred from initialize() or inert runtime fields may not have a
# declaration owner. Use their R6 receiver when available, without changing
# the member's own type or class (a field can itself contain an R6 instance).
member_r6_member_info <- function(value, receiver) {
    if (is.null(receiver$r6_class) || !is.null(value$r6_owner) ||
            !any(receiver$r6_role %in% c("instance", "private", "super"))) return(value)
    value$r6_owner <- receiver$r6_class
    kind <- if (identical(value$reason, "active_property")) {
        "active binding"
    } else {
        paste(if (identical(receiver$r6_role, "private")) "private" else "public",
            if (!is.null(value$function_expr) || "function" %in% value$type) "method" else "field")
    }
    value$r6_member_kind <- kind
    value
}

# Binding kinds come from declarations, not their current values. R6 locks
# methods independently of lock_objects; active writes invoke opaque setters.
# A writable data field remains writable after receiving a function.
member_r6_writable <- function(object, name) {
    !any(member_lookup(object$r6_bindings, name) %in% c("method", "active", "locked")) &&
        (!isTRUE(object$r6_locked) || name %in% names(object$fields))
}

# Member replacement mutates an R6 environment without rebinding its lexical
# reference. Keep its declared surface, but discard values of writable fields
# touched by the statement; replacement expressions and setters stay inert.
# TODO: Propagate changed fields across aliases and private/super r6_self links
# during cursor recovery. The initializer's r6_identity tracking does not apply
# here, so those independent snapshots can still expose earlier field values.
member_r6_local_write <- function(node, name, value, index, budget) {
    is_reference <- function(value) {
        identical(value$type, "environment") && !is.null(value$r6_class) &&
            any(value$r6_role %in% c("instance", "private", "super"))
    }
    if (is.null(value$binding_expr) && !is_reference(value)) return(NULL)
    rebound <- FALSE
    fields <- character()
    dynamic <- FALSE
    member_walk(node, function(x) {
        if (member_head(x, "for") && identical(member_name(x[[2L]]), name)) rebound <<- TRUE
        if (!member_head(x, "<-") && !member_head(x, "=") && !member_head(x, ":=")) return(NULL)
        target <- x[[2L]]
        field <- NULL
        replacement <- FALSE
        while (is.call(target) && length(target) > 1L) {
            replacement <- member_head(target, "$") || member_head(target, "[[")
            field <- if (replacement && length(target) >= 3L &&
                    (member_head(target, "$") || is.character(target[[3L]]))) member_name(target[[3L]]) else NULL
            target <- target[[2L]]
        }
        if (!identical(member_name(target), name)) return(NULL)
        if (!replacement) {
            rebound <<- TRUE
        } else if (is.null(field)) {
            dynamic <<- TRUE
        } else {
            fields <<- union(fields, field)
        }
        NULL
    }, descend_functions = FALSE)
    if (rebound || (!dynamic && !length(fields))) return(NULL)
    value <- member_completion_value(value, index, budget)
    if (!is_reference(value)) return(NULL)
    if (dynamic) fields <- names(value$fields)
    for (field in fields) {
        if (!member_r6_writable(value, field)) next
        previous <- member_lookup(value$fields, field)
        unknown <- member_value(reason = "unknown_local_write")
        unknown$r6_owner <- previous$r6_owner
        unknown$r6_member_kind <- previous$r6_member_kind
        value$fields[field] <- list(unknown)
    }
    value
}

# Initialization is inspected as syntax with inert self/private shapes. The
# declared surface survives opaque initialization; proven field assignments and
# finite named-list copies can add methods without constructing a live object.
member_r6_construct <- function(callee, actuals, index, budget, depth, trail) {
    instance <- callee$result_shape
    package <- callee$r6_package
    if (!is.null(package)) {
        package_index <- member_namespace_index(index, package)
        if (!is.null(package_index)) index <- package_index
        if (!identical(package, index$package)) return(instance)
        if (!is.null(index$document_bindings)) {
            index <- list2env(as.list(index), parent = emptyenv())
            index$document_bindings <- index$attached_roots <- NULL
        }
    }
    initialize <- callee$r6_initialize
    fn <- initialize$function_expr
    if (!member_head(fn, "function")) return(instance)
    # Track the original instance independently of the self binding and the
    # initializer's ignored return value. Aliases carry the same inert identity.
    budget$r6_identity <- if (is.null(budget$r6_identity)) 1L else budget$r6_identity + 1L
    instance$r6_identity <- budget$r6_identity
    fn[[3L]] <- as.call(list(as.name("{"), fn[[3L]]))
    env <- index$package_roots
    for (name in names(initialize$closure)) env[name] <- initialize$closure[name]
    # The initializer does not pass through member_access(). It sees the
    # `super` of its declaring class and the instance's private table; neither
    # is linked to the instance, so both carry the levels their methods bind
    # their own contexts from.
    if (!is.null(initialize$r6_owner_level)) {
        env$super <- member_r6_super(instance$r6_layers, initialize$r6_owner_level)
    }
    env$self <- instance
    env$.__r6_instance__ <- instance
    env$private <- member_r6_private_layers(instance$r6_private, member_r6_all_layers(instance))
    env$.__r6_initialize__ <- member_value(function_expr = fn, closure = env)
    env$.__r6_initialize__$r6_initialize <- TRUE
    for (i in seq_along(actuals)) env[paste0(".__r6_arg", i)] <- list(actuals[[i]])
    call <- as.call(c(list(as.name(".__r6_initialize__")),
            stats::setNames(lapply(seq_along(actuals), function(i) as.name(paste0(".__r6_arg", i))), names(actuals))))
    result <- member_infer(call, index, env, budget = budget, depth = depth + 1L, trail = trail)
    if (identical(result$type, "environment") && !is.null(result$fields)) {
        for (name in names(result$fields)) {
            if (!is.na(name) && nzchar(name)) instance$fields[name] <- result$fields[name]
        }
    }
    instance$r6_identity <- NULL
    instance
}

member_r6_context <- function(expr, index, bindings, budget) {
    args <- as.list(expr)[-1L]
    instance <- member_r6_shape(expr, index, bindings, budget)$fields$new$result_shape
    super <- member_r6_super(instance$r6_layers, instance$r6_level)
    super$r6_self <- instance
    private <- member_r6_private_layers(instance$r6_private, member_r6_all_layers(instance))
    private$r6_self <- instance
    scope <- list(self = instance)
    if (length(private$fields)) scope$private <- private
    if (!is.null(args$inherit) && !identical(args$inherit, quote(NULL))) scope$super <- super
    if (any(instance$r6_bindings == "active")) {
        scope$.__active__ <- member_value(type = "list", r6_class = instance$r6_class, r6_role = "active")
    }
    if (isFALSE(args$portable)) {
        # Non-portable methods enclose the public environment, whose parent
        # contains private bindings. Public bindings win on lookup. A bare
        # method name does not pass through member_access(): it gets the
        # contexts of its declaring class and no receiver.
        bare <- function(fields) lapply(fields, member_r6_bind, instance)
        scope <- utils::modifyList(bare(private$fields), utils::modifyList(bare(instance$fields), scope))
        scope$.__enclos_env__ <- member_value(type = "environment", r6_class = instance$r6_class, r6_role = "enclosure")
        if (isTRUE(instance$r6_cloneable)) {
            scope$clone <- member_value(function_expr = quote(function(deep = FALSE) NULL),
                r6_class = instance$r6_class, r6_role = "clone")
        }
    }
    scope
}

member_r6_runtime_shape <- function(generator, index, trail = list()) {
    if (length(trail) > 16L || any(vapply(trail, identical, logical(1L), generator))) {
        return(member_value(reason = "inheritance_cycle"))
    }
    get_plain <- function(name) member_binding(generator, name)[[2L]]
    public <- get_plain("public_methods")
    public_fields <- get_plain("public_fields")
    private_methods <- get_plain("private_methods")
    private_fields <- get_plain("private_fields")
    active <- get_plain("active")
    inherited <- get_plain("inherit")
    if (!is.list(public) || is.object(public)) {
        return(member_value(reason = "unsupported_r6"))
    }
    # R6 installs the same generated clone() into every cloneable class. Keep
    # its signature, not its body. Only the entry in the method storage is
    # considered, and only when it is R6's own function: a replacement made
    # with Class$set() and fields that merely hold code are left as they are.
    if (isTRUE(get_plain("cloneable")) && member_r6_generated_clone(public[["clone"]], generator)) {
        public$clone <- member_r6_clone_stub()
    }
    convert <- function(values) {
        as.call(c(list(as.name("list")), lapply(values, function(x) {
            if (typeof(x) == "closure") {
                member_function_syntax(x)
            } else if (!is.object(x) && is.atomic(x) && length(x) <= 100L) {
                x
            } else {
                as.name(".__unknown_field__")
            }
        })))
    }
    bindings <- list()
    # Inheritance may be an expression resolved in the generator's parent scope.
    # Read a direct binding only; never evaluate get_inherit() or an expression.
    if (is.symbol(inherited)) {
        scope <- get_plain("parent_env")
        if (is.environment(scope)) {
            record <- member_binding(scope, as.character(inherited))
            parent <- record[[2L]]
            if (identical(record[[1L]], "value") && is.environment(parent)) {
                bindings$.__r6_parent__ <- member_r6_runtime_shape(parent, index, c(trail, list(generator)))
            }
        }
    }
    expr <- as.call(list(as.call(list(as.name("::"), as.name("R6"), as.name("R6Class"))),
            classname = get_plain("classname"), public = convert(c(public_fields, public)),
            private = convert(c(private_fields, private_methods)),
            active = convert(active), inherit = if (is.null(inherited)) NULL else as.name(".__r6_parent__"),
            lock_objects = get_plain("lock_objects"), cloneable = get_plain("cloneable")
        ))
    shape <- member_r6_shape(expr, index, bindings, NULL)
    shape$fields$new$r6_package <- index$package
    shape
}

# Stored R6 instances are described from inert snapshots of their current
# public/private environments. Preserve bound receiver syntax without creating
# another instance or invoking an active binding.
member_r6_instance_shape <- function(value, snapshot = NULL, package = "") {
    if (!"R6" %in% value$classes) return(value)
    classes <- setdiff(value$classes, "R6")
    class <- list(name = if (length(classes)) classes[[1L]] else NULL, package = package)
    # Package extraction assigns namespace method identities after decoding.
    # Updating their owner must not expand already decorated closure scopes.
    if (identical(value$r6_role, "instance")) {
        value$r6_class <- class
        value$r6_private$r6_class <- value$r6_super$r6_class <- class
        for (name in names(value$fields)) value$fields[[name]]$r6_owner <- class
        return(value)
    }
    enclosure <- member_lookup(value$fields, ".__enclos_env__")
    enclosure_record <- member_lookup(snapshot$fields, ".__enclos_env__")
    private <- if (!is.null(value$r6_private)) value$r6_private else member_lookup(enclosure$fields, "private")
    super <- if (!is.null(value$r6_super)) value$r6_super else member_lookup(enclosure$fields, "super")
    if (is.null(private)) private <- member_lookup(value$fields, "private")
    if (is.null(super)) super <- member_lookup(value$fields, "super")
    if (is.null(private)) private <- member_value(type = "environment", fields = list())
    if (is.null(super)) super <- member_value(type = "environment", fields = list())
    private$r6_class <- super$r6_class <- class
    private$r6_role <- "private"
    super$r6_role <- "super"
    locked <- isTRUE(snapshot$r6_locked)
    decorate <- function(fields, records, section) {
        bindings <- character()
        for (name in names(fields)) {
            field <- fields[[name]]
            record <- member_lookup(records, name)
            active <- identical(record$kind, "active") || identical(field$reason, "active_property") ||
                identical(field$reason, "getter_not_invoked")
            method <- isTRUE(record$r6_bound)
            field$r6_owner <- class
            field$r6_member_kind <- if (active) "active binding" else paste(section, if (method) "method" else "field")
            if (active) {
                field$reason <- "active_property"
            } else if (method) {
                field$receiver_name <- "self"
                field$closure$self <- NULL
            }
            bindings[name] <- if (active) "active" else if (method && !isFALSE(record$locked)) "method" else
                if (isTRUE(record$locked)) "locked" else "data"
            fields[name] <- list(field)
        }
        list(fields = fields, bindings = bindings)
    }
    private_record <- member_lookup(enclosure_record$fields, "private")
    if (is.null(private_record)) private_record <- member_lookup(snapshot$fields, "private")
    private_result <- decorate(private$fields, private_record$fields, "private")
    super_record <- member_lookup(enclosure_record$fields, "super")
    if (is.null(super_record)) super_record <- member_lookup(snapshot$fields, "super")
    super_result <- decorate(super$fields, super_record$fields, "public")
    super$fields <- super_result$fields
    super$r6_bindings <- super_result$bindings
    private$fields <- private_result$fields
    private$r6_bindings <- private_result$bindings
    private$r6_locked <- locked
    value$fields[c(".__enclos_env__", "self", "private", "super")] <- NULL
    public_result <- decorate(value$fields, snapshot$fields, "public")
    value$fields <- public_result$fields
    value$type <- "environment"
    value$r6_class <- class
    value$r6_role <- "instance"
    value$r6_private <- private
    value$r6_super <- super
    value$r6_locked <- locked
    value$r6_bindings <- public_result$bindings
    value
}

member_r6_clone_stub <- function() {
    as.function(c(alist(deep = FALSE), list(as.name(".__unknown_field__"))), envir = baseenv())
}

member_r6_same_function <- function(a, b) {
    if (typeof(a) != "closure" || typeof(b) != "closure") return(FALSE)
    if (!identical(names(formals(a)), names(formals(b)))) return(FALSE)
    a <- utils::removeSource(a)
    b <- utils::removeSource(b)
    identical(formals(a), formals(b)) && identical(body(a), body(b))
}

member_r6_generated_clone <- function(value, generator = NULL) {
    if (typeof(value) != "closure") return(FALSE)
    generated <- tryCatch(
        get("generator_funs", envir = asNamespace("R6"), inherits = FALSE)$clone_method,
        error = function(e) NULL)
    if (member_r6_same_function(value, generated)) return(TRUE)
    # A binary package built against another R6 version serializes that R6
    # version's `clone_method` onto the generator itself (with the generator as
    # its enclosing environment), whereas Class$set("public", "clone", ...,
    # overwrite = TRUE) only replaces `public_methods$clone` with a closure
    # from the caller's environment.
    if (is.null(generator) && is.environment(environment(value)) &&
            "R6ClassGenerator" %in% attr(environment(value), "class", exact = TRUE)) {
        generator <- environment(value)
    }
    if (is.environment(generator) && identical(environment(value), generator)) {
        record <- member_binding(generator, "clone_method")
        if (identical(record[[1L]], "value") && member_r6_same_function(value, record[[2L]])) {
            return(TRUE)
        }
    }
    FALSE
}

member_r6_standard_generator_fn <- function(value, field, generator) {
    if (typeof(value) != "closure" || !is.environment(generator)) return(FALSE)
    if (identical(field, "clone_method")) return(member_r6_generated_clone(value, generator))
    funs <- tryCatch(
        get("generator_funs", envir = asNamespace("R6"), inherits = FALSE),
        error = function(e) NULL)
    !is.null(funs) && field %in% names(funs) &&
        (!identical(field, "new") || identical(environment(value), generator)) &&
        member_r6_same_function(value, funs[[field]])
}
