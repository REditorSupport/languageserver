# Syntax-only metadata extraction for the r-polars implementation.
# Source inference.R before this file. No r-polars package is loaded.

static_polars_index <- function(source_root) {
    paths <- sort(list.files(file.path(source_root, "R"), "\\.[Rr]$", full.names = TRUE))
    definitions <- list()
    locations <- list()
    for (path in paths) {
        code <- parse(path, keep.source = FALSE)
        for (expr in code) {
            if (!static_head(expr, "<-") || length(expr) != 3L) next
            key <- static_key(expr[[2L]])
            if (is.null(key)) next
            definitions[key] <- list(expr[[3L]])
            locations[key] <- list(basename(path))
        }
    }

    index <- new.env(parent = emptyenv())
    index$definitions <- definitions
    index$package <- "polars"
    index$locations <- locations
    index$registries <- list()
    index$members <- list()
    index$raw_fields <- list()
    index$wrapped_types <- list()
    index$native_types <- list()
    index$native_member_prefixes <- list()
    index$wrapper_functions <- "wrap"
    index$nonreturning_functions <- c("abort", "stop")
    index$cache <- new.env(parent = emptyenv())
    index$roots <- list(pl = static_value(type = "pl"))
    index$namespace_roots <- list("polars::pl" = static_value(type = "pl"))
    index$properties <- list()

    # This is a syntax adapter for r-polars' registry convention, not a table
    # of scan_csv/filter/group_by return types.
    stores <- definitions[["POLARS_STORE_ENVS"]]
    stopifnot(static_head(stores, "list"))
    stores <- as.list(stores)[-1L]
    for (prefix in names(stores)) {
        registry <- static_name(stores[[prefix]])
        if (is.null(registry)) next
        keys <- names(definitions)[startsWith(names(definitions), prefix)]
        members <- setNames(keys, substring(keys, nchar(prefix) + 1L))
        index$registries[registry] <- list(members)
    }

    # Inspect the actual $ methods to associate public classes with registries.
    # Expr subclasses can consult more than one registry, in dispatch order.
    for (key in names(definitions)[startsWith(names(definitions), "$.polars_")]) {
        type <- substring(key, 3L)
        registries <- character()
        static_walk(definitions[[key]], function(node) {
            if (static_head(node, "<-") &&
                    static_head(node[[3L]], "names")) {
                registry <- static_name(node[[3L]][[2L]])
                if (registry %in% names(index$registries)) {
                    registries <<- c(registries, registry)
                }
            }
        })
        members <- unlist(index$registries[unique(registries)], use.names = FALSE)
        member_names <- unlist(lapply(index$registries[unique(registries)], names))
        names(members) <- member_names
        index$members[type] <- list(members[!duplicated(names(members))])
    }
    index$members["pl"] <- list(index$registries[["pl"]])

    # Native wrapper classes and public wrapper output shapes are explicit in
    # class(e) <- c(...). No object is constructed to discover its fields.
    constructors <- names(definitions)[
        startsWith(names(definitions), "wrap.polars::") |
        startsWith(names(definitions), ".savvy_wrap_") |
        startsWith(names(definitions), "namespace_expr_")]
    for (key in constructors) {
        type <- NULL
        fields <- character()
        raw_fields <- character()
        static_walk(definitions[[key]], function(node) {
            if (static_head(node, "<-") && static_head(node[[2L]], "class")) {
                classes <- static_strings(node[[3L]])
                if (length(classes)) type <<- classes[[1L]]
            }
            if (static_head(node, "<-") && static_head(node[[2L]], "$")) {
                field <- static_name(node[[2L]][[3L]])
                if (!is.null(field)) fields <<- c(fields, field)
                if (identical(static_name(node[[3L]]), "x")) {
                    raw_fields <<- c(raw_fields, field)
                }
            }
            if (static_head(node, "makeActiveBinding")) {
                field <- static_strings(node[[2L]])
                fields <<- c(fields, field)
            }
        })
        if (is.null(type)) next
        old <- index$members[[type]]
        if (is.null(old)) old <- character()
        new_fields <- setdiff(fields, names(old))
        old <- c(old, setNames(rep(NA_character_, length(new_fields)), new_fields))
        index$members[type] <- list(old)
        if (startsWith(key, ".savvy_wrap_")) {
            index$native_types[key] <- list(type)
            index$native_member_prefixes[type] <- list(substring(key, 13L))
        }
        if (startsWith(key, "wrap.polars::")) {
            raw_type <- substring(key, 6L)
            index$wrapped_types[raw_type] <- list(type)
            index$raw_fields[type] <- list(setNames(rep(raw_type, length(raw_fields)), raw_fields))
            # Some wrappers also construct another raw field, e.g. Selector
            # stores a PlRExpr in _rexpr. Analyze that initializer, not just x.
            static_walk(definitions[[key]], function(node) {
                if (static_head(node, "<-") && static_head(node[[2L]], "$")) {
                    field <- static_name(node[[2L]][[3L]])
                    value <- static_infer(node[[3L]], index)
                    if (!is.null(field) && !is.null(value$type)) {
                        index$raw_fields[[type]][field] <- value$type
                    }
                }
            })
        }
    }

    # The Expr constructor installs namespaces via an active-binding loop.
    # Recognize its registry convention as metadata; never invoke the getter.
    namespaces <- index$registries[["polars_namespaces_expr"]]
    index$namespace_types <- list()
    for (name in names(namespaces)) {
        fn <- definitions[[namespaces[[name]]]]
        type <- NULL
        static_walk(fn, function(node) {
            if (static_head(node, "<-") && static_head(node[[2L]], "class")) {
                values <- static_strings(node[[3L]])
                if (length(values)) type <<- values[[1L]]
            }
        })
        if (!is.null(type)) index$namespace_types[name] <- list(type)
    }
    expr_members <- index$members[["polars_expr"]]
    index$members["polars_expr"] <- list(c(expr_members,
        setNames(rep(NA_character_, length(index$namespace_types)), names(index$namespace_types))))
    index$properties["polars_expr"] <- list(index$namespace_types)
    index
}
