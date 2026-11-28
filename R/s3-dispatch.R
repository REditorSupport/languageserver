# Source histories refer to accepted syntax. Global values are resolved lazily
# at the call position; an alias keeps the value from before its assignment.
s3_source_index <- function(items) {
    histories <- new.env(hash = TRUE, parent = emptyenv())
    functions <- new.env(hash = TRUE, parent = emptyenv())
    imports <- registrations <- list()
    aliases <- character()
    for (i in seq_along(items)) {
        item <- items[[i]]
        expr <- item$expr
        names <- character()
        kind <- "effect"
        if (member_head(expr, "<-") || member_head(expr, "=")) {
            lhs <- expr[[2L]]
            rhs <- expr[[3L]]
            if (is.symbol(lhs)) {
                names <- member_name(lhs)
                kind <- "binding"
                if (member_head(rhs, "function")) functions[[names]] <- s3_generic_descriptor(rhs)
                if (is.symbol(rhs) || member_head(rhs, "::") || member_head(rhs, ":::")) aliases <- c(aliases, names)
            } else if (is.call(lhs) && length(lhs) > 1L && is.symbol(lhs[[2L]])) {
                names <- member_name(lhs[[2L]])
                kind <- "mutation"
            }
        } else if (member_head(expr, "library") || member_head(expr, "require")) {
            imports[[length(imports) + 1L]] <- list(
                item = i,
                package = if (length(expr) > 1L) member_name(expr[[2L]]) else NULL
            )
        } else if (member_head(expr, "data")) {
            args <- as.list(expr)[-1L]
            argument_names <- names(args) %s3_or% rep("", length(args))
            names <- vapply(
                args[!argument_names %in% c("package", "list", "envir", "overwrite", "verbose")],
                function(x) member_name(x) %s3_or% "", character(1L)
            )
            kind <- "data"
        } else if (is.call(expr) && identical(s3_call_name(expr[[1L]])$name, "registerS3method")) {
            registrations[[length(registrations) + 1L]] <- i
        } else if (is.call(expr) && !member_head(expr, "function")) {
            literal_write <- if (isTRUE(member_name(expr[[1L]]) %in% c("assign", "rm", "remove")) &&
                    length(expr) > 1L && is.character(expr[[2L]])) {
                expr[[2L]]
            } else {
                character()
            }
            names <- unique(c(
                literal_write, member_assigned_names(expr),
                unlist(lapply(as.list(expr)[-1L], member_syntax_names), use.names = FALSE)
            ))
        }
        for (name in names[nzchar(names)]) {
            histories[[name]] <- c(histories[[name]], list(list(
                item = i, kind = kind,
                start = item$start, end = item$end
            )))
        }
    }
    list(
        histories = as.list(histories, all.names = TRUE), functions = as.list(functions, all.names = TRUE), imports = imports,
        registrations = registrations, aliases = unique(aliases)
    )
}

s3_has_member_hints <- function(hints, classes) {
    length(attr(hints, "occurrences", exact = TRUE)$type) > 0L &&
        (length(attr(hints, "classes", exact = TRUE)) > 0L ||
            any(vapply(classes, function(x) x$type %in% c("R6", "S7Generic", "S7Method"), logical(1L))))
}

s3_source_view <- function(document, peer = FALSE) {
    parsed <- document$parse_data
    data <- parsed$member_data
    if (is.null(data)) {
        return(NULL)
    }
    items <- data$items
    index <- data$s3
    partial_peer <- peer && (isTRUE(parsed$parse_error) || !is.null(parsed$partial_complete_roots))
    if (partial_peer) {
        cached <- parsed$complete_s3_view
        if (!is.null(cached) && identical(cached$uri, document$uri)) return(cached)
        ids <- member_complete_item_ids(parsed)
        if (length(ids) < length(items)) {
            # Histories, imports and registrations index the filtered items by position.
            items <- items[ids]
            index <- s3_source_index(items)
        }
    }
    view <- list(index = index %s3_or% s3_source_index(items), items = items,
        uri = document$uri, content_key = data$content_key,
        member_hints = s3_has_member_hints(data$type_hints, parsed$class_data$declarations),
        hint_inheritance = isTRUE(attr(data$type_hints, "has_inheritance", exact = TRUE)))
    if (partial_peer && is.environment(parsed)) parsed$complete_s3_view <- view
    view
}

s3_source_value <- function(name, context) {
    for (i in seq_along(context$sources)) {
        source <- context$sources[[i]]
        at <- if (identical(source$uri, context$source_uri)) context$at else c(Inf, Inf)
        history <- source$index$histories[[name]]
        position <- member_history_position(history, at)
        if (!position) next
        record <- history[[position]]
        key <- paste(source$uri, name, position, paste(at, collapse = ":"), sep = ":")
        if (exists(key, context$memo, inherits = FALSE)) {
            return(context$memo[[key]])
        }
        if (key %in% context$trail) {
            return(s3_unknown("binding_cycle"))
        }
        item <- source$items[[record$item]]
        cache_key <- if (is.null(context$hint_wait) && !is.null(context$binding_cache) && !is.null(item$cache_prefix) && s3_binding_context_unchanged(context)) {
            paste(source$uri, name, position, item$cache_prefix, s3_binding_context_key(context, i), sep = ":")
        }
        cached <- analysis_cache_get(context$binding_cache, cache_key)
        if (cached$hit) {
            context$memo[[key]] <- cached$value
            return(cached$value)
        }
        old_at <- context$at
        old_uri <- context$source_uri
        context$trail <- c(context$trail, key)
        on.exit(
            {
                context$at <- old_at
                context$source_uri <- old_uri
                context$trail <- utils::head(context$trail, -1L)
            },
            add = TRUE
        )
        context$at <- item$start
        context$source_uri <- source$uri
        expr <- item$expr
        if (identical(record$kind, "binding")) {
            result <- context$infer(expr[[3L]], context$root)
            if (!is.null(result$fn)) {
                implementation <- if (member_head(expr[[3L]], "function")) {
                    list(uri = source$uri, start = item$start, end = item$end, columns = item$implementation_columns)
                } else {
                    result$source$implementation
                }
                result$source <- list(
                    uri = source$uri, line = item$start[[1L]] + 1L, binding = name, implementation = implementation,
                    comments = item$comments, version = source$version
                )
            }
        } else if (identical(record$kind, "data")) {
            args <- as.list(expr)[-1L]
            package <- if (is.character(args$package)) args$package else NULL
            result <- if (!is.null(s3_get_value("data", context$root, context,
                        source_only = TRUE
                    ))) {
                s3_unknown("shadowed_data")
            } else {
                s3_package_object(name, package, context)
            }
        } else {
            # Replay a mutation in a scratch scope seeded with the preceding
            # object. Unknown calls invalidate it instead of preserving a class.
            scope <- new.env(parent = context$root)
            previous <- s3_source_value(name, context)
            if (!is.null(previous)) assign(name, previous, scope)
            context$infer(expr, scope)
            result <- get0(name, scope, inherits = FALSE) %s3_or% s3_unknown("opaque_effect")
        }
        context$memo[[key]] <- result
        if (!isTRUE(context$exhausted) && s3_binding_context_unchanged(context) &&
                length(result$classes) && s3_binding_cacheable(result)) {
            analysis_cache_set(context$binding_cache, cache_key, result)
        }
        return(result)
    }
    NULL
}

s3_get_value <- function(name, scope, context, source_only = FALSE) {
    if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
        return(NULL)
    }
    value <- get0(name, scope, inherits = TRUE)
    if (!is.null(value)) {
        return(value)
    }
    value <- s3_source_value(name, context)
    if (!is.null(value)) {
        return(value)
    }
    if (source_only) {
        return(NULL)
    }
    fn <- s3_namespace_function(name, NULL, context)
    if (!is.null(fn)) {
        return(s3_value(NULL,
                fn = as.call(list(as.name("function"), fn$formals, NULL)), info = fn,
                origin = fn$owner, function_name = sub("^[^:]*::", "", fn$summary %s3_or% fn$name)
            ))
    }
    NULL
}

s3_namespace_function <- function(name, package = NULL, context, include_base = TRUE) {
    packages <- if (!is.null(package)) package else rev(unique(c(startup_packages, context$imports)))
    if (!include_base) packages <- setdiff(packages, "base")
    for (package in packages) {
        catalog <- context$catalogs[[package]]
        if (is.null(catalog)) next
        record <- catalog$functions[[name]]
        if (!is.null(record)) {
            return(s3_catalog(context, package)$functions[[name]])
        }
        if (name %in% catalog$callables) {
            return(list(name = name, package = package, owner = package))
        }
    }
    NULL
}

s3_package_object <- function(name, package = NULL, context) {
    packages <- if (!is.null(package)) package else rev(unique(c("datasets", context$imports)))
    for (package in packages) {
        catalog <- context$catalogs[[package]]
        object <- if (name %in% c(catalog$object_keys, names(catalog$objects))) {
            s3_catalog(
                context,
                package
            )$objects[[name]]
        }
        if (!is.null(object)) {
            return(object)
        }
    }
    s3_unknown("no_object_metadata")
}

s3_dispatch_path <- function(classes) {
    if (isTRUE(attr(classes, "incomplete_s3_path", exact = TRUE))) as.character(classes) else c(classes, "default")
}

s3_lookup_method <- function(generic, classes, scope, context, owner = "source") {
    if (!length(classes) || anyNA(classes) || isTRUE(context$registration_unknown)) {
        return(NULL)
    }
    registrations <- get0(".registrations", scope, inherits = TRUE)
    for (class in s3_dispatch_path(classes)) {
        key <- paste(generic, class, sep = ".")
        value <- s3_get_value(key, scope, context, source_only = TRUE)
        if (!is.null(value)) {
            if (is.null(value$fn)) {
                return(NULL)
            }
            if (!is.null(value$info)) {
                if (is.null(value$info$formals)) return(NULL)
                method <- value$info
                method$binding_source <- value$source
                return(method)
            }
            return(list(
                name = key, fn = value$fn, closure = value$closure,
                formals = value$fn[[2L]], source = value$source, body_hash = s3_function_identity(value$fn)
            ))
        }
        # Attached exports belong to the calling environment; base bindings
        # are considered only after the generic's registration table.
        visible <- s3_namespace_function(key, NULL, context, include_base = FALSE)
        if (!is.null(visible) && !is.null(visible$formals)) {
            return(visible)
        }
        namespace <- context$namespace_records[[paste(owner, key, sep = "::")]] %s3_or% context$namespace_records[[key]]
        registered <- registrations[[paste(owner, key, sep = "::")]] %s3_or% namespace
        if (!is.null(registered$fn)) {
            if (!is.null(registered$info)) {
                if (is.null(registered$info$formals)) return(NULL)
                method <- registered$info
                method$binding_source <- registered$source
                return(method)
            }
            return(list(
                name = registered$name %s3_or% key, fn = registered$fn,
                registration = registered$registration,
                formals = registered$fn[[2L]], closure = registered$closure, source = registered$source, body_hash = s3_function_identity(registered$fn)
            ))
        }
        # Registered methods use generic ownership, including private methods.
        if (owner != "source") {
            for (package in names(context$catalogs)) {
                catalog <- context$catalogs[[package]]
                id <- paste(owner, key, sep = "::")
                method <- if (id %in% c(catalog$method_keys, names(catalog$methods))) {
                    s3_catalog(
                        context,
                        package
                    )$methods[[id]]
                }
                if (!is.null(method)) {
                    return(method)
                }
            }
        }
        if (isTRUE(context$incomplete)) {
            return(NULL)
        }
        visible <- s3_namespace_function(key, "base", context)
        if (!is.null(visible) && !is.null(visible$formals)) {
            return(visible)
        }
    }
    NULL
}

s3_receiver_method <- function(generic, input, scope, context, owner = "source") {
    if (!length(input$class_options)) return(s3_lookup_method(generic, input$classes, scope, context, owner))
    methods <- lapply(input$class_options, function(classes) s3_lookup_method(generic, classes, scope, context, owner))
    first <- methods[[1L]]
    if (!is.null(first) && all(vapply(methods[-1L], identical, logical(1L), first))) first
}

# Class hints can use the package and workspace member metadata as well as
# the S3 catalogs. Replacing either must invalidate a cached dispatch choice.
s3_member_metadata_key <- function(workspace) {
    metadata <- workspace$member_metadata
    if (is.null(metadata)) return(NULL)
    list(identity = if (is.function(metadata$identity)) metadata$identity() else metadata,
        revision = if (is.function(metadata$revision)) metadata$revision())
}

s3_context_sources <- function(uri, workspace, document) {
    sources <- list(s3_source_view(document))
    others <- if (!is.null(workspace$documents)) workspace_document_uris(workspace, uri) else character()
    for (other in setdiff(others, uri)) {
        candidate <- workspace$documents$get(other, NULL)
        if (!is.null(candidate) && identical(candidate$version, candidate$parse_data$version)) {
            sources[[length(sources) + 1L]] <- s3_source_view(candidate, peer = TRUE)
        }
    }
    if (!is.null(workspace$index) && isTRUE(workspace$index$enabled)) {
        package_root <- workspace$index$package_root_for_uri(uri)
        uris <- if (!is.null(package_root)) workspace$index$package_source_uris(package_root) else workspace$index$source_closure(uri, update = FALSE)
        for (other in setdiff(uris, c(uri, others))) {
            summary <- workspace$index$summaries$get(other, NULL)
            if (!is.null(summary$s3)) sources[[length(sources) + 1L]] <- summary$s3
        }
    }
    Filter(Negate(is.null), sources)
}

s3_context <- function(uri, workspace, document, at) {
    context <- new.env(parent = emptyenv())
    context$workspace <- workspace
    context$document <- document
    context$at <- at
    context$source_uri <- uri
    context$root <- new.env(parent = emptyenv())
    context$memo <- new.env(parent = emptyenv())
    context$trail <- character()
    context$registration_unknown <- FALSE
    context$namespace_records <- list()
    context$deadline <- proc.time()[[3L]] + if (identical(Sys.getenv("TESTTHAT"), "true")) 5 else 0.25
    context$catalogs <- list()
    context$metadata <- workspace$s3_metadata
    metadata <- workspace$s3_metadata
    if (!is.null(metadata)) for (package in metadata$keys()) context$catalogs[[package]] <- if (is.function(metadata$catalog)) metadata$catalog(package) else metadata$get(package)
    context$sources <- s3_context_sources(uri, workspace, document)
    imports <- lapply(context$sources, function(source) {
        items <- source$index$imports
        if (identical(source$uri, uri)) items <- Filter(function(import) member_before(source$items[[import$item]]$end, at), items)
        unlist(lapply(items, `[[`, "package"), use.names = FALSE)
    })
    imports_at <- function(per_source) unique(c(workspace$imported_packages, unlist(per_source, use.names = FALSE), character()))
    context$imports <- imports_at(imports)
    required <- unique(c(startup_packages, context$imports, document$parse_data$member_data$packages))
    repeat {
        dependencies <- unique(unlist(lapply(context$catalogs[required], `[[`, "dependencies"), use.names = FALSE))
        missing <- setdiff(dependencies, required)
        if (!length(missing)) break
        required <- union(required, missing)
    }
    context$incomplete <- any(vapply(required, function(package) {
        catalog <- context$catalogs[[package]]
        is.null(catalog) || !is.null(catalog$error)
    }, logical(1L)))
    analyzer <- s3_analyzer(context, context$root)
    for (s in seq_along(context$sources)) {
        source <- context$sources[[s]]
        if (!length(source$index$registrations)) next
        # registerS3method() targets the generic visible at registration. A
        # later library() can mask it before the call being analyzed.
        previous_imports <- context$imports
        import_rows <- vapply(source$index$imports, `[[`, integer(1L), "item")
        last_prefix <- -1L
        for (i in source$index$registrations) {
            item <- source$items[[i]]
            if (!identical(source$uri, uri) || member_before(item$end, at)) {
                old_at <- context$at
                old_uri <- context$source_uri
                context$at <- item$start
                context$source_uri <- source$uri
                prefix <- findInterval(i - 1L, import_rows)
                if (prefix != last_prefix) {
                    scoped_imports <- imports
                    scoped_imports[s] <- list(unlist(lapply(source$index$imports[seq_len(prefix)], `[[`, "package"), use.names = FALSE))
                    context$imports <- imports_at(scoped_imports)
                    last_prefix <- prefix
                }
                context$registration_site <- list(uri = source$uri, at = item$start, call = item$expr)
                analyzer$infer(item$expr, context$root)
                context$registration_site <- NULL
                context$at <- old_at
                context$source_uri <- old_uri
                if (isTRUE(get0(".dynamic_registration", context$root))) context$registration_unknown <- TRUE
            }
        }
        context$imports <- previous_imports
    }
    root <- s3_package_root(workspace, uri)
    namespace <- list(key = "", records = list(), valid = TRUE)
    if (!is.null(root)) {
        namespace <- s3_namespace_snapshot(root, workspace)
        if (!isTRUE(namespace$valid)) context$registration_unknown <- TRUE
        for (record in namespace$records) {
            value <- s3_get_value(record$method, context$root, context, source_only = TRUE)
            if (is.null(value$fn)) next
            key <- paste(record$generic, record$class, sep = ".")
            if (!is.null(record$package)) key <- paste(record$package, key, sep = "::")
            value$name <- record$method
            context$namespace_records[[key]] <- value
        }
    }
    context$analyzer <- analyzer
    context$binding_root <- as.list(context$root, all.names = TRUE)
    context$binding_imports <- context$imports
    if (!isTRUE(context$incomplete) && !isTRUE(context$registration_unknown) && !isTRUE(context$exhausted) &&
            is.function(metadata$revision) && is.function(metadata$identity) &&
            all(vapply(context$sources, function(source) !is.null(source$content_key), logical(1L)))) {
        context$binding_cache <- analysis_cache(document, "s3-bindings",
            list(metadata = metadata$identity(), revision = metadata$revision(), namespace = namespace$key,
                member_metadata = s3_member_metadata_key(workspace), roxygen = roxygen_type_hints_enabled()),
            max_bytes = 4 * 1024^2)
        context$binding_keys <- new.env(parent = emptyenv())
        context$source_uris <- vapply(context$sources, `[[`, "", "uri")
        context$source_keys <- vapply(context$sources, `[[`, "", "content_key")
        context$hint_inheritance <- hint_package_inherits(workspace, document) ||
            any(vapply(context$sources, function(source) isTRUE(source$hint_inheritance), logical(1L)))
        context$registrations <- unlist(lapply(context$sources, function(source) {
            vapply(source$index$registrations, function(i) {
                item <- source$items[[i]]
                if (!identical(source$uri, uri) || member_before(item$end, at)) item$cache_prefix %s3_or% "" else ""
            }, "")
        }), use.names = FALSE)
    }
    context
}

# The binding prefix covers its own file up to the assignment. Hash the other
# sources in search order, once for each file actually supplying a binding.
s3_binding_context_key <- function(context, i) {
    uri <- context$source_uris[[i]]
    key <- context$binding_keys[[uri]]
    if (is.null(key)) {
        # A donor can follow the call in its own file, beyond the ordinary
        # binding-prefix fingerprint. Keep the full source key in that case.
        peers <- list(sources = context$source_uris[-i], content = if (isTRUE(context$hint_inheritance)) context$source_keys else context$source_keys[-i],
            imports = context$binding_imports, registrations = context$registrations,
            startup = startup_packages)
        key <- digest::digest(peers, algo = "xxhash64")
        context$binding_keys[[uri]] <- key
    }
    key
}

s3_binding_context_unchanged <- function(context) {
    identical(context$imports, context$binding_imports) &&
        identical(as.list(context$root, all.names = TRUE), context$binding_root)
}

# Recover the enclosing call with its lexical scopes and following arguments.
# The same path is used for complete source and bounded incomplete recovery.
s3_call_location <- function(document, point, hover = FALSE, call = NULL) {
    if (!check_r_region(document, point)) {
        return(NULL)
    }
    token <- if (hover) document$detect_token(point)
    if (is.null(call)) call <- document$detect_call(point)
    if (hover && nzchar(token$token)) {
        end <- token$range$end
        following <- substring(document$line0(end$row), end$col + 1L)
        opening <- regexpr("^[ \t]*[(]", following)
        if (opening[[1L]] == 1L) {
            call <- document$detect_call(list(
                row = end$row,
                col = end$col + attr(opening, "match.length")[[1L]]
            ))
        }
    }
    # Control-flow and function headers are deliberately reported without a
    # callee by detect_call(), even though their keyword is a valid token.
    if (!is.character(call$token) || length(call$token) != 1L ||
            is.na(call$token) || !nzchar(call$token)) return(NULL)
    location <- call$symbol_location
    if (is.null(location)) location <- member_call_location(document, point, call, symbols = TRUE)
    if (is.null(location) || !is.null(location$cursor$accessor)) {
        return(NULL)
    }
    location$call <- call
    location$request_point <- point
    location$hover_token <- token
    location
}

s3_scoped_call <- function(document, location, context) {
    recovered <- member_recover_scope(document, location$point, location$cursor, document$parse_data$member_data)
    if (is.null(recovered)) {
        return(NULL)
    }
    root <- recovered$context %s3_or% s3_recover_call(document, location, recovered) %s3_or% recovered$parsed
    path <- member_cursor_path(root, recovered$sentinel, NULL)
    if (!length(path)) {
        return(NULL)
    }
    node <- root
    pipe_rhs <- NULL
    scope <- context$root
    ast <- integer()
    while (length(path)) {
        step <- path[[1L]]
        if (step == 3L && is.call(node) && is.symbol(node[[1L]]) &&
                member_name(node[[1L]]) %in% c("%>%", "%<>%", "%T>%")) {
            if (!is.null(s3_get_value(member_name(node[[1L]]), scope, context, source_only = TRUE))) return(NULL)
            # The placeholder is lexical throughout the RHS, but magrittr
            # supplies an implicit first argument only to the outermost call.
            input <- context$infer(node[[2L]], scope)
            scope <- new.env(parent = scope)
            assign(".", input, scope)
            pipe_rhs <- c(ast, step)
        }
        if (member_head(node, "function")) {
            scope <- new.env(parent = scope)
            hints <- roxygen_inference_hints(node)
            inherited <- if (isTRUE(attr(hints, "roxygen_inherits", exact = TRUE))) s3_inherited_params(hints, context)
            for (name in names(node[[2L]])) {
                hint <- if (is.null(inherited)) hints$params[[name]] else inherited
                if (!is.null(hint)) s3_bind_hinted_formal(name, hint, scope, context) else assign(name, s3_unknown("formal"), scope)
            }
        } else if (member_head(node, "{") || is.expression(node)) {
            sequence <- member_cursor_sequence(node, path, ast, recovered$source,
                narrow = TRUE, name = location$call$token, call_reads = TRUE)
            for (i in sequence$selected[sequence$selected < sequence$target]) context$infer(sequence$children[[i]], scope)
            node <- sequence$children[[sequence$target]]
            ast <- c(sequence$ast_path, sequence$path[[1L]])
            path <- sequence$path[-1L]
            next
        }
        if (is.call(node) && step == 1L && ((member_head(node[[step]], "::") || member_head(
            node[[step]],
            ":::"
        )) && identical(member_name(node[[step]][[3L]]), recovered$sentinel))) {
            node[[1L]][[3L]] <- as.name(location$call$token)
            if (identical(ast, pipe_rhs)) node <- s3_pipe_arguments(node)
            return(list(call = node, scope = scope, root = root, path = ast, start = recovered$start, source = recovered$source))
        }
        if (is.call(node) && step == 1L && is.symbol(node[[step]]) &&
                identical(member_name(node[[step]]), recovered$sentinel)) {
            node[[1L]] <- if (is.null(location$call$package)) {
                as.name(location$call$token)
            } else {
                as.call(list(
                    as.name(location$call$accessor), as.name(location$call$package),
                    as.name(location$call$token)
                ))
            }
            if (identical(ast, pipe_rhs)) node <- s3_pipe_arguments(node)
            return(list(call = node, scope = scope, root = root, path = ast, start = recovered$start, source = recovered$source))
        }
        node <- node[[step]]
        ast <- c(ast, step)
        path <- path[-1L]
    }
    NULL
}

s3_match_arguments <- function(formals, actuals) {
    keys <- names(formals)
    dots <- match("...", keys)
    before <- if (is.na(dots)) seq_along(keys) else seq_len(dots - 1L)
    names <- names(actuals)
    if (!any(nzchar(names))) {
        n <- length(actuals)
        if (n <= length(before)) return(seq_len(n))
        if (is.na(dots)) return(NULL)
        return(c(before, rep.int(dots, n - length(before))))
    }
    result <- rep(NA_integer_, length(actuals))
    used <- integer()
    # R matches all exact names before considering any partial names.
    exact <- which(nzchar(names) & names %in% setdiff(keys, "..."))
    for (i in exact) {
        candidate <- match(names[[i]], keys)
        if (candidate %in% used) return(NULL)
        result[[i]] <- candidate
        used <- c(used, candidate)
    }
    partial <- setdiff(before, used)
    for (i in which(nzchar(names) & is.na(result))) {
        candidates <- partial[startsWith(keys[partial], names[[i]])]
        if (length(candidates) > 1L || any(candidates %in% used)) {
            return(NULL)
        }
        if (length(candidates) == 1L && keys[[candidates]] != "...") {
            result[[i]] <- candidates
            used <- c(used, candidates)
        }
    }
    available <- setdiff(before, used)
    for (i in which(is.na(result) & !nzchar(names))) {
        if (!length(available)) break
        result[[i]] <- available[[1L]]
        available <- available[-1L]
    }
    if (anyNA(result)) {
        if (is.na(dots)) {
            return(NULL)
        }
        result[is.na(result)] <- dots
    }
    result
}

resolve_s3_call <- function(uri, workspace, document, point, hover = FALSE, call = NULL, hint_wait = NULL) {
    if (!isTRUE(lsp_settings$get("s3_dispatch")) ||
            !identical(document$version, document$parse_data$version)) {
        return(NULL)
    }
    location <- s3_call_location(document, point, hover, call)
    if (is.null(location) || !s3_may_dispatch(workspace, document, location)) {
        return(NULL)
    }
    revision <- if (is.function(workspace$s3_metadata$revision)) workspace$s3_metadata$revision() else NULL
    identity <- list(s3 = if (is.function(workspace$s3_metadata$identity)) workspace$s3_metadata$identity() else workspace$s3_metadata,
        members = s3_member_metadata_key(workspace), roxygen = roxygen_type_hints_enabled())
    if (!identical(document$s3_identity, identity)) {
        document$s3_calls$clear()
        document$s3_identity <- identity
    }
    scope_generation <- if (is.function(workspace$index$scope_generation)) workspace$index$scope_generation(uri) else NULL
    cache_key <- paste(document$version, document$parse_generation, workspace$s3_revision, workspace$index$revision,
        paste(unlist(scope_generation), collapse = ":"), s3_namespace_generation(workspace, uri), revision, location$call$opening$row,
        location$call$opening$col,
        sep = ":"
    )
    cache <- document$s3_calls
    if (is.null(hint_wait) && !is.null(revision) && cache$has(cache_key)) {
        return(cache$get(cache_key))
    }
    result <- NULL
    context <- NULL
    on.exit(if (is.null(hint_wait) && !is.null(revision) && !isTRUE(context$exhausted)) {
        cache$set(cache_key, result)
    }, add = TRUE)
    context <- s3_context(uri, workspace, document, c(location$point$row, location$point$col))
    context$hint_wait <- hint_wait
    scoped <- s3_scoped_call(document, location, context)
    if (is.null(scoped)) {
        return(NULL)
    }
    callee <- context$infer(scoped$call[[1L]], scoped$scope)
    descriptor <- if (!is.null(callee$info)) callee$info$generic else s3_generic_descriptor(callee$fn)
    if (is.null(descriptor) || !is.null(s3_get_value("UseMethod", scoped$scope, context, source_only = TRUE))) {
        return(NULL)
    }
    actuals <- as.list(scoped$call)[-1L]
    generic_formals <- if (!is.null(callee$info)) callee$info$formals else callee$fn[[2L]]
    matched <- s3_match_arguments(generic_formals, actuals)
    if (is.null(matched)) {
        return(NULL)
    }
    supplied <- which(matched == match(descriptor$formal, names(generic_formals)))
    if (length(supplied) > 1L) {
        return(NULL)
    }
    input_expr <- if (length(supplied)) actuals[supplied] else list(quote(expr = ))
    input <- if (!identical(input_expr[[1L]], quote(expr = ))) {
        context$infer(input_expr[[1L]], scoped$scope)
    } else {
        default_scope <- new.env(parent = if (is.environment(callee$closure)) callee$closure else context$root)
        arguments <- s3_match_values(generic_formals, lapply(actuals, context$infer, scope = scoped$scope),
            default_scope, context$infer)
        get0(descriptor$formal, arguments, inherits = FALSE)
    }
    hints <- roxygen_inference_hints(callee$fn)
    if (length(supplied) && isTRUE(input$reason %in% c("field", "element", "formal", "subset_receiver", "custom_subset", "shadowed_callable",
                "no_summary:::", "no_summary:::@", "no_summary:::slot", "no_summary:::prop")) &&
            !identical(input_expr[[1L]], quote(expr = )) && any(vapply(context$sources, function(s) isTRUE(s$member_hints), logical(1L)))) {
        method_call <- input$reason %in% c("no_summary:::", "shadowed_callable")
        input <- s3_apply_type_hint(input, s3_class_member_input(input_expr[[1L]], scoped, context, method_call))
    }
    if (!is.null(hints)) {
        input <- s3_apply_type_hint(input, {
            types <- if (isTRUE(attr(hints, "roxygen_inherits", exact = TRUE))) {
                s3_inherited_params(hints, context)(descriptor$formal)
            } else {
                hints$params[[descriptor$formal]]
            }
            if (length(types)) s3_type_hint(types, context, scope = callee$closure, source = callee$source$implementation)
        })
    }
    owner <- if (!is.null(callee$info)) callee$info$owner else "source"
    method <- s3_receiver_method(descriptor$name, input, scoped$scope, context, owner)
    if (is.null(method)) {
        return(NULL)
    }
    # Methods can omit generic formals. Validate the supplied arguments
    # against the selected method, including arguments accepted by dots.
    method_matched <- s3_match_arguments(method$formals, actuals)
    if (is.null(method_matched)) {
        return(NULL)
    }
    formals <- method$formals
    argument_names <- names(formals)
    # A closed generic can forward renamed positional parameters, but only
    # its own formal names can be offered as named argument completions.
    if (!"..." %in% names(generic_formals)) {
        argument_names <- intersect(names(formals), names(generic_formals))
        dots <- match("...", names(formals))
        positional <- if (is.na(dots)) names(formals) else names(formals)[seq_len(dots - 1L)]
        forwarded <- utils::head(positional, length(generic_formals))
        if (!is.na(dots) && length(generic_formals) > length(positional)) forwarded <- c(forwarded, "...")
        formals <- as.pairlist(formals[names(formals) %in% union(argument_names, forwarded)])
    }
    signature <- get_signature(
        quote_completion_name(location$call$token),
        as.call(list(as.name("function"), formals, NULL))
    )
    description <- if (length(input$class_options)) {
        paste0("S3 method `", method$name, "` for possible classes ",
            paste(sprintf("`%s`", vapply(input$class_options, `[[`, "", 1L)), collapse = " or "), ".")
    } else {
        name <- method$name %s3_or% paste(descriptor$name, input$classes[[1L]], sep = ".")
        sprintf("S3 method `%s` for class `%s`.", name, paste(input$classes, collapse = ", "))
    }
    result <- list(
        signature = signature, formals = as.list(formals), argument_names = argument_names, method = method,
        generic = descriptor$name, generic_package = if (owner == "source") NULL else owner,
        classes = input$classes, supplied = unique(names(method$formals)[method_matched[!vapply(
            actuals,
            function(x) identical(x, quote(expr = )), logical(1L)
        )]]),
        actuals = actuals, matched_parameters = names(method$formals)[method_matched],
        opening = location$call$opening, location = location, uri = uri,
        version = document$version, revision = revision, source_revision = workspace$s3_revision,
        description = description
    )
    result$method$closure <- NULL
    result$method$fn <- NULL
    result$method$registration <- NULL
    result
}

s3_index_summary <- function(content, uri, expressions = NULL, parse_data = NULL, incomplete_lines = integer(),
    hints = parse_data$member_data$type_hints, classes = parse_data$class_data$declarations) {
    if (!is.null(parse_data$member_data)) {
        items <- parse_data$member_data$items
        if (isTRUE(parse_data$parse_error) || !is.null(parse_data$partial_complete_roots)) {
            items <- items[member_complete_item_ids(parse_data)]
        }
    } else if (!is.null(expressions)) {
        expressions <- top_level_statements(expressions, incomplete_lines)
        refs <- attr(expressions, "srcref")
        items <- lapply(seq_along(expressions), function(i) {
            ref <- refs[[i]]
            list(
                expr = expressions[[i]], start = c(ref[[1L]] - 1L, ref[[5L]] - 1L),
                end = c(ref[[3L]] - 1L, ref[[6L]])
            )
        })
    } else {
        return(NULL)
    }
    # Older or standalone index builders may not supply member fingerprints.
    # Compute the same bounded source-prefix keys before compacting bodies.
    if (length(items) && any(vapply(items, function(item) is.null(item$cache_prefix), logical(1L)))) {
        prefix <- ""
        previous <- c(0L, 0L)
        for (i in seq_along(items)) {
            prefix <- member_source_prefix(content, previous, items[[i]]$end, prefix)
            previous <- items[[i]]$end
            items[[i]]$cache_prefix <- prefix
        }
    }
    # An enormous factory cannot be analyzed within a request. Retain its
    # generic descriptor/formals without duplicating its body in disk indexes.
    items <- lapply(items, function(item) {
        line <- item$start[[1L]] + 1L
        end <- item$end
        if (line >= 1L && line <= length(content) && end[[1L]] >= 0L && end[[1L]] < length(content) &&
                (member_head(item$expr, "<-") || member_head(item$expr, "=")) && member_head(item$expr[[3L]], "function")) {
            item$implementation_columns <- c(code_point_to_unit(content[[line]], item$start[[2L]]),
                code_point_to_unit(content[[end[[1L]] + 1L]], end[[2L]]))
        }
        start <- detect_comments(content, line - 1L) + 1L
        if (start < line && line - start <= 64L) item$comments <- content[seq.int(start, line - 1L)]
        if (as.numeric(utils::object.size(item$expr)) > 65536L) {
            expr <- item$expr
            if ((member_head(expr, "<-") || member_head(expr, "=")) && member_head(expr[[3L]], "function")) {
                if (is.null(s3_generic_descriptor(expr[[3L]]))) expr[[3L]][[3L]] <- quote(.__s3_opaque_factory__)
            } else {
                expr <- quote(.__s3_opaque_effect__)
            }
            item$expr <- expr
        }
        item
    })
    inherited <- if (is.null(parse_data$member_data)) roxygen_has_inheritance(content) else
        isTRUE(attr(parse_data$member_data$type_hints, "has_inheritance", exact = TRUE))
    list(uri = uri, items = items, index = s3_source_index(items), hint_inheritance = inherited,
        member_hints = s3_has_member_hints(hints, classes),
        content_key = parse_data$member_data$content_key %s3_or% digest::digest(content, algo = "xxhash64"))
}

s3_recover_call <- function(document, location, recovered) {
    start <- recovered$start
    last <- min(document$nline, location$point$row + 129L)
    if (document$is_rmarkdown) {
        cell <- literate_r_cell_at(document$regions, location$point$row)
        if (is.null(cell)) return(NULL)
        if (start[[1L]] < cell$body_start - 1L) start <- c(cell$body_start - 1L, 0L)
        last <- min(last, cell$body_end)
    }
    lines <- document_cursor_lines(document, seq.int(start[[1L]] + 1L, last), location$point$row)
    offset <- location$point$row - start[[1L]] + 1L
    lines[[offset]] <- paste0(
        location$cursor$before, recovered$sentinel,
        substring(lines[[offset]], location$cursor$end + 1L)
    )
    original <- lines[[1L]]
    lines[[1L]] <- sub("^[ \t]*;[ \t]*", "", substring(original, start[[2L]] + 1L))
    first_col <- nchar(original) - nchar(lines[[1L]])
    # Both the normal read and the closing-delimiter recovery retain hints on
    # the same function as its original declaration.
    hints <- document$parse_data$member_data$type_hints
    parse_call <- if (length(hints) || length(attr(hints, "classes", exact = TRUE)) ||
            roxygen_has_type_span(lines) || roxygen_has_inheritance(lines)) function(text) {
        member_parse_recovered(text, NULL, start[[1L]], first_col, type_hints = hints,
            hint_comments = document$parse_data$member_data$hint_comments)
    } else str2expression
    for (end in seq.int(offset, length(lines))) {
        text <- lines[seq_len(end)]
        if (sum(nchar(text, type = "bytes")) > 65536L) break
        parsed <- tryCatch(parse_call(text), error = function(e) NULL)
        if (!is.null(parsed) && !is.null(member_cursor_path(parsed, recovered$sentinel, NULL))) {
            return(parsed)
        }
    }
    for (end in rev(seq.int(offset, length(lines)))) {
        text <- lines[seq_len(end)]
        if (sum(nchar(text, type = "bytes")) > 65536L) next
        for (quote in c("", "\"", "'", "`")) {
            fixed <- text
            fixed[[length(fixed)]] <- paste0(fixed[[length(fixed)]], quote)
            parsed <- tryCatch(parse_call(paste0(
                paste(fixed, collapse = "\n"), "\n",
                missing_closing_delimiters(fixed)
            )), error = function(e) NULL)
            if (!is.null(parsed) && !is.null(member_cursor_path(parsed, recovered$sentinel, NULL))) {
                return(parsed)
            }
        }
    }
    NULL
}

s3_pipe_arguments <- function(call) {
    args <- as.list(call)[-1L]
    if (any(vapply(args, identical, logical(1L), quote(.)))) return(call)
    as.call(c(list(call[[1L]], quote(.)), args))
}

s3_pipe_call <- function(lhs, call) {
    if (!is.call(call) || member_head(call, "{")) {
        return(NULL)
    }
    replace <- function(node) {
        if (is.symbol(node) && identical(member_name(node), ".")) {
            return(lhs)
        }
        if (is.call(node)) for (i in seq_along(node)) node[i] <- list(replace(node[[i]]))
        node
    }
    replace(s3_pipe_arguments(call))
}

s3_catalog_ordinary_call <- function(catalog, token) {
    token %in% catalog$callables && is.null(catalog$functions[[token]]$generic)
}

s3_peer_packages <- function(workspace, document) {
    if (is.function(workspace$get_s3_package_imports)) {
        cached <- workspace$get_s3_package_imports(document$uri)
        if (!is.null(cached)) return(cached)
    }
    packages <- character()
    add_source <- function(source, packages) {
        for (import in source$index$imports) {
            packages <- union(packages, import$package)
        }
        packages
    }
    others <- workspace_document_uris(workspace, document$uri)
    for (uri in setdiff(others, document$uri)) {
        candidate <- workspace$documents$get(uri, NULL)
        if (!is.null(candidate) && identical(candidate$version, candidate$parse_data$version)) {
            packages <- add_source(s3_source_view(candidate, peer = TRUE), packages)
        }
    }
    index <- workspace$index
    if (isTRUE(index$enabled)) {
        root <- index$package_root_for_uri(document$uri)
        uris <- if (is.null(root)) index$source_closure(document$uri) else index$package_source_uris(root)
        for (uri in setdiff(uris, c(document$uri, others))) packages <- add_source(index$summaries$get(uri, NULL)$s3, packages)
    }
    if (is.function(workspace$set_s3_package_imports)) workspace$set_s3_package_imports(document$uri, packages)
    packages
}

s3_call_packages <- function(workspace, document, at) {
    packages <- unique(c(workspace$startup_packages %s3_or% startup_packages, workspace$imported_packages))
    source <- s3_source_view(document)
    for (import in source$index$imports) {
        if (member_before(source$items[[import$item]]$end, at)) packages <- union(packages, import$package)
    }
    recorded <- vapply(source$index$imports, function(import) import$package %s3_or% "", "")
    # Current-document imports depend on the cursor and remain uncached.
    # Nested library requests are still treated as possible masking bindings.
    packages <- union(packages, setdiff(document$requested_packages$packages, recorded))
    union(packages, s3_peer_packages(workspace, document))
}

s3_known_ordinary_call <- function(metadata, token, packages) {
    # Inspect the first possible binding in search-path order. An unresolved
    # attached package may mask every callable in an earlier namespace.
    for (package in rev(unique(packages))) {
        catalog <- metadata$catalog(package)
        if (is.null(catalog) || !is.null(catalog$error)) return(FALSE)
        if (token %in% catalog$callables) return(s3_catalog_ordinary_call(catalog, token))
    }
    FALSE
}

s3_source_binding <- function(workspace, document, at, token) {
    # Navigation retains source backticks; call tokens can be unquoted.
    for (name in unique(c(token, encodeString(token, quote = "`")))) {
        if (isTRUE(workspace$index$enabled) && workspace$index$has_definition(name, uri = document$uri)) return(TRUE)
        if (length(navigation_definitions(document$parse_data, at[[1L]] + 1L, at[[2L]] + 1L,
                    name, last = TRUE))) return(TRUE)
    }
    FALSE
}

s3_may_dispatch <- function(workspace, document, location, packages = NULL) {
    if (!is.null(location$call$package) && is.function(workspace$s3_metadata$catalog)) {
        catalog <- workspace$s3_metadata$catalog(location$call$package)
        if (s3_catalog_ordinary_call(catalog, location$call$token)) return(FALSE)
    }
    data <- document$parse_data$member_data
    at <- c(location$point$row, location$point$col)
    metadata <- workspace$s3_metadata
    # A nested ordinary call needs no S3 context unless a known generic or
    # a source binding (including formals and aliases) can change its meaning.
    if (is.null(location$call$package) && !isTRUE(document$parse_data$parse_error) &&
            !is.null(document$parse_data$syntax_data) && !is.null(document$parse_data$navigation_data) &&
            is.function(metadata$catalog) && !s3_source_binding(workspace, document, at, location$call$token)) {
        packages <- packages %s3_or% s3_call_packages(workspace, document, at)
        if (s3_known_ordinary_call(metadata, location$call$token, packages)) return(FALSE)
    }
    # Lexical scopes and aliases use full recovery; ordinary top-level calls
    # can be rejected by their source declaration or the compact catalog.
    lo <- 1L
    hi <- length(data$items)
    while (lo <= hi) {
        middle <- as.integer(floor((lo + hi) / 2L))
        if (member_before(data$items[[middle]]$end, at)) lo <- middle + 1L else hi <- middle - 1L
    }
    if (lo <= length(data$items)) {
        item <- data$items[[lo]]
        if (member_before(item$start, at) && !is.null(member_item_source(document, data, lo))) {
            return(TRUE)
        }
    }
    if (is.null(location$call$package)) {
        history <- data$s3$histories[[location$call$token]]
        position <- member_history_position(history, at)
        if (position) {
            record <- history[[position]]
            expr <- data$items[[record$item]]$expr
            rhs <- if (member_head(expr, "<-") || member_head(expr, "=")) expr[[3L]] else expr
            return(!member_head(rhs, "function") || !is.null(s3_generic_descriptor(rhs)))
        }
    }
    if (is.null(location$call$package) && !is.null(workspace$index) && isTRUE(workspace$index$enabled)) {
        if (workspace$index$has_definition(location$call$token)) {
            return(TRUE)
        }
    }
    metadata <- workspace$s3_metadata
    if (is.null(metadata)) {
        return(TRUE)
    }
    if (is.function(metadata$may_dispatch) && !metadata$may_dispatch(location$call$token)) {
        return(FALSE)
    }
    packages <- if (!is.null(location$call$package)) location$call$package else metadata$keys()
    for (package in packages) {
        record <- metadata$catalog(package)$functions[[location$call$token]]
        if (!is.null(record$generic)) {
            return(TRUE)
        }
    }
    FALSE
}

s3_catalog <- function(context, package) {
    catalog <- context$catalogs[[package]]
    if (isTRUE(catalog$.lazy)) {
        catalog <- context$metadata$get(package)
        context$catalogs[[package]] <- catalog
    }
    catalog
}

#' Check existing namespaces without loading packages or preparing S3 catalogs
#' @noRd
s3_cached_callee <- function(workspace, document, location, packages) {
    if (is.null(workspace$namespaces)) return(FALSE)
    package <- location$call$package
    if (is.null(package) && s3_source_binding(workspace, document,
            c(location$point$row, location$point$col), location$call$token)) return(FALSE)
    visible <- if (is.null(package)) rev(unique(packages)) else package
    loaded <- loadedNamespaces()
    for (candidate in visible) {
        if (!candidate %in% loaded) return(FALSE)
        namespace <- workspace$namespaces$get(candidate, NULL)
        if (is.null(namespace)) return(FALSE)
        found <- tryCatch(namespace$exists_funct(location$call$token,
                exported_only = location$call$accessor != ":::"), error = function(e) FALSE)
        if (isTRUE(found)) {
            # Startup generics can still provide ordinary help if their
            # catalogs failed. For a requested package being prepared, a
            # cached generic signature must not outrun its method catalog.
            catalog <- workspace$s3_metadata$catalog(candidate)
            if (candidate %in% startup_packages || (!is.null(catalog) && is.null(catalog$error))) return(TRUE)
            binding <- s3_namespace_binding(getNamespace(candidate), location$call$token, materialize = FALSE)
            if (!identical(binding[[1L]], "value") || !is.function(binding[[2L]])) return(FALSE)
            fn <- binding[[2L]]
            if (is.primitive(fn)) return(TRUE)
            syntax <- as.call(list(as.name("function"), formals(fn), body(fn)))
            return(is.null(s3_generic_descriptor(syntax)))
        }
    }
    FALSE
}

s3_member_constructor_ready <- function(workspace, document, location, packages) {
    if (!isTRUE(lsp_settings$get("member_completion")) || !member_symbol_required(workspace, document)) return(FALSE)
    metadata <- workspace$member_metadata
    if (is.null(metadata) || !is.function(metadata$catalog)) return(FALSE)
    required <- if (is.null(location$call$package)) setdiff(packages, startup_packages) else location$call$package
    ready <- vapply(required, function(package) {
        catalog <- metadata$catalog(package)
        !is.null(catalog) && is.null(catalog$error) && !isTRUE(catalog$dataset_only)
    }, logical(1L))
    # Missing attached metadata may mask the factory used by a declaration.
    if (!all(ready)) return(FALSE)
    symbol <- member_constructor_symbol(document$uri, workspace, document, location)
    !is.null(symbol$signature)
}

s3_hint_worker_waiting <- function(workspace, document, manager, types = NULL) {
    if (is.null(workspace$s3_hint_workers) || !workspace$s3_hint_workers$has(document$uri) || !manager$has(document$uri)) return(FALSE)
    data <- document$parse_data$member_data
    packages <- if (is.null(types)) data$type_hint_packages else unique(unlist(data$type_hint_dependencies[types], use.names = FALSE))
    metadata <- workspace$member_metadata
    any(vapply(packages, function(package) {
        catalog <- if (is.function(metadata$catalog)) metadata$catalog(package) else if (is.function(metadata$get)) metadata$get(package, NULL)
        is.null(catalog) || isTRUE(catalog$dataset_only) || !is.null(catalog$error)
    }, logical(1L)))
}

# An inherited donor can be known to the shallow package index before its
# document has been parsed. Promote only the source actually consulted, then
# use the ordinary class-model worker once its parse has been accepted.
s3_record_topic_wait <- function(wait, workspace, root) {
    if (!is.null(wait) && is.function(wait$server$queue_context_reply) &&
            is.function(workspace$index$package_topics_pending) && workspace$index$package_topics_pending(root)) {
        wait$topic_roots <- union(wait$topic_roots, root)
    }
    invisible(NULL)
}

s3_record_hint_wait <- function(wait, workspace, document, types, uri = document$uri) {
    if (is.null(wait) || !length(uri)) return(invisible(NULL))
    if (is.null(document)) {
        if (!is.function(wait$server$queue_context_reply) ||
                !is.function(workspace$index$helpers_pending)) return(invisible(NULL))
        if (!workspace$documents$has(uri)) {
            if (!is.function(wait$server$load_index_document)) return(invisible(NULL))
            wait$server$load_index_document(workspace, uri)
        }
        indexed_uri <- index_canonical_uri(uri)
        if (workspace$index$helpers_pending(indexed_uri)) {
            wait$parses <- union(wait$parses, indexed_uri)
            if (is.function(wait$server$parse_task_manager$promote)) wait$server$parse_task_manager$promote(uri)
        }
        document <- workspace$documents$get(uri, NULL)
    }
    if (!is.null(document) && s3_hint_worker_waiting(workspace, document, wait$manager, types)) {
        wait$owners[[uri]] <- union(wait$owners[[uri]], types)
    }
    invisible(NULL)
}

hint_request_blocked <- function(wait, self, workspace, uri, document, request_id) {
    if (length(wait$parses)) self$queue_context_reply(uri, workspace, helpers = wait$parses)
    for (root in wait$topic_roots) {
        self$queue_context_reply(uri, workspace, topic_root = root, request_id = request_id, version = document$version)
    }
    workers <- workspace$s3_hint_workers
    caller <- if (identical(document$uri, uri)) document else workspace$documents$get(uri, NULL)
    for (owner in setdiff(names(wait$owners), uri)) {
        if (!is.null(request_id)) {
            waiters <- workers$get(owner)
            waiters[[request_id_key(request_id)]] <- list(uri = uri, version = document$version, types = wait$owners[[owner]])
            workers$set(owner, waiters)
            caller$s3_hint_subscriber <- TRUE
        }
        if (is.function(wait$manager$promote)) wait$manager$promote(owner)
    }
    length(wait$owners) > 0L || length(wait$parses) > 0L || length(wait$topic_roots) > 0L
}

hint_package_inherits <- function(workspace, document) {
    if (!roxygen_type_hints_enabled()) return(FALSE)
    isTRUE(attr(document$parse_data$member_data$type_hints, "has_inheritance", exact = TRUE)) ||
        (is.function(workspace$index$package_has_hint_inheritance) && workspace$index$package_has_hint_inheritance(document$uri))
}

hint_package_cold <- function(self, workspace, document) {
    index <- workspace$index
    if (!isTRUE(index$enabled) || !is.function(index$package_root_for_uri)) return(FALSE)
    root <- index$package_root_for_uri(document$uri)
    if (is.null(root)) return(FALSE)
    if (is.function(index$package_topics_pending) && index$package_topics_pending(root)) return(TRUE)
    if (is.function(self$parse_task_manager$has_work) && self$parse_task_manager$has_work()) return(TRUE)
    if (!is.function(self$load_index_document) || !is.function(index$package_source_uris)) return(FALSE)
    revision <- c(index$revision, index$topology_revision, workspace$s3_revision, workspace$documents$size())
    cache <- analysis_cache(workspace, "cold-hint-sources", revision, max_bytes = 64 * 1024L)
    found <- analysis_cache_get(cache, root)
    if (found$hit) return(found$value)
    missing <- setdiff(index$package_source_uris(root), workspace$documents$keys())
    cold <- any(vapply(missing, function(uri) isTRUE(index$summaries$get(uri, NULL)$has_type_hints), logical(1L)))
    analysis_cache_set(cache, root, cold)
    cold
}

s3_metadata_waiting <- function(self, uri, document, point = NULL, hover = FALSE, call = NULL, request_id = NULL) {
    if (!isTRUE(lsp_settings$get("s3_dispatch"))) return(FALSE)
    resolve_pending <- is.function(self$resolve_task_manager$has) && self$resolve_task_manager$has(uri)
    manager <- self$member_task_manager %s3_or% self$resolve_task_manager
    package_source <- !is.null(document$parse_data$member_data$document_package)
    if (!resolve_pending && (!is.function(manager$has) || (!package_source && is.function(manager$has_work) && !manager$has_work()))) return(FALSE)
    workspace <- if (is.function(self$get_workspace)) self$get_workspace(uri)
    workers <- workspace$s3_hint_workers
    active <- !is.null(workers) && workers$size() && is.function(manager$has) && roxygen_type_hints_enabled()
    cold <- package_source && !is.null(point) && hint_package_inherits(workspace, document) && hint_package_cold(self, workspace, document)
    if (active || cold) {
        caller <- if (identical(document$uri, uri)) document else workspace$documents$get(uri, NULL)
        if (!is.null(request_id) && isTRUE(caller$s3_hint_subscriber)) {
            s3_forget_hint_waiters(workspace, uri, request_id, caller)
        }
        if (is.null(point)) {
            if (s3_hint_worker_waiting(workspace, document, manager)) return(TRUE)
        } else if (cold || workers$has(uri) || any(workers$keys() %in% workspace_document_uris(workspace, uri))) {
            # Resolve in the caller's scope and inspect only the class models
            # actually read. Other queued documents do not enter this search.
            wait <- new.env(parent = emptyenv())
            wait$manager <- manager
            wait$server <- self
            resolve_s3_call(uri, workspace, document, point, hover, call, hint_wait = wait)
            if (hint_request_blocked(wait, self, workspace, uri, document, request_id)) return(TRUE)
        }
    }
    if (!resolve_pending) return(FALSE)
    packages <- character()
    if (!is.null(point)) {
        location <- s3_call_location(document, point, hover, call)
        if (is.null(location)) return(FALSE)
        # A pending library() or namespace request must not hold ordinary
        # functions hostage to metadata needed only for S3 dispatch.
        package <- location$call$package
        if (!is.null(package)) {
            catalog <- workspace$s3_metadata$catalog(package)
            if (s3_catalog_ordinary_call(catalog, location$call$token)) return(FALSE)
        }
        packages <- s3_call_packages(workspace, document, c(location$point$row, location$point$col))
        # Constructor formals come from member metadata and cannot be refined
        # by S3 dispatch catalogs. Share the same verified constructor path as
        # signature/hover providers instead of blocking on an unrelated worker.
        if (s3_member_constructor_ready(workspace, document, location, packages)) return(FALSE)
        # Missing S3 catalogs affect method-specific refinement, not an
        # already-known callable's ordinary signature/help. A missing cached
        # namespace still blocks this path when it could mask the callee.
        if (s3_cached_callee(workspace, document, location, packages)) return(FALSE)
        if (!s3_may_dispatch(workspace, document, location, packages)) {
            history <- document$parse_data$member_data$s3$histories[[location$call$token]]
            if (is.null(package) && member_history_position(history, c(point$row, point$col))) return(FALSE)
            if (is.null(package) && s3_known_ordinary_call(workspace$s3_metadata, location$call$token, packages)) return(FALSE)
            # Unknown calls may belong to a generic in the package currently
            # being resolved. Preserve the wait until its catalog arrives.
        }
    }
    requested <- unique(c(startup_packages, document$requested_packages$packages, document$requested_packages$namespace_packages, packages))
    any(vapply(requested, function(package) {
        catalog <- workspace$s3_metadata$catalog(package)
        is.null(catalog) || !is.null(catalog$error)
    }, logical(1L)))
}

s3_queue_ready_document <- function(self, workspace, uri) {
    if (is.null(self$pending_replies) || !is.function(self$pending_replies$get)) {
        return(invisible(NULL))
    }
    document <- workspace$documents$get(uri, NULL)
    if (is.null(document)) {
        return(invisible(NULL))
    }
    if (is.function(self$queue_pending_replies)) {
        self$queue_pending_replies(uri)
    } else {
        drain_pending_replies(self, uri, document$version)
    }
}

s3_forget_hint_waiters <- function(workspace, uri, request_id = NULL, document = workspace$documents$get(uri, NULL)) {
    workers <- workspace$s3_hint_workers
    removed <- character()
    if (is.null(workers) || !workers$size() || !isTRUE(document$s3_hint_subscriber)) return(invisible(removed))
    retained <- FALSE
    for (owner in workers$keys()) {
        waiters <- workers$get(owner)
        keys <- if (is.null(request_id)) names(waiters) else request_id_key(request_id)
        for (key in keys) {
            if (identical(waiters[[key]]$uri, uri)) {
                waiters[[key]] <- NULL
                removed <- union(removed, key)
            }
        }
        if (length(waiters) != length(workers$get(owner))) workers$set(owner, waiters)
        retained <- retained || any(vapply(waiters, function(waiter) identical(waiter$uri, uri), logical(1L)))
    }
    document$s3_hint_subscriber <- retained
    invisible(removed)
}

s3_pending_hint_request <- function(self, uri, keys, version) {
    if (!length(keys) || is.null(self$pending_replies)) return(FALSE)
    queues <- self$pending_replies$get(uri, NULL)
    any(vapply(queues, function(queue) {
        queue$size() && any(vapply(queue$as_list(), function(item) {
            identical(item$version, version) && request_id_key(item$id) %in% keys
        }, logical(1L)))
    }, logical(1L)))
}

s3_wake_hint_dependents <- function(self, workspace, uri, forget = FALSE, recheck = FALSE) {
    workers <- workspace$s3_hint_workers
    if (is.null(workers) || !workers$has(uri)) return(invisible(NULL))
    waiters <- workers$get(uri)
    manager <- self$member_task_manager %s3_or% self$resolve_task_manager
    document <- workspace$documents$get(uri, NULL)
    finished <- forget || is.null(document) || !is.function(manager$has) || !s3_hint_worker_waiting(workspace, document, manager)
    ready <- character()
    for (key in names(waiters)) {
        waiter <- waiters[[key]]
        # A completion or cancellation can supersede the original request
        # while another provider for this URI remains queued.
        if (s3_pending_hint_request(self, waiter$uri, key, waiter$version)) {
            if (!finished && !recheck && s3_hint_worker_waiting(workspace, document, manager, waiter$types)) next
            ready <- union(ready, waiter$uri)
        }
        waiters[[key]] <- NULL
    }
    if (finished) {
        workers$remove(uri)
        if (!is.null(document)) document$s3_hint_worker <- FALSE
    } else {
        workers$set(uri, waiters)
    }
    for (caller in ready) s3_queue_ready_document(self, workspace, caller)
    invisible(NULL)
}

s3_queue_ready <- function(self, uri) {
    if (!is.function(self$get_workspace)) return(invisible(NULL))
    workspace <- self$get_workspace(uri)
    s3_wake_hint_dependents(self, workspace, uri)
    s3_queue_ready_document(self, workspace, uri)
}

s3_release_hint_document <- function(self, workspace, uri, document) {
    if (is.null(workspace$s3_hint_workers) || !workspace$s3_hint_workers$size()) return(invisible(NULL))
    s3_forget_hint_waiters(workspace, uri, document = document)
    s3_wake_hint_dependents(self, workspace, uri, forget = TRUE)
    if (!is.null(document)) document$s3_hint_worker <- FALSE
    invisible(NULL)
}

s3_namespace_snapshot <- function(root, owner = NULL) {
    path <- file.path(root, "NAMESPACE")
    if (!file.exists(path)) return(list(key = "", records = list(), valid = TRUE))
    if (file.info(path)$size > 65536L) return(list(key = "oversized", records = list(), valid = FALSE))
    text <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
    if (is.null(text)) return(list(key = "unreadable", records = list(), valid = FALSE))
    key <- digest::digest(text, algo = "xxhash64")
    cache <- if (!is.null(owner)) analysis_cache(owner, "namespace-declarations", max_bytes = 1024^2, max_entries = 32L)
    cached <- analysis_cache_get(cache, paste(path, key, sep = ":"))
    if (cached$hit) return(cached$value)
    code <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) NULL)
    records <- list()
    for (expr in code) {
        if (!member_head(expr, "S3method") || length(expr) < 3L) next
        generic <- s3_call_name(expr[[2L]])
        if (is.null(generic)) generic <- list(name = member_name(expr[[2L]]), package = NULL)
        class <- member_name(expr[[3L]])
        if (is.null(generic$name) || is.null(class)) next
        method <- if (length(expr) > 3L) member_name(expr[[4L]]) else paste(generic$name, class, sep = ".")
        records[[length(records) + 1L]] <- list(
            generic = generic$name, package = generic$package,
            class = class, method = method
        )
    }
    value <- list(key = key, records = records, valid = !is.null(code))
    analysis_cache_set(cache, paste(path, key, sep = ":"), value)
}

s3_namespace_source <- function(root) s3_namespace_snapshot(root)$records

s3_package_root <- function(workspace, uri) {
    root <- if (!is.null(workspace$index) && isTRUE(workspace$index$enabled)) {
        workspace$index$package_root_for_uri(uri)
    }
    if (is.null(root) && length(workspace$root) == 1L && is_package(workspace$root)) root <- workspace$root
    root
}

s3_namespace_generation <- function(workspace, uri) {
    root <- s3_package_root(workspace, uri)
    if (is.null(root)) return("")
    s3_namespace_snapshot(root, workspace)$key
}
