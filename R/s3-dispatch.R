# Source histories refer to accepted syntax. Global values are resolved lazily
# at the call position; an alias keeps the value from before its assignment.
s3_source_index <- function(items) {
    histories <- new.env(hash = TRUE, parent = emptyenv())
    functions <- new.env(hash = TRUE, parent = emptyenv())
    imports <- registrations <- list()
    for (i in seq_along(items)) {
        item <- items[[i]]
        expr <- item$expr
        names <- character()
        kind <- "effect"
        if (member_head(expr, "<-") || member_head(expr, "=") || member_head(expr, "->")) {
            lhs <- if (member_head(expr, "->")) expr[[3L]] else expr[[2L]]
            rhs <- if (member_head(expr, "->")) expr[[2L]] else expr[[3L]]
            if (is.symbol(lhs)) {
                names <- member_name(lhs)
                kind <- "binding"
                if (member_head(rhs, "function")) functions[[names]] <- s3_generic_descriptor(rhs)
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
        registrations = registrations
    )
}

s3_source_view <- function(document) {
    data <- document$parse_data$member_data
    if (is.null(data)) {
        return(NULL)
    }
    list(index = data$s3 %s3_or% s3_source_index(data$items), items = data$items,
        uri = document$uri, content_key = data$content_key)
}

s3_source_value <- function(name, context) {
    for (source in context$sources) {
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
        cache_key <- if (!is.null(context$binding_cache) && s3_binding_context_unchanged(context) && !is.null(item$cache_prefix) &&
                !is.null(context$binding_contexts[[source$uri]])) {
            paste(source$uri, name, position, item$cache_prefix, context$binding_contexts[[source$uri]], sep = ":")
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
            rhs <- if (member_head(expr, "->")) expr[[2L]] else expr[[3L]]
            result <- context$infer(rhs, context$root)
            if (!is.null(result$fn)) {
                result$source <- list(
                    uri = source$uri, line = item$start[[1L]] + 1L,
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

s3_lookup_method <- function(generic, classes, scope, context, owner = "source") {
    if (!length(classes) || anyNA(classes) || isTRUE(context$registration_unknown)) {
        return(NULL)
    }
    registrations <- get0(".registrations", scope, inherits = TRUE)
    for (class in c(classes, "default")) {
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
        registered <- registrations[[key]] %s3_or% namespace
        if (!is.null(registered$fn)) {
            if (!is.null(registered$info)) {
                if (is.null(registered$info$formals)) return(NULL)
                method <- registered$info
                method$binding_source <- registered$source
                return(method)
            }
            return(list(
                name = registered$name %s3_or% key, fn = registered$fn,
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

s3_context <- function(uri, workspace, document, at) {
    context <- new.env(parent = emptyenv())
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
    context$sources <- list(s3_source_view(document))
    others <- if (!is.null(workspace$documents)) workspace_document_uris(workspace, uri) else character()
    for (other in setdiff(others, uri)) {
        candidate <- workspace$documents$get(other, NULL)
        if (!is.null(candidate) && identical(candidate$version, candidate$parse_data$version)) {
            context$sources <- c(context$sources, list(s3_source_view(candidate)))
        }
    }
    if (!is.null(workspace$index) && isTRUE(workspace$index$enabled)) {
        package_root <- workspace$index$package_root_for_uri(uri)
        uris <- if (!is.null(package_root)) workspace$index$package_source_uris(package_root) else workspace$index$source_closure(uri, update = FALSE)
        for (other in setdiff(uris, c(uri, others))) {
            summary <- workspace$index$summaries$get(other, NULL)
            if (!is.null(summary$s3)) context$sources <- c(context$sources, list(summary$s3))
        }
    }
    context$sources <- Filter(Negate(is.null), context$sources)
    context$imports <- workspace$imported_packages %s3_or% character()
    for (source in context$sources) {
        for (import in source$index$imports) {
            item <- source$items[[import$item]]
            if (!identical(source$uri, uri) || member_before(item$end, at)) {
                context$imports <- union(context$imports, import$package)
            }
        }
    }
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
    for (source in context$sources) {
        for (i in source$index$registrations) {
            item <- source$items[[i]]
            if (!identical(source$uri, uri) || member_before(item$end, at)) {
                old_at <- context$at
                old_uri <- context$source_uri
                context$at <- item$start
                context$source_uri <- source$uri
                analyzer$infer(item$expr, context$root)
                context$at <- old_at
                context$source_uri <- old_uri
                if (isTRUE(get0(".dynamic_registration", context$root))) context$registration_unknown <- TRUE
            }
        }
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
    context$binding_contexts <- list()
    if (!isTRUE(context$incomplete) && !isTRUE(context$registration_unknown) && !isTRUE(context$exhausted) &&
            is.function(metadata$revision) && is.function(metadata$identity) &&
            all(vapply(context$sources, function(source) !is.null(source$content_key), logical(1L)))) {
        context$binding_cache <- analysis_cache(document, "s3-bindings",
            list(metadata = metadata$identity(), revision = metadata$revision(), namespace = namespace$key),
            max_bytes = 4 * 1024^2)
        source_keys <- vapply(context$sources, `[[`, "", "content_key")
        source_uris <- vapply(context$sources, `[[`, "", "uri")
        registrations <- unlist(lapply(context$sources, function(source) {
            vapply(source$index$registrations, function(i) {
                item <- source$items[[i]]
                if (!identical(source$uri, uri) || member_before(item$end, at)) item$cache_prefix %s3_or% "" else ""
            }, "")
        }), use.names = FALSE)
        for (i in seq_along(source_uris)) {
            context$binding_contexts[[source_uris[[i]]]] <- digest::digest(list(
                sources = source_uris[-i], content = source_keys[-i], imports = context$imports,
                registrations = registrations, startup = startup_packages), algo = "xxhash64")
        }
    }
    context
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
    location <- member_call_location(document, point, call, symbols = TRUE)
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
    pipe_lhs <- NULL
    scope <- context$root
    ast <- integer()
    for (step in path) {
        if (is.call(node) && member_name(node[[1L]]) %in% c("%>%", "%<>%", "%T>%") && step == 3L) {
            if (!is.null(s3_get_value(member_name(node[[1L]]), scope, context, source_only = TRUE))) return(NULL)
            pipe_lhs <- node[[2L]]
        }
        if (member_head(node, "function")) {
            scope <- new.env(parent = scope)
            for (name in names(node[[2L]])) assign(name, s3_unknown("formal"), scope)
        } else if (member_head(node, "{") || is.expression(node)) {
            children <- if (is.expression(node)) as.list(node) else as.list(node)[-1L]
            target <- step - if (is.expression(node)) 0L else 1L
            summary <- recovered$source$scopes[[member_source_key(ast)]]
            selected <- if (!is.null(summary)) {
                member_scope_slice(
                    summary, target,
                    member_syntax_names(children[[target]])
                )
            } else {
                seq_len(target)
            }
            for (i in selected[selected < target]) context$infer(children[[i]], scope)
        }
        if (is.call(node) && step == 1L && ((member_head(node[[step]], "::") || member_head(
            node[[step]],
            ":::"
        )) && identical(member_name(node[[step]][[3L]]), recovered$sentinel))) {
            node[[1L]][[3L]] <- as.name(location$call$token)
            if (!is.null(pipe_lhs)) node <- s3_pipe_call(pipe_lhs, node)
            return(list(call = node, scope = scope))
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
            if (!is.null(pipe_lhs)) node <- s3_pipe_call(pipe_lhs, node)
            return(list(call = node, scope = scope, root = root, path = ast))
        }
        node <- node[[step]]
        ast <- c(ast, step)
    }
    NULL
}

s3_match_arguments <- function(formals, actuals) {
    keys <- names(formals)
    names <- names(actuals) %s3_or% rep("", length(actuals))
    result <- rep(NA_integer_, length(actuals))
    used <- integer()
    dots <- match("...", keys)
    before <- if (is.na(dots)) seq_along(keys) else seq_len(dots - 1L)
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

resolve_s3_call <- function(uri, workspace, document, point, hover = FALSE, call = NULL) {
    if (!isTRUE(lsp_settings$get("s3_dispatch")) ||
            !identical(document$version, document$parse_data$version)) {
        return(NULL)
    }
    location <- s3_call_location(document, point, hover, call)
    if (is.null(location) || !s3_may_dispatch(workspace, document, location)) {
        return(NULL)
    }
    revision <- if (is.function(workspace$s3_metadata$revision)) workspace$s3_metadata$revision() else NULL
    identity <- if (is.function(workspace$s3_metadata$identity)) workspace$s3_metadata$identity() else workspace$s3_metadata
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
    if (!is.null(revision) && cache$has(cache_key)) {
        return(cache$get(cache_key))
    }
    result <- NULL
    context <- NULL
    on.exit(if (!is.null(revision) && !isTRUE(context$exhausted)) {
        cache$set(cache_key, result)
    }, add = TRUE)
    context <- s3_context(uri, workspace, document, c(location$point$row, location$point$col))
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
    owner <- if (!is.null(callee$info)) callee$info$owner else "source"
    method <- s3_lookup_method(descriptor$name, input$classes, scoped$scope, context, owner)
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
        description = sprintf("S3 method `%s` for class `%s`.", method$name %s3_or% paste(descriptor$name,
                input$classes[[1L]],
                sep = "."
            ), paste(input$classes, collapse = ", "))
    )
    result$method$closure <- NULL
    result$method$fn <- NULL
    result
}

s3_index_summary <- function(content, uri, expressions = NULL, parse_data = NULL) {
    if (!is.null(parse_data$member_data)) {
        items <- parse_data$member_data$items
    } else if (!is.null(expressions)) {
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
    list(uri = uri, items = items, index = s3_source_index(items),
        content_key = parse_data$member_data$content_key %s3_or% digest::digest(content, algo = "xxhash64"))
}

s3_recover_call <- function(document, location, recovered) {
    source <- normalize_parse_content(document$content, document$is_rmarkdown)
    start <- recovered$start
    last <- min(length(source), location$point$row + 129L)
    lines <- source[seq.int(start[[1L]] + 1L, last)]
    offset <- location$point$row - start[[1L]] + 1L
    lines[[offset]] <- paste0(
        location$cursor$before, recovered$sentinel,
        substring(source[[location$point$row + 1L]], location$cursor$end + 1L)
    )
    lines[[1L]] <- sub("^[ \t]*;[ \t]*", "", substring(lines[[1L]], start[[2L]] + 1L))
    for (end in seq.int(offset, length(lines))) {
        text <- lines[seq_len(end)]
        if (sum(nchar(text, type = "bytes")) > 65536L) break
        parsed <- tryCatch(str2expression(text), error = function(e) NULL)
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
            parsed <- tryCatch(str2expression(paste0(
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

s3_pipe_call <- function(lhs, call) {
    if (!is.call(call) || member_head(call, "{")) {
        return(NULL)
    }
    direct <- any(vapply(as.list(call)[-1L], function(x) is.symbol(x) && identical(member_name(x), "."), logical(1L)))
    replace <- function(node) {
        if (is.symbol(node) && identical(member_name(node), ".")) {
            return(lhs)
        }
        if (is.call(node)) for (i in seq_along(node)) node[i] <- list(replace(node[[i]]))
        node
    }
    call <- replace(call)
    if (!direct) call <- as.call(c(list(call[[1L]]), list(lhs), as.list(call)[-1L]))
    call
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
            packages <- add_source(s3_source_view(candidate), packages)
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
        if (member_before(item$start, at) && !is.null(item$source)) {
            return(TRUE)
        }
    }
    if (is.null(location$call$package)) {
        history <- data$s3$histories[[location$call$token]]
        position <- member_history_position(history, at)
        if (position) {
            record <- history[[position]]
            expr <- data$items[[record$item]]$expr
            rhs <- if (member_head(expr, "->")) {
                expr[[2L]]
            } else if (member_head(expr, "<-") || member_head(
                expr,
                "="
            )) {
                expr[[3L]]
            } else {
                expr
            }
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
        if (isTRUE(found)) return(TRUE)
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

s3_metadata_waiting <- function(self, uri, document, point = NULL, hover = FALSE, call = NULL) {
    if (!isTRUE(lsp_settings$get("s3_dispatch")) || is.null(self$resolve_task_manager) ||
            !is.function(self$resolve_task_manager$has) ||
            !self$resolve_task_manager$has(uri)) {
        return(FALSE)
    }
    workspace <- self$get_workspace(uri)
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
            if (s3_known_ordinary_call(workspace$s3_metadata, location$call$token, packages)) return(FALSE)
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

s3_queue_ready <- function(self, uri) {
    if (is.null(self$pending_replies) || !is.function(self$pending_replies$get)) {
        return(invisible(NULL))
    }
    document <- self$get_workspace(uri)$documents$get(uri, NULL)
    if (is.null(document)) {
        return(invisible(NULL))
    }
    if (is.function(self$queue_pending_replies)) {
        self$queue_pending_replies(uri)
    } else {
        drain_pending_replies(self, uri, document$version)
    }
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
