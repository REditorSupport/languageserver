# Inert namespace catalogs are prepared in the resolution worker. Request paths
# only read signatures, class summaries and registered method identities.
S3MetadataCache <- R6::R6Class("S3MetadataCache",
    private = list(snapshots = NULL, decoded = NULL, semantic_revision = 0L, cache_identity = NULL, generic_names = NULL),
    public = list(
        initialize = function(max_bytes = 16 * 1024^2, max_entries = 64L) {
            private$snapshots <- ByteLruCache$new(max_bytes, max_entries)
            private$decoded <- ByteLruCache$new(min(max_bytes, 8 * 1024^2), 8L)
            private$cache_identity <- new.env(parent = emptyenv())
            private$generic_names <- new.env(hash = TRUE, parent = emptyenv())
        },
        get = function(key, default = NULL) {
            if (private$decoded$has(key)) {
                return(private$decoded$get(key))
            }
            entry <- private$snapshots$get(key)
            if (is.null(entry)) {
                return(default)
            }
            value <- unserialize(memDecompress(entry$compressed, "gzip"))
            private$decoded$set(key, value)
            value
        },
        catalog = function(key) {
            entry <- private$snapshots$get(key)
            if (!is.null(entry)) entry$catalog
        },
        keys = function() private$snapshots$keys(),
        has = function(key) private$snapshots$has(key),
        may_dispatch = function(name) exists(name, private$generic_names, inherits = FALSE),
        revision = function() private$semantic_revision,
        identity = function() private$cache_identity,
        bytes = function() private$snapshots$bytes() + private$decoded$bytes(),
        set = function(key, value, protect = character()) {
            if (!is.list(value) || !identical(value$schema, 1L)) {
                return(invisible(NULL))
            }
            previous <- self$catalog(key)
            if (identical(previous$generation, value$generation)) {
                return(invisible(NULL))
            }
            catalog <- value[c("package", "generation", "dependencies", "callables", "error")]
            catalog$functions <- lapply(value$functions, function(fn) fn[c("name", "owner", "generic", "summary")])
            catalog$method_keys <- names(value$methods)
            catalog$object_keys <- names(value$objects)
            catalog$generic_names <- names(value$functions)[vapply(value$functions,
                    function(fn) !is.null(fn$generic), logical(1L))]
            catalog$.lazy <- TRUE
            private$snapshots$set(key, list(
                catalog = catalog,
                compressed = memCompress(serialize(value, NULL), "gzip")
            ), protect)
            private$decoded$remove(key)
            generics <- unique(unlist(lapply(private$snapshots$keys(), function(package) {
                private$snapshots$get(package)$catalog$generic_names
            }), use.names = FALSE))
            private$generic_names <- list2env(stats::setNames(rep(list(TRUE), length(generics)), generics),
                parent = emptyenv())
            private$semantic_revision <- private$semantic_revision + 1L
            invisible(value)
        },
        clear = function() {
            private$snapshots$clear()
            private$decoded$clear()
            private$generic_names <- new.env(hash = TRUE, parent = emptyenv())
            private$semantic_revision <- private$semantic_revision + 1L
        }
    )
)

s3_generic_descriptor <- function(fn) {
    if (!member_head(fn, "function")) {
        return(NULL)
    }
    body <- fn[[3L]]
    if (member_head(body, "{") && length(body) == 2L) body <- body[[2L]]
    if (member_head(body, "return") && length(body) == 2L) body <- body[[2L]]
    if (!member_head(body, "UseMethod") || length(body) < 2L ||
            !is.character(body[[2L]]) || length(body[[2L]]) != 1L) {
        return(NULL)
    }
    if (!length(fn[[2L]])) {
        return(NULL)
    }
    formal <- if (length(body) == 2L) names(fn[[2L]])[[1L]] else member_name(body[[3L]])
    if (!is.symbol(if (length(body) > 2L) body[[3L]] else as.name(formal)) ||
            is.null(formal) || !formal %in% names(fn[[2L]])) {
        return(NULL)
    }
    list(name = body[[2L]], formal = formal)
}

s3_summary_names <- list(
    base = c(
        "structure", "list", "data.frame", "c", ":", "identity", "invisible",
        "factor", "ordered", "as.Date", "as.POSIXct", "as.POSIXlt", "as.difftime",
        "matrix", "array", "table", "as.table", "as.data.frame", "as.numeric", "as.double",
        "as.integer", "as.character", "as.logical", "as.raw", "seq_len", "seq_along", "rep", "unclass"
    ),
    stats = c(
        "ts", "lm", "glm", "aov", "loess", "smooth.spline", "nls", "prcomp",
        "princomp", "hclust", "kmeans", "t.test", "wilcox.test", "cor.test", "chisq.test"
    ),
    utils = "read.csv", tibble = c("tibble", "as_tibble"),
    data.table = c("data.table", "as.data.table", "setDT", "setDF"),
    dplyr = c("group_by", "rowwise", "mutate", "select", "filter", "arrange", "summarise", "bind_rows", "left_join"),
    MASS = c("rlm", "glm.nb", "polr"), survival = c("coxph", "survfit"),
    rpart = "rpart", nlme = "lme"
)

s3_function_record <- function(fn, name, package) {
    if (!is.function(fn) || is.object(fn)) {
        return(NULL)
    }
    owner <- if (is.primitive(fn)) "base" else if (isNamespace(environment(fn))) unname(getNamespaceName(environment(fn))) else package
    syntax <- if (typeof(fn) == "closure") member_function_syntax(fn) else NULL
    arguments <- if (is.null(syntax)) args(fn) else NULL
    formals <- if (!is.null(syntax)) syntax[[2L]] else if (is.function(arguments)) formals(arguments) else NULL
    generic <- s3_generic_descriptor(syntax)
    summary <- NULL
    for (candidate in s3_summary_names[[owner]]) {
        original <- member_binding(asNamespace(owner), candidate, materialize = TRUE)
        if (identical(original[[1L]], "value") && identical(original[[2L]], fn)) {
            summary <- paste(owner, candidate, sep = "::")
            break
        }
    }
    list(
        name = name, package = package, owner = owner, formals = formals,
        body_hash = if (!is.null(syntax)) s3_function_identity(syntax, owner),
        generic = generic, summary = summary,
        identity = digest::digest(list(owner, formals, if (!is.null(generic) || !is.null(summary)) syntax),
            algo = "xxhash64"
        )
    )
}

s3_runtime_shape <- function(object, depth = 0L) {
    if (depth > 2L || isS4(object) || is.environment(object) || is.function(object)) {
        return(NULL)
    }
    dims <- attr(object, "dim", exact = TRUE)
    plain <- unclass(object)
    fields <- if (is.data.frame(object) && length(plain) <= 256L) {
        lapply(plain, s3_runtime_shape, depth = depth + 1L)
    }
    s3_value(classes = .class2(object), type = typeof(object), fields = fields, dimensions = dims,
        explicit_class = !is.null(attr(object, "class", exact = TRUE)))
}

s3_prepare_namespace <- function(package) {
    ns <- asNamespace(package)
    exports <- getNamespaceExports(ns)
    if (length(exports) > 10000L) stop("Namespace exceeds S3 metadata budget")
    functions <- objects <- methods <- list()
    callables <- character()
    for (name in exports) {
        binding <- s3_namespace_binding(ns, name)
        if (!identical(binding[[1L]], "value")) next
        if (is.function(binding[[2L]])) {
            callables <- c(callables, name)
            record <- s3_function_record(binding[[2L]], name, package)
            if (!is.null(record) && (!is.null(record$generic) || !is.null(record$summary))) functions[[name]] <- record
        }
    }
    # Namespace re-exports and registrations can contain actual closures rather
    # than names. Read cells with [[, and preserve the implementation's owner.
    registrations <- tryCatch(getNamespaceInfo(ns, "S3methods"), error = function(e) NULL)
    if (is.matrix(registrations)) {
        for (i in seq_len(nrow(registrations))) {
            generic <- registrations[[i, 1L]]
            class <- registrations[[i, 2L]]
            method <- registrations[[i, 3L]]
            if (!is.character(generic) || !is.character(class)) next
            key <- paste(generic, class, sep = ".")
            generic_fn <- tryCatch(get(generic, ns), error = function(e) NULL)
            owner <- if (is.function(generic_fn) && isNamespace(environment(generic_fn))) {
                unname(getNamespaceName(environment(generic_fn)))
            } else if (is.primitive(generic_fn)) {
                "base"
            } else {
                NULL
            }
            table <- if (!is.null(owner)) get0(".__S3MethodsTable__.", asNamespace(owner), inherits = FALSE)
            fn <- if (is.environment(table)) get0(key, table, inherits = FALSE)
            if (!is.function(fn)) {
                fn <- if (is.function(method)) {
                    method
                } else if (is.character(method)) {
                    binding <- member_binding(ns, method, materialize = TRUE)
                    if (identical(binding[[1L]], "value")) binding[[2L]]
                }
            }
            record <- s3_function_record(fn, if (is.character(method)) method else key, package)
            if (!is.null(record)) {
                if (record$name %in% exports) functions[[record$name]] <- record
                record$generic <- generic
                record$generic_package <- owner
                record$class <- class
                methods[[paste(owner, key, sep = "::")]] <- record
            }
        }
    }
    # Base has no namespace S3method declarations. Its live registry also
    # contains methods installed by package load hooks, including replacements
    # for conventional base bindings. Retain these under the owning generic.
    table <- get0(".__S3MethodsTable__.", ns, inherits = FALSE)
    generics <- Filter(function(record) !is.null(record$generic) && identical(record$owner, package), functions)
    if (is.environment(table)) {
        registered_names <- ls(table, all.names = TRUE)
        for (generic_record in generics) {
            generic <- generic_record$generic$name
            prefix <- paste0(generic, ".")
            for (key in registered_names[startsWith(registered_names, prefix)]) {
                id <- paste(package, key, sep = "::")
                if (!is.null(methods[[id]])) next
                binding <- member_binding(table, key, materialize = TRUE)
                if (!identical(binding[[1L]], "value")) next
                fn <- binding[[2L]]
                method_package <- if (is.function(fn) && isNamespace(environment(fn))) {
                    unname(getNamespaceName(environment(fn)))
                } else {
                    package
                }
                record <- s3_function_record(fn, key, method_package)
                if (is.null(record)) next
                record$generic <- generic
                record$generic_package <- package
                record$class <- substring(key, nchar(prefix) + 1L)
                methods[[id]] <- record
            }
        }
    }
    # Only functions visible from the ordinary calling environment may be
    # considered unregistered methods. Private generic.class bindings do not
    # become applicable just because their namespace is loaded.
    visible <- if (package == "base") ls(ns, all.names = TRUE) else exports
    for (name in visible) {
        if (!grepl(".", name, fixed = TRUE)) next
        binding <- s3_namespace_binding(ns, name)
        if (!identical(binding[[1L]], "value") || !is.function(binding[[2L]])) next
        record <- s3_function_record(binding[[2L]], name, package)
        if (!is.null(record)) functions[[name]] <- record
    }
    lazy <- tryCatch(names(getNamespaceInfo(ns, "lazydata")), error = function(e) character())
    object_names <- unique(c(if (package == "datasets") exports else character(), lazy))
    for (name in object_names) {
        binding <- s3_namespace_binding(ns, name)
        if (!identical(binding[[1L]], "value") && name %in% lazy) {
            data_env <- getNamespaceInfo(ns, "lazydata")
            binding <- member_binding(data_env, name, materialize = TRUE)
        }
        if (identical(binding[[1L]], "value")) {
            shape <- s3_runtime_shape(binding[[2L]])
            if (!is.null(shape)) objects[[name]] <- shape
        }
    }
    dependencies <- if (package == "base") character() else names(getNamespaceImports(ns))
    result <- list(
        schema = 1L, package = package, functions = functions, callables = callables, objects = objects,
        methods = methods, dependencies = dependencies,
        identity = list(
            path = find.package(package), version = as.character(utils::packageVersion(package)),
            r = as.character(getRversion())
        )
    )
    size <- as.numeric(utils::object.size(result))
    if (size > 8 * 1024^2) stop(sprintf("Namespace %s exceeds S3 metadata budget (%d bytes)", package, size))
    result$generation <- digest::digest(result, algo = "xxhash64")
    result
}

s3_prepare_packages <- function(packages) {
    namespaces <- list()
    pending <- unique(c(startup_packages, packages))
    # Load the requested namespaces before snapshotting any owning registry.
    # A dependency's load hook may replace a method on a startup generic.
    while (length(pending) && length(namespaces) < 64L) {
        package <- pending[[1L]]
        pending <- pending[-1L]
        if (package %in% names(namespaces)) next
        namespaces[[package]] <- tryCatch({
            ns <- asNamespace(package)
            list(dependencies = if (package == "base") character() else names(getNamespaceImports(ns)))
        }, error = function(e) list(dependencies = character(), error = conditionMessage(e)))
        pending <- unique(c(pending, setdiff(namespaces[[package]]$dependencies, names(namespaces))))
    }
    result <- list()
    for (package in names(namespaces)) {
        result[[package]] <- tryCatch(s3_prepare_namespace(package), error = function(e) {
            list(
                schema = 1L, package = package, functions = list(), objects = list(), methods = list(),
                dependencies = character(), generation = "", error = conditionMessage(e)
            )
        })
    }
    result
}

s3_namespace_binding <- function(ns, name) {
    # Re-exports can resolve through the imports environment into base.
    # Stop at an existing binding so active bindings never expose a fallback.
    base <- asNamespace("base")
    scopes <- if (identical(ns, base)) list(ns) else list(ns, parent.env(ns), base)
    for (scope in scopes) {
        if (exists(name, scope, inherits = FALSE)) {
            return(member_binding(scope, name, materialize = TRUE))
        }
    }
    member_binding(ns, name, materialize = TRUE)
}

s3_function_identity <- function(fn, owner = "local") {
    syntax <- as.call(list(as.name("function"), fn[[2L]], fn[[3L]]))
    digest::digest(list(owner, deparse1(syntax, collapse = "\n")), algo = "sha256")
}
