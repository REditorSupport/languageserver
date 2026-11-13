# Shared storage for immutable package metadata. Providers choose their type,
# identity/context, and validator; live functions and document state stay local.
# Content hashes are computed on first use and after filesystem changes. The
# change time and file identity detect replacements preserving size and mtime;
# access time is excluded because reading a database must not invalidate it.
package_metadata_file_checksum <- function(files) unname(tools::md5sum(files))

package_metadata_sort <- function(x) {
    if (length(x)) sort(as.character(x), method = "radix") else character()
}

package_metadata_file_fingerprint <- local({
    cache <- NULL
    function(files) {
        if (is.null(cache)) cache <<- ByteLruCache$new(4 * 1024^2, 4096L)
        files <- package_metadata_sort(unique(files))
        info <- fs::file_info(files, fail = FALSE)
        stamp <- list(files, as.numeric(info$size), info$type, as.numeric(info$modification_time),
            as.numeric(info$change_time), info$device_id, info$inode)
        key <- digest::digest(files, algo = "xxhash64")
        previous <- cache$get(key)
        if (!is.null(previous) && identical(previous$stamp, stamp)) return(previous$value)
        hashes <- package_metadata_file_checksum(files)
        if (any(is.na(hashes) & !is.na(info$size) & info$type != "directory")) stop("Cannot fingerprint package metadata files")
        value <- digest::digest(list(files, hashes), algo = "xxhash64")
        cache$set(key, list(stamp = stamp, value = value))
        value
    }
})

package_metadata_normalize_path <- function(path) {
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        return("")
    }
    normalizePath(path, winslash = "/", mustWork = FALSE)
}

package_metadata_version <- local({
    cache <- NULL
    function(package, path, lib_paths, files_fp) {
        if (is.null(cache)) cache <<- ByteLruCache$new(1024^2, 2048L)
        key <- paste0(path, "\n", files_fp)
        cached <- cache$get(key)
        if (!is.null(cached)) return(cached)
        ver <- as.character(utils::packageVersion(package, lib.loc = lib_paths))
        cache$set(key, ver)
        ver
    }
})

package_metadata_identity <- function(package, lib_paths = .libPaths(), context = NULL) {
    path <- package_metadata_normalize_path(find.package(package, lib.loc = lib_paths))
    pkg_rds <- file.path(path, "Meta", "package.rds")
    files <- c(file.path(path, c("DESCRIPTION", "NAMESPACE")),
        if (file.exists(pkg_rds)) pkg_rds,
        unlist(lapply(c("R", "data", "help"), function(directory) {
            list.files(file.path(path, directory), pattern = "\\.(rdb|rdx|rds)$", full.names = TRUE)
        }), use.names = FALSE), list.files(file.path(path, "libs"), full.names = TRUE, recursive = TRUE))
    files_fp <- package_metadata_file_fingerprint(files)
    list(package = package, path = path,
        version = package_metadata_version(package, path, lib_paths, files_fp), r = as.character(getRversion()),
        files = files_fp, context = context)
}

package_metadata_function_signature <- local({
    cache <- NULL
    previous <- NULL
    function(fn) {
        context <- list(syntax = member_function_syntax, formatter = deparse1,
            options = lapply(c("digits", "scipen", "deparse.cutoff"), getOption))
        if (is.null(cache) || !identical(context, previous)) {
            cache <<- ByteLruCache$new(64 * 1024^2, 4096L)
            previous <<- context
        }
        key <- .Call("member_function_key_c", fn, PACKAGE = "languageserver")
        record <- cache$get(key)
        arguments <- formals(fn)
        code <- body(fn)
        if (!is.null(record) && identical(record$arguments, arguments) && identical(record$code, code)) return(record$value)
        value <- deparse1(member_function_syntax(fn))
        # Keep syntax, rather than closure environments. Compare it on hits
        # even if an evicted/deleted function's address has been reused.
        cache$set(key, list(arguments = arguments, code = code, value = value))
        value
    }
})

package_metadata_signature <- function(value) {
    if (typeof(value) == "closure") return(package_metadata_function_signature(value))
    if (is.call(value) || is.expression(value) || is.pairlist(value)) return(deparse1(value))
    if (is.list(value)) return(lapply(value, package_metadata_signature))
    value
}

package_metadata_validation_record <- function(token, name) package_metadata_signature(token)

# Worker-local validation tokens may contain live function references. Inspect
# mutable bindings on every pass, but normalize only changed facts. Nothing in
# this memo is written to disk or used to execute package/document methods.
package_metadata_validation_records <- local({
    cache <- NULL
    function(key, tokens, context, normalize = package_metadata_validation_record) {
        if (is.null(cache)) cache <<- ByteLruCache$new(32 * 1024^2, 64L)
        if (is.null(tokens)) {
            cache$remove(key)
            return(NULL)
        }
        context <- list(inputs = context, normalize = normalize, signature = package_metadata_signature)
        previous <- cache$get(key)
        if (!identical(previous$context, context)) previous <- NULL
        if (!is.null(previous) && identical(previous$tokens, tokens)) return(previous$records)
        records <- stats::setNames(lapply(names(tokens), function(name) {
            token <- tokens[[name]]
            if (!is.null(previous$records[[name]]) && identical(previous$tokens[[name]], token)) return(previous$records[[name]])
            normalize(token, name)
        }), names(tokens))
        cache$set(key, list(context = context, tokens = tokens, records = records))
        records
    }
})

package_metadata_namespace_record <- function(token, name) {
    list(name = name, kind = token$kind,
        value = digest::digest(package_metadata_signature(token$value), algo = "xxhash64"))
}

package_metadata_namespace_state <- function(ns, package = unname(getNamespaceName(ns))) {
    # Cache keys must describe the loaded values too: .onLoad(), tracing and
    # package reinstalls can change them without changing an export's name.
    dollar_context <- member_snapshot_dollar_context(ns)
    modes <- member_s7_capabilities()
    materialize <- isNamespace(ns)
    binding_names <- package_metadata_sort(ls(ns, all.names = TRUE, sorted = FALSE))
    tokens <- stats::setNames(lapply(binding_names, function(name) {
        record <- member_binding(ns, name, materialize = materialize)
        value <- record[[2L]]
        snapshot <- if (identical(record[[1L]], "value") && !is.object(value) &&
                is.atomic(value) && length(value) <= 256L) {
            value
        } else {
            member_snapshot(value, max_bindings = 50000L, s7_modes = modes, lookup_scope = ns,
                dollar_context = dollar_context, function_syntax = package_metadata_function_signature)
        }
        list(kind = record[[1L]], value = snapshot)
    }), binding_names)
    records <- package_metadata_validation_records(paste0(package, ":members"), tokens,
        list(namespace = ns, snapshot = member_snapshot), package_metadata_namespace_record)
    has_hooks <- any(c(".onLoad", ".onAttach", ".First.lib", ".Last.lib") %in% binding_names)
    lazy_records <- NULL
    if (materialize && has_hooks) {
        lazydata <- member_namespace_lazydata(ns)
        if (is.environment(lazydata)) {
            lazy_names <- package_metadata_sort(ls(lazydata, all.names = TRUE, sorted = FALSE))
            if (length(lazy_names)) {
                lazy_tokens <- stats::setNames(lapply(lazy_names, function(name) {
                    record <- member_binding(lazydata, name, materialize = TRUE)
                    value <- record[[2L]]
                    snapshot <- if (identical(record[[1L]], "value") && !is.object(value) &&
                            is.atomic(value) && length(value) <= 256L) {
                        value
                    } else {
                        member_snapshot(value, max_bindings = 50000L, s7_modes = modes, lookup_scope = ns,
                            dollar_context = dollar_context, function_syntax = package_metadata_function_signature)
                    }
                    list(kind = record[[1L]], value = snapshot)
                }), lazy_names)
                lazy_records <- unname(package_metadata_validation_records(
                    paste0(package, ":lazydata"), lazy_tokens,
                    list(namespace = lazydata, snapshot = member_snapshot), package_metadata_namespace_record))
            }
        }
    }
    state <- if (is.null(lazy_records)) {
        list(bindings = unname(records), subsets = member_subset_registry(ns, package))
    } else {
        list(bindings = unname(records), lazydata = lazy_records, subsets = member_subset_registry(ns, package))
    }
    digest::digest(state, algo = "xxhash64")
}

package_metadata_live_identity <- function(package, lib_paths = .libPaths(), context = NULL) {
    identity <- package_metadata_identity(package, lib_paths, context)
    identity$namespace <- package_metadata_namespace_state(asNamespace(package))
    identity$bindings <- s3_metadata_context(package)
    identity$cacheable <- !is.null(identity$bindings)
    identity
}

# Help can come from a library other than the first installed/loaded namespace.
# Track the database actually read, without hashing unrelated native binaries
# on every documentation request.
package_metadata_help_identity <- function(hfile, package = NULL) {
    paths <- as.character(hfile)
    if (!length(paths)) {
        if (is.null(package)) return(NULL)
        paths <- file.path(find.package(package), "help", ".missing-topic")
    }
    roots <- unique(dirname(dirname(paths)))
    files <- unlist(lapply(roots, function(root) {
        c(file.path(root, c("DESCRIPTION", "Meta/Rd.rds", "help/AnIndex")),
            list.files(file.path(root, "help"), pattern = "\\.(rdb|rdx|rds)$", full.names = TRUE))
    }), use.names = FALSE)
    list(package = basename(roots[[1L]]), path = roots, r = as.character(getRversion()),
        files = package_metadata_file_fingerprint(files))
}

# R caches lazy-load database bytes by filename and offers no public flush API.
# Read an immutable copy under its content fingerprint so replacing the actual
# database cannot reuse R's old bytes. Copies are made only on a documentation
# miss, reused for other topics, and removed with the session temporary folder.
package_metadata_help_file <- local({
    cache <- NULL
    function(hfile) {
        identity <- package_metadata_help_identity(hfile)
        paths <- as.character(hfile)
        if (is.null(identity)) return(hfile)
        if (is.null(cache)) cache <<- ByteLruCache$new(package_metadata_cache_max_bytes, 128L)
        root <- file.path(tempdir(), "languageserver-help", identity$files)
        copies <- vapply(paths, function(path) {
            package <- basename(dirname(dirname(path)))
            directory <- file.path(root, package, "help")
            database <- file.path(dirname(path), paste0(package, c(".rdb", ".rdx")))
            targets <- file.path(directory, basename(database))
            if (!all(file.exists(targets))) {
                dir.create(directory, recursive = TRUE, showWarnings = FALSE)
                if (!all(file.copy(database, targets, overwrite = TRUE))) stop("Cannot copy help database")
            }
            file.path(directory, basename(path))
        }, character(1L), USE.NAMES = FALSE)
        if (!cache$has(root)) {
            size <- sum(file.info(list.files(root, full.names = TRUE, recursive = TRUE))$size)
            before <- cache$keys()
            cache$set(root, root, additional_bytes = size)
            if (!cache$has(root)) {
                unlink(root, recursive = TRUE)
                stop("Help database exceeds cache budget")
            }
            for (expired in setdiff(before, cache$keys())) unlink(expired, recursive = TRUE)
        } else {
            cache$get(root)
        }
        attributes(copies) <- attributes(hfile)
        copies
    }
})

package_metadata_help_context <- local({
    previous <- NULL
    key <- NULL
    function() {
        context <- list(locale = Sys.getlocale(),
            options = lapply(c("width", "useFancyQuotes", "help.htmlmath", "help.option"), getOption),
            renderer = list(get_help_rd, tools::Rd2txt, tools::Rd2HTML))
        if (!identical(previous, context)) {
            previous <<- context
            key <<- digest::digest(context, algo = "xxhash64")
        }
        key
    }
})

package_metadata_cache_format <- 3L
package_metadata_cache_max_bytes <- 256 * 1024^2

package_metadata_server_load_state <- new.env(parent = emptyenv())

package_metadata_server_loaded <- function() {
    ns <- asNamespace("languageserver")
    path <- getNamespaceInfo(ns, "path")
    dll <- getLoadedDLLs()[["languageserver"]][["path"]]
    files <- c(package_metadata_sort(list.files(file.path(path, "R"), full.names = TRUE)), dll)
    package_metadata_server_load_state$namespace <- ns
    package_metadata_server_load_state$files <- unname(tools::md5sum(files))
}

package_metadata_server_constant <- function(value, depth = 0L) {
    if (depth > 12L || is.object(value)) return(FALSE)
    if (is.atomic(value) || is.null(value)) return(length(value) <= 10000L)
    if (typeof(value) == "closure") return(TRUE)
    if (is.call(value) || is.expression(value) || is.pairlist(value)) return(TRUE)
    is.list(value) && all(vapply(value, package_metadata_server_constant, logical(1L), depth + 1L))
}

package_metadata_server_syntax <- function(value) {
    if (typeof(value) == "closure") return(package_metadata_function_signature(value))
    if (is.call(value) || is.expression(value) || is.pairlist(value)) return(deparse1(value))
    if (is.list(value)) return(lapply(value, package_metadata_server_syntax))
    value
}

package_metadata_cache_root <- function() {
    value <- getOption("languageserver.package_cache_dir")
    if (is.null(value)) {
        value <- Sys.getenv(
            "R_LANGSVR_PACKAGE_CACHE_DIR",
            tools::R_user_dir("languageserver", "cache")
        )
        if (tolower(trimws(value)) == "false") return(FALSE)
    }
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) return(FALSE)
    value
}

package_metadata_cache_directory <- function() {
    value <- package_metadata_cache_root()
    if (identical(value, FALSE)) NULL else file.path(value, "package-metadata")
}

package_metadata_cache_allowed <- function(identity) {
    is.list(identity) && !identical(identity$cacheable, FALSE) &&
        !(is.list(identity$context) && identical(identity$context$cacheable, FALSE))
}

package_metadata_cache_server <- local({
    identity <- NULL
    previous <- NULL
    function() {
        ns <- asNamespace("languageserver")
        if (!identical(package_metadata_server_load_state$namespace, ns)) package_metadata_server_loaded()
        nms <- package_metadata_sort(ls(ns, all.names = TRUE, sorted = FALSE))
        nms <- nms[nms != ".packageName"]
        out_vals <- vector("list", length(nms))
        names(out_vals) <- nms
        keep <- logical(length(nms))
        for (i in seq_along(nms)) {
            record <- member_binding(ns, nms[[i]], materialize = TRUE)
            value <- record[[2L]]
            if (typeof(value) == "closure") {
                out_vals[[i]] <- value
                keep[[i]] <- TRUE
            } else if (is.environment(value) && "R6ClassGenerator" %in% attr(value, "class", exact = TRUE)) {
                out_vals[[i]] <- list(public = value$public_methods, private = value$private_methods, active = value$active)
                keep[[i]] <- TRUE
            } else if (package_metadata_server_constant(value)) {
                out_vals[[i]] <- value
                keep[[i]] <- TRUE
            }
        }
        values <- out_vals[keep]
        state <- list(namespace = ns, values = values, files = package_metadata_server_load_state$files)
        if (!is.null(identity) && identical(state, previous)) return(identity)
        previous <<- state
        identity <<- list(version = getNamespaceInfo(ns, "spec")[["version"]],
            implementation = digest::digest(lapply(values, package_metadata_server_syntax), algo = "xxhash64"),
            loaded_files = package_metadata_server_load_state$files)
        identity
    }
})

package_metadata_cache_path <- function(type, identity, key = NULL) {
    directory <- package_metadata_cache_directory()
    if (is.null(directory)) return(NULL)
    if (!package_metadata_cache_allowed(identity)) return(NULL)
    hash <- digest::digest(list(format = package_metadata_cache_format, type = type, key = key, identity = identity,
            server = package_metadata_cache_server()), algo = "xxhash64")
    file.path(directory, paste0(identity$package, "-", type, "-", hash, ".rds"))
}

package_metadata_cache_read <- function(path, identity, validate = is.list) {
    if (identical(package_metadata_cache_root(), FALSE) || !package_metadata_cache_allowed(identity) ||
            is.null(path) || !file.exists(path)) return(NULL)
    tryCatch({
        size <- file.info(path)$size
        if (is.na(size) || size > package_metadata_cache_max_bytes) return(NULL)
        record <- readRDS(path)
        if (!is.list(record) || is.object(record) || !identical(record$format, package_metadata_cache_format) ||
                !identical(record$server, package_metadata_cache_server()) ||
                !identical(record$identity, identity) ||
                !is.raw(record$payload) || length(record$payload) > package_metadata_cache_max_bytes ||
                !identical(record$checksum, digest::digest(record$payload, algo = "xxhash64", serialize = FALSE))) return(NULL)
        value <- unserialize(record$payload)
        if (!isTRUE(validate(value))) return(NULL)
        suppressWarnings(Sys.setFileTime(path, Sys.time()))
        value
    }, error = function(e) NULL)
}

package_metadata_cache_write <- function(path, identity, value) {
    if (identical(package_metadata_cache_root(), FALSE) || !package_metadata_cache_allowed(identity) ||
            is.null(path)) return(invisible(NULL))
    if (as.numeric(utils::object.size(value)) > package_metadata_cache_max_bytes) return(invisible(NULL))
    temporary <- NULL
    on.exit(if (!is.null(temporary)) unlink(temporary), add = TRUE)
    tryCatch(suppressWarnings({
        directory <- dirname(path)
        dir.create(directory, recursive = TRUE, showWarnings = FALSE)
        temporary <- tempfile("metadata-", tmpdir = directory)
        payload <- serialize(value, NULL)
        if (length(payload) > package_metadata_cache_max_bytes) return(invisible(NULL))
        saveRDS(list(format = package_metadata_cache_format, server = package_metadata_cache_server(), identity = identity,
                checksum = digest::digest(payload, algo = "xxhash64", serialize = FALSE), payload = payload), temporary)
        if (!file.rename(temporary, path)) {
            # Windows cannot replace an existing cache file with file.rename.
            # A racing reader can safely miss the cache and prepare its model.
            unlink(path)
            if (!file.rename(temporary, path)) return(invisible(NULL))
        }
        files <- list.files(directory, pattern = "\\.rds$", full.names = TRUE)
        info <- file.info(files)
        order <- order(info$mtime, decreasing = TRUE, na.last = TRUE)
        sizes <- info$size[order]
        sizes[is.na(sizes)] <- 0
        expired <- order[seq_along(order) > 512L | cumsum(sizes) > package_metadata_cache_max_bytes]
        if (length(expired)) unlink(files[expired])
    }), error = function(e) NULL)
    invisible(NULL)
}

package_metadata_context <- function(packages, lib_paths = .libPaths()) {
    lib_paths <- package_metadata_startup_lib_paths(lib_paths)
    traversal <- package_metadata_transitive_packages(packages, lib_paths)
    identity_packages <- if (isTRUE(traversal$complete)) {
        unique(c(packages, traversal$packages))
    } else {
        packages
    }
    identities <- lapply(identity_packages, function(package) {
        tryCatch(package_metadata_startup_identity(package, lib_paths = lib_paths),
            error = function(e) list(package = package, error = conditionMessage(e)))
    })
    bindings <- s3_metadata_context(packages)
    list(packages = identities, bindings = bindings,
        cacheable = !is.null(bindings) && !any(vapply(identities, function(identity) !is.null(identity$error), logical(1L))))
}

package_metadata_startup_lib_paths <- function(lib_paths = .libPaths()) {
    unique(normalizePath(c(lib_paths, .Library.site, .Library),
            winslash = "/", mustWork = FALSE))
}

package_metadata_dcf_packages <- local({
    cache <- NULL
    function(desc_file, fields = c("Depends", "Imports")) {
        if (!file.exists(desc_file)) {
            return(structure(character(), complete = FALSE))
        }
        if (is.null(cache)) cache <<- ByteLruCache$new(1024^2, 2048L)
        fp <- tryCatch(package_metadata_file_fingerprint(desc_file), error = function(e) NULL)
        key <- if (!is.null(fp)) paste(c(fp, fields), collapse = "\n")
        if (!is.null(key)) {
            cached <- cache$get(key)
            if (!is.null(cached)) return(cached)
        }
        dcf <- tryCatch(read.dcf(desc_file, fields = unique(c("Package", fields))), error = function(e) NULL)
        if (!is.matrix(dcf) || nrow(dcf) < 1L || !("Package" %in% colnames(dcf)) ||
                is.na(dcf[1L, "Package"]) || !nzchar(trimws(dcf[1L, "Package"]))) {
            return(structure(character(), complete = FALSE))
        }
        keep_fields <- intersect(fields, colnames(dcf))
        values <- if (length(keep_fields)) as.character(dcf[1L, keep_fields]) else character()
        values <- values[!is.na(values) & nzchar(values)]
        res <- if (!length(values)) {
            character()
        } else {
            entries <- trimws(unlist(strsplit(values, ",", fixed = TRUE), use.names = FALSE))
            entries <- trimws(sub("\\s*\\([\\s\\S]*\\)$", "", entries, perl = TRUE))
            setdiff(entries[nzchar(entries)], "R")
        }
        if (!is.null(key)) cache$set(key, res)
        res
    }
})

package_metadata_namespace_imports <- local({
    cache <- NULL
    function(path) {
        if (is.null(cache)) cache <<- ByteLruCache$new(1024^2, 2048L)
        ns_info_file <- file.path(path, "Meta", "nsInfo.rds")
        ns_file <- file.path(path, "NAMESPACE")
        candidates <- c(ns_info_file, ns_file)
        present <- file.exists(candidates)
        fp <- if (any(present)) {
            tryCatch(package_metadata_file_fingerprint(candidates[present]), error = function(e) NULL)
        } else {
            ""
        }
        key <- if (!is.null(fp)) paste(c(path, as.character(present), fp), collapse = "\n")
        if (!is.null(key)) {
            cached <- cache$get(key)
            if (!is.null(cached)) return(cached)
        }
        pkgs <- character()
        if (present[[1L]]) {
            ns_info <- tryCatch(readRDS(ns_info_file), error = function(e) NULL)
            if (is.list(ns_info)) {
                imp_pkgs <- unlist(lapply(ns_info$imports, function(entry) {
                    if (is.character(entry)) {
                        entry
                    } else if (is.list(entry) && length(entry)) {
                        as.character(entry[[1L]])
                    } else {
                        character()
                    }
                }), use.names = FALSE)
                cls_pkgs <- unlist(lapply(ns_info$importClasses, function(entry) {
                    if (is.list(entry) && length(entry)) as.character(entry[[1L]]) else character()
                }), use.names = FALSE)
                mth_pkgs <- unlist(lapply(ns_info$importMethods, function(entry) {
                    if (is.list(entry) && length(entry)) as.character(entry[[1L]]) else character()
                }), use.names = FALSE)
                pkgs <- c(pkgs, imp_pkgs, cls_pkgs, mth_pkgs, package_metadata_delayed_s3_packages(ns_info))
            }
        }
        syntax <- package_metadata_namespace_syntax(ns_file)
        pkgs <- unique(c(pkgs, syntax$packages))
        pkgs <- setdiff(pkgs[!is.na(pkgs) & nzchar(pkgs)], "R")
        if (!isTRUE(syntax$valid)) attr(pkgs, "complete") <- FALSE
        if (!is.null(key)) cache$set(key, pkgs)
        pkgs
    }
})

# S3method(pkg::generic, class) registers a delayed packageEvent() hook whose
# effect depends on whether pkg is already loaded (base::registerS3methods).
package_metadata_delayed_s3_packages <- function(ns_info) {
    methods <- if (is.list(ns_info)) ns_info$S3methods
    if (!is.matrix(methods) || ncol(methods) < 4L) return(character())
    delayed <- as.character(methods[, 4L])
    unique(delayed[!is.na(delayed) & nzchar(delayed)])
}

# Syntax-only NAMESPACE walk: never evaluates anything. parseNamespaceFile()
# evaluates if () conditions, which may call requireNamespace() and run a
# foreign .onLoad() in the server, so every branch of every if/else is
# collected instead (a conservative superset of possible dependencies).
# Branch choice cannot change runtime state of an installed package:
# loadNamespace() reads the install-time resolved Meta/nsInfo.rds, which
# package_metadata_package_runtime_stable() requires and the startup manifest
# fingerprints.
package_metadata_namespace_syntax <- local({
    cache <- NULL
    function(ns_file) {
        result <- list(packages = character(), valid = TRUE)
        if (!file.exists(ns_file)) return(result)
        if (is.null(cache)) cache <<- ByteLruCache$new(1024^2, 2048L)
        fp <- tryCatch(package_metadata_file_fingerprint(ns_file), error = function(e) NULL)
        if (!is.null(fp)) {
            cached <- cache$get(fp)
            if (!is.null(cached)) return(cached)
        }
        exprs <- tryCatch(parse(ns_file, keep.source = FALSE), error = function(e) NULL)
        if (is.null(exprs)) {
            result$valid <- FALSE
            if (!is.null(fp)) cache$set(fp, result)
            return(result)
        }
        packages <- character()
        arg_name <- function(arg) {
            if (is.name(arg) || (is.character(arg) && length(arg) == 1L)) as.character(arg) else ""
        }
        walk <- function(expr) {
            if (!is.call(expr) || !is.name(expr[[1L]])) return(invisible(NULL))
            op <- as.character(expr[[1L]])
            args <- as.list(expr)[-1L]
            if (identical(op, "{")) {
                for (sub_expr in args) walk(sub_expr)
            } else if (identical(op, "if")) {
                for (branch in args[-1L]) walk(branch)
            } else if (op %in% c("<-", "=") && length(args) >= 2L) {
                # parseNamespaceFile() parses the value of an assignment as a directive.
                walk(args[[2L]])
            } else if (identical(op, "import")) {
                arg_names <- names(args)
                keep <- if (is.null(arg_names)) rep(TRUE, length(args)) else arg_names != "except"
                packages <<- c(packages, vapply(args[keep], arg_name, character(1L)))
            } else if (op %in% c("importFrom", "importClassFrom", "importClassesFrom", "importMethodsFrom") && length(args)) {
                packages <<- c(packages, arg_name(args[[1L]]))
            } else if (identical(op, "S3method") && length(args)) {
                generic <- args[[1L]]
                if (is.call(generic) && length(generic) == 3L &&
                        (identical(generic[[1L]], as.name("::")) || identical(generic[[1L]], as.name(":::")))) {
                    packages <<- c(packages, arg_name(generic[[2L]]))
                }
            }
            invisible(NULL)
        }
        for (expr in as.list(exprs)) walk(expr)
        packages <- unique(packages[!is.na(packages) & nzchar(packages)])
        res <- list(packages = setdiff(packages, "R"), valid = TRUE)
        if (!is.null(fp)) cache$set(fp, res)
        res
    }
})

package_metadata_transitive_limit <- 2048L

package_metadata_transitive_packages <- function(
    packages,
    lib_paths = .libPaths(),
    limit = package_metadata_transitive_limit) {
    if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || limit < 0) {
        return(list(packages = character(), missing = character(), complete = FALSE))
    }
    lib_paths <- package_metadata_startup_lib_paths(lib_paths)
    visited <- character()
    installed <- character()
    missing <- character()
    pending <- unique(as.character(packages))
    pending <- pending[!is.na(pending) & nzchar(pending)]
    while (length(pending)) {
        package <- pending[[1L]]
        pending <- pending[-1L]
        if (package %in% visited) next
        path <- tryCatch(find.package(package, lib.loc = lib_paths, quiet = TRUE),
            error = function(e) character())
        if (length(path) != 1L || !nzchar(path)) {
            visited <- c(visited, package)
            missing <- c(missing, package)
            next
        }
        if (length(installed) >= limit) {
            return(list(packages = installed, missing = missing, complete = FALSE))
        }
        visited <- c(visited, package)
        installed <- c(installed, package)
        deps <- if (identical(package, "base")) {
            character()
        } else {
            dcf <- package_metadata_dcf_packages(file.path(path, "DESCRIPTION"))
            if (identical(attr(dcf, "complete", exact = TRUE), FALSE)) {
                return(list(packages = installed, missing = missing, complete = FALSE))
            }
            imports <- package_metadata_namespace_imports(path)
            if (identical(attr(imports, "complete", exact = TRUE), FALSE)) {
                return(list(packages = installed, missing = missing, complete = FALSE))
            }
            unique(c(as.character(dcf), as.character(imports)))
        }
        deps <- setdiff(deps, visited)
        if (length(deps)) pending <- unique(c(pending, deps))
    }
    list(packages = installed, missing = missing, complete = TRUE)
}

package_metadata_nspackloader_lines <- local({
    cached <- NULL
    function() {
        if (!is.null(cached)) return(cached)
        loader <- file.path(R.home("share"), "R", "nspackloader.R")
        if (!file.exists(loader)) return(character())
        cached <<- tryCatch(readLines(loader, warn = FALSE), error = function(e) character())
        cached
    }
})

package_metadata_rdx_has_hooks <- function(rdx_file) {
    rdx <- tryCatch(readRDS(rdx_file), error = function(e) NULL)
    if (!is.list(rdx) || !is.list(rdx$variables)) return(TRUE)
    vars <- names(rdx$variables)
    if (is.null(vars)) return(length(rdx$variables) > 0L)
    any(vars %in% c(".onLoad", ".onAttach", ".First.lib", ".Last.lib")) ||
        any(startsWith(vars, ".__A__") | startsWith(vars, ".__T__") | startsWith(vars, ".__C__"))
}

# Persistent startup bundles bypass isolated-worker s3_metadata_context() and
# live namespace inspection. Restrict filesystem-only reuse to base R .Library
# startup packages and declarative lazy-loaded packages with no load hooks,
# load actions, S4 metadata tables, or native DLL initializers.
package_metadata_package_runtime_stable <- local({
    cache <- NULL
    function(package, lib_paths = .libPaths()) {
        lib_paths <- package_metadata_startup_lib_paths(lib_paths)
        path <- tryCatch(find.package(package, lib.loc = lib_paths, quiet = TRUE),
            error = function(e) character())
        if (length(path) != 1L || !nzchar(path)) return(FALSE)
        pkg_dir <- normalizePath(path, winslash = "/", mustWork = FALSE)
        if (package %in% startup_packages) {
            base_dir <- normalizePath(file.path(.Library, package), winslash = "/", mustWork = FALSE)
            if (identical(pkg_dir, base_dir)) return(TRUE)
        }
        if (dir.exists(file.path(path, "libs"))) return(FALSE)
        pkg_rds <- file.path(path, "Meta", "package.rds")
        ns_info_file <- file.path(path, "Meta", "nsInfo.rds")
        if (!file.exists(pkg_rds) || !file.exists(ns_info_file)) return(FALSE)
        r_dir <- file.path(path, "R")
        r_files <- if (dir.exists(r_dir)) {
            package_metadata_sort(list.files(r_dir, all.files = TRUE, no.. = TRUE))
        } else {
            character()
        }
        data_dir <- file.path(path, "data")
        data_files <- if (dir.exists(data_dir)) {
            package_metadata_sort(list.files(data_dir, all.files = TRUE, no.. = TRUE))
        } else {
            character()
        }
        if (is.null(cache)) cache <<- ByteLruCache$new(1024^2, 2048L)
        tracked <- c(
            pkg_rds,
            ns_info_file,
            if (length(r_files)) file.path(r_dir, r_files),
            if (length(data_files)) file.path(data_dir, data_files)
        )
        fp <- tryCatch(package_metadata_file_fingerprint(tracked), error = function(e) NULL)
        key <- if (!is.null(fp)) paste(c(pkg_dir, r_files, "|", data_files, "|", fp), collapse = "\n")
        if (!is.null(key)) {
            cached <- cache$get(key)
            if (!is.null(cached)) return(cached)
        }
        store <- function(val) {
            if (!is.null(key)) cache$set(key, val)
            val
        }
        ns_info <- tryCatch(readRDS(ns_info_file), error = function(e) NULL)
        if (!is.list(ns_info)) return(store(FALSE))
        if (length(ns_info$dynlibs) > 0L || length(ns_info$nativeRoutines) > 0L ||
                length(ns_info$exportClasses) > 0L || length(ns_info$exportMethods) > 0L ||
                length(ns_info$exportClassPatterns) > 0L ||
                length(package_metadata_delayed_s3_packages(ns_info)) > 0L) {
            return(store(FALSE))
        }
        if (dir.exists(r_dir)) {
            codename <- strsplit(package, "_", fixed = TRUE)[[1L]][[1L]]
            allowed_r_files <- c(codename, paste0(codename, c(".rdx", ".rdb")), "sysdata.rdx", "sysdata.rdb")
            if (length(r_files) && !all(r_files %in% allowed_r_files)) {
                return(store(FALSE))
            }
            code_file <- file.path(r_dir, codename)
            rdx_file <- file.path(r_dir, paste0(codename, ".rdx"))
            rdb_file <- file.path(r_dir, paste0(codename, ".rdb"))
            loader_lines <- package_metadata_nspackloader_lines()
            if (file.exists(code_file)) {
                if (!file.exists(rdx_file) || !file.exists(rdb_file) || !length(loader_lines)) {
                    return(store(FALSE))
                }
                code_lines <- tryCatch(readLines(code_file, warn = FALSE), error = function(e) NULL)
                if (!identical(code_lines, loader_lines)) return(store(FALSE))
            } else if (length(r_files) > 0L) {
                if (!file.exists(rdx_file) || !file.exists(rdb_file)) return(store(FALSE))
            }
            if (file.exists(rdx_file) != file.exists(rdb_file)) {
                return(store(FALSE))
            }
            if (file.exists(rdx_file) && package_metadata_rdx_has_hooks(rdx_file)) {
                return(store(FALSE))
            }
            sys_rdx <- file.path(r_dir, "sysdata.rdx")
            sys_rdb <- file.path(r_dir, "sysdata.rdb")
            if (file.exists(sys_rdx) != file.exists(sys_rdb)) {
                return(store(FALSE))
            }
            if (file.exists(sys_rdx) && package_metadata_rdx_has_hooks(sys_rdx)) {
                return(store(FALSE))
            }
        }
        if (dir.exists(data_dir)) {
            if (length(data_files) &&
                    !all(data_files %in% c("Rdata.rdb", "Rdata.rdx", "Rdata.rds", "datalist"))) {
                return(store(FALSE))
            }
            data_rdx <- file.path(data_dir, "Rdata.rdx")
            data_rdb <- file.path(data_dir, "Rdata.rdb")
            if (file.exists(data_rdx) != file.exists(data_rdb)) {
                return(store(FALSE))
            }
            if (file.exists(data_rdx) && package_metadata_rdx_has_hooks(data_rdx)) {
                return(store(FALSE))
            }
        }
        store(TRUE)
    }
})

package_metadata_runtime_stable <- function(packages, lib_paths = .libPaths()) {
    packages <- unique(as.character(packages))
    packages <- packages[!is.na(packages) & nzchar(packages)]
    if (!length(packages)) return(TRUE)
    lib_paths <- package_metadata_startup_lib_paths(lib_paths)
    all(vapply(packages, package_metadata_package_runtime_stable, logical(1L), lib_paths = lib_paths))
}

# Lazy-load databases of non-base packages whose content was not produced by
# R itself (base R startup packages are trusted and skipped).
package_metadata_lazyload_files <- function(packages, lib_paths = .libPaths()) {
    lib_paths <- package_metadata_startup_lib_paths(lib_paths)
    files <- character()
    for (package in unique(as.character(packages))) {
        path <- tryCatch(find.package(package, lib.loc = lib_paths, quiet = TRUE),
            error = function(e) character())
        if (length(path) != 1L || !nzchar(path)) next
        pkg_dir <- normalizePath(path, winslash = "/", mustWork = FALSE)
        base_dir <- normalizePath(file.path(.Library, package), winslash = "/", mustWork = FALSE)
        if (package %in% startup_packages && identical(pkg_dir, base_dir)) next
        codename <- strsplit(package, "_", fixed = TRUE)[[1L]][[1L]]
        candidates <- c(file.path(path, "R", paste0(codename, ".rdx")),
            file.path(path, "R", "sysdata.rdx"), file.path(path, "data", "Rdata.rdx"))
        files <- c(files, candidates[file.exists(candidates)])
    }
    files
}

package_metadata_rdb_decode <- function(raw_bytes, compressed) {
    if (isFALSE(compressed)) return(raw_bytes)
    if (length(raw_bytes) < 4L) stop("Truncated lazy-load record")
    u_len <- readBin(raw_bytes[1:4], "integer", n = 1L, size = 4L, endian = "big")
    if (is.na(u_len) || u_len < 0L) stop("Invalid lazy-load length")
    if (isTRUE(compressed) || identical(compressed, 1L) || identical(compressed, 1)) {
        out <- memDecompress(raw_bytes[-(1:4)], "gzip")
        if (length(out) != u_len) stop("Truncated lazy-load decompression")
        return(out)
    }
    if (length(raw_bytes) < 5L || raw_bytes[5L] == as.raw(0L)) {
        stop("Truncated lazy-load header")
    }
    type <- rawToChar(raw_bytes[5L])
    payload <- raw_bytes[-(1:5)]
    out <- switch(type,
        Z = {
            if (length(payload) < 6L ||
                    !identical(payload[1:6], as.raw(c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00)))) {
                xz_hdr <- as.raw(c(
                    0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00, 0x00, 0x00, 0xff, 0x12, 0xd9, 0x41,
                    0x02, 0x00, 0x21, 0x01, 0x1c, 0x00, 0x00, 0x00, 0x10, 0xcf, 0x58, 0xcc
                ))
                payload <- c(xz_hdr, payload)
            }
            memDecompress(payload, "xz")
        },
        "2" = memDecompress(payload, "bzip2"),
        "1" = memDecompress(payload, "gzip"),
        "0" = payload,
        stop("Unknown lazy-load compression"))
    if (length(out) != u_len) stop("Truncated lazy-load decompression")
    out
}

# Pure in-process XDR header scan of .rdx/.rdb records.
# Returns TRUE if any top-level variable, lazyKey, or namespace-free eagerKey
# environment binding is a promise (SEXPTYPE == 5L) or unreadable, FALSE if all
# variables and standard namespace references are valid non-promises, and NA if
# user environment references or unknown XDR headers require the isolated
# baseenv() scan worker.
package_metadata_rdb_scan_headers <- function(rdx_file) {
    rdx <- tryCatch(readRDS(rdx_file), error = function(e) NULL)
    rdb_file <- sub("\\.rdx$", ".rdb", rdx_file)
    size <- file.info(rdb_file)$size
    if (!is.list(rdx) || is.na(size)) return(TRUE)
    bytes <- tryCatch(readBin(rdb_file, "raw", size), error = function(e) NULL)
    if (is.null(bytes)) return(TRUE)
    sexp_type <- function(record) {
        if (length(record) < 18L || record[1L] != as.raw(0x58) || record[2L] != as.raw(0x0a)) return(NA_integer_)
        ver <- readBin(record[3:6], "integer", n = 1L, size = 4L, endian = "big")
        pos <- if (identical(ver, 2L)) {
            15L
        } else if (identical(ver, 3L)) {
            enc_len <- readBin(record[15:18], "integer", n = 1L, size = 4L, endian = "big")
            if (is.na(enc_len) || enc_len < 0L) return(NA_integer_)
            19L + enc_len
        } else {
            return(NA_integer_)
        }
        if (length(record) < pos + 3L) return(NA_integer_)
        as.integer(record[pos + 3L])
    }
    check_key <- function(key) {
        key <- as.numeric(key)
        if (length(key) != 2L || anyNA(key) || key[[1L]] < 0 || key[[2L]] < 0 || key[[1L]] + key[[2L]] > size) {
            return(5L)
        }
        record <- tryCatch(
            package_metadata_rdb_decode(bytes[key[[1L]] + seq_len(key[[2L]])], rdx$compressed),
            error = function(e) NULL
        )
        if (is.null(record)) return(5L)
        sexp_type(record)
    }
    needs_worker <- length(rdx$references) > 4L
    var_names <- names(rdx$variables)
    for (i in seq_along(rdx$variables)) {
        st <- check_key(rdx$variables[[i]])
        if (is.na(st)) {
            needs_worker <- TRUE
        } else if (identical(st, 5L)) {
            return(TRUE)
        } else if (st %in% c(247L, 254L)) {
            nm <- if (!is.null(var_names)) var_names[[i]] else ""
            if (!(nm %in% c(".__NAMESPACE__.", ".__S3MethodsTable__."))) {
                needs_worker <- TRUE
            }
        }
    }
    if (length(rdx$references) > 0L) {
        hook <- function(x) new.env(parent = emptyenv())
        for (ref in rdx$references) {
            for (key in if (is.list(ref)) ref$lazyKeys) {
                st <- check_key(key)
                if (is.na(st)) {
                    needs_worker <- TRUE
                } else if (identical(st, 5L)) {
                    return(TRUE)
                }
            }
            eager <- if (is.list(ref)) ref$eagerKey else ref
            eager <- as.numeric(eager)
            if (length(eager) != 2L || anyNA(eager) || eager[[1L]] < 0 ||
                    eager[[2L]] < 0 || eager[[1L]] + eager[[2L]] > size) {
                return(TRUE)
            }
            raw_rec <- tryCatch(
                package_metadata_rdb_decode(bytes[eager[[1L]] + seq_len(eager[[2L]])], rdx$compressed),
                error = function(e) NULL
            )
            if (is.null(raw_rec)) return(TRUE)
            if (any(raw_rec == as.raw(0xf8) | raw_rec == as.raw(0xf9))) {
                needs_worker <- TRUE
            } else {
                val <- tryCatch(unserialize(raw_rec, refhook = hook), error = function(e) e)
                if (inherits(val, "error")) return(TRUE)
                bindings <- if (is.list(val)) val$bindings
                if (is.list(bindings) && length(bindings) && any(vapply(seq_along(bindings), function(i) {
                    typeof(bindings[[i]]) == "promise"
                }, logical(1L)))) {
                    return(TRUE)
                }
            }
        }
    }
    if (needs_worker) NA else FALSE
}

# Runs in an isolated process: unserializing lazy-load records may load the
# namespaces referenced by closures. Each record is decoded without forcing
# promises; a top-level promise (delayedAssign()) or a promise binding in a
# serialized environment means the value depends on load-time evaluation.
package_metadata_rdb_scan <- function(files) {
    hook <- function(x) new.env(parent = emptyenv())
    decode <- function(raw_bytes, compressed) {
        if (isFALSE(compressed)) return(raw_bytes)
        if (length(raw_bytes) < 4L) stop("Truncated lazy-load record")
        u_len <- readBin(raw_bytes[1:4], "integer", n = 1L, size = 4L, endian = "big")
        if (is.na(u_len) || u_len < 0L) stop("Invalid lazy-load length")
        if (isTRUE(compressed) || identical(compressed, 1L) || identical(compressed, 1)) {
            out <- memDecompress(raw_bytes[-(1:4)], "gzip")
            if (length(out) != u_len) stop("Truncated lazy-load decompression")
            return(out)
        }
        if (length(raw_bytes) < 5L || raw_bytes[5L] == as.raw(0L)) {
            stop("Truncated lazy-load header")
        }
        type <- rawToChar(raw_bytes[5L])
        payload <- raw_bytes[-(1:5)]
        out <- switch(type,
            Z = {
                if (length(payload) < 6L ||
                        !identical(payload[1:6], as.raw(c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00)))) {
                    xz_hdr <- as.raw(c(
                        0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00, 0x00, 0x00, 0xff, 0x12, 0xd9, 0x41,
                        0x02, 0x00, 0x21, 0x01, 0x1c, 0x00, 0x00, 0x00, 0x10, 0xcf, 0x58, 0xcc
                    ))
                    payload <- c(xz_hdr, payload)
                }
                memDecompress(payload, "xz")
            },
            "2" = memDecompress(payload, "bzip2"),
            "1" = memDecompress(payload, "gzip"),
            "0" = payload,
            stop("Unknown lazy-load compression"))
        if (length(out) != u_len) stop("Truncated lazy-load decompression")
        out
    }
    sexp_type <- function(record) {
        if (length(record) < 18L || record[1L] != as.raw(0x58) || record[2L] != as.raw(0x0a)) return(NA_integer_)
        ver <- readBin(record[3:6], "integer", n = 1L, size = 4L, endian = "big")
        pos <- if (identical(ver, 2L)) {
            15L
        } else if (identical(ver, 3L)) {
            enc_len <- readBin(record[15:18], "integer", n = 1L, size = 4L, endian = "big")
            if (is.na(enc_len) || enc_len < 0L) return(NA_integer_)
            19L + enc_len
        } else {
            return(NA_integer_)
        }
        if (length(record) < pos + 3L) return(NA_integer_)
        as.integer(record[pos + 3L])
    }
    scan_one <- function(rdx_file) {
        rdx <- readRDS(rdx_file)
        rdb_file <- sub("\\.rdx$", ".rdb", rdx_file)
        size <- file.info(rdb_file)$size
        if (!is.list(rdx) || is.na(size)) return(TRUE)
        bytes <- readBin(rdb_file, "raw", size)
        fetch_raw <- function(key) {
            key <- as.numeric(key)
            if (length(key) != 2L || anyNA(key) || key[[1L]] < 0 || key[[2L]] < 0 || key[[1L]] + key[[2L]] > size) {
                stop("Invalid lazy-load key")
            }
            decode(bytes[key[[1L]] + seq_len(key[[2L]])], rdx$compressed)
        }
        fetch <- function(key) {
            unserialize(fetch_raw(key), refhook = hook)
        }
        is_promise_key <- function(key) {
            record <- tryCatch(fetch_raw(key), error = function(e) NULL)
            if (is.null(record)) return(TRUE)
            st <- sexp_type(record)
            if (!is.na(st)) return(identical(st, 5L))
            value <- tryCatch(unserialize(record, refhook = hook), error = function(e) e)
            inherits(value, "error") || typeof(value) == "promise"
        }
        for (key in rdx$variables) {
            if (is_promise_key(key)) return(TRUE)
        }
        for (ref in rdx$references) {
            eager <- if (is.list(ref)) ref$eagerKey else ref
            value <- fetch(eager)
            bindings <- if (is.list(value)) value$bindings
            if (is.list(bindings) && length(bindings) && any(vapply(seq_along(bindings), function(i) {
                typeof(bindings[[i]]) == "promise"
            }, logical(1L)))) {
                return(TRUE)
            }
            # Source-reference environments keep large bindings lazily.
            for (key in if (is.list(ref)) ref$lazyKeys) {
                if (is_promise_key(key)) return(TRUE)
            }
        }
        FALSE
    }
    scan_file <- function(rdx_file) tryCatch(scan_one(rdx_file), error = function(e) TRUE)
    stats::setNames(vapply(files, scan_file, logical(1L)), files)
}

package_metadata_promises_absent <- local({
    verdicts <- NULL
    scan_worker <- NULL
    file_key <- function(rdx_file) {
        tryCatch(
            package_metadata_file_fingerprint(c(rdx_file, sub("\\.rdx$", ".rdb", rdx_file))),
            error = function(e) NULL
        )
    }
    function(packages, lib_paths = .libPaths()) {
        lib_paths <- package_metadata_startup_lib_paths(lib_paths)
        files <- package_metadata_lazyload_files(packages, lib_paths)
        if (!length(files)) return(TRUE)
        if (is.null(verdicts)) verdicts <<- ByteLruCache$new(2 * 1024^2, 2048L)
        keys <- vapply(files, function(f) {
            k <- file_key(f)
            if (is.null(k)) "" else as.character(k)
        }, character(1L))
        if (any(!nzchar(keys))) return(FALSE)
        flagged <- logical(length(files))
        missing_idx <- integer()
        for (i in seq_along(files)) {
            k <- keys[[i]]
            cached <- verdicts$get(k)
            if (!is.null(cached)) {
                flagged[[i]] <- isTRUE(cached)
            } else {
                header_verdict <- package_metadata_rdb_scan_headers(files[[i]])
                if (!is.na(header_verdict)) {
                    verdicts$set(k, isTRUE(header_verdict))
                    flagged[[i]] <- isTRUE(header_verdict)
                } else {
                    missing_idx <- c(missing_idx, i)
                }
            }
        }
        if (length(missing_idx)) {
            if (is.null(scan_worker)) {
                fn <- package_metadata_rdb_scan
                environment(fn) <- baseenv()
                attributes(fn) <- NULL
                attributes(body(fn)) <- NULL
                scan_worker <<- fn
            }
            missing_keys <- keys[missing_idx]
            rep_idx <- missing_idx[!duplicated(missing_keys)]
            uncached_files <- files[rep_idx]
            uncached_keys <- keys[rep_idx]
            scanned <- tryCatch(callr::r(scan_worker, args = list(files = uncached_files),
                    libpath = lib_paths, system_profile = FALSE, user_profile = FALSE,
                    timeout = if (identical(Sys.getenv("R_COVR"), "true")) 60 else 20),
                error = function(e) {
                    logger$info("startup bundle lazy-load scan:", e)
                    NULL
                })
            if (!is.logical(scanned) || length(scanned) != length(uncached_files) || anyNA(scanned)) {
                return(FALSE)
            }
            for (j in seq_along(uncached_keys)) {
                k <- uncached_keys[[j]]
                val <- isTRUE(scanned[[j]])
                verdicts$set(k, val)
                flagged[keys == k] <- val
            }
        }
        !any(flagged)
    }
})

package_metadata_missing_absent <- function(missing_packages, lib_paths = .libPaths()) {
    if (is.null(missing_packages)) return(TRUE)
    if (!is.character(missing_packages)) return(FALSE)
    missing_packages <- unique(missing_packages[!is.na(missing_packages) & nzchar(missing_packages)])
    if (!length(missing_packages)) return(TRUE)
    lib_paths <- package_metadata_startup_lib_paths(lib_paths)
    !any(vapply(missing_packages, function(package) {
        path <- tryCatch(find.package(package, lib.loc = lib_paths, quiet = TRUE),
            error = function(e) character())
        length(path) == 1L && nzchar(path)
    }, logical(1L)))
}

# loadNamespace() also reads Meta/nsInfo.rds (in preference to NAMESPACE),
# Meta/package.rds, Meta/features.rds and the R/<codename> loader script.
package_metadata_startup_identity <- function(package, lib_paths = .libPaths()) {
    identity <- package_metadata_identity(package, lib_paths = lib_paths)
    path <- identity$path
    codename <- strsplit(package, "_", fixed = TRUE)[[1L]][[1L]]
    files <- c(file.path(path, "Meta", c("nsInfo.rds", "package.rds", "features.rds")),
        file.path(path, "R", codename), file.path(path, "data", "datalist"))
    present <- file.exists(files)
    identity$startup_files <- list(present = present,
        files = if (any(present)) package_metadata_file_fingerprint(files[present]) else "")
    identity
}

package_metadata_watch_paths <- function(packages, pkg_dirs, lib_paths) {
    sub_dirs <- file.path(
        rep(pkg_dirs, each = 5L),
        rep(c("Meta", "R", "data", "help", "libs"), times = length(pkg_dirs))
    )
    existing_subdirs <- sub_dirs[dir.exists(sub_dirs)]
    r_data_dirs <- file.path(
        rep(pkg_dirs, each = 2L),
        rep(c("R", "data"), times = length(pkg_dirs))
    )
    r_data_dirs <- r_data_dirs[dir.exists(r_data_dirs)]
    f_r_data <- list.files(r_data_dirs, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    help_dirs <- file.path(pkg_dirs, "help")
    help_dirs <- help_dirs[dir.exists(help_dirs)]
    f_help <- list.files(help_dirs, pattern = "\\.(rdb|rdx|rds)$", full.names = TRUE)
    libs_dirs <- file.path(pkg_dirs, "libs")
    libs_dirs <- libs_dirs[dir.exists(libs_dirs)]
    f_libs <- c(
        list.dirs(libs_dirs, full.names = TRUE, recursive = TRUE),
        list.files(libs_dirs, all.files = TRUE, no.. = TRUE, full.names = TRUE, recursive = TRUE)
    )
    meta_candidates <- file.path(
        rep(pkg_dirs, each = 3L),
        "Meta",
        rep(c("nsInfo.rds", "package.rds", "features.rds"), times = length(pkg_dirs))
    )
    meta_files <- meta_candidates[file.exists(meta_candidates)]
    fixed <- c(file.path(pkg_dirs, "DESCRIPTION"), file.path(pkg_dirs, "NAMESPACE"))
    shadow <- unlist(lapply(seq_along(packages), function(i) {
        p_dir <- pkg_dirs[[i]]
        parent <- dirname(p_dir)
        idx <- match(parent, lib_paths)
        if (!is.na(idx) && idx > 1L) {
            file.path(lib_paths[seq_len(idx - 1L)], packages[[i]], "DESCRIPTION")
        } else {
            character()
        }
    }), use.names = FALSE)
    package_metadata_sort(
        unique(c(pkg_dirs, existing_subdirs, fixed, meta_files, f_r_data, f_help, f_libs, shadow))
    )
}

package_metadata_file_info_stamp <- function(watch_paths) {
    info <- fs::file_info(watch_paths, fail = FALSE)
    list(
        as.numeric(info$size),
        info$type,
        as.numeric(info$modification_time),
        as.numeric(info$change_time),
        info$device_id,
        info$inode
    )
}

package_metadata_manifest <- local({
    cache <- NULL
    function(packages, lib_paths = .libPaths()) {
        lib_paths <- package_metadata_startup_lib_paths(lib_paths)
        packages <- unique(as.character(packages))
        packages <- packages[!is.na(packages) & nzchar(packages)]
        if (!length(packages)) return(list())
        if (is.null(cache)) cache <<- ByteLruCache$new(4 * 1024^2, 256L)
        fp_cache <- environment(package_metadata_file_fingerprint)$cache
        key <- paste(c(packages, "|", lib_paths), collapse = "\n")
        if (!is.null(fp_cache) && fp_cache$size() > 0L) {
            cached <- cache$get(key)
            if (!is.null(cached) &&
                    identical(package_metadata_file_info_stamp(cached$watch_paths), cached$stamp)) {
                return(cached$manifest)
            }
        }
        manifest <- list()
        for (package in packages) {
            identity <- tryCatch(package_metadata_startup_identity(package, lib_paths = lib_paths),
                error = function(e) NULL)
            if (is.null(identity)) return(NULL)
            manifest[[package]] <- identity
        }
        pkg_dirs <- vapply(manifest, `[[`, character(1L), "path")
        watch_paths <- package_metadata_watch_paths(packages, pkg_dirs, lib_paths)
        stamp <- package_metadata_file_info_stamp(watch_paths)
        cache$set(key, list(manifest = manifest, watch_paths = watch_paths, stamp = stamp))
        manifest
    }
})

package_metadata_manifest_valid <- function(manifest, lib_paths = .libPaths()) {
    if (!is.list(manifest) || is.object(manifest)) return(FALSE)
    manifest_names <- names(manifest)
    if (length(manifest) && (is.null(manifest_names) || any(!nzchar(manifest_names)))) {
        return(FALSE)
    }
    if (!length(manifest_names)) return(TRUE)
    current <- package_metadata_manifest(manifest_names, lib_paths = lib_paths)
    identical(current, manifest)
}

package_metadata_identity_matches_manifest <- function(identity, manifest_entry) {
    if (!is.list(identity) || is.object(identity) || !is.null(identity$error) ||
            !is.list(manifest_entry) || is.object(manifest_entry)) {
        return(FALSE)
    }
    if (!identical(identity$package, manifest_entry$package) ||
            !identical(package_metadata_normalize_path(identity$path),
                package_metadata_normalize_path(manifest_entry$path)) ||
            !identical(identity$version, manifest_entry$version) ||
            !identical(identity$r, manifest_entry$r) ||
            !identical(identity$files, manifest_entry$files)) {
        return(FALSE)
    }
    if (!is.null(identity$startup_files) &&
            !identical(identity$startup_files, manifest_entry$startup_files)) {
        return(FALSE)
    }
    TRUE
}

package_metadata_context_matches_manifest <- function(context, manifest) {
    if (!is.list(manifest) || is.object(manifest)) return(FALSE)
    if (!length(manifest)) return(TRUE)
    if (!is.list(context) || is.object(context) || identical(context$cacheable, FALSE)) {
        return(FALSE)
    }
    ctx_packages <- context$packages
    if (!is.list(ctx_packages) || is.object(ctx_packages)) return(FALSE)
    seen <- character()
    for (entry in ctx_packages) {
        if (!is.list(entry) || !is.character(entry$package) ||
                length(entry$package) != 1L || !nzchar(entry$package)) {
            return(FALSE)
        }
        pkg <- entry$package
        if (!(pkg %in% names(manifest)) ||
                !package_metadata_identity_matches_manifest(entry, manifest[[pkg]])) {
            return(FALSE)
        }
        seen <- c(seen, pkg)
    }
    all(names(manifest) %in% seen)
}

package_metadata_identities_match_manifest <- function(identities, manifest) {
    if (!is.list(manifest) || is.object(manifest)) return(FALSE)
    if (!length(manifest)) return(TRUE)
    if (!is.list(identities) || is.object(identities)) return(FALSE)
    id_names <- names(identities)
    if (length(identities) && (is.null(id_names) || any(!nzchar(id_names)))) {
        return(FALSE)
    }
    if (!all(names(manifest) %in% id_names)) return(FALSE)
    for (pkg in id_names) {
        if (!(pkg %in% names(manifest)) ||
                !package_metadata_identity_matches_manifest(identities[[pkg]], manifest[[pkg]])) {
            return(FALSE)
        }
    }
    TRUE
}

package_metadata_bundle_key_matches_manifest <- function(bundle_key, manifest) {
    if (!is.list(bundle_key) || !is.list(manifest)) return(FALSE)
    pre_manifest <- bundle_key$manifest
    if (is.null(pre_manifest) && is.list(bundle_key$identity)) {
        pre_manifest <- bundle_key$identity$packages
    }
    if (is.null(pre_manifest)) return(TRUE)
    if (!is.list(pre_manifest) || !all(names(pre_manifest) %in% names(manifest))) {
        return(FALSE)
    }
    for (pkg in names(pre_manifest)) {
        if (!package_metadata_identity_matches_manifest(pre_manifest[[pkg]], manifest[[pkg]])) {
            return(FALSE)
        }
    }
    TRUE
}

package_metadata_startup_s3_identities_valid <- function(catalogs, manifest) {
    if (!is.list(catalogs) || is.object(catalogs) ||
            !is.list(manifest) || is.object(manifest)) {
        return(FALSE)
    }
    catalog_names <- names(catalogs)
    if (!length(catalogs) || is.null(catalog_names) || any(!nzchar(catalog_names))) {
        return(FALSE)
    }
    if (!all(catalog_names %in% names(manifest))) return(FALSE)
    for (pkg in catalog_names) {
        catalog <- catalogs[[pkg]]
        if (!is.list(catalog) ||
                !package_metadata_identity_matches_manifest(catalog$identity, manifest[[pkg]]) ||
                !package_metadata_context_matches_manifest(catalog$identity$context, manifest)) {
            return(FALSE)
        }
    }
    worker_identities <- attr(catalogs, "identities", exact = TRUE)
    if (!is.null(worker_identities) &&
            !package_metadata_identities_match_manifest(worker_identities, manifest)) {
        return(FALSE)
    }
    TRUE
}

package_metadata_startup_members_identities_valid <- function(
    snapshots,
    manifest,
    identities = NULL) {
    if (!is.list(snapshots) || is.object(snapshots) ||
            !is.list(manifest) || is.object(manifest)) {
        return(FALSE)
    }
    snapshot_names <- names(snapshots)
    if (length(snapshots) && (is.null(snapshot_names) || any(!nzchar(snapshot_names)))) {
        return(FALSE)
    }
    if (!all(snapshot_names %in% names(manifest))) return(FALSE)
    for (pkg in snapshot_names) {
        snapshot <- snapshots[[pkg]]
        if (!is.list(snapshot) ||
                !package_metadata_identity_matches_manifest(snapshot$identity, manifest[[pkg]]) ||
                !package_metadata_context_matches_manifest(snapshot$identity$context, manifest)) {
            return(FALSE)
        }
    }
    if (!package_metadata_identities_match_manifest(identities, manifest)) {
        return(FALSE)
    }
    TRUE
}

package_metadata_startup_bundle_key <- local({
    cache <- NULL
    function(type, packages, lib_paths = .libPaths()) {
        if (is.null(package_metadata_cache_directory())) return(NULL)
        lib_paths <- package_metadata_startup_lib_paths(lib_paths)
        key_packages <- unique(c(startup_packages, as.character(packages)))
        key_packages <- key_packages[!is.na(key_packages) & nzchar(key_packages)]
        if (is.null(cache)) cache <<- ByteLruCache$new(4 * 1024^2, 256L)
        fp_cache <- environment(package_metadata_file_fingerprint)$cache
        graph_key <- paste(
            c(key_packages, "|", lib_paths, "|", as.character(package_metadata_transitive_limit)),
            collapse = "\n"
        )
        resolved <- NULL
        if (!is.null(fp_cache) && fp_cache$size() > 0L) {
            cached <- cache$get(graph_key)
            if (!is.null(cached) &&
                    package_metadata_missing_absent(cached$missing, lib_paths) &&
                    identical(package_metadata_file_info_stamp(cached$watch_paths), cached$stamp)) {
                resolved <- cached
            }
        }
        if (is.null(resolved)) {
            traversal <- package_metadata_transitive_packages(key_packages, lib_paths)
            if (!isTRUE(traversal$complete) ||
                    !all(key_packages %in% traversal$packages) ||
                    !package_metadata_runtime_stable(traversal$packages, lib_paths)) {
                return(NULL)
            }
            manifest <- package_metadata_manifest(traversal$packages, lib_paths)
            if (is.null(manifest)) return(NULL)
            m_cache <- environment(package_metadata_manifest)$cache
            m_key <- paste(c(traversal$packages, "|", lib_paths), collapse = "\n")
            m_entry <- m_cache$get(m_key)
            resolved <- list(
                manifest = manifest,
                missing = traversal$missing,
                watch_paths = m_entry$watch_paths,
                stamp = m_entry$stamp
            )
            cache$set(graph_key, resolved)
        }
        identity <- list(
            package = "startup",
            requested_packages = as.character(packages),
            lib_paths = lib_paths,
            packages = resolved$manifest
        )
        path <- package_metadata_cache_path(type, identity)
        if (is.null(path)) return(NULL)
        list(
            type = type,
            path = path,
            identity = identity,
            lib_paths = lib_paths,
            manifest = resolved$manifest,
            missing = resolved$missing
        )
    }
})

package_metadata_startup_s3_complete <- function(catalogs, requested_packages) {
    if (!is.list(catalogs) || is.object(catalogs)) return(FALSE)
    catalog_names <- names(catalogs)
    if (length(catalogs) && (is.null(catalog_names) || any(!nzchar(catalog_names)))) {
        return(FALSE)
    }
    valid <- vapply(catalogs, function(catalog) {
        is.list(catalog) && identical(catalog$schema, 1L) && is.null(catalog$error)
    }, logical(1L))
    dependencies <- unlist(lapply(catalogs[valid], `[[`, "dependencies"), use.names = FALSE)
    required <- unique(c(startup_packages, as.character(requested_packages), dependencies))
    all(valid) && all(required %in% names(catalogs))
}

package_metadata_startup_s3_read <- function(bundle_key, requested_packages) {
    if (is.null(bundle_key)) return(NULL)
    bundle <- package_metadata_cache_read(bundle_key$path, bundle_key$identity, function(value) {
        if (!is.list(value) || is.object(value) ||
                !package_metadata_startup_s3_complete(value$catalogs, requested_packages)) {
            return(FALSE)
        }
        required <- unique(c(startup_packages, as.character(requested_packages), names(value$catalogs)))
        required <- required[!is.na(required) & nzchar(required)]
        all(required %in% names(value$manifest)) &&
            package_metadata_bundle_key_matches_manifest(bundle_key, value$manifest) &&
            package_metadata_manifest_valid(value$manifest, bundle_key$lib_paths) &&
            package_metadata_startup_s3_identities_valid(value$catalogs, value$manifest) &&
            package_metadata_missing_absent(value$missing, bundle_key$lib_paths) &&
            package_metadata_runtime_stable(names(value$manifest), bundle_key$lib_paths)
    })
    if (!is.null(bundle)) bundle$catalogs
}

package_metadata_startup_s3_write <- function(bundle_key, requested_packages, catalogs) {
    if (is.null(bundle_key) || !package_metadata_startup_s3_complete(catalogs, requested_packages)) {
        return(invisible(NULL))
    }
    required <- unique(c(startup_packages, as.character(requested_packages), names(catalogs)))
    required <- required[!is.na(required) & nzchar(required)]
    traversal <- package_metadata_transitive_packages(required, bundle_key$lib_paths)
    if (!isTRUE(traversal$complete)) return(invisible(NULL))
    packages <- unique(c(required, traversal$packages))
    if (!package_metadata_runtime_stable(packages, bundle_key$lib_paths) ||
            !package_metadata_promises_absent(packages, bundle_key$lib_paths)) {
        return(invisible(NULL))
    }
    manifest <- package_metadata_manifest(packages, bundle_key$lib_paths)
    if (is.null(manifest) ||
            !package_metadata_bundle_key_matches_manifest(bundle_key, manifest) ||
            !package_metadata_startup_s3_identities_valid(catalogs, manifest)) {
        return(invisible(NULL))
    }
    package_metadata_cache_write(bundle_key$path, bundle_key$identity,
        list(manifest = manifest, missing = traversal$missing, catalogs = catalogs))
}

package_metadata_target_packages <- function(packages) {
    packages <- unique(as.character(packages))
    packages[!is.na(packages) & nzchar(packages)]
}

package_metadata_startup_members_snapshots_valid <- function(snapshots) {
    if (!is.list(snapshots) || is.object(snapshots)) return(FALSE)
    if (identical(attr(snapshots, "complete", exact = TRUE), FALSE)) return(FALSE)
    if (!length(snapshots)) return(TRUE)
    snapshot_names <- names(snapshots)
    if (is.null(snapshot_names) || any(!nzchar(snapshot_names))) return(FALSE)
    all(vapply(snapshots, function(value) {
        is.list(value) && isTRUE(value$dataset_only) && !is.null(member_index_thaw(value))
    }, logical(1L)))
}

package_metadata_startup_members_unpack <- function(result, requested_packages = character()) {
    target_packages <- package_metadata_target_packages(requested_packages)
    if (!is.list(result) || is.object(result)) {
        return(list(
            .startup_members_unpacked = TRUE,
            snapshots = NULL,
            inspected = character(),
            identities = NULL,
            complete = FALSE,
            valid = FALSE
        ))
    }
    if (isTRUE(result$.startup_members_unpacked)) {
        return(result)
    }
    if (isTRUE(result$.startup_members_result)) {
        snapshots <- result$snapshots
        valid <- package_metadata_startup_members_snapshots_valid(snapshots)
        clean_snapshots <- snapshots
        if (valid && !is.null(attr(clean_snapshots, "identities", exact = TRUE))) {
            attr(clean_snapshots, "identities") <- NULL
        }
        inspected <- if (is.character(result$inspected)) {
            package_metadata_target_packages(result$inspected)
        } else {
            character()
        }
        identities <- result$identities
        if (is.null(identities)) {
            identities <- attr(snapshots, "identities", exact = TRUE)
        }
        if (is.null(identities) && valid && length(clean_snapshots) > 0L) {
            ctx_pkgs <- clean_snapshots[[1L]]$identity$context$packages
            if (is.list(ctx_pkgs) && length(ctx_pkgs) > 0L) {
                pkg_names <- vapply(ctx_pkgs, function(x) {
                    if (is.list(x) && is.character(x$package) && length(x$package) == 1L) x$package else ""
                }, character(1L))
                if (all(nzchar(pkg_names))) {
                    identities <- stats::setNames(ctx_pkgs, pkg_names)
                }
            }
        }
        complete <- valid &&
            isTRUE(result$complete) &&
            length(result$errors) == 0L &&
            all(target_packages %in% inspected) &&
            all(names(clean_snapshots) %in% inspected)
        return(list(
            .startup_members_unpacked = TRUE,
            snapshots = if (valid) clean_snapshots else NULL,
            inspected = inspected,
            identities = identities,
            complete = complete,
            valid = valid
        ))
    }
    valid <- package_metadata_startup_members_snapshots_valid(result)
    attr_inspected <- attr(result, "inspected", exact = TRUE)
    attr_identities <- attr(result, "identities", exact = TRUE)
    inspected <- if (is.character(attr_inspected)) {
        package_metadata_target_packages(attr_inspected)
    } else {
        unique(c(target_packages, names(result)))
    }
    complete <- valid &&
        !identical(attr(result, "complete", exact = TRUE), FALSE) &&
        all(target_packages %in% inspected)
    clean_snapshots <- result
    if (valid && (!is.null(attr(clean_snapshots, "complete", exact = TRUE)) ||
                !is.null(attr(clean_snapshots, "inspected", exact = TRUE)) ||
                !is.null(attr(clean_snapshots, "identities", exact = TRUE)) ||
                !is.null(attr(clean_snapshots, "errors", exact = TRUE)))) {
        attr(clean_snapshots, "complete") <- NULL
        attr(clean_snapshots, "inspected") <- NULL
        attr(clean_snapshots, "identities") <- NULL
        attr(clean_snapshots, "errors") <- NULL
    }
    if (is.null(attr_identities) && valid && length(clean_snapshots) > 0L) {
        ctx_pkgs <- clean_snapshots[[1L]]$identity$context$packages
        if (is.list(ctx_pkgs) && length(ctx_pkgs) > 0L) {
            pkg_names <- vapply(ctx_pkgs, function(x) {
                if (is.list(x) && is.character(x$package) && length(x$package) == 1L) x$package else ""
            }, character(1L))
            if (all(nzchar(pkg_names))) {
                attr_identities <- stats::setNames(ctx_pkgs, pkg_names)
            }
        }
    }
    list(
        .startup_members_unpacked = TRUE,
        snapshots = if (valid) clean_snapshots else NULL,
        inspected = inspected,
        identities = attr_identities,
        complete = complete,
        valid = valid
    )
}

package_metadata_startup_members_valid <- function(snapshots, requested_packages = character()) {
    unpacked <- package_metadata_startup_members_unpack(snapshots, requested_packages)
    isTRUE(unpacked$valid) && isTRUE(unpacked$complete)
}

package_metadata_startup_members_read <- function(bundle_key, requested_packages) {
    if (is.null(bundle_key)) return(NULL)
    target_packages <- package_metadata_target_packages(requested_packages)
    bundle <- package_metadata_cache_read(bundle_key$path, bundle_key$identity, function(value) {
        if (!is.list(value) || is.object(value) ||
                !package_metadata_startup_members_snapshots_valid(value$snapshots) ||
                !is.character(value$inspected) ||
                !all(target_packages %in% value$inspected) ||
                !all(names(value$snapshots) %in% value$inspected)) {
            return(FALSE)
        }
        required <- unique(c(startup_packages, target_packages, names(value$snapshots)))
        required <- required[!is.na(required) & nzchar(required)]
        all(required %in% names(value$manifest)) &&
            package_metadata_bundle_key_matches_manifest(bundle_key, value$manifest) &&
            package_metadata_manifest_valid(value$manifest, bundle_key$lib_paths) &&
            package_metadata_startup_members_identities_valid(
                value$snapshots, value$manifest, value$identities) &&
            package_metadata_missing_absent(value$missing, bundle_key$lib_paths) &&
            package_metadata_runtime_stable(names(value$manifest), bundle_key$lib_paths)
    })
    if (!is.null(bundle)) bundle$snapshots
}

package_metadata_startup_members_write <- function(bundle_key, requested_packages, snapshots) {
    if (is.null(bundle_key)) return(invisible(NULL))
    unpacked <- package_metadata_startup_members_unpack(snapshots, requested_packages)
    if (!isTRUE(unpacked$valid) || !isTRUE(unpacked$complete)) {
        return(invisible(NULL))
    }
    clean_snapshots <- unpacked$snapshots
    target_packages <- package_metadata_target_packages(requested_packages)
    s4_s7 <- unlist(lapply(clean_snapshots, function(snapshot) {
        c(snapshot$s4_dependencies, snapshot$s7_dependencies)
    }), use.names = FALSE)
    required <- unique(c(startup_packages, target_packages, names(clean_snapshots)))
    required <- required[!is.na(required) & nzchar(required)]
    traversal <- package_metadata_transitive_packages(c(required, s4_s7), bundle_key$lib_paths)
    if (!isTRUE(traversal$complete)) return(invisible(NULL))
    packages <- unique(c(required, traversal$packages))
    if (!package_metadata_runtime_stable(packages, bundle_key$lib_paths) ||
            !package_metadata_promises_absent(packages, bundle_key$lib_paths)) {
        return(invisible(NULL))
    }
    manifest <- package_metadata_manifest(packages, bundle_key$lib_paths)
    if (is.null(manifest) ||
            !package_metadata_bundle_key_matches_manifest(bundle_key, manifest) ||
            !package_metadata_startup_members_identities_valid(clean_snapshots, manifest, unpacked$identities)) {
        return(invisible(NULL))
    }
    package_metadata_cache_write(bundle_key$path, bundle_key$identity,
        list(
            manifest = manifest,
            identities = unpacked$identities,
            missing = traversal$missing,
            inspected = unpacked$inspected,
            snapshots = clean_snapshots
        ))
}
