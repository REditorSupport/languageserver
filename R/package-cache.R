# Shared storage for immutable package metadata. Providers choose their type,
# identity/context, and validator; live functions and document state stay local.
# Content hashes are computed on first use and after filesystem changes. The
# change time and file identity detect replacements preserving size and mtime;
# access time is excluded because reading a database must not invalidate it.
package_metadata_file_checksum <- function(files) unname(tools::md5sum(files))

package_metadata_file_fingerprint <- local({
    cache <- NULL
    function(files) {
        if (is.null(cache)) cache <<- ByteLruCache$new(1024^2, 128L)
        files <- sort(unique(files))
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

package_metadata_identity <- function(package, lib_paths = .libPaths(), context = NULL) {
    path <- find.package(package, lib.loc = lib_paths)
    files <- c(file.path(path, c("DESCRIPTION", "NAMESPACE")),
        unlist(lapply(c("R", "data", "help"), function(directory) {
            list.files(file.path(path, directory), pattern = "\\.(rdb|rdx|rds)$", full.names = TRUE)
        }), use.names = FALSE), list.files(file.path(path, "libs"), full.names = TRUE, recursive = TRUE))
    list(package = package, path = path,
        version = as.character(utils::packageVersion(package, lib.loc = lib_paths)), r = as.character(getRversion()),
        files = package_metadata_file_fingerprint(files), context = context)
}

package_metadata_signature <- function(value) {
    if (is.call(value) || is.expression(value) || is.pairlist(value)) return(deparse1(value))
    if (is.list(value)) return(lapply(value, package_metadata_signature))
    value
}

package_metadata_namespace_state <- function(ns) {
    # Cache keys must describe the loaded values too: .onLoad(), tracing and
    # package reinstalls can change them without changing an export's name.
    dollar_context <- member_snapshot_dollar_context(ns)
    modes <- member_s7_capabilities()
    records <- lapply(ls(ns, all.names = TRUE), function(name) {
        record <- member_binding(ns, name, materialize = TRUE)
        value <- record[[2L]]
        snapshot <- if (identical(record[[1L]], "value") && !is.object(value) &&
                is.atomic(value) && length(value) <= 256L) {
            value
        } else {
            member_snapshot(value, max_bindings = 50000L, s7_modes = modes, lookup_scope = ns,
                dollar_context = dollar_context)
        }
        list(name = name, kind = record[[1L]], value = digest::digest(package_metadata_signature(snapshot), algo = "xxhash64"))
    })
    digest::digest(list(bindings = records, subsets = member_subset_registry(ns, unname(getNamespaceName(ns)))),
        algo = "xxhash64")
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
    files <- c(list.files(file.path(path, "R"), full.names = TRUE), dll)
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
    if (typeof(value) == "closure") return(deparse1(member_function_syntax(value)))
    if (is.call(value) || is.expression(value) || is.pairlist(value)) return(deparse1(value))
    if (is.list(value)) return(lapply(value, package_metadata_server_syntax))
    value
}

package_metadata_cache_root <- function() {
    value <- getOption("languageserver.package_cache_dir",
        Sys.getenv("R_LANGSVR_PACKAGE_CACHE_DIR", tools::R_user_dir("languageserver", "cache")))
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
        values <- list()
        for (name in ls(ns, all.names = TRUE)) {
            if (identical(name, ".packageName")) next
            record <- member_binding(ns, name, materialize = TRUE)
            value <- record[[2L]]
            if (typeof(value) == "closure") {
                values[name] <- list(value)
            } else if (is.environment(value) && "R6ClassGenerator" %in% attr(value, "class", exact = TRUE)) {
                methods <- list(public = value$public_methods, private = value$private_methods, active = value$active)
                values[name] <- list(methods)
            } else if (package_metadata_server_constant(value)) {
                values[name] <- list(value)
            }
        }
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

package_metadata_context <- function(packages) {
    identities <- lapply(packages, function(package) {
        tryCatch(package_metadata_identity(package), error = function(e) list(package = package, error = conditionMessage(e)))
    })
    bindings <- s3_metadata_context(packages)
    list(packages = identities, bindings = bindings,
        cacheable = !is.null(bindings) && !any(vapply(identities, function(identity) !is.null(identity$error), logical(1L))))
}
