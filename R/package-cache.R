# Shared storage for immutable package metadata. Providers choose their type,
# identity/context, and validator; live functions and document state stay local.
package_metadata_identity <- function(package, lib_paths = .libPaths(), context = NULL) {
    path <- find.package(package, lib.loc = lib_paths)
    files <- c(file.path(path, c("DESCRIPTION", "NAMESPACE")),
        unlist(lapply(c("R", "data", "help"), function(directory) {
            list.files(file.path(path, directory), pattern = "\\.(rdb|rdx|rds)$", full.names = TRUE)
        }), use.names = FALSE), list.files(file.path(path, "libs"), full.names = TRUE, recursive = TRUE))
    info <- file.info(files)
    list(package = package, path = path,
        version = as.character(utils::packageVersion(package, lib.loc = lib_paths)), r = as.character(getRversion()),
        files = digest::digest(list(files, info$size, as.numeric(info$mtime)), algo = "xxhash64"), context = context)
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
    identity
}

package_metadata_cache_format <- 2L

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

package_metadata_cache_server <- local({
    identity <- NULL
    function() {
        if (!is.null(identity)) return(identity)
        ns <- asNamespace("languageserver")
        values <- list()
        for (name in ls(ns, all.names = TRUE)) {
            if (identical(name, ".packageName")) next
            record <- member_binding(ns, name, materialize = TRUE)
            value <- record[[2L]]
            if (typeof(value) == "closure") {
                values[name] <- list(deparse1(member_function_syntax(value)))
            } else if (is.environment(value) && "R6ClassGenerator" %in% attr(value, "class", exact = TRUE)) {
                methods <- list(public = value$public_methods, private = value$private_methods, active = value$active)
                values[name] <- list(lapply(methods, function(section) {
                    lapply(section, function(method) {
                        if (typeof(method) == "closure") deparse1(member_function_syntax(method))
                    })
                }))
            } else if (!is.object(value) && is.atomic(value) && length(value) <= 10000L) {
                values[name] <- list(value)
            } else if (is.list(value) && all(vapply(value, is.atomic, logical(1L)))) {
                values[name] <- list(value)
            }
        }
        dll <- getLoadedDLLs()[["languageserver"]][["path"]]
        identity <<- list(version = getNamespaceInfo(ns, "spec")[["version"]],
            implementation = digest::digest(values, algo = "xxhash64"), native = unname(tools::md5sum(dll)))
        identity
    }
})

package_metadata_cache_path <- function(type, identity, key = NULL) {
    directory <- package_metadata_cache_directory()
    if (is.null(directory)) return(NULL)
    if (is.list(identity$context) && identical(identity$context$cacheable, FALSE)) return(NULL)
    hash <- digest::digest(list(format = package_metadata_cache_format, type = type, key = key, identity = identity,
            server = package_metadata_cache_server()), algo = "xxhash64")
    file.path(directory, paste0(identity$package, "-", type, "-", hash, ".rds"))
}

package_metadata_cache_read <- function(path, identity, validate = is.list) {
    if (is.null(path) || !file.exists(path)) return(NULL)
    tryCatch({
        record <- readRDS(path)
        if (!identical(record$format, package_metadata_cache_format) || !identical(record$server, package_metadata_cache_server()) ||
                !identical(record$identity, identity) ||
                !is.raw(record$payload) ||
                !identical(record$checksum, digest::digest(record$payload, algo = "xxhash64", serialize = FALSE))) return(NULL)
        value <- unserialize(record$payload)
        if (!isTRUE(validate(value))) return(NULL)
        suppressWarnings(Sys.setFileTime(path, Sys.time()))
        value
    }, error = function(e) NULL)
}

package_metadata_cache_write <- function(path, identity, value) {
    if (is.null(path)) return(invisible(NULL))
    temporary <- NULL
    on.exit(if (!is.null(temporary)) unlink(temporary), add = TRUE)
    tryCatch(suppressWarnings({
        directory <- dirname(path)
        dir.create(directory, recursive = TRUE, showWarnings = FALSE)
        temporary <- tempfile("metadata-", tmpdir = directory)
        payload <- serialize(value, NULL)
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
        expired <- order[seq_along(order) > 512L | cumsum(sizes) > 256 * 1024^2]
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
