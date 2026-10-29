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
    hash <- digest::digest(list(format = 1L, type = type, key = key, identity = identity,
            server = package_metadata_cache_server()), algo = "xxhash64")
    file.path(directory, paste0(identity$package, "-", type, "-", hash, ".rds"))
}

package_metadata_cache_read <- function(path, identity, validate = is.list) {
    if (is.null(path) || !file.exists(path)) return(NULL)
    tryCatch({
        record <- readRDS(path)
        if (!identical(record$format, 1L) || !identical(record$server, package_metadata_cache_server()) ||
                !identical(record$identity, identity) ||
                !isTRUE(validate(record$value))) return(NULL)
        suppressWarnings(Sys.setFileTime(path, Sys.time()))
        record$value
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
        saveRDS(list(format = 1L, server = package_metadata_cache_server(), identity = identity, value = value), temporary)
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
    list(packages = lapply(packages, function(package) {
        tryCatch(package_metadata_identity(package), error = function(e) list(package = package, error = conditionMessage(e)))
    }), bindings = s3_metadata_context(packages))
}
