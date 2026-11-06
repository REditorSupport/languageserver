# Resolution reuse validates installed files without loading a namespace or
# running package hooks on the server's request loop. Batch file stats on warm
# requests; rebuild the dependency closure only after an installation changes.
package_resolution_installations <- local({
    cache <- NULL
    stamp <- function(files) {
        if (.Platform$OS.type == "windows") {
            info <- fs::file_info(files, fail = FALSE, follow = TRUE)
            return(list(files = files, size = as.numeric(info$size), type = info$type,
                    mtime = as.numeric(info$modification_time), ctime = as.numeric(info$change_time),
                    device = info$device_id, inode = info$inode))
        }
        # POSIX ctime changes even when size and mtime are restored. Base stats
        # follow symlink targets and avoid fs's per-file overhead on this path;
        # Windows needs fs's change-time and file-identity fields instead.
        info <- file.info(files, extra_cols = FALSE)
        list(files = files, size = info$size, directory = info$isdir, mode = as.integer(info$mode),
            mtime = as.numeric(info$mtime), ctime = as.numeric(info$ctime))
    }
    function(packages, lib_paths) {
        if (is.null(cache)) cache <<- ByteLruCache$new(8 * 1024^2, 64L)
        packages <- sort(unique(normalize_package_request(packages)))
        key <- digest::digest(list(packages, lib_paths), algo = "xxhash64")
        previous <- cache$get(key)
        tryCatch({
            if (!is.null(previous) && identical(stamp(previous$files), previous$stamp)) {
                return(previous$result)
            }
            pending <- packages
            visited <- character()
            files <- character()
            guard <- stamp(character())
            complete <- TRUE
            while (length(pending)) {
                package <- pending[[1L]]
                pending <- pending[-1L]
                if (package %in% visited) next
                if (length(visited) >= 512L || length(files) > 4096L) {
                    complete <- FALSE
                    break
                }
                visited <- c(visited, package)
                # Check package candidates in every library so an earlier
                # installation invalidates precedence. Unrelated library-root
                # files (including covr worker traces) are not package changes.
                candidates <- unlist(lapply(lib_paths, function(library) {
                    file.path(library, package, c("DESCRIPTION", "Meta/package.rds"))
                }), use.names = FALSE)
                before <- stamp(candidates)
                guard <- stats::setNames(lapply(names(guard), function(name) {
                    c(guard[[name]], before[[name]])
                }), names(guard))
                files <- c(files, candidates)
                path <- find.package(package, lib.loc = lib_paths, quiet = TRUE)
                if (length(path) != 1L) {
                    complete <- FALSE
                    next
                }
                description <- file.path(path, "DESCRIPTION")
                directories <- file.path(path, c("R", "data", "help", "libs", "Meta"))
                before <- stamp(c(path, description, directories))
                guard <- stats::setNames(lapply(names(guard), function(name) {
                    c(guard[[name]], before[[name]])
                }), names(guard))
                dependencies <- read.dcf(description, fields = c("Depends", "Imports", "LinkingTo"))
                dependencies <- dependencies[!is.na(dependencies)]
                dependencies <- trimws(unlist(strsplit(dependencies, ",", fixed = TRUE), use.names = FALSE))
                dependencies <- trimws(sub("[[:space:]]*\\(.*\\)$", "", dependencies))
                pending <- union(pending, setdiff(dependencies[nzchar(dependencies)], c("R", visited)))
                # Installed R databases live directly in these directories;
                # native libraries can also have one architecture directory.
                # Avoid recursively scanning arbitrary package resources.
                nested <- list.dirs(file.path(path, "libs"), recursive = FALSE, full.names = TRUE)
                nested <- nested[!grepl("\\.dSYM$", nested)]
                if (any(vapply(nested, function(directory) {
                    children <- list.dirs(directory, recursive = FALSE)
                    any(!grepl("\\.dSYM$", children))
                }, logical(1L)))) complete <- FALSE
                metadata <- c(unlist(lapply(setdiff(directories, file.path(path, "libs")), function(directory) {
                    # R/<package> is the executable namespace loader and has
                    # no extension. Include it alongside the lazy databases.
                    if (identical(directory, file.path(path, "R"))) {
                        list.files(directory, full.names = TRUE)
                    } else {
                        list.files(directory, pattern = "\\.(rdb|rdx|rds)$", full.names = TRUE)
                    }
                }), use.names = FALSE), unlist(lapply(c(file.path(path, "libs"), nested), function(directory) {
                    list.files(directory, full.names = TRUE)
                }), use.names = FALSE))
                metadata <- metadata[!grepl("\\.dSYM$", metadata)]
                files <- c(files, path, description, file.path(path, "NAMESPACE"), directories, nested, metadata)
                if (length(files) > 4096L) {
                    complete <- FALSE
                    break
                }
            }
            files <- sort(unique(files))
            token <- stamp(files)
            # A reinstall during enumeration must not associate a new file
            # token with a dependency closure read from the old installation.
            if (!identical(guard, stamp(guard$files))) complete <- FALSE
            result <- list(key = digest::digest(token, algo = "xxhash64"),
                complete = complete, packages = visited)
            if (complete) cache$set(key, list(files = files, stamp = token, result = result))
            result
        }, error = function(e) list(key = NULL, complete = FALSE, packages = packages))
    }
})

package_resolution_request <- function(packages, namespace_packages, prepare, prepare_s3,
    lib_paths = .libPaths(), cache_dir = package_metadata_cache_root(), installations = NULL) {
    packages <- normalize_package_request(packages)
    namespace_packages <- normalize_package_request(unique(namespace_packages))
    if (is.null(installations)) {
        installations <- package_resolution_installations(unique(c(startup_packages, packages, namespace_packages)), lib_paths)
    }
    list(packages = packages, namespace_packages = namespace_packages,
        prepare = prepare, prepare_s3 = prepare_s3, lib_paths = lib_paths, cache_dir = cache_dir,
        settings = list(members = isTRUE(lsp_settings$get("member_completion")),
            s3 = isTRUE(lsp_settings$get("s3_dispatch"))),
        locale = Sys.getlocale(), r = as.character(getRversion()),
        installations = installations)
}

package_resolution_cacheable <- function(result, request) {
    requested <- list(packages = request$packages, namespace_packages = request$namespace_packages)
    if (!isTRUE(request$installations$complete) || !is.list(result) ||
            !identical(result$requested, requested) ||
            !all(request$packages %in% result$packages)) return(FALSE)
    # Hooks can attach additional packages beyond DESCRIPTION dependencies.
    # Such results remain useful, but need a new isolated worker next time.
    if (!all(result$packages %in% request$installations$packages)) return(FALSE)
    catalog_ok <- function(catalog) {
        is.list(catalog) && identical(catalog$schema, 1L) && is.null(catalog$error) &&
            is.null(catalog$preparation_error) && !isTRUE(catalog$dataset_only)
    }
    if (isTRUE(request$prepare)) {
        required <- unique(c(setdiff(result$packages, startup_packages), request$namespace_packages))
        if (!all(required %in% names(result$members)) ||
                !all(vapply(result$members, catalog_ok, logical(1L)))) return(FALSE)
    }
    if (isTRUE(request$prepare_s3)) {
        required <- unique(c(startup_packages, result$packages, request$namespace_packages,
                unlist(lapply(result$s3, `[[`, "dependencies"), use.names = FALSE)))
        if (!all(required %in% names(result$s3)) ||
                !all(vapply(result$s3, catalog_ok, logical(1L))) ||
                !all(required %in% request$installations$packages)) return(FALSE)
    }
    TRUE
}

package_task_sharing <- function(self, uri, document, request, callback, error, refresh = FALSE) {
    if (!isTRUE(request$installations$complete)) return(NULL)
    workspace <- self$get_workspace(uri)
    requested <- document$requested_packages
    current_document <- function() {
        identical(self$get_workspace(uri), workspace) &&
            identical(workspace$documents$get(uri, NULL), document) &&
            identical(document$requested_packages, requested)
    }
    sharing <- new.env(parent = emptyenv())
    sharing$key <- digest::digest(request, algo = "xxhash64")
    sharing$refresh <- refresh
    sharing$valid <- function() {
        current_document() &&
            identical(.libPaths(), request$lib_paths) &&
            identical(package_metadata_cache_root(), request$cache_dir) &&
            identical(Sys.getlocale(), request$locale) &&
            identical(request$settings, list(members = isTRUE(lsp_settings$get("member_completion")),
                    s3 = isTRUE(lsp_settings$get("s3_dispatch"))))
    }
    sharing$current <- function() {
        identical(request, package_resolution_request(request$packages, request$namespace_packages,
                request$prepare, request$prepare_s3))
    }
    sharing$callback <- callback
    sharing$error <- error
    sharing$complete <- callback
    sharing$fail <- error
    sharing$cacheable <- function(result) package_resolution_cacheable(result, request)
    sharing$invalidated <- function() {
        if (!current_document()) return(invisible(NULL))
        # An obsolete metadata result cannot satisfy an earlier save/refresh.
        # Retain that request through pending parses; current buffers can retry
        # immediately using their accepted imports and the new installations.
        document$member_refresh <- TRUE
        parsed <- document$parse_data
        if (is.null(parsed) || !identical(parsed$version, document$version) ||
                !identical(document$parsed_content, document$content) ||
                (isTRUE(parsed$parse_error) && !length(parsed$member_data$packages)) ||
                is.null(self$resolve_task_manager)) return(invisible(NULL))
        packages <- if (is.null(requested)) request$packages else requested$packages
        namespace_packages <- if (is.null(requested)) request$namespace_packages else requested$namespace_packages
        self$resolve_task_manager$add_task(uri,
            resolve_task(self, uri, document, packages, namespace_packages = namespace_packages))
        document$member_refresh <- FALSE
        invisible(NULL)
    }
    sharing$settled <- function() s3_queue_ready(self, uri)
    sharing
}
