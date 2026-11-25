# Both caller globs and helper filenames are relative to the workspace root.
project_helper_relative_paths <- function(paths, glob = FALSE) {
    paths <- gsub("\\", "/", paths, fixed = TRUE)
    invalid <- is.na(paths) | !nzchar(paths) | endsWith(paths, "/") |
        grepl(r"(^(?:/|~|[[:alpha:]][[:alnum:].+-]*:)|(?:^|/)\.\.(?:/|$))", paths, perl = TRUE)
    if (!glob) invalid <- invalid | grepl("[*?]", paths)
    paths <- as.character(fs::path_norm(paths[!invalid]))
    paths[paths != "."]
}

# Apply the same file and parent-directory globs as workspace discovery. Work
# with normalized relative paths so siblings share their ancestor checks.
project_helper_allowed <- function(paths, include, exclude) {
    keep <- index_glob_matches(paths, include) & !index_glob_matches(paths, exclude)
    if (!length(exclude)) return(keep)
    parents <- dirname(paths)
    while (any(keep & parents != ".")) {
        dirs <- unique(parents[keep & parents != "."])
        excluded <- dirs[index_glob_matches(paste0(dirs, "/"), exclude)]
        keep[parents %in% excluded] <- FALSE
        parents <- dirname(parents)
    }
    keep
}

project_helper_rules <- function(root, settings, include = NULL, exclude = NULL) {
    if (!length(root) || !nzchar(root) || is.null(names(settings)) ||
            !(is.list(settings) || is.character(settings))) return(list())
    rules <- list()
    for (i in seq_along(settings)) {
        caller <- project_helper_relative_paths(names(settings)[[i]], glob = TRUE)
        helpers <- settings[[i]]
        # LSP JSON arrays arrive as lists. R options normally use vectors.
        if (is.list(helpers) && is.null(names(helpers)) && all(vapply(helpers, is.character, logical(1L)))) {
            helpers <- unlist(helpers, use.names = FALSE)
        }
        if (!length(caller) || !is.character(helpers)) next
        helpers <- unique(project_helper_relative_paths(helpers))
        if (!is.null(include)) helpers <- helpers[project_helper_allowed(helpers, include, exclude)]
        if (!length(helpers)) next
        paths <- file.path(root, helpers)
        rules[[length(rules) + 1L]] <- list(
            regex = index_glob_regex(caller), paths = stats::setNames(paths, path_to_uri(paths)))
    }
    rules
}

# Resolve existing ancestors, including symlinks, for newly created editor
# buffers. Check at lookup time so a changed link cannot reuse an old rule.
project_helper_in_root <- function(paths, real_root) {
    missing <- !file.exists(paths)
    while (any(missing)) {
        parents <- dirname(paths[missing])
        climb <- parents != paths[missing] & !file.exists(parents)
        paths[missing] <- parents
        missing[missing] <- climb
    }
    real <- normalizePath(paths, winslash = "/", mustWork = FALSE)
    real == real_root | startsWith(real, paste0(sub("/+$", "", real_root), "/"))
}

project_helper_paths <- function(rules, root, uri) {
    if (!length(rules)) return(character())
    path <- path_from_uri(uri)
    if (!length(path) || !nzchar(path) || !path_has_parent(path, root)) return(character())
    relative <- fs::path_rel(path, start = root)
    paths <- unlist(lapply(rules, function(rule) {
        if (index_glob_match(relative, NULL, regex = rule$regex)) rule$paths
    }))
    paths <- paths[!duplicated(paths) & paths != path]
    if (!length(paths)) return(character())
    real_root <- normalizePath(root, winslash = "/", mustWork = FALSE)
    contained <- project_helper_in_root(c(path, paths), real_root)
    if (!contained[[1L]] || !any(contained[-1L])) return(character())
    paths[contained[-1L]]
}
