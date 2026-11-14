startup_bundle_rcmd_install <- function(
    pkgs,
    library,
    wd,
    flags = c("--no-docs", "--no-help", "--no-byte-compile", "--no-test-load"),
    batch_size = 12L
) {
    wd_norm <- normalizePath(wd, winslash = "/", mustWork = TRUE)
    lib_norm <- normalizePath(library, winslash = "/", mustWork = TRUE)
    expect_true(is.character(pkgs) && length(pkgs) >= 1L)
    expect_identical(pkgs, basename(pkgs))
    expect_true(all(nzchar(pkgs)))
    expect_true(all(dir.exists(file.path(wd_norm, pkgs))))

    lib_arg <- if (identical(dirname(lib_norm), wd_norm)) {
        basename(lib_norm)
    } else {
        lib_norm
    }
    lib_paths <- c(lib_norm, .libPaths())
    # All fixtures declare their DESCRIPTION Imports explicitly so installation
    # succeeds cleanly even under R CMD check's _R_CHECK_INSTALL_DEPENDS_=true.
    install_env <- c(callr::rcmd_safe_env(), "_R_CHECK_INSTALL_DEPENDS_" = "true")
    batches <- unname(split(pkgs, ceiling(seq_along(pkgs) / batch_size)))
    cmdargs_list <- vector("list", length(batches))

    for (idx in seq_along(batches)) {
        cmdargs <- c(flags, paste0("--library=", lib_arg), batches[[idx]])
        cmdargs_list[[idx]] <- cmdargs
        inst <- callr::rcmd(
            "INSTALL",
            cmdargs = cmdargs,
            libpath = lib_paths,
            wd = wd_norm,
            env = install_env,
            stdout = "|",
            stderr = "|",
            system_profile = FALSE,
            user_profile = FALSE
        )
        expect_identical(inst$status, 0L, info = paste(inst$stdout, inst$stderr, sep = "\n"))
    }

    invisible(list(library = lib_norm, batches = batches, cmdargs = cmdargs_list))
}

startup_bundle_write_pkg_source <- function(root, name, spec) {
    src <- file.path(root, name)
    dir.create(file.path(src, "R"), recursive = TRUE, showWarnings = FALSE)
    version <- if (!is.null(spec$version)) spec$version else "0.0.1"
    writeLines(c(
        paste("Package:", name),
        paste("Version:", version),
        "Title: Startup Bundle Test Fixture",
        "Description: Fixture package for startup bundle cache tests.",
        "License: MIT",
        "Author: Test Fixture",
        "Maintainer: Test Fixture <fixture@example.org>",
        spec$description
    ), file.path(src, "DESCRIPTION"))
    writeLines(spec$namespace, file.path(src, "NAMESPACE"))
    writeLines(spec$code, file.path(src, "R", "code.R"))
    if (length(spec$datasets)) {
        dir.create(file.path(src, "data"), recursive = TRUE, showWarnings = FALSE)
        for (ds_name in names(spec$datasets)) {
            ds_env <- new.env(parent = emptyenv())
            assign(ds_name, spec$datasets[[ds_name]], envir = ds_env)
            save(
                list = ds_name,
                file = file.path(src, "data", paste0(ds_name, ".rda")),
                envir = ds_env,
                version = 2
            )
        }
    }
    src
}

startup_bundle_clone_installed_pkg <- local({
    rds_cache <- new.env(parent = emptyenv())
    read_cached_rds <- function(path) {
        if (exists(path, envir = rds_cache, inherits = FALSE)) {
            return(get(path, envir = rds_cache, inherits = FALSE))
        }
        val <- readRDS(path)
        assign(path, val, envir = rds_cache)
        val
    }
    function(
        src_pkg_dir,
        dest_lib,
        new_pkg,
        version = "0.0.1",
        imports = character(),
        depends = character(),
        exports = character(),
        description_extra = character()
    ) {
        old_pkg <- basename(src_pkg_dir)
        dest <- file.path(dest_lib, new_pkg)
        unlink(dest, recursive = TRUE)
        dir.create(dest, recursive = TRUE, showWarnings = FALSE)
        expect_true(all(file.copy(
            list.files(src_pkg_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE),
            dest,
            recursive = TRUE
        )))
        r_dir <- file.path(dest, "R")
        for (ext in c("", ".rdx", ".rdb")) {
            from <- file.path(r_dir, paste0(old_pkg, ext))
            to <- file.path(r_dir, paste0(new_pkg, ext))
            if (file.exists(from) && !identical(from, to)) {
                expect_true(file.rename(from, to))
            }
        }
        has_lazydata <- file.exists(file.path(dest, "data", "Rdata.rdb"))
        desc_lines <- c(
            paste("Package:", new_pkg),
            paste("Version:", version),
            "Title: Cloned Fixture Package",
            paste("Description: Cloned installed fixture package", new_pkg),
            "License: MIT",
            "Author: Test Fixture",
            "Maintainer: Test Fixture <fixture@example.org>",
            if (length(depends)) paste("Depends:", paste(depends, collapse = ", ")),
            if (length(imports)) paste("Imports:", paste(imports, collapse = ", ")),
            if (has_lazydata) "LazyData: true",
            description_extra
        )
        writeLines(desc_lines, file.path(dest, "DESCRIPTION"))

        ns_lines <- c(
            if (length(imports)) sprintf("import(%s)", imports),
            if (length(exports)) sprintf("export(%s)", exports)
        )
        writeLines(ns_lines, file.path(dest, "NAMESPACE"))

        src_pkg_rds <- file.path(src_pkg_dir, "Meta", "package.rds")
        pkg_rds_file <- file.path(dest, "Meta", "package.rds")
        if (file.exists(pkg_rds_file)) {
            pr <- read_cached_rds(src_pkg_rds)
            pr$DESCRIPTION[["Package"]] <- new_pkg
            pr$DESCRIPTION[["Version"]] <- version
            if (length(imports)) {
                pr$DESCRIPTION[["Imports"]] <- paste(imports, collapse = ", ")
            } else {
                pr$DESCRIPTION <- pr$DESCRIPTION[setdiff(names(pr$DESCRIPTION), "Imports")]
            }
            saveRDS(pr, pkg_rds_file)
        }

        src_ns_rds <- file.path(src_pkg_dir, "Meta", "nsInfo.rds")
        ns_rds_file <- file.path(dest, "Meta", "nsInfo.rds")
        if (file.exists(ns_rds_file)) {
            nr <- read_cached_rds(src_ns_rds)
            nr$imports <- lapply(imports, identity)
            if (length(exports)) nr$exports <- as.character(exports)
            saveRDS(nr, ns_rds_file)
        }
        dest
    }
})

# Build and install all required fixture packages once in 3 small batches
# (15 total package installs across the entire file), then clone per test via file.copy().
startup_bundle_shared_state <- local({
    state <- NULL
    function() {
        if (!is.null(state) && dir.exists(state$library)) return(state)
        root <- tempfile("startup-bundle-shared-")
        dir.create(root, recursive = TRUE)
        spaced_base <- file.path(root, "spaced parent")
        wd <- file.path(spaced_base, "pkg sources")
        library <- file.path(spaced_base, "external library")
        v2_library <- file.path(root, "v2_library")
        dir.create(wd, recursive = TRUE)
        dir.create(library, recursive = TRUE)
        dir.create(v2_library, recursive = TRUE)

        specs_batch1 <- list(
            transdepfixture = list(
                namespace = c(
                    "export(dep_helper)",
                    "export(racedep_generic)",
                    "S3method(racedep_generic, default)"
                ),
                code = c(
                    "dep_helper <- function(x) x",
                    "racedep_generic <- function(x, ...) UseMethod('racedep_generic')",
                    "racedep_generic.default <- function(x, old = 1) dep_helper(x)"
                )
            ),
            lazyfailfixture = list(
                description = "LazyData: true",
                namespace = "export(lazyfail_helper)",
                code = c(
                    "lazyfail_helper <- function() 1L",
                    ".onLoad <- function(libname, pkgname) {",
                    "    marker <- Sys.getenv('LANGSVR_LAZYFAIL_MARKER', unset = NA_character_)",
                    "    if (!is.na(marker) && nzchar(marker) && file.exists(marker)) {",
                    "        stop('transient .onLoad failure')",
                    "    }",
                    "}"
                ),
                datasets = list(transient_table = data.frame(col_x = 1:2, col_y = c("a", "b")))
            ),
            s3envhookfixture = list(
                namespace = "export(choose)",
                code = c(
                    "choose <- function(x, ...) UseMethod('choose')",
                    ".onLoad <- function(libname, pkgname) {",
                    "    mode <- Sys.getenv('LANGSVR_S3_HOOK_MODE', unset = 'old')",
                    "    ns <- asNamespace(pkgname)",
                    "    if (identical(mode, 'new')) {",
                    "        registerS3method('choose', 'default', function(x, new = 2) x + new, envir = ns)",
                    "    } else {",
                    "        registerS3method('choose', 'default', function(x, old = 1) x + old, envir = ns)",
                    "    }",
                    "}"
                )
            ),
            sneakyhookfixture = list(
                description = "LazyData: true",
                namespace = c("export(choose)", "export(sneaky_api)"),
                code = c(
                    "choose <- function(x, ...) UseMethod('choose')",
                    "sneaky_api <- function(x, static_arg = 1) UseMethod('sneaky_api')",
                    ".onLoad <- function(libname, pkgname) {",
                    "    mode <- Sys.getenv('LANGSVR_SNEAKY_MODE', unset = NA_character_)",
                    "    if (identical(mode, 'active')) {",
                    "        ns <- asNamespace(pkgname)",
                    "        registerS3method('choose', 'default', function(x, new = 2) x + new, envir = ns)",
                    "        replacement <- function(x, dynamic_arg = 2) UseMethod('sneaky_api')",
                    "        environment(replacement) <- ns",
                    "        assign('sneaky_api', replacement, envir = ns)",
                    "        lazy_env <- getNamespaceInfo(ns, 'lazydata')",
                    "        assign('sneaky_config', list(dynamic_key = 2L), envir = lazy_env)",
                    "    }",
                    "}"
                ),
                datasets = list(sneaky_config = list(static_key = 1L))
            ),
            transhookdep = list(
                namespace = "export(dep_noop)",
                code = c(
                    "dep_noop <- function(x) x",
                    ".onLoad <- function(libname, pkgname) {",
                    "    mode <- Sys.getenv('LANGSVR_TRANS_HOOK_MODE', unset = 'alpha')",
                    "    if (identical(mode, 'beta')) {",
                    "        registerS3method('mean', 'trans_hook_cls', function(x, beta_arg = 2, ...) x)",
                    "    } else {",
                    "        registerS3method('mean', 'trans_hook_cls', function(x, alpha_arg = 1, ...) x)",
                    "    }",
                    "}"
                )
            ),
            delayeds3fixture = list(
                description = "Imports: tools",
                namespace = c("export(plain_fn)", "S3method(tools::toRd, delayeds3class)"),
                code = c(
                    "plain_fn <- function(x) x",
                    "toRd.delayeds3class <- function(obj, ...) 'delayed'"
                )
            ),
            condsentinel = list(
                namespace = "export(sentinel_fn)",
                code = c(
                    "sentinel_fn <- function(x) x",
                    ".onLoad <- function(libname, pkgname) {",
                    "    path <- Sys.getenv('LANGSVR_COND_SENTINEL_MARKER')",
                    "    if (nzchar(path)) writeLines(pkgname, path)",
                    "    options(condsentinel.loaded = TRUE)",
                    "}"
                )
            ),
            condelse = list(
                namespace = "export(else_fn)",
                code = "else_fn <- function(x) x"
            )
        )

        specs_batch2 <- list(
            delayedpromisefixture = list(
                namespace = c("export(choose)", "S3method(choose, default)"),
                code = c(
                    "choose <- function(x, ...) UseMethod('choose')",
                    "delayedAssign('choose.default',",
                    "    if (identical(Sys.getenv('LANGSVR_DELAYED_MODE'), 'new')) {",
                    "        function(x, new = 2) x + new",
                    "    } else {",
                    "        function(x, old = 1) x + old",
                    "    })"
                )
            ),
            nestedpromisefixture = list(
                namespace = "export(nested_value)",
                code = c(
                    "nested_env <- new.env()",
                    "delayedAssign('inner', Sys.getenv('LANGSVR_DELAYED_MODE'), assign.env = nested_env)",
                    "nested_value <- local(function() inner, envir = nested_env)"
                )
            ),
            startupbundlefixture = list(
                description = c("Imports: transdepfixture", "LazyData: true"),
                namespace = c(
                    "import(transdepfixture)",
                    "export(bundle_generic)",
                    "S3method(bundle_generic, default)"
                ),
                code = c(
                    "bundle_generic <- function(x) UseMethod('bundle_generic')",
                    "bundle_generic.default <- function(x) dep_helper(x)"
                ),
                datasets = list(bundle_table = data.frame(col_a = 1:3, col_b = c("a", "b", "c")))
            ),
            derivedchainfixture = list(
                description = c("Imports: transdepfixture", "LazyData: true"),
                namespace = c("import(transdepfixture)", "export(derived_fn)"),
                code = c(
                    "derived_fn <- function() 1L",
                    ".onLoad <- function(libname, pkgname) {",
                    "    desc_path <- system.file('DESCRIPTION', package = 'chain70')",
                    "    dcf <- if (nzchar(desc_path) && file.exists(desc_path)) read.dcf(desc_path, fields = 'Note') else matrix(character(), 0L, 0L)",
                    "    col_name <- if (nrow(dcf) >= 1L) as.character(dcf[1L, 1L]) else NA_character_",
                    "    if (is.na(col_name) || !nzchar(col_name)) col_name <- 'old_column'",
                    "    lazy_env <- getNamespaceInfo(asNamespace(pkgname), 'lazydata')",
                    "    df <- data.frame(1L)",
                    "    names(df) <- col_name",
                    "    assign('derived_table', df, envir = lazy_env)",
                    "}"
                ),
                datasets = list(derived_table = data.frame(old_column = 1L))
            ),
            transhooktop = list(
                description = "Imports: transhookdep",
                namespace = c("import(transhookdep)", "export(top_fn)"),
                code = "top_fn <- function(x) dep_noop(x)"
            ),
            condfixture = list(
                description = "Imports: condsentinel, condelse",
                namespace = c(
                    "export(cond_fn)",
                    "if (requireNamespace('condsentinel', quietly = TRUE)) {",
                    "    import(condsentinel)",
                    "} else {",
                    "    importFrom(condelse, else_fn)",
                    "}",
                    "if (!identical(Sys.setenv(LANGSVR_COND_EVALUATED = 'yes'), TRUE)) import(condnever)"
                ),
                code = "cond_fn <- function(x) x"
            ),
            condstable = list(
                description = "Imports: condelse",
                namespace = c(
                    "export(stable_fn)",
                    "if (getRversion() >= '3.0.0') importFrom(condelse, else_fn) else import(condold)"
                ),
                code = "stable_fn <- function(x) x"
            )
        )

        for (nm in names(specs_batch1)) startup_bundle_write_pkg_source(wd, nm, specs_batch1[[nm]])
        for (nm in names(specs_batch2)) startup_bundle_write_pkg_source(wd, nm, specs_batch2[[nm]])

        # Install all 15 packages in 2 batches (8 in Batch 1, 7 in Batch 2)
        # inside paths containing spaces!
        inst_info <- startup_bundle_rcmd_install(
            c(names(specs_batch1), names(specs_batch2)),
            library = library,
            wd = wd,
            batch_size = 8L
        )

        # Clone pure-R declarative fixtures (nodatasetfixture, declarativefixture) from condelse
        startup_bundle_clone_installed_pkg(
            file.path(library, "condelse"),
            library,
            "nodatasetfixture",
            exports = "else_fn"
        )
        startup_bundle_clone_installed_pkg(
            file.path(library, "condelse"),
            library,
            "declarativefixture",
            exports = "else_fn"
        )

        # Also build v0.0.2 of transdepfixture (with formals x, new = 1) in v2_library (15th/16th install)
        v2_wd <- file.path(root, "v2_src")
        dir.create(v2_wd, recursive = TRUE)
        startup_bundle_write_pkg_source(v2_wd, "transdepfixture", list(
            version = "0.0.2",
            namespace = c(
                "export(dep_helper)",
                "export(racedep_generic)",
                "S3method(racedep_generic, default)"
            ),
            code = c(
                "dep_helper <- function(x) x",
                "racedep_generic <- function(x, ...) UseMethod('racedep_generic')",
                "racedep_generic.default <- function(x, new = 1) dep_helper(x)"
            )
        ))
        startup_bundle_rcmd_install(
            "transdepfixture",
            library = v2_library,
            wd = v2_wd,
            flags = c("--no-staged-install", "--no-test-load", "--no-docs", "--no-help", "--no-byte-compile"),
            batch_size = 10L
        )

        state <<- list(
            root = root,
            wd = wd,
            library = library,
            v2_library = v2_library,
            inst_info = inst_info
        )
        state
    }
})

# Unload every loaded namespace that was loaded from one of `libraries`, so no
# namespace outlives the temporary library it came from: a namespace whose
# package directory is gone breaks later code that walks loadedNamespaces()
# (e.g. utils::packageVersion()). Returns the namespaces that could not be
# unloaded.
startup_bundle_unload_library_namespaces <- function(libraries) {
    normalize <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)
    roots <- unique(normalize(libraries))
    from_libraries <- function() {
        Filter(function(pkg) {
            path <- tryCatch(getNamespaceInfo(pkg, "path"), error = function(e) NULL)
            if (!is.character(path) || length(path) != 1L || is.na(path)) return(FALSE)
            # Compare the containing library: unlike the package directory
            # itself, it still exists when a test removed the package.
            normalize(dirname(path)) %in% roots
        }, loadedNamespaces())
    }
    # unloadNamespace() refuses to unload a namespace that is still imported by
    # a loaded one, so keep sweeping (dependents go first) until nothing is
    # left or a sweep makes no progress.
    repeat {
        remaining <- from_libraries()
        if (!length(remaining)) break
        for (pkg in remaining) try(unloadNamespace(pkg), silent = TRUE)
        if (identical(from_libraries(), remaining)) break
    }
    from_libraries()
}

startup_bundle_install_fixtures <- function(
    pkgs = c("transdepfixture", "startupbundlefixture"),
    .local_envir = parent.frame()
) {
    shared <- startup_bundle_shared_state()
    root <- withr::local_tempdir(.local_envir = .local_envir)
    library <- file.path(root, "library")
    dir.create(library)
    for (pkg in pkgs) {
        expect_true(file.copy(file.path(shared$library, pkg), library, recursive = TRUE))
    }
    # Registered after local_tempdir(), so it runs before the library is deleted.
    # Tests add further packages to this library (clones, imports of clones), so
    # unload whatever was loaded from it rather than only `pkgs`.
    withr::defer(
        expect_identical(startup_bundle_unload_library_namespaces(library), character()),
        envir = .local_envir
    )
    list(
        root = root,
        library = library,
        startup_pkg = "startupbundlefixture",
        dep_pkg = "transdepfixture",
        v2_library = shared$v2_library
    )
}

startup_bundle_s3_fixture <- function(packages, deps = list(), lib_paths = .libPaths()) {
    lib_paths <- package_metadata_startup_lib_paths(lib_paths)
    all_pkgs <- unique(c(startup_packages, packages, unlist(deps, use.names = FALSE)))
    traversal <- package_metadata_transitive_packages(all_pkgs, lib_paths)
    manifest_pkgs <- if (isTRUE(traversal$complete)) unique(c(all_pkgs, traversal$packages)) else all_pkgs
    manifest <- package_metadata_manifest(manifest_pkgs, lib_paths)
    context <- if (!is.null(manifest)) list(packages = unname(manifest), cacheable = TRUE)
    stats::setNames(lapply(all_pkgs, function(pkg) {
        pkg_deps <- if (!is.null(deps[[pkg]])) deps[[pkg]] else character()
        list(
            schema = 1L,
            package = pkg,
            generation = paste0("gen-", pkg),
            dependencies = pkg_deps,
            functions = list(),
            callables = list(),
            methods = list(),
            objects = list(),
            identity = tryCatch(
                package_metadata_identity(pkg, lib_paths = lib_paths, context = context),
                error = function(e) NULL
            )
        )
    }), all_pkgs)
}

startup_bundle_members_fixture <- function(packages = "datasets", lib_paths = .libPaths()) {
    lib_paths <- package_metadata_startup_lib_paths(lib_paths)
    ws_pkgs <- tryCatch(workspace_startup_packages(), error = function(e) character())
    all_pkgs <- unique(c(startup_packages, ws_pkgs, packages))
    traversal <- package_metadata_transitive_packages(all_pkgs, lib_paths)
    manifest_pkgs <- if (isTRUE(traversal$complete)) unique(c(all_pkgs, traversal$packages)) else all_pkgs
    manifest <- package_metadata_manifest(manifest_pkgs, lib_paths)
    context <- if (!is.null(manifest)) list(packages = unname(manifest), cacheable = TRUE)
    stats::setNames(lapply(packages, function(pkg) {
        ns <- new.env(parent = baseenv())
        data <- new.env(parent = emptyenv())
        data$sample_frame <- data.frame(alpha = 1L, beta = "x")
        index <- member_package_index(member_namespace_input(
            ns, package = pkg, exports = character(), lazydata = data, lazydata_only = TRUE
        ))
        index$dataset_only <- TRUE
        index$identity <- tryCatch(
            package_metadata_identity(pkg, lib_paths = lib_paths, context = context),
            error = function(e) NULL
        )
        member_index_freeze(index)
    }), packages)
}

test_that("workspace_startup_packages uses a baseenv closure without loading languageserver", {
    closure_env <- environment(workspace_startup_packages)
    expect_true(is.function(closure_env$probe))
    expect_identical(environment(closure_env$probe), baseenv())
    expect_null(attributes(closure_env$probe))
    expect_null(attributes(body(closure_env$probe)))

    serialized_refs <- list()
    invisible(serialize(closure_env$probe, NULL, refhook = function(x) {
        serialized_refs[[length(serialized_refs) + 1L]] <<- x
        NULL
    }))
    expect_length(serialized_refs, 0L)

    captured_fn <- NULL
    captured_profiles <- NULL
    fresh_startup_packages <- workspace_startup_packages
    fresh_env <- new.env(parent = parent.env(closure_env))
    fresh_env$cached <- NULL
    fresh_env$probe <- closure_env$probe
    environment(fresh_startup_packages) <- fresh_env

    (function() {
        testthat::local_mocked_bindings(
            r = function(func, ..., system_profile = FALSE, user_profile = FALSE) {
                captured_fn <<- func
                captured_profiles <<- c(system = system_profile, user = user_profile)
                func()
            },
            .package = "callr"
        )
        pkgs <- fresh_startup_packages()
        expect_identical(pkgs, rev(.packages()))
    })()
    expect_true(is.function(captured_fn))
    expect_identical(environment(captured_fn), baseenv())
    expect_null(attributes(captured_fn))
    expect_identical(captured_profiles, c(system = TRUE, user = TRUE))

    trace_dir <- withr::local_tempdir()
    child_check <- withr::with_envvar(c(COVERAGE_DIR = trace_dir), {
        probe <- closure_env$probe
        runner <- as.function(
            list(
                fn = alist(fn = )$fn,
                call(
                    "list",
                    packages = call("fn"),
                    ls_loaded = call("%in%", "languageserver", call("loadedNamespaces"))
                )
            ),
            envir = baseenv()
        )
        out <- callr::r(
            runner,
            args = list(fn = probe),
            system_profile = FALSE,
            user_profile = FALSE
        )
        direct_pkgs <- callr::r(
            probe,
            system_profile = FALSE,
            user_profile = FALSE
        )
        out$direct_packages <- direct_pkgs
        out
    })
    expect_false(child_check$ls_loaded)
    expect_true("base" %in% child_check$packages)
    expect_identical(child_check$direct_packages, child_check$packages)
    expect_length(list.files(trace_dir, pattern = "^covr_trace_"), 0L)
})

startup_bundle_inspect_diagnostics_manager <- function() {
    fn <- function() {
        ns <- asNamespace("languageserver")
        orig_task_manager <- ns$TaskManager
        orig_workspace <- ns$Workspace
        restore_binding <- function(name, value) {
            unlockBinding(name, ns)
            assign(name, value, ns)
            lockBinding(name, ns)
        }
        server <- NULL
        on.exit({
            restore_binding("TaskManager", orig_task_manager)
            restore_binding("Workspace", orig_workspace)
            if (!is.null(server)) {
                server$parse_task_manager$stop()
                server$diagnostics_task_manager$stop()
                server$formatting_task_manager$stop()
                server$index_task_manager$stop()
                server$resolve_task_manager$stop()
                server$member_task_manager$stop()
            }
        }, add = TRUE)

        recorded <- list()
        spy_task_manager <- list(new = function(id, ..., min_idle_sessions = 1) {
            recorded[[id]] <<- list(id = id, min_idle_sessions = min_idle_sessions)
            effective_idle <- if (identical(id, "parse")) 0 else min_idle_sessions
            orig_task_manager$new(id, ..., min_idle_sessions = effective_idle)
        })
        fake_workspace <- list(index = list(enabled = FALSE))
        restore_binding("TaskManager", spy_task_manager)
        restore_binding("Workspace", list(new = function(root) fake_workspace))

        server <- ns$LanguageServer$new("localhost", NULL)
        diag_private <- server$diagnostics_task_manager$.__enclos_env__$private
        list(
            recorded_idle = recorded$diagnostics$min_idle_sessions,
            private_idle = diag_private$min_idle_sessions,
            session_count = length(diag_private$sessions)
        )
    }
    environment(fn) <- baseenv()
    callr::r(fn, libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE)
}

test_that("LanguageServer initializes diagnostics_task_manager with min_idle_sessions = 0", {
    inspected <- startup_bundle_inspect_diagnostics_manager()
    expect_identical(inspected$recorded_idle, 0)
    expect_equal(inspected$private_idle, 0)
    expect_identical(inspected$session_count, 0L)
})

test_that("Startup S3 and member bundles hit warm disk cache without loading namespaces", {
    fixtures <- startup_bundle_install_fixtures()
    cache_dir <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = cache_dir)
    withr::local_libpaths(c(fixtures$library, .libPaths()))
    target_packages <- c(startup_packages, fixtures$startup_pkg)

    allow_workers <- TRUE
    s3_worker_calls <- 0L
    mem_worker_calls <- 0L
    live_id_calls <- 0L
    s3_ctx_calls <- 0L
    real_s3_prepare <- workspace_startup_s3_prepare
    real_mem_prepare <- workspace_startup_members_prepare
    real_live_id <- package_metadata_live_identity
    real_s3_ctx <- s3_metadata_context
    first_s3 <- NULL
    first_mem <- NULL
    (function() {
        testthat::local_mocked_bindings(
            workspace_startup_packages = function() target_packages,
            workspace_startup_s3_prepare = function(requested) {
                s3_worker_calls <<- s3_worker_calls + 1L
                if (!allow_workers) stop("S3 worker must not run on warm bundle hit")
                real_s3_prepare(requested)
            },
            workspace_startup_members_prepare = function(requested) {
                mem_worker_calls <<- mem_worker_calls + 1L
                if (!allow_workers) stop("Members worker must not run on warm bundle hit")
                real_mem_prepare(requested)
            },
            package_metadata_live_identity = function(...) {
                live_id_calls <<- live_id_calls + 1L
                if (!allow_workers) stop("package_metadata_live_identity must not run on bundle hit")
                real_live_id(...)
            },
            s3_metadata_context = function(...) {
                s3_ctx_calls <<- s3_ctx_calls + 1L
                if (!allow_workers) stop("s3_metadata_context must not run on bundle hit")
                real_s3_ctx(...)
            },
            .package = "languageserver"
        )

        expect_false(fixtures$startup_pkg %in% loadedNamespaces())
        expect_false(fixtures$dep_pkg %in% loadedNamespaces())

        s3_cold <- workspace_startup_s3_cache()
        first_s3 <<- s3_cold()
        expect_identical(s3_worker_calls, 1L)
        expect_true(all(c(startup_packages, fixtures$startup_pkg, fixtures$dep_pkg) %in% names(first_s3)))

        mem_cold <- workspace_startup_members_cache()
        first_mem <<- mem_cold()
        expect_identical(mem_worker_calls, 1L)
        expect_true(all(c("datasets", fixtures$startup_pkg) %in% names(first_mem)))
        expect_identical(
            sort(names(first_mem[[fixtures$startup_pkg]]$roots$bundle_table$fields)),
            c("col_a", "col_b")
        )

        expect_identical(live_id_calls, 0L)
        expect_identical(s3_ctx_calls, 0L)
        expect_false(fixtures$startup_pkg %in% loadedNamespaces())
        expect_false(fixtures$dep_pkg %in% loadedNamespaces())

        bundle_files <- list.files(package_metadata_cache_directory(), full.names = TRUE)
        expect_length(grep("^startup-startup-s3-.*\\.rds$", basename(bundle_files)), 1L)
        expect_length(grep("^startup-startup-members-bundle-.*\\.rds$", basename(bundle_files)), 1L)

        # Mutating base S3 methods in the server process must not invalidate
        # filesystem-keyed startup bundles.
        s3_table <- get(".__S3MethodsTable__.", asNamespace("base"))
        polluted_method <- "print.startup_bundle_s3_table_fixture"
        withr::defer(if (exists(polluted_method, envir = s3_table, inherits = FALSE)) {
            rm(list = polluted_method, envir = s3_table)
        })
        assign(polluted_method, function(x, ...) x, envir = s3_table)

        allow_workers <<- FALSE
        before_loaded <- loadedNamespaces()
        s3_warm <- workspace_startup_s3_cache()
        mem_warm <- workspace_startup_members_cache()
        expect_identical(s3_warm(), first_s3)
        expect_identical(mem_warm(), first_mem)
        expect_identical(s3_worker_calls, 1L)
        expect_identical(mem_worker_calls, 1L)
        expect_identical(live_id_calls, 0L)
        expect_identical(s3_ctx_calls, 0L)
        expect_identical(loadedNamespaces(), before_loaded)
        expect_false(fixtures$startup_pkg %in% loadedNamespaces())
        expect_false(fixtures$dep_pkg %in% loadedNamespaces())
    })()

    # Also verify cross-process sharing across unmodified R processes:
    # write a default startup_packages bundle in the parent process (reusing the
    # already-prepared base R catalogs/snapshots from first_s3 / first_mem so no
    # second cold worker subprocess is needed), then verify warm_proc and miss_proc
    # in unmodified child R processes.
    cross_cache <- withr::local_tempdir()
    base_libs <- .libPaths()[-1L]
    default_pkgs <- workspace_startup_packages()
    worker_rds <- list.files(
        file.path(cache_dir, "package-metadata"),
        pattern = "^base-s3-.*\\.rds$",
        full.names = TRUE
    )
    worker_server <- if (length(worker_rds)) {
        readRDS(worker_rds[[1L]])$server
    } else {
        package_metadata_cache_server()
    }
    withr::with_options(list(languageserver.package_cache_dir = cross_cache), {
        withr::with_libpaths(base_libs, {
            testthat::with_mocked_bindings(
                package_metadata_cache_server = function() worker_server,
                .package = "languageserver",
                {
                    base_s3_key <- package_metadata_startup_bundle_key("startup-s3", default_pkgs, base_libs)
                    base_mem_key <- package_metadata_startup_bundle_key(
                        "startup-members-bundle", default_pkgs, base_libs)
                    base_pkgs <- names(first_s3)[names(first_s3) %in% names(base_s3_key$manifest)]
                    base_manifest <- base_s3_key$manifest
                    base_s3_cats <- first_s3[base_pkgs]
                    for (pkg in base_pkgs) {
                        base_s3_cats[[pkg]]$identity <- package_metadata_identity(
                            pkg,
                            lib_paths = base_libs,
                            context = list(packages = unname(base_manifest), cacheable = TRUE)
                        )
                    }
                    base_mem_snaps <- first_mem["datasets"]
                    base_mem_snaps$datasets$identity <- package_metadata_identity(
                        "datasets",
                        lib_paths = base_libs,
                        context = list(packages = unname(base_manifest), cacheable = TRUE)
                    )
                    package_metadata_startup_s3_write(base_s3_key, default_pkgs, base_s3_cats)
                    package_metadata_startup_members_write(
                        base_mem_key,
                        default_pkgs,
                        list(
                            .startup_members_result = TRUE,
                            snapshots = base_mem_snaps,
                            inspected = default_pkgs,
                            errors = list(),
                            identities = base_manifest,
                            complete = TRUE
                        )
                    )
                }
            )
        })
    })

    spy_fn <- function(cache_dir) {
        options(languageserver.package_cache_dir = cache_dir)
        real_r <- callr::r
        probe_calls <- 0L
        worker_calls <- 0L
        ns_callr <- asNamespace("callr")
        unlockBinding("r", ns_callr)
        assign("r", function(func, ...) {
            if (is.function(func) && identical(environment(func), baseenv()) &&
                    identical(body(func), quote(rev(.packages())))) {
                probe_calls <<- probe_calls + 1L
                return(real_r(func, ...))
            }
            worker_calls <<- worker_calls + 1L
            stop("Unexpected startup worker callr::r on warm cross-process hit")
        }, ns_callr)
        lockBinding("r", ns_callr)
        ws <- languageserver:::Workspace$new(NULL)
        list(
            probe_calls = probe_calls,
            worker_calls = worker_calls,
            s3 = ws$s3_metadata$keys(),
            members = ws$member_metadata$keys()
        )
    }
    environment(spy_fn) <- baseenv()

    warm_proc <- callr::r(spy_fn, args = list(cache_dir = cross_cache),
        libpath = base_libs, system_profile = FALSE, user_profile = FALSE)
    expect_identical(warm_proc$probe_calls, 1L)
    expect_identical(warm_proc$worker_calls, 0L)
    expect_setequal(warm_proc$s3, startup_packages)
    expect_setequal(warm_proc$members, "datasets")

    # Confirm the spy is non-vacuous: an empty cache dir intercepts both workers.
    miss_proc <- callr::r(spy_fn, args = list(cache_dir = withr::local_tempdir()),
        libpath = base_libs, system_profile = FALSE, user_profile = FALSE)
    expect_identical(miss_proc$probe_calls, 1L)
    expect_identical(miss_proc$worker_calls, 2L)
    expect_length(miss_proc$s3, 0L)
    expect_length(miss_proc$members, 0L)
})

test_that("Startup bundles invalidate when startup, transitive, or server state changes", {
    fixtures <- startup_bundle_install_fixtures()
    cache_dir <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = cache_dir)
    withr::local_libpaths(c(fixtures$library, .libPaths()))
    target_packages <- c(startup_packages, fixtures$startup_pkg)
    testthat::local_mocked_bindings(
        workspace_startup_packages = function() target_packages,
        .package = "languageserver"
    )

    s3_calls <- 0L
    mem_calls <- 0L
    deps_map <- stats::setNames(list(fixtures$dep_pkg), fixtures$startup_pkg)
    make_s3 <- function() {
        workspace_startup_s3_cache(prepare = function(requested) {
            s3_calls <<- s3_calls + 1L
            startup_bundle_s3_fixture(requested$packages, deps = deps_map, lib_paths = requested$lib_paths)
        }, persist = TRUE)
    }
    make_mem <- function() {
        workspace_startup_members_cache(prepare = function(requested) {
            mem_calls <<- mem_calls + 1L
            startup_bundle_members_fixture(c("datasets", fixtures$startup_pkg), lib_paths = requested$lib_paths)
        }, persist = TRUE)
    }

    expect_true(fixtures$dep_pkg %in% names(make_s3()()))
    expect_true(fixtures$startup_pkg %in% names(make_mem()()))
    expect_identical(s3_calls, 1L)
    expect_identical(mem_calls, 1L)

    # Fresh closures hit disk cache without invoking prepare.
    expect_true(fixtures$dep_pkg %in% names(make_s3()()))
    expect_true(fixtures$startup_pkg %in% names(make_mem()()))
    expect_identical(s3_calls, 1L)
    expect_identical(mem_calls, 1L)

    # 1. Transitive dependency DESCRIPTION modified on disk (even preserving size & mtime).
    dep_desc <- file.path(fixtures$library, fixtures$dep_pkg, "DESCRIPTION")
    stamp <- file.info(dep_desc)
    desc_lines <- readLines(dep_desc)
    writeLines(sub("Startup Bundle Test Fixture", "Startup Bundle Test Updated", desc_lines, fixed = TRUE), dep_desc)
    Sys.setFileTime(dep_desc, stamp$mtime)
    expect_true(fixtures$dep_pkg %in% names(make_s3()()))
    expect_true(fixtures$startup_pkg %in% names(make_mem()()))
    expect_identical(s3_calls, 2L)
    expect_identical(mem_calls, 2L)
    # Updated bundles hit again on subsequent fresh closures.
    make_s3()()
    make_mem()()
    expect_identical(s3_calls, 2L)
    expect_identical(mem_calls, 2L)

    # 2. Transitive dependency NAMESPACE modified on disk.
    dep_ns <- file.path(fixtures$library, fixtures$dep_pkg, "NAMESPACE")
    writeLines(c(readLines(dep_ns), "# transitive namespace change"), dep_ns)
    make_s3()()
    make_mem()()
    expect_identical(s3_calls, 3L)
    expect_identical(mem_calls, 3L)
    make_s3()()
    make_mem()()
    expect_identical(s3_calls, 3L)
    expect_identical(mem_calls, 3L)

    # 3. Startup package DESCRIPTION modified on disk.
    startup_desc <- file.path(fixtures$library, fixtures$startup_pkg, "DESCRIPTION")
    writeLines(c(readLines(startup_desc), "# startup description change"), startup_desc)
    make_s3()()
    make_mem()()
    expect_identical(s3_calls, 4L)
    expect_identical(mem_calls, 4L)

    # 4. Startup package NAMESPACE modified on disk.
    startup_ns <- file.path(fixtures$library, fixtures$startup_pkg, "NAMESPACE")
    writeLines(c(readLines(startup_ns), "# startup namespace change"), startup_ns)
    make_s3()()
    make_mem()()
    expect_identical(s3_calls, 5L)
    expect_identical(mem_calls, 5L)
    make_s3()()
    make_mem()()
    expect_identical(s3_calls, 5L)
    expect_identical(mem_calls, 5L)

    # 5. Server implementation fingerprint change invalidates both bundles.
    ns <- asNamespace("languageserver")
    update_binding <- function(name, value) {
        unlockBinding(name, ns)
        assign(name, value, ns)
        lockBinding(name, ns)
    }
    original_intrinsics <- member_base_intrinsics
    withr::defer(update_binding("member_base_intrinsics", original_intrinsics))
    update_binding("member_base_intrinsics", c(original_intrinsics, "startup_bundle_server_change"))
    make_s3()()
    make_mem()()
    expect_identical(s3_calls, 6L)
    expect_identical(mem_calls, 6L)
    update_binding("member_base_intrinsics", original_intrinsics)

    # 6. Removing transitive dependency from disk invalidates existing bundles.
    unlink(file.path(fixtures$library, fixtures$dep_pkg), recursive = TRUE)
    deps_map <- list()
    make_s3()()
    make_mem()()
    expect_identical(s3_calls, 7L)
    expect_identical(mem_calls, 7L)
})

test_that("Incomplete S3 catalogs and failed workers are never persisted to disk", {
    cache_dir <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = cache_dir)
    testthat::local_mocked_bindings(
        workspace_startup_packages = function() startup_packages,
        .package = "languageserver"
    )

    complete_catalogs <- startup_bundle_s3_fixture(startup_packages)
    partial_catalogs <- complete_catalogs["base"]
    errored_catalogs <- complete_catalogs
    errored_catalogs$stats$error <- "worker error"
    bad_schema_catalogs <- complete_catalogs
    bad_schema_catalogs$stats$schema <- 2L
    missing_dep_catalogs <- complete_catalogs
    missing_dep_catalogs$stats$dependencies <- "unprepared_dep_pkg"

    now <- 0
    s3_calls <- 0L
    load_s3 <- workspace_startup_s3_cache(prepare = function(requested) {
        s3_calls <<- s3_calls + 1L
        if (s3_calls == 1L) stop("worker timed out")
        if (s3_calls == 2L) return(partial_catalogs)
        if (s3_calls == 3L) return(errored_catalogs)
        if (s3_calls == 4L) return(bad_schema_catalogs)
        if (s3_calls == 5L) return(missing_dep_catalogs)
        complete_catalogs
    }, clock = function() now, persist = TRUE)

    s3_bundle_files <- function() {
        dir <- package_metadata_cache_directory()
        if (is.null(dir) || !dir.exists(dir)) return(character())
        list.files(dir, pattern = "^startup-startup-s3-.*\\.rds$", full.names = TRUE)
    }

    expect_identical(load_s3(), list())
    expect_length(s3_bundle_files(), 0L)
    now <- 29
    expect_identical(load_s3(), list())
    expect_identical(s3_calls, 1L)

    now <- 30
    expect_identical(load_s3(), partial_catalogs)
    expect_length(s3_bundle_files(), 0L)
    now <- 89
    expect_identical(load_s3(), partial_catalogs)
    expect_identical(s3_calls, 2L)

    now <- 90
    expect_identical(load_s3(), errored_catalogs)
    expect_length(s3_bundle_files(), 0L)

    now <- 150
    expect_identical(load_s3(), bad_schema_catalogs)
    expect_length(s3_bundle_files(), 0L)

    now <- 210
    expect_identical(load_s3(), missing_dep_catalogs)
    expect_length(s3_bundle_files(), 0L)

    now <- 270
    expect_identical(load_s3(), complete_catalogs)
    expect_identical(s3_calls, 6L)
    expect_length(s3_bundle_files(), 1L)

    fresh_calls <- 0L
    fresh_s3 <- workspace_startup_s3_cache(prepare = function(requested) {
        fresh_calls <<- fresh_calls + 1L
        stop("must hit persisted complete bundle")
    }, persist = TRUE)
    expect_identical(fresh_s3(), complete_catalogs)
    expect_identical(fresh_calls, 0L)

    mem_calls <- 0L
    mem_fixture <- startup_bundle_members_fixture("datasets")
    load_mem <- workspace_startup_members_cache(prepare = function(requested) {
        mem_calls <<- mem_calls + 1L
        if (mem_calls == 1L) stop("member worker failed")
        mem_fixture
    }, persist = TRUE)
    expect_identical(load_mem(), list())
    mem_files <- list.files(package_metadata_cache_directory(),
        pattern = "^startup-startup-members-bundle-.*\\.rds$", full.names = TRUE)
    expect_length(mem_files, 0L)
    expect_identical(load_mem(), mem_fixture)
    expect_identical(mem_calls, 2L)
    mem_files <- list.files(package_metadata_cache_directory(),
        pattern = "^startup-startup-members-bundle-.*\\.rds$", full.names = TRUE)
    expect_length(mem_files, 1L)
})

test_that("Corrupted startup bundle files recover cleanly and disabled cache dirs bypass disk I/O", {
    cache_dir <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = cache_dir)
    testthat::local_mocked_bindings(
        workspace_startup_packages = function() startup_packages,
        .package = "languageserver"
    )

    s3_fixture <- startup_bundle_s3_fixture(startup_packages)
    mem_fixture <- startup_bundle_members_fixture("datasets")
    s3_calls <- 0L
    mem_calls <- 0L
    run_s3 <- function() {
        workspace_startup_s3_cache(prepare = function(requested) {
            s3_calls <<- s3_calls + 1L
            s3_fixture
        }, persist = TRUE)()
    }
    run_mem <- function() {
        workspace_startup_members_cache(prepare = function(requested) {
            mem_calls <<- mem_calls + 1L
            mem_fixture
        }, persist = TRUE)()
    }

    expect_identical(run_s3(), s3_fixture)
    expect_identical(run_mem(), mem_fixture)
    expect_identical(s3_calls, 1L)
    expect_identical(mem_calls, 1L)

    s3_path <- list.files(package_metadata_cache_directory(),
        pattern = "^startup-startup-s3-.*\\.rds$", full.names = TRUE)
    mem_path <- list.files(package_metadata_cache_directory(),
        pattern = "^startup-startup-members-bundle-.*\\.rds$", full.names = TRUE)
    expect_length(s3_path, 1L)
    expect_length(mem_path, 1L)

    writeLines("truncated rds bytes", s3_path)
    writeLines("truncated rds bytes", mem_path)
    expect_identical(run_s3(), s3_fixture)
    expect_identical(run_mem(), mem_fixture)
    expect_identical(s3_calls, 2L)
    expect_identical(mem_calls, 2L)
    expect_identical(run_s3(), s3_fixture)
    expect_identical(run_mem(), mem_fixture)
    expect_identical(s3_calls, 2L)
    expect_identical(mem_calls, 2L)

    s3_record <- readRDS(s3_path)
    s3_record$payload <- serialize(list(manifest = list(), catalogs = list()), NULL)
    saveRDS(s3_record, s3_path)
    mem_record <- readRDS(mem_path)
    mem_record$payload <- serialize(list(manifest = list(), snapshots = list()), NULL)
    saveRDS(mem_record, mem_path)
    expect_identical(run_s3(), s3_fixture)
    expect_identical(run_mem(), mem_fixture)
    expect_identical(s3_calls, 3L)
    expect_identical(mem_calls, 3L)

    disabled_dir <- file.path(withr::local_tempdir(), "must-not-exist")
    withr::local_options(languageserver.package_cache_dir = FALSE)
    expect_null(package_metadata_startup_bundle_key("startup-s3", startup_packages))
    expect_identical(run_s3(), s3_fixture)
    expect_identical(run_mem(), mem_fixture)
    expect_identical(s3_calls, 4L)
    expect_identical(mem_calls, 4L)
    expect_identical(run_s3(), s3_fixture)
    expect_identical(run_mem(), mem_fixture)
    expect_identical(s3_calls, 5L)
    expect_identical(mem_calls, 5L)
    expect_false(dir.exists(disabled_dir))

    withr::local_options(languageserver.package_cache_dir = "")
    expect_identical(package_metadata_cache_root(), FALSE)
    expect_null(package_metadata_startup_bundle_key("startup-s3", startup_packages))

    env_cache_dir <- file.path(withr::local_tempdir(), "env-cache")
    withr::local_options(languageserver.package_cache_dir = NULL)
    withr::local_envvar(R_LANGSVR_PACKAGE_CACHE_DIR = env_cache_dir)
    expect_identical(package_metadata_cache_root(), env_cache_dir)

    withr::local_envvar(R_LANGSVR_PACKAGE_CACHE_DIR = "FALSE")
    expect_identical(package_metadata_cache_root(), FALSE)
    expect_null(package_metadata_startup_bundle_key("startup-members-bundle", startup_packages))
    expect_identical(run_s3(), s3_fixture)
    expect_identical(run_mem(), mem_fixture)
    expect_identical(s3_calls, 6L)
    expect_identical(mem_calls, 6L)
    expect_identical(run_s3(), s3_fixture)
    expect_identical(run_mem(), mem_fixture)
    expect_identical(s3_calls, 7L)
    expect_identical(mem_calls, 7L)
    expect_false(dir.exists(env_cache_dir))

    withr::local_envvar(R_LANGSVR_PACKAGE_CACHE_DIR = "false")
    expect_identical(package_metadata_cache_root(), FALSE)

    if (.Platform$OS.type != "windows") {
        withr::local_envvar(R_LANGSVR_PACKAGE_CACHE_DIR = "")
        expect_identical(package_metadata_cache_root(), FALSE)
        expect_null(package_metadata_startup_bundle_key("startup-members-bundle", startup_packages))
    }
})

test_that("Partial per-package startup member failures do not persist or lock out recovery, while zero-dataset packages persist", {
    fail_pkg <- "lazyfailfixture"
    nodata_pkg <- "nodatasetfixture"
    fixtures <- startup_bundle_install_fixtures(c(fail_pkg, nodata_pkg, "transdepfixture", "startupbundlefixture"))
    root <- fixtures$root
    library <- fixtures$library
    marker_file <- file.path(root, "transient-fail.marker")

    libs <- c(library, .libPaths())
    cache_dir <- withr::local_tempdir()
    writeLines("fail", marker_file)

    # Use startupbundlefixture (2 rows) as the healthy dataset companion so we do not
    # cold-index all 104 base R datasets repeatedly in subprocesses.
    healthy_pkg <- "startupbundlefixture"

    # 1. Worker reports per-package failure explicitly even though healthy_pkg succeeded.
    status_res <- withr::with_envvar(c(LANGSVR_LAZYFAIL_MARKER = marker_file), {
        withr::local_libpaths(libs)
        detailed <- member_prepare_startup_data(
            c(healthy_pkg, fail_pkg),
            lib_paths = libs,
            cache_dir = cache_dir,
            include_status = TRUE
        )
        direct <- member_prepare_startup_data(
            c(healthy_pkg, fail_pkg),
            lib_paths = libs,
            cache_dir = cache_dir
        )
        list(
            complete = detailed$complete,
            inspected = detailed$inspected,
            error_pkgs = names(detailed$errors),
            snapshot_pkgs = names(detailed$snapshots),
            valid_detailed = package_metadata_startup_members_valid(
                detailed, c(healthy_pkg, fail_pkg)),
            valid_direct = package_metadata_startup_members_valid(
                direct, c(healthy_pkg, fail_pkg))
        )
    })
    expect_false(status_res$complete)
    expect_identical(status_res$inspected, healthy_pkg)
    expect_identical(status_res$error_pkgs, fail_pkg)
    expect_identical(status_res$snapshot_pkgs, healthy_pkg)
    expect_false(status_res$valid_detailed)
    expect_false(status_res$valid_direct)

    # 2. Cross-process recovery via workspace_startup_members_cache() (whose worker runs
    #    in an isolated callr::r process): partial cold run returns healthy_pkg, persists no
    #    bundle, and after marker removal a fresh cache closure recovers both packages.
    run_partial_or_recovered <- function() {
        withr::local_options(languageserver.package_cache_dir = cache_dir)
        withr::local_libpaths(libs)
        testthat::local_mocked_bindings(
            workspace_startup_packages = function() c(healthy_pkg, fail_pkg),
            .package = "languageserver"
        )
        load_mem <- workspace_startup_members_cache()
        res <- load_mem()
        dir <- package_metadata_cache_directory()
        bundles <- if (!is.null(dir) && dir.exists(dir)) {
            list.files(dir, pattern = "^startup-startup-members-bundle-.*\\.rds$")
        } else {
            character()
        }
        list(
            packages = names(res),
            fields = if (!is.null(res$lazyfailfixture)) {
                sort(names(res$lazyfailfixture$roots$transient_table$fields))
            } else {
                character()
            },
            bundles = bundles
        )
    }

    cold_partial <- withr::with_envvar(c(LANGSVR_LAZYFAIL_MARKER = marker_file), {
        run_partial_or_recovered()
    })
    expect_identical(cold_partial$packages, healthy_pkg)
    expect_length(cold_partial$bundles, 0L)

    unlink(marker_file)
    direct_ok <- withr::with_libpaths(libs, {
        testthat::with_mocked_bindings(
            startup_packages = "base",
            .package = "languageserver",
            member_prepare_startup_data(
                c("base", healthy_pkg),
                lib_paths = libs,
                cache_dir = cache_dir,
                include_status = FALSE
            )
        )
    })
    expect_false(is.null(attr(direct_ok, "identities", exact = TRUE)))
    unpacked_ok <- package_metadata_startup_members_unpack(direct_ok, c("base", healthy_pkg))
    expect_true(unpacked_ok$valid)
    expect_true(unpacked_ok$complete)
    expect_false(is.null(unpacked_ok$identities))
    expect_null(attr(unpacked_ok$snapshots, "identities", exact = TRUE))

    recovered_proc <- withr::with_envvar(c(LANGSVR_LAZYFAIL_MARKER = marker_file), {
        run_partial_or_recovered()
    })
    expect_setequal(recovered_proc$packages, c(healthy_pkg, fail_pkg))
    expect_identical(recovered_proc$fields, c("col_x", "col_y"))

    # 3. Same-process recovery AND bundle persistence gate when bundle_key is non-NULL:
    #    incomplete .startup_members_result does not persist or freeze cached state.
    inproc_cache <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = inproc_cache)
    withr::local_libpaths(libs)
    stable_pkgs <- c("datasets", nodata_pkg)
    testthat::local_mocked_bindings(
        workspace_startup_packages = function() stable_pkgs,
        .package = "languageserver"
    )
    expect_false(is.null(package_metadata_startup_bundle_key("startup-members-bundle", stable_pkgs, libs)))

    attempt <- 0L
    ds_snap <- startup_bundle_members_fixture("datasets", lib_paths = libs)
    load_mem <- workspace_startup_members_cache(prepare = function(requested) {
        attempt <<- attempt + 1L
        if (attempt == 1L) {
            return(list(
                .startup_members_result = TRUE,
                snapshots = ds_snap,
                inspected = "datasets",
                errors = stats::setNames(list("transient failure"), nodata_pkg),
                complete = FALSE
            ))
        }
        list(
            .startup_members_result = TRUE,
            snapshots = ds_snap,
            inspected = stable_pkgs,
            errors = list(),
            complete = TRUE
        )
    }, persist = TRUE)

    expect_identical(load_mem(), ds_snap)
    expect_identical(attempt, 1L)
    expect_length(list.files(package_metadata_cache_directory(),
            pattern = "^startup-startup-members-bundle-.*\\.rds$"), 0L)

    expect_identical(load_mem(), ds_snap)
    expect_identical(attempt, 2L)
    expect_length(list.files(package_metadata_cache_directory(),
            pattern = "^startup-startup-members-bundle-.*\\.rds$"), 1L)
    expect_identical(load_mem(), ds_snap)
    expect_identical(attempt, 2L)

    # 4. Warm bundle hit for legitimate zero-dataset packages
    #    (both pure zero-dataset c("base", "stats", "methods") and mixed healthy_pkg + nodata_pkg).
    zero_cache <- withr::local_tempdir()
    run_zero_case <- function(pkgs, forbid_worker) {
        withr::local_options(languageserver.package_cache_dir = zero_cache)
        withr::local_libpaths(libs)
        worker_calls <- 0L
        forbid_triggered <- FALSE
        real_prep <- workspace_startup_members_prepare
        testthat::local_mocked_bindings(
            workspace_startup_packages = function() pkgs,
            workspace_startup_members_prepare = function(requested) {
                worker_calls <<- worker_calls + 1L
                if (forbid_worker) {
                    forbid_triggered <<- TRUE
                    stop("Must hit persisted bundle for zero-dataset startup packages")
                }
                real_prep(requested)
            },
            .package = "languageserver"
        )
        res <- workspace_startup_members_cache()()
        bundles <- list.files(package_metadata_cache_directory(),
            pattern = "^startup-startup-members-bundle-.*\\.rds$")
        list(
            packages = names(res),
            len = length(res),
            bundles = length(bundles),
            worker_calls = worker_calls,
            forbid_triggered = forbid_triggered
        )
    }

    pure_zero_pkgs <- c("base", "stats", "methods")
    cold_zero <- run_zero_case(pure_zero_pkgs, forbid_worker = FALSE)
    expect_identical(cold_zero$len, 0L)
    expect_identical(cold_zero$bundles, 1L)
    expect_identical(cold_zero$worker_calls, 1L)

    warm_zero <- run_zero_case(pure_zero_pkgs, forbid_worker = TRUE)
    expect_false(warm_zero$forbid_triggered)
    expect_identical(warm_zero$worker_calls, 0L)
    expect_identical(warm_zero$len, 0L)

    mixed_pkgs <- c(healthy_pkg, nodata_pkg)
    mixed_cold <- run_zero_case(mixed_pkgs, forbid_worker = FALSE)
    expect_identical(mixed_cold$packages, healthy_pkg)
    expect_identical(mixed_cold$bundles, 2L)
    expect_identical(mixed_cold$worker_calls, 1L)

    mixed_warm <- run_zero_case(mixed_pkgs, forbid_worker = TRUE)
    expect_false(mixed_warm$forbid_triggered)
    expect_identical(mixed_warm$worker_calls, 0L)
    expect_identical(mixed_warm$packages, healthy_pkg)
})

test_that("Transitive dependency manifests traverse >64 packages, invalidate deep DESCRIPTION changes, and reject truncated graphs", {
    shared <- startup_bundle_shared_state()
    root <- withr::local_tempdir()
    library <- file.path(root, "library")
    dir.create(library)

    # Verify the shared install batching and space-containing paths invariant.
    inst_info <- shared$inst_info
    expect_gt(length(inst_info$batches), 1L)
    for (batch_args in inst_info$cmdargs) {
        expect_lt(nchar(paste(c("Rcmd.exe", "INSTALL", batch_args), collapse = " ")), 500L)
        expect_lt(150L + sum(nchar(batch_args) + 9L), 600L)
    }
    expect_true(grepl(" ", shared$wd, fixed = TRUE))
    expect_true(grepl(" ", shared$library, fixed = TRUE))

    # 1. Build a 75-package transitive chain (chain01 -> ... -> chain75; >80 total with startup_packages)
    #    by cloning installed template packages in pure R (< 0.2s).
    n_chain <- 75L
    chain_names <- sprintf("chain%02d", seq_len(n_chain))
    leaf_tmpl <- file.path(shared$library, "condelse")
    lazy_tmpl <- file.path(shared$library, "startupbundlefixture")

    for (i in rev(seq_len(n_chain))) {
        pkg_name <- chain_names[[i]]
        imp <- if (i < n_chain) chain_names[[i + 1L]] else character()
        tmpl <- if (i == 1L) lazy_tmpl else leaf_tmpl
        startup_bundle_clone_installed_pkg(
            tmpl,
            library,
            pkg_name,
            imports = imp,
            exports = if (i == 1L) "bundle_generic" else "else_fn"
        )
    }

    # Clone derivedchainfixture and point its Imports at chain01.
    derived_pkg <- "derivedchainfixture"
    startup_bundle_clone_installed_pkg(
        file.path(shared$library, derived_pkg),
        library,
        derived_pkg,
        imports = "chain01",
        exports = "derived_fn"
    )
    # Runs before local_tempdir() removes `library`; covers the chain and any
    # other namespace loaded from it.
    withr::defer(expect_identical(startup_bundle_unload_library_namespaces(library), character()))

    libs <- c(library, .libPaths())
    traversal <- package_metadata_transitive_packages(c(startup_packages, "chain01"), libs)
    expect_true(traversal$complete)
    expect_true(all(chain_names %in% traversal$packages))
    expect_gt(length(traversal$packages), 80L)

    # 2. Warm bundle hit for declarative >64 chain, then invalidation
    #    when package #70 (chain70/DESCRIPTION) changes on disk.
    chain_cache <- withr::local_tempdir()
    run_chain_case <- function(forbid_worker) {
        withr::local_options(languageserver.package_cache_dir = chain_cache)
        withr::local_libpaths(libs)
        worker_calls <- 0L
        forbid_triggered <- FALSE
        real_prep <- workspace_startup_members_prepare
        testthat::local_mocked_bindings(
            workspace_startup_packages = function() c("base", "chain01"),
            workspace_startup_members_prepare = function(requested) {
                worker_calls <<- worker_calls + 1L
                if (forbid_worker) {
                    forbid_triggered <<- TRUE
                    stop("Expected warm member bundle hit for 75-package chain")
                }
                real_prep(requested)
            },
            .package = "languageserver"
        )
        res <- workspace_startup_members_cache()()
        bundle_file <- list.files(package_metadata_cache_directory(),
            pattern = "^startup-startup-members-bundle-.*\\.rds$", full.names = TRUE)
        manifest_pkgs <- if (length(bundle_file) == 1L) {
            names(unserialize(readRDS(bundle_file)$payload)$manifest)
        } else {
            character()
        }
        list(
            worker_calls = worker_calls,
            forbid_triggered = forbid_triggered,
            packages = names(res),
            manifest_pkgs = manifest_pkgs
        )
    }

    cold_chain <- run_chain_case(forbid_worker = FALSE)
    expect_identical(cold_chain$worker_calls, 1L)
    expect_true("chain01" %in% cold_chain$packages)
    expect_true(all(chain_names %in% cold_chain$manifest_pkgs))

    warm_chain <- run_chain_case(forbid_worker = TRUE)
    expect_false(warm_chain$forbid_triggered)
    expect_identical(warm_chain$worker_calls, 0L)
    expect_identical(warm_chain$packages, cold_chain$packages)

    # Also check observable derived column before modifying chain70/DESCRIPTION.
    derived_cache <- withr::local_tempdir()
    run_derived <- function() {
        withr::local_options(languageserver.package_cache_dir = derived_cache)
        withr::local_libpaths(libs)
        testthat::local_mocked_bindings(
            workspace_startup_packages = function() c("base", "derivedchainfixture"),
            .package = "languageserver"
        )
        res <- workspace_startup_members_cache()()
        names(res$derivedchainfixture$roots$derived_table$fields)
    }

    expect_identical(run_derived(), "old_column")

    # Modify chain70/DESCRIPTION (preserving mtime) -> invalidates declarative bundle
    # AND refreshes derivedchainfixture's column to new_column in a fresh worker!
    desc70 <- file.path(library, "chain70", "DESCRIPTION")
    stamp70 <- file.info(desc70)
    writeLines(c(readLines(desc70), "Note: new_column"), desc70)
    Sys.setFileTime(desc70, stamp70$mtime)

    invalidated_chain <- run_chain_case(forbid_worker = FALSE)
    expect_identical(invalidated_chain$worker_calls, 1L)
    expect_identical(run_derived(), "new_column")

    # 3. Explicit truncation rejection when limit is exceeded: neither startup-s3
    #    nor startup-members-bundle persists an incomplete manifest.
    trunc_trav <- package_metadata_transitive_packages(c(startup_packages, "chain01"), libs, limit = 64L)
    expect_false(trunc_trav$complete)
    expect_length(trunc_trav$packages, 64L)

    trunc_cache <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = trunc_cache)
    withr::local_libpaths(libs)
    full_key <- package_metadata_startup_bundle_key("startup-s3", c(startup_packages, "chain01"), libs)
    full_mem_key <- package_metadata_startup_bundle_key(
        "startup-members-bundle", c(startup_packages, "chain01"), libs)
    expect_false(is.null(full_key))
    expect_false(is.null(full_mem_key))

    (function() {
        testthat::local_mocked_bindings(
            package_metadata_transitive_limit = 64L,
            .package = "languageserver"
        )
        expect_null(package_metadata_startup_bundle_key(
            "startup-s3", c(startup_packages, "chain01"), libs))
        expect_null(package_metadata_startup_bundle_key(
            "startup-members-bundle", c(startup_packages, "chain01"), libs))

        s3_cats <- startup_bundle_s3_fixture(c(startup_packages, chain_names), lib_paths = libs)
        mem_snaps <- startup_bundle_members_fixture(c("datasets", "chain01"), lib_paths = libs)
        package_metadata_startup_s3_write(full_key, c(startup_packages, "chain01"), s3_cats)
        package_metadata_startup_members_write(full_mem_key, c(startup_packages, "chain01"), mem_snaps)
        dir <- package_metadata_cache_directory()
        written <- if (!is.null(dir) && dir.exists(dir)) list.files(dir) else character()
        expect_length(written, 0L)
    })()

    # 4. Cyclic Depends/Imports and uninstalled references terminate cleanly without corruption.
    cyc_lib <- file.path(root, "cyclic-lib")
    dir.create(file.path(cyc_lib, "cycA"), recursive = TRUE)
    dir.create(file.path(cyc_lib, "cycB"), recursive = TRUE)
    writeLines(c(
        "Package: cycA",
        "Version: 0.0.1",
        "Depends: cycB, cycA",
        "Imports: uninstalled_optional_dep (>= 1.0.0)"
    ), file.path(cyc_lib, "cycA", "DESCRIPTION"))
    writeLines("importFrom(cycB, b_fn)", file.path(cyc_lib, "cycA", "NAMESPACE"))
    writeLines(c(
        "Package: cycB",
        "Version: 0.0.1",
        "Imports: cycA"
    ), file.path(cyc_lib, "cycB", "DESCRIPTION"))
    writeLines("import(cycA)", file.path(cyc_lib, "cycB", "NAMESPACE"))

    cyc_trav <- package_metadata_transitive_packages("cycA", c(cyc_lib, .libPaths()))
    expect_true(cyc_trav$complete)
    expect_setequal(cyc_trav$packages, c("cycA", "cycB"))
    expect_identical(cyc_trav$missing, "uninstalled_optional_dep")

    # 5. Missing or corrupt DESCRIPTION marks dcf and transitive traversal incomplete (D5).
    expect_false(isTRUE(attr(
        package_metadata_dcf_packages(file.path(cyc_lib, "nonexistent", "DESCRIPTION")),
        "complete"
    )))
    dir.create(file.path(cyc_lib, "cycB", "Meta"), showWarnings = FALSE)
    saveRDS(list(DESCRIPTION = c(Package = "cycB", Version = "0.0.1")),
        file.path(cyc_lib, "cycB", "Meta", "package.rds"))
    bad_desc <- file.path(cyc_lib, "cycB", "DESCRIPTION")
    writeLines("Version: 0.0.1", bad_desc)
    expect_false(isTRUE(attr(package_metadata_dcf_packages(bad_desc), "complete")))
    expect_false(package_metadata_transitive_packages("cycA", c(cyc_lib, .libPaths()))$complete)
    writeLines("", bad_desc)
    expect_false(package_metadata_transitive_packages("cycA", c(cyc_lib, .libPaths()))$complete)
    unlink(bad_desc)
    expect_false(isTRUE(attr(package_metadata_dcf_packages(bad_desc), "complete")))
})

test_that("Startup bundles preserve runtime load-hook validation across direct, no-op, and transitive hooks", {
    expect_true(package_metadata_runtime_stable(startup_packages))

    direct_pkg <- "s3envhookfixture"
    sneaky_pkg <- "sneakyhookfixture"
    trans_dep_pkg <- "transhookdep"
    trans_top_pkg <- "transhooktop"
    fixtures <- startup_bundle_install_fixtures(c(direct_pkg, sneaky_pkg, trans_dep_pkg, trans_top_pkg))
    libs <- c(fixtures$library, .libPaths())

    expect_false(package_metadata_package_runtime_stable(direct_pkg, libs))
    expect_false(package_metadata_package_runtime_stable(sneaky_pkg, libs))
    expect_false(package_metadata_package_runtime_stable(trans_dep_pkg, libs))
    expect_true(package_metadata_package_runtime_stable(trans_top_pkg, libs))
    expect_null(package_metadata_startup_bundle_key(
        "startup-s3", c(startup_packages, trans_top_pkg), libs))
    expect_null(package_metadata_startup_bundle_key(
        "startup-members-bundle", c(startup_packages, sneaky_pkg), libs))

    direct_cache <- withr::local_tempdir()
    sneaky_cache <- withr::local_tempdir()
    trans_cache <- withr::local_tempdir()

    # Run all 3 hook validation cases inside a single isolated child R process
    # (with startup_packages narrowed to "base" inside the worker so base R's other
    # 7 packages are not re-indexed 6 times) while keeping the server process clean.
    hook_worker <- function(direct_cache, sneaky_cache, trans_cache) {
        ns <- asNamespace("languageserver")
        unlockBinding("startup_packages", ns)
        assign("startup_packages", "base", ns)
        lockBinding("startup_packages", ns)

        set_startup <- function(pkgs) {
            unlockBinding("workspace_startup_packages", ns)
            assign("workspace_startup_packages", function() pkgs, ns)
            lockBinding("workspace_startup_packages", ns)
        }
        run_s3 <- function() {
            ns$workspace_startup_s3_cache(
                prepare = function(req) ns$s3_prepare_packages(req$packages, cache_dir = req$cache_dir, lib_paths = req$lib_paths),
                persist = TRUE
            )()
        }
        run_mem <- function() {
            ns$workspace_startup_members_cache(
                prepare = function(req) ns$member_prepare_startup_data(req$packages, req$lib_paths, req$cache_dir, include_status = TRUE),
                persist = TRUE
            )()
        }
        s3_bundles <- function() {
            dir <- ns$package_metadata_cache_directory()
            if (!is.null(dir) && dir.exists(dir)) list.files(dir, pattern = "^startup-startup-s3-.*\\.rds$") else character()
        }

        # Case 1: direct choose.default across old -> new -> old
        options(languageserver.package_cache_dir = direct_cache)
        set_startup(c("base", "s3envhookfixture"))
        run_direct <- function(mode) {
            if ("s3envhookfixture" %in% loadedNamespaces()) unloadNamespace("s3envhookfixture")
            Sys.setenv(LANGSVR_S3_HOOK_MODE = mode)
            cats <- run_s3()
            m <- cats$s3envhookfixture$methods[["s3envhookfixture::choose.default"]]
            list(formals = names(m$formals), body_hash = m$body_hash, bundles = s3_bundles())
        }
        res_old <- run_direct("old")
        res_new <- run_direct("new")
        res_old_again <- run_direct("old")

        # Case 2: sneaky no-op mode (env var unset) -> active mode (env var = 'active')
        options(languageserver.package_cache_dir = sneaky_cache)
        set_startup(c("base", "sneakyhookfixture"))
        run_sneaky <- function(mode) {
            if ("sneakyhookfixture" %in% loadedNamespaces()) unloadNamespace("sneakyhookfixture")
            if (is.na(mode)) Sys.unsetenv("LANGSVR_SNEAKY_MODE") else Sys.setenv(LANGSVR_SNEAKY_MODE = mode)
            cats <- run_s3()
            mem <- run_mem()
            m <- cats$sneakyhookfixture$methods[["sneakyhookfixture::choose.default"]]
            api <- cats$sneakyhookfixture$functions$sneaky_api
            list(
                has_choose_default = !is.null(m),
                choose_formals = if (!is.null(m)) names(m$formals) else character(),
                api_formals = names(api$formals),
                cfg_fields = names(mem$sneakyhookfixture$roots$sneaky_config$fields)
            )
        }
        sneaky_noop <- run_sneaky(NA_character_)
        sneaky_active <- run_sneaky("active")

        # Case 3: transitive load hook (transhooktop has no .onLoad, transhookdep does)
        options(languageserver.package_cache_dir = trans_cache)
        set_startup(c("base", "transhooktop"))
        run_trans <- function(mode) {
            if ("transhooktop" %in% loadedNamespaces()) unloadNamespace("transhooktop")
            if ("transhookdep" %in% loadedNamespaces()) unloadNamespace("transhookdep")
            base_s3 <- get(".__S3MethodsTable__.", asNamespace("base"))
            if (exists("mean.trans_hook_cls", envir = base_s3, inherits = FALSE)) {
                rm("mean.trans_hook_cls", envir = base_s3)
            }
            Sys.setenv(LANGSVR_TRANS_HOOK_MODE = mode)
            cats <- run_s3()
            names(cats$base$methods[["base::mean.trans_hook_cls"]]$formals)
        }
        trans_alpha <- run_trans("alpha")
        trans_beta <- run_trans("beta")

        list(
            res_old = res_old, res_new = res_new, res_old_again = res_old_again,
            sneaky_noop = sneaky_noop, sneaky_active = sneaky_active,
            trans_alpha = trans_alpha, trans_beta = trans_beta
        )
    }
    environment(hook_worker) <- baseenv()
    out <- callr::r(
        hook_worker,
        args = list(direct_cache = direct_cache, sneaky_cache = sneaky_cache, trans_cache = trans_cache),
        libpath = libs,
        system_profile = FALSE,
        user_profile = FALSE
    )

    expect_identical(out$res_old$formals, c("x", "old"))
    expect_identical(out$res_new$formals, c("x", "new"))
    expect_false(identical(out$res_old$body_hash, out$res_new$body_hash))
    expect_identical(out$res_old_again$formals, c("x", "old"))
    expect_identical(out$res_old_again$body_hash, out$res_old$body_hash)
    expect_length(out$res_old$bundles, 0L)
    expect_length(out$res_new$bundles, 0L)

    expect_false(out$sneaky_noop$has_choose_default)
    expect_identical(out$sneaky_noop$api_formals, c("x", "static_arg"))
    expect_identical(out$sneaky_noop$cfg_fields, "static_key")
    expect_true(out$sneaky_active$has_choose_default)
    expect_identical(out$sneaky_active$choose_formals, c("x", "new"))
    expect_identical(out$sneaky_active$api_formals, c("x", "dynamic_arg"))
    expect_identical(out$sneaky_active$cfg_fields, "dynamic_key")

    expect_identical(out$trans_alpha, c("x", "alpha_arg", "..."))
    expect_identical(out$trans_beta, c("x", "beta_arg", "..."))
    expect_false(any(c(direct_pkg, sneaky_pkg, trans_dep_pkg, trans_top_pkg) %in% loadedNamespaces()))
})

test_that("Serialized promises, delayed S3 registrations, and loader metadata guard startup bundles", {
    fixtures <- startup_bundle_install_fixtures(c(
        "delayedpromisefixture",
        "nestedpromisefixture",
        "delayeds3fixture",
        "declarativefixture"
    ))
    library <- fixtures$library
    libs <- c(library, .libPaths())

    # Static checks pass for serialized promises, so the isolated scan must flag them.
    expect_true(package_metadata_package_runtime_stable("delayedpromisefixture", libs))
    expect_true(package_metadata_package_runtime_stable("nestedpromisefixture", libs))
    expect_false(package_metadata_promises_absent("delayedpromisefixture", libs))
    expect_false(package_metadata_promises_absent("nestedpromisefixture", libs))
    expect_true(package_metadata_promises_absent(c(startup_packages, "declarativefixture"), libs))
    expect_true(package_metadata_promises_absent(startup_packages, libs))
    scanned <- package_metadata_rdb_scan(package_metadata_lazyload_files(
        c("delayedpromisefixture", "nestedpromisefixture", "declarativefixture"), libs))
    expect_identical(unname(scanned), c(TRUE, TRUE, FALSE))
    hdr_scanned <- vapply(
        package_metadata_lazyload_files(
            c("delayedpromisefixture", "nestedpromisefixture", "declarativefixture"), libs),
        package_metadata_rdb_scan_headers,
        logical(1L)
    )
    expect_identical(unname(hdr_scanned), c(TRUE, NA, FALSE))
    expect_false(any(c("delayedpromisefixture", "nestedpromisefixture") %in% loadedNamespaces()))

    # Standard declarative package .rdx/.rdb databases resolve in-process with 0 callr::r subprocesses.
    pa_env <- environment(package_metadata_promises_absent)
    if (!is.null(pa_env$verdicts)) pa_env$verdicts$clear()
    scan_subprocs <- 0L
    real_callr_r <- callr::r
    (function() {
        testthat::local_mocked_bindings(
            r = function(func, ...) {
                scan_subprocs <<- scan_subprocs + 1L
                real_callr_r(func, ...)
            },
            .package = "callr"
        )
        expect_true(package_metadata_promises_absent("declarativefixture", libs))
        expect_false(package_metadata_promises_absent("delayedpromisefixture", libs))
    })()
    expect_identical(scan_subprocs, 0L)

    # Raw LZMA2 (compress = 3L, type == "Z") lazy-load databases decode cleanly as FALSE,
    # while truncated < 12-byte "xz" payloads are flagged TRUE in-process.
    xz_dir <- withr::local_tempdir()
    xz_env <- new.env(parent = emptyenv())
    xz_env$alpha <- 1:200
    xz_prefix <- file.path(xz_dir, "xz_clean")
    tools:::makeLazyLoadDB(xz_env, xz_prefix, compress = 3L)
    expect_false(package_metadata_rdb_scan_headers(paste0(xz_prefix, ".rdx")))
    expect_identical(unname(package_metadata_rdb_scan(paste0(xz_prefix, ".rdx"))), FALSE)

    xz_prom_env <- new.env(parent = emptyenv())
    delayedAssign("p", Sys.time(), assign.env = xz_prom_env)
    xz_prom_prefix <- file.path(xz_dir, "xz_prom")
    tools:::makeLazyLoadDB(xz_prom_env, xz_prom_prefix, compress = 3L)
    expect_true(package_metadata_rdb_scan_headers(paste0(xz_prom_prefix, ".rdx")))
    expect_identical(unname(package_metadata_rdb_scan(paste0(xz_prom_prefix, ".rdx"))), TRUE)

    xz_nested_env <- new.env(parent = emptyenv())
    xz_sub <- new.env(parent = emptyenv())
    delayedAssign("sp", Sys.time(), assign.env = xz_sub)
    xz_nested_env$sub <- xz_sub
    xz_nested_prefix <- file.path(xz_dir, "xz_nested")
    tools:::makeLazyLoadDB(xz_nested_env, xz_nested_prefix, compress = 3L)
    expect_true(package_metadata_rdb_scan_headers(paste0(xz_nested_prefix, ".rdx")))
    expect_identical(unname(package_metadata_rdb_scan(paste0(xz_nested_prefix, ".rdx"))), TRUE)
    expect_identical(package_metadata_normalize_path(NA_character_), "")
    expect_true(package_metadata_manifest_valid(list()))

    corrupt_xz_rdx <- file.path(xz_dir, "xz_corrupt.rdx")
    corrupt_xz_rdb <- file.path(xz_dir, "xz_corrupt.rdb")
    saveRDS(list(variables = list(a = c(0L, 10L)), references = list(), compressed = 3L), corrupt_xz_rdx)
    writeBin(c(as.raw(c(0, 0, 0, 5)), charToRaw("Z"), as.raw(1:5)), corrupt_xz_rdb)
    expect_true(package_metadata_rdb_scan_headers(corrupt_xz_rdx))
    expect_identical(unname(package_metadata_rdb_scan(corrupt_xz_rdx)), TRUE)

    # 1. Cross-mode check: a warm process in one mode must not leak into another.
    delayed_cache <- withr::local_tempdir()
    delayed_worker <- function(cache_dir) {
        options(languageserver.package_cache_dir = cache_dir)
        ns <- asNamespace("languageserver")
        unlockBinding("startup_packages", ns)
        assign("startup_packages", "base", ns)
        lockBinding("startup_packages", ns)
        unlockBinding("workspace_startup_packages", ns)
        assign("workspace_startup_packages", function() c("base", "delayedpromisefixture"), ns)
        lockBinding("workspace_startup_packages", ns)
        run_mode <- function(mode) {
            if ("delayedpromisefixture" %in% loadedNamespaces()) unloadNamespace("delayedpromisefixture")
            Sys.setenv(LANGSVR_DELAYED_MODE = mode)
            cats <- ns$workspace_startup_s3_cache(
                prepare = function(req) ns$s3_prepare_packages(req$packages, cache_dir = req$cache_dir, lib_paths = req$lib_paths),
                persist = TRUE
            )()
            method <- cats$delayedpromisefixture$methods[["delayedpromisefixture::choose.default"]]
            dir <- ns$package_metadata_cache_directory()
            list(
                formals = names(method$formals),
                bundles = if (!is.null(dir) && dir.exists(dir)) list.files(dir, pattern = "^startup-startup-s3-") else character()
            )
        }
        list(old_mode = run_mode("old"), new_mode = run_mode("new"))
    }
    environment(delayed_worker) <- baseenv()
    delayed_out <- callr::r(delayed_worker, args = list(cache_dir = delayed_cache),
        libpath = libs, system_profile = FALSE, user_profile = FALSE)
    expect_identical(delayed_out$old_mode$formals, c("x", "old"))
    expect_identical(delayed_out$new_mode$formals, c("x", "new"))
    expect_length(delayed_out$old_mode$bundles, 0L)
    expect_length(delayed_out$new_mode$bundles, 0L)

    # 2. Delayed cross-package S3 registrations are dependencies and unstable.
    expect_false(package_metadata_package_runtime_stable("delayeds3fixture", libs))
    delayed_path <- find.package("delayeds3fixture", lib.loc = libs)
    expect_true("tools" %in% package_metadata_namespace_imports(delayed_path))
    expect_true("tools" %in% package_metadata_transitive_packages("delayeds3fixture", libs)$packages)
    expect_null(package_metadata_startup_bundle_key("startup-s3", c(startup_packages, "delayeds3fixture"), libs))

    # 3. Meta/ files and the loader script are part of the startup manifest.
    cache_dir <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = cache_dir)
    withr::local_libpaths(libs)
    requested <- c(startup_packages, "declarativefixture")
    testthat::local_mocked_bindings(
        workspace_startup_packages = function() requested,
        .package = "languageserver"
    )
    calls <- 0L
    run_s3 <- function() {
        workspace_startup_s3_cache(prepare = function(requested) {
            calls <<- calls + 1L
            startup_bundle_s3_fixture(requested$packages, lib_paths = requested$lib_paths)
        }, persist = TRUE)()
    }
    run_s3()
    run_s3()
    expect_identical(calls, 1L)

    pkg_path <- find.package("declarativefixture", lib.loc = libs)
    ns_info_file <- file.path(pkg_path, "Meta", "nsInfo.rds")
    ns_info <- readRDS(ns_info_file)
    ns_info$exportPatterns <- "^declared_never_matches$"
    saveRDS(ns_info, ns_info_file)
    run_s3()
    expect_identical(calls, 2L)
    run_s3()
    expect_identical(calls, 2L)

    features_file <- file.path(pkg_path, "Meta", "features.rds")
    if (file.exists(features_file)) {
        features <- readRDS(features_file)
        features$startup_bundle_fixture <- TRUE
        saveRDS(features, features_file)
        run_s3()
        expect_identical(calls, 3L)
    }

    loader <- file.path(pkg_path, "R", "declarativefixture")
    orig_loader <- readLines(loader)
    writeLines(c(orig_loader, "invisible(Sys.getenv('LANGSVR_DELAYED_MODE'))"), loader)
    expect_false(package_metadata_package_runtime_stable("declarativefixture", libs))
    expect_null(package_metadata_startup_bundle_key("startup-s3", requested, libs))
    writeLines(orig_loader, loader)
    expect_true(package_metadata_package_runtime_stable("declarativefixture", libs))

    # Asymmetric .rdx/.rdb pairs and unexpected files in R/ are rejected (D5).
    extra_r_file <- file.path(pkg_path, "R", "unexpected.R")
    writeLines("x <- 1", extra_r_file)
    expect_false(package_metadata_package_runtime_stable("declarativefixture", libs))
    unlink(extra_r_file)
    expect_true(package_metadata_package_runtime_stable("declarativefixture", libs))

    sys_rdx <- file.path(pkg_path, "R", "sysdata.rdx")
    saveRDS(list(variables = list(), references = list(), compressed = TRUE), sys_rdx)
    expect_false(package_metadata_package_runtime_stable("declarativefixture", libs))
    unlink(sys_rdx)
    expect_true(package_metadata_package_runtime_stable("declarativefixture", libs))

    data_dir <- file.path(pkg_path, "data")
    dir.create(data_dir, showWarnings = FALSE)
    data_rdx <- file.path(data_dir, "Rdata.rdx")
    saveRDS(list(variables = list(), references = list(), compressed = TRUE), data_rdx)
    expect_false(package_metadata_package_runtime_stable("declarativefixture", libs))
    unlink(data_dir, recursive = TRUE)
    expect_true(package_metadata_package_runtime_stable("declarativefixture", libs))

    unlink(file.path(pkg_path, "Meta", "package.rds"))
    expect_false(package_metadata_package_runtime_stable("declarativefixture", libs))
})

test_that("Conditional NAMESPACE directives are collected syntactically without evaluation in the server", {
    fixtures <- startup_bundle_install_fixtures(c(
        "condsentinel",
        "condelse",
        "condfixture",
        "condstable"
    ))
    root <- fixtures$root
    library <- fixtures$library
    marker <- file.path(root, "sentinel-onload")
    libs <- c(library, .libPaths())
    unlink(marker)
    withr::local_envvar(c(LANGSVR_COND_SENTINEL_MARKER = marker, LANGSVR_COND_EVALUATED = NA))
    withr::local_options(condsentinel.loaded = NULL)
    withr::local_libpaths(libs)
    expect_false("condsentinel" %in% loadedNamespaces())

    # Verify DESCRIPTION declares installation dependencies for R CMD check (_R_CHECK_INSTALL_DEPENDS_=true).
    expect_setequal(
        package_metadata_dcf_packages(file.path(library, "condfixture", "DESCRIPTION")),
        c("condsentinel", "condelse")
    )
    expect_identical(
        package_metadata_dcf_packages(file.path(library, "condstable", "DESCRIPTION")),
        "condelse"
    )

    # Fallback walk over a raw NAMESPACE: every branch, no evaluation.
    cond_path <- find.package("condfixture", lib.loc = libs)
    syntax <- package_metadata_namespace_syntax(file.path(cond_path, "NAMESPACE"))
    expect_true(syntax$valid)
    expect_setequal(syntax$packages, c("condsentinel", "condelse", "condnever"))
    raw_ns <- file.path(root, "NAMESPACE")
    writeLines(c(
        sprintf("if (file.create(%s)) { import(rawa) } else if (FALSE) import(rawb)",
            encodeString(file.path(root, "raw-marker"), quote = "'")),
        "if (stop('never')) S3method(rawc::print, cls)",
        "x <- importFrom(rawassign, f)",
        "importClassFrom(rawcls, SomeClass)",
        "y = if (TRUE) import(rawd) else import(rawe)"
    ), raw_ns)
    raw <- package_metadata_namespace_syntax(raw_ns)
    expect_setequal(raw$packages, c("rawa", "rawb", "rawc", "rawassign", "rawcls", "rawd", "rawe"))
    expect_false(file.exists(file.path(root, "raw-marker")))
    writeLines("import(", raw_ns)
    expect_false(package_metadata_namespace_syntax(raw_ns)$valid)
    expect_false(isTRUE(attr(package_metadata_namespace_imports(root), "complete")))

    # Install-time resolution (Meta/nsInfo.rds) plus the syntactic superset.
    ns_info <- readRDS(file.path(cond_path, "Meta", "nsInfo.rds"))
    expect_identical(vapply(ns_info$imports, function(x) x[[1L]], character(1L)), "condsentinel")
    imports <- package_metadata_namespace_imports(cond_path)
    expect_true(all(c("condsentinel", "condelse", "condnever") %in% imports))
    traversal <- package_metadata_transitive_packages("condfixture", libs)
    expect_true(traversal$complete)
    expect_true(all(c("condfixture", "condsentinel", "condelse") %in% traversal$packages))
    expect_true("condnever" %in% traversal$missing)

    # The sentinel's .onLoad makes the closure unstable; no bundle key.
    expect_true(package_metadata_package_runtime_stable("condfixture", libs))
    expect_false(package_metadata_package_runtime_stable("condsentinel", libs))
    cache_dir <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = cache_dir)
    expect_null(package_metadata_startup_bundle_key("startup-s3", c(startup_packages, "condfixture"), libs))
    calls <- 0L
    requested <- c(startup_packages, "condfixture")
    testthat::local_mocked_bindings(
        workspace_startup_packages = function() requested,
        .package = "languageserver"
    )
    workspace_startup_s3_cache(prepare = function(requested) {
        calls <<- calls + 1L
        startup_bundle_s3_fixture(requested$packages, lib_paths = requested$lib_paths)
    }, persist = TRUE)()
    expect_identical(calls, 1L)
    expect_length(list.files(package_metadata_cache_directory(), pattern = "^startup-startup-s3-"), 0L)

    # Conditional directives alone do not block reuse: runtime reads nsInfo.rds.
    stable <- package_metadata_transitive_packages("condstable", libs)
    expect_true("condelse" %in% stable$packages)
    expect_true("condold" %in% stable$missing)
    expect_true(package_metadata_missing_absent(stable$missing, libs))
    expect_false(package_metadata_missing_absent(c(stable$missing, "condelse"), libs))
    expect_false(package_metadata_missing_absent(123L, libs))
    expect_false(is.null(package_metadata_startup_bundle_key("startup-s3", c(startup_packages, "condstable"), libs)))

    # Without Meta/nsInfo.rds loadNamespace() would evaluate NAMESPACE itself:
    # the syntactic superset still applies, but the package is never stable.
    nsless <- file.path(root, "nsless")
    dir.create(nsless)
    for (package in c("condfixture", "condstable")) {
        expect_true(file.copy(find.package(package, lib.loc = libs), nsless, recursive = TRUE))
        unlink(file.path(nsless, package, "Meta", "nsInfo.rds"))
    }
    nsless_libs <- c(nsless, libs)
    nsless_path <- find.package("condfixture", lib.loc = nsless_libs)
    expect_identical(normalizePath(dirname(nsless_path)), normalizePath(nsless))
    nsless_imports <- package_metadata_namespace_imports(nsless_path)
    expect_true(all(c("condsentinel", "condelse", "condnever") %in% nsless_imports))
    for (package in c("condfixture", "condstable")) {
        expect_false(package_metadata_package_runtime_stable(package, nsless_libs))
        expect_null(package_metadata_startup_bundle_key("startup-s3", c(startup_packages, package), nsless_libs))
    }

    # The server process never evaluated a condition or loaded the sentinel.
    expect_false("condsentinel" %in% loadedNamespaces())
    expect_false(file.exists(marker))
    expect_null(getOption("condsentinel.loaded"))
    expect_identical(Sys.getenv("LANGSVR_COND_EVALUATED"), "")
})

test_that("Worker package identities are validated before persisting or reading startup S3 and member bundles", {
    shared <- startup_bundle_shared_state()
    fixtures <- startup_bundle_install_fixtures(c("transdepfixture", "startupbundlefixture"))
    cache_dir <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = cache_dir)
    libs <- c(fixtures$library, .libPaths())
    withr::local_libpaths(libs)

    # Add a 2-hop DESCRIPTION-only transitive dependency:
    # startupbundlefixture -> transdepfixture (1-hop) -> racedep2fixture (2-hop DESCRIPTION Imports).
    startup_bundle_clone_installed_pkg(
        file.path(shared$library, "condelse"),
        fixtures$library,
        "racedep2fixture",
        version = "0.0.1",
        exports = "else_fn"
    )
    add_racedep2_import <- function(pkg_dir) {
        desc_file <- file.path(pkg_dir, "DESCRIPTION")
        writeLines(c(readLines(desc_file), "Imports: racedep2fixture"), desc_file)
        pkg_rds_file <- file.path(pkg_dir, "Meta", "package.rds")
        if (file.exists(pkg_rds_file)) {
            pr <- readRDS(pkg_rds_file)
            pr$DESCRIPTION[["Imports"]] <- "racedep2fixture"
            saveRDS(pr, pkg_rds_file)
        }
    }
    add_racedep2_import(file.path(fixtures$library, "transdepfixture"))

    target_packages <- c("base", fixtures$startup_pkg)
    requested <- list(packages = target_packages, lib_paths = libs, cache_dir = cache_dir)
    testthat::local_mocked_bindings(
        startup_packages = "base",
        workspace_startup_packages = function() target_packages,
        .package = "languageserver"
    )

    # Verify workspace_startup_s3_prepare forwards lib_paths = requested$lib_paths.
    forwarded_args <- NULL
    (function() {
        testthat::local_mocked_bindings(
            r = function(func, args = list(), ...) {
                forwarded_args <<- args
                list()
            },
            .package = "callr"
        )
        workspace_startup_s3_prepare(requested)
    })()
    expect_identical(forwarded_args$lib_paths, libs)

    prepare_both_in_worker <- function(requested) {
        worker_fn <- function(req) {
            ns <- asNamespace("languageserver")
            unlockBinding("startup_packages", ns)
            assign("startup_packages", "base", ns)
            lockBinding("startup_packages", ns)
            list(
                s3 = ns$s3_prepare_packages(req$packages, cache_dir = req$cache_dir, lib_paths = req$lib_paths),
                mem = ns$member_prepare_startup_data(req$packages, req$lib_paths, req$cache_dir, include_status = TRUE)
            )
        }
        environment(worker_fn) <- baseenv()
        callr::r(
            worker_fn,
            args = list(req = requested),
            libpath = requested$lib_paths,
            system_profile = FALSE,
            user_profile = FALSE
        )
    }

    # Prepare S3 and member metadata at version 0.0.1 of transdepfixture (formals: x, old)
    # and version 0.0.1 of 2-hop racedep2fixture.
    key_s3_before <- package_metadata_startup_bundle_key("startup-s3", target_packages, libs)
    key_mem_before <- package_metadata_startup_bundle_key("startup-members-bundle", target_packages, libs)
    expect_false(is.null(key_s3_before))
    expect_false(is.null(key_mem_before))
    expect_true("racedep2fixture" %in% names(key_s3_before$manifest))

    prepared_001 <- prepare_both_in_worker(requested)
    s3_prepared_001 <- prepared_001$s3
    mem_prepared_001 <- prepared_001$mem
    expect_identical(
        names(s3_prepared_001$transdepfixture$methods[["transdepfixture::racedep_generic.default"]]$formals),
        c("x", "old")
    )

    # 1. 2-hop DESCRIPTION-only dependency (racedep2fixture) updated 0.0.1 -> 0.0.2 between
    #    worker preparation and bundle persistence must be rejected for both S3 and members.
    racedep2_desc <- file.path(fixtures$library, "racedep2fixture", "DESCRIPTION")
    writeLines(sub("Version: 0.0.1", "Version: 0.0.2", readLines(racedep2_desc), fixed = TRUE), racedep2_desc)
    key_s3_hop2 <- package_metadata_startup_bundle_key("startup-s3", target_packages, libs)
    key_mem_hop2 <- package_metadata_startup_bundle_key("startup-members-bundle", target_packages, libs)

    package_metadata_startup_s3_write(key_s3_before, target_packages, s3_prepared_001)
    package_metadata_startup_s3_write(key_s3_hop2, target_packages, s3_prepared_001)
    package_metadata_startup_members_write(key_mem_before, target_packages, mem_prepared_001)
    package_metadata_startup_members_write(key_mem_hop2, target_packages, mem_prepared_001)
    package_metadata_startup_members_write(key_mem_hop2, target_packages, mem_prepared_001$snapshots)
    expect_null(package_metadata_startup_s3_read(key_s3_hop2, target_packages))
    expect_null(package_metadata_startup_members_read(key_mem_hop2, target_packages))
    writeLines(sub("Version: 0.0.2", "Version: 0.0.1", readLines(racedep2_desc), fixed = TRUE), racedep2_desc)

    # 2. Reinstall/update 1-hop transitive dependency transdepfixture to version 0.0.2 (formals: x, new)
    #    between worker preparation and bundle persistence.
    unlink(file.path(fixtures$library, "transdepfixture"), recursive = TRUE)
    expect_true(file.copy(
        file.path(fixtures$v2_library, "transdepfixture"),
        fixtures$library,
        recursive = TRUE
    ))
    add_racedep2_import(file.path(fixtures$library, "transdepfixture"))
    key_s3_after <- package_metadata_startup_bundle_key("startup-s3", target_packages, libs)
    key_mem_after <- package_metadata_startup_bundle_key("startup-members-bundle", target_packages, libs)

    # Both pre-worker bundle_key and post-worker bundle_key must reject stale 0.0.1 worker output.
    package_metadata_startup_s3_write(key_s3_before, target_packages, s3_prepared_001)
    package_metadata_startup_s3_write(key_s3_after, target_packages, s3_prepared_001)
    package_metadata_startup_members_write(key_mem_before, target_packages, mem_prepared_001)
    package_metadata_startup_members_write(key_mem_after, target_packages, mem_prepared_001)
    package_metadata_startup_members_write(key_mem_after, target_packages, mem_prepared_001$snapshots)

    expect_null(package_metadata_startup_s3_read(key_s3_after, target_packages))
    expect_null(package_metadata_startup_members_read(key_mem_after, target_packages))

    # 3. Also verify direct startup package update (startupbundlefixture 0.0.1 -> 0.0.2)
    #    between preparation and persistence is rejected by workspace_startup_*_cache().
    race_s3_attempt <- 0L
    race_s3 <- workspace_startup_s3_cache(prepare = function(req) {
        race_s3_attempt <<- race_s3_attempt + 1L
        startup_desc <- file.path(fixtures$library, fixtures$startup_pkg, "DESCRIPTION")
        writeLines(sub("Version: 0.0.1", "Version: 0.0.2", readLines(startup_desc), fixed = TRUE), startup_desc)
        s3_prepared_001
    }, persist = TRUE)()
    expect_identical(race_s3_attempt, 1L)
    dir <- package_metadata_cache_directory()
    expect_length(
        if (!is.null(dir) && dir.exists(dir)) list.files(dir, pattern = "^startup-startup-s3-.*\\.rds$") else character(),
        0L
    )

    # Restore startupbundlefixture DESCRIPTION to 0.0.1 while keeping transdepfixture at 0.0.2.
    startup_desc <- file.path(fixtures$library, fixtures$startup_pkg, "DESCRIPTION")
    writeLines(sub("Version: 0.0.2", "Version: 0.0.1", readLines(startup_desc), fixed = TRUE), startup_desc)

    # 4. Fresh worker preparation at transdepfixture 0.0.2 succeeds, persists, and returns (x, new).
    prepared_002 <- prepare_both_in_worker(requested)
    s3_warm <- workspace_startup_s3_cache(prepare = function(req) prepared_002$s3, persist = TRUE)()
    mem_warm <- workspace_startup_members_cache(prepare = function(req) prepared_002$mem, persist = TRUE)()
    expect_identical(
        names(s3_warm$transdepfixture$methods[["transdepfixture::racedep_generic.default"]]$formals),
        c("x", "new")
    )
    expect_false(is.null(package_metadata_startup_s3_read(key_s3_after, target_packages)))
    expect_false(is.null(package_metadata_startup_members_read(key_mem_after, target_packages)))

    # 5. Tampered on-disk bundles with mismatched or NULL catalog/snapshot/worker identity vs manifest are rejected on read.
    expect_false(is.null(attr(prepared_002$mem$snapshots, "identities", exact = TRUE)))
    orig_s3_rec <- readRDS(key_s3_after$path)
    orig_mem_rec <- readRDS(key_mem_after$path)
    tamper_s3 <- function(fn) {
        rec <- orig_s3_rec
        val <- fn(unserialize(rec$payload))
        rec$payload <- serialize(val, NULL)
        rec$checksum <- digest::digest(rec$payload, algo = "xxhash64", serialize = FALSE)
        saveRDS(rec, key_s3_after$path)
        package_metadata_startup_s3_read(key_s3_after, target_packages)
    }
    tamper_mem <- function(fn) {
        rec <- orig_mem_rec
        val <- fn(unserialize(rec$payload))
        rec$payload <- serialize(val, NULL)
        rec$checksum <- digest::digest(rec$payload, algo = "xxhash64", serialize = FALSE)
        saveRDS(rec, key_mem_after$path)
        package_metadata_startup_members_read(key_mem_after, target_packages)
    }
    expect_null(tamper_s3(function(v) {
        v$catalogs$transdepfixture$identity$version <- "0.0.1"
        v
    }))
    expect_null(tamper_s3(function(v) {
        v$catalogs$base$identity$context <- NULL
        v
    }))
    expect_null(tamper_s3(function(v) {
        v$catalogs$base$identity$context$packages <- NULL
        v
    }))
    expect_null(tamper_mem(function(v) {
        v$identities$racedep2fixture$version <- "0.0.2"
        v
    }))
    expect_null(tamper_mem(function(v) {
        v$identities <- NULL
        v
    }))
    expect_null(tamper_mem(function(v) {
        v$snapshots[[1L]]$identity$context <- NULL
        v
    }))
    expect_null(tamper_mem(function(v) {
        v$snapshots[[1L]]$identity$context$packages <- NULL
        v
    }))

    # 6. Zero-dataset startup package TOCTOU update is rejected even when passing mem_prep$snapshots directly.
    startup_bundle_clone_installed_pkg(
        file.path(shared$library, "condstable"),
        fixtures$library,
        "zerodatafixture",
        version = "0.0.1",
        imports = "racedep2fixture",
        exports = "stable_fn"
    )
    # Preparing `zerodatafixture` below loads it and its import `racedep2fixture`
    # into this process. Unload the importer first, then its import, before the
    # fixture library is removed (the helper's sweep then finds nothing left).
    withr::defer({
        for (pkg in c("zerodatafixture", "racedep2fixture")) {
            if (pkg %in% loadedNamespaces()) try(unloadNamespace(pkg), silent = TRUE)
        }
    })
    zero_targets <- c("base", "zerodatafixture")
    zero_prep <- member_prepare_startup_data(zero_targets, libs, cache_dir, include_status = TRUE)
    expect_length(zero_prep$snapshots, 0L)
    expect_false(is.null(attr(zero_prep$snapshots, "identities", exact = TRUE)))
    writeLines(sub("Version: 0.0.1", "Version: 0.0.2", readLines(racedep2_desc), fixed = TRUE), racedep2_desc)
    key_zero_after <- package_metadata_startup_bundle_key("startup-members-bundle", zero_targets, libs)
    package_metadata_startup_members_write(key_zero_after, zero_targets, zero_prep$snapshots)
    expect_null(package_metadata_startup_members_read(key_zero_after, zero_targets))
    writeLines(sub("Version: 0.0.2", "Version: 0.0.1", readLines(racedep2_desc), fixed = TRUE), racedep2_desc)

    # 7. Mutating Meta/package.rds in isolation refreshes package_metadata_identity()$version.
    id_before_rds <- package_metadata_identity("racedep2fixture", lib_paths = libs)
    expect_identical(id_before_rds$version, "0.0.1")
    racedep2_rds <- file.path(fixtures$library, "racedep2fixture", "Meta", "package.rds")
    pr <- readRDS(racedep2_rds)
    pr$DESCRIPTION[["Version"]] <- "0.0.9"
    saveRDS(pr, racedep2_rds)
    id_after_rds <- package_metadata_identity("racedep2fixture", lib_paths = libs)
    expect_identical(id_after_rds$version, "0.0.9")
    expect_false(identical(id_before_rds$files, id_after_rds$files))

    # 8. Bundle keys are locale-independent but specific to the library path list.
    extra_lib <- withr::local_tempdir()
    key_base_only <- package_metadata_startup_bundle_key("startup-s3", target_packages, libs)
    key_with_extra <- package_metadata_startup_bundle_key("startup-s3", target_packages, c(extra_lib, libs))
    expect_false(identical(key_base_only$path, key_with_extra$path))
    collate_c <- withr::with_collate("C", {
        environment(package_metadata_file_fingerprint)$cache$clear()
        environment(package_metadata_manifest)$cache$clear()
        environment(package_metadata_startup_bundle_key)$cache$clear()
        package_metadata_startup_bundle_key("startup-s3", target_packages, libs)$path
    })
    # Probe whether a non-C collation is available without leaving it set: the
    # process collation must be the same after this test as before it.
    collate_before <- Sys.getlocale("LC_COLLATE")
    utf8_loc <- withr::with_collate(
        collate_before,
        tryCatch(Sys.setlocale("LC_COLLATE", "en_US.UTF-8"), warning = function(w) "", error = function(e) "")
    )
    expect_identical(Sys.getlocale("LC_COLLATE"), collate_before)
    if (nzchar(utf8_loc)) {
        collate_utf8 <- withr::with_collate("en_US.UTF-8", {
            expect_identical(Sys.getlocale("LC_COLLATE"), utf8_loc)
            environment(package_metadata_file_fingerprint)$cache$clear()
            environment(package_metadata_manifest)$cache$clear()
            environment(package_metadata_startup_bundle_key)$cache$clear()
            package_metadata_startup_bundle_key("startup-s3", target_packages, libs)$path
        })
        expect_identical(collate_c, collate_utf8)
    }
    expect_identical(Sys.getlocale("LC_COLLATE"), collate_before)
})
