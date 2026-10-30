test_that("Persistent package models survive reads and recover from corrupt cache files", {
    directory <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = directory)
    package_metadata_cache_server()
    prepared <- 0L
    prepare_input <- member_namespace_input
    testthat::local_mocked_bindings(member_namespace_input = function(...) {
        prepared <<- prepared + 1L
        prepare_input(...)
    }, .package = "languageserver")
    first <- member_prepare_package("collections")
    expect_identical(prepared, 1L)
    second <- member_prepare_package("collections")
    expect_identical(prepared, 1L)
    expect_identical(second, first)
    path <- package_metadata_cache_path("members", first$identity)
    expect_true(file.exists(path))
    cross_session <- file.path(directory, "cross-session.rds")
    callr::r(function(path, identity, value) {
        languageserver:::package_metadata_cache_write(path, identity, value)
    }, args = list(cross_session, first$identity, first), libpath = .libPaths(),
    system_profile = FALSE, user_profile = FALSE)
    restored <- callr::r(function(path, identity) {
        languageserver:::package_metadata_cache_read(path, identity)
    }, args = list(cross_session, first$identity), libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE)
    expect_identical(restored, first)
    writeLines("corrupt cache", path)
    expect_null(package_metadata_cache_read(path, first$identity))
    third <- member_prepare_package("collections")
    expect_identical(prepared, 2L)
    expect_identical(package_metadata_cache_read(package_metadata_cache_path("members", third$identity), third$identity), third)
    changed <- member_prepare_package("collections", context = "changed_namespace_context")
    expect_identical(prepared, 3L)
    expect_false(identical(changed$identity, first$identity))
    expect_null(package_metadata_cache_read(path, changed$identity))
})

test_that("Persistent caching can be disabled without affecting package preparation", {
    withr::local_options(languageserver.package_cache_dir = FALSE)
    expect_null(package_metadata_cache_directory())
    expect_null(package_metadata_cache_path("members", list(package = "fixture")))
    expect_null(package_metadata_cache_read(NULL, list()))
    expect_silent(package_metadata_cache_write(NULL, list(), list()))
})

test_that("Disabling disk caching also rejects previously computed paths", {
    withr::local_options(languageserver.package_cache_dir = withr::local_tempdir())
    identity <- list(package = "disablefixture")
    path <- package_metadata_cache_path("fixture", identity)
    package_metadata_cache_write(path, identity, list(value = 1))
    unsafe <- identity
    unsafe$context <- list(cacheable = FALSE)
    expect_null(package_metadata_cache_read(path, unsafe))
    package_metadata_cache_write(path, unsafe, list(value = 2))
    expect_identical(unserialize(readRDS(path)$payload), list(value = 1))
    withr::local_options(languageserver.package_cache_dir = FALSE)
    expect_null(package_metadata_cache_read(path, identity))
    package_metadata_cache_write(path, identity, list(value = 2))
    expect_identical(unserialize(readRDS(path)$payload), list(value = 1))
})

test_that("Server fingerprints refresh after live implementation and nested constant changes", {
    original <- package_metadata_cache_server()
    ns <- asNamespace("languageserver")
    update <- function(name, value) {
        unlockBinding(name, ns)
        assign(name, value, ns)
        lockBinding(name, ns)
    }
    constants <- member_base_intrinsics
    withr::defer(update("member_base_intrinsics", constants))
    update("member_base_intrinsics", c(constants, "cache_review_fixture"))
    expect_false(identical(package_metadata_cache_server(), original))
    update("member_base_intrinsics", constants)
    expect_identical(package_metadata_cache_server(), original)
    expect_true(package_metadata_server_constant(list(nested = list(value = TRUE))))
    expect_false(identical(package_metadata_server_syntax(list(nested = list(value = TRUE))),
            package_metadata_server_syntax(list(nested = list(value = FALSE)))))
    expect_false(package_metadata_server_constant(list(runtime = new.env())))
})

test_that("Server file compatibility describes the loaded implementation", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    native <- file.path(root, "server-native")
    writeBin(charToRaw("first"), native)
    previous <- as.list(package_metadata_server_load_state)
    withr::defer({
        package_metadata_server_load_state$files <- previous$files
        package_metadata_server_load_state$namespace <- previous$namespace
    })
    load <- package_metadata_server_loaded
    environment(load) <- list2env(list(getLoadedDLLs = function() list(languageserver = list(path = native)),
            getNamespaceInfo = function(ns, which) root), parent = environment(load))
    load()
    loaded <- package_metadata_cache_server()
    writeBin(charToRaw("other"), native)
    expect_identical(package_metadata_cache_server(), loaded)
    load()
    expect_false(identical(package_metadata_cache_server(), loaded))
})

test_that("Oversized and malformed persistent entries are cache misses", {
    withr::local_options(languageserver.package_cache_dir = withr::local_tempdir())
    testthat::local_mocked_bindings(package_metadata_cache_max_bytes = 512, .package = "languageserver")
    identity <- list(package = "boundedfixture")
    path <- package_metadata_cache_path("fixture", identity)
    package_metadata_cache_write(path, identity, raw(1000L))
    expect_false(file.exists(path))
    dir.create(dirname(path), recursive = TRUE)
    writeBin(raw(1000L), path)
    expect_null(package_metadata_cache_read(path, identity))
    saveRDS("invalid record", path)
    expect_null(package_metadata_cache_read(path, identity))
})

test_that("Cache types, format revisions and server versions remain isolated", {
    withr::local_options(languageserver.package_cache_dir = withr::local_tempdir())
    identity <- list(package = "fixture", revision = 1L)
    paths <- vapply(c("members", "s3", "documentation"), package_metadata_cache_path, character(1L), identity = identity)
    expect_length(unique(paths), 3L)
    path <- paths[[1L]]
    package_metadata_cache_write(path, identity, list(value = "valid"))
    expect_identical(package_metadata_cache_read(path, identity), list(value = "valid"))
    expect_null(package_metadata_cache_read(path, identity, is.character))
    record <- readRDS(path)
    record$payload <- serialize(list(value = "silently modified"), NULL)
    saveRDS(record, path)
    expect_null(package_metadata_cache_read(path, identity))
    expect_null(package_metadata_cache_path("s3", list(package = "fixture", context = list(cacheable = FALSE))))
    record$payload <- serialize(list(value = "valid"), NULL)
    record$format <- 999L
    saveRDS(record, path)
    expect_null(package_metadata_cache_read(path, identity))
    record$format <- package_metadata_cache_format
    record$server$version <- "999.0.0"
    saveRDS(record, path)
    expect_null(package_metadata_cache_read(path, identity))
})

test_that("Content fingerprints are reused and detect replacements preserving size and mtime", {
    root <- withr::local_tempdir()
    path <- file.path(root, "fixture.rds")
    writeBin(charToRaw("first"), path)
    first <- package_metadata_file_fingerprint(path)
    checksum <- package_metadata_file_checksum
    computed <- 0L
    testthat::local_mocked_bindings(package_metadata_file_checksum = function(files) {
        computed <<- computed + 1L
        checksum(files)
    }, .package = "languageserver")
    expect_identical(package_metadata_file_fingerprint(path), first)
    expect_identical(computed, 0L)
    stamp <- file.info(path)
    writeBin(charToRaw("other"), path)
    Sys.setFileTime(path, stamp$mtime)
    expect_identical(file.info(path)$size, stamp$size)
    expect_equal(file.info(path)$mtime, stamp$mtime)
    expect_false(identical(package_metadata_file_fingerprint(path), first))
    expect_identical(computed, 1L)
    expect_silent(package_metadata_file_fingerprint(path))
    expect_identical(computed, 1L)
})

test_that("Loaded namespace changes cannot poison models for another library", {
    root <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = file.path(root, "cache"))
    original <- .libPaths()
    withr::defer(.libPaths(original))
    package <- "lscachefixture"
    withr::defer(if (package %in% loadedNamespaces()) unloadNamespace(package))
    install <- function(field, lib) {
        dir.create(lib)
        source <- file.path(root, paste0("source-", field))
        dir.create(source)
        dir.create(file.path(source, "R"))
        dir.create(file.path(source, "man"))
        writeLines(c("Package: lscachefixture", "Version: 0.0.1", "Title: Cache Test Fixture",
                "Description: Test package metadata cache invalidation.", "License: MIT",
                "Author: Test", "Maintainer: Test <test@example.com>"), file.path(source, "DESCRIPTION"))
        writeLines(c("export(api, target, adapt)", "S3method(adapt, default)"), file.path(source, "NAMESPACE"))
        writeLines(c(paste0("api <- list(", field, " = TRUE)"), "target <- function(value = 1e5) value",
                "adapt <- function(x) UseMethod('adapt')", "adapt.default <- function(x) list(done = TRUE)"),
            file.path(source, "R", "api.R"))
        writeLines(c("\\name{api}", "\\alias{api}", if (field == "new") "\\alias{late_topic}",
                paste0("\\title{The ", field, " documentation}"),
                paste0("\\description{The ", field, " description.}")), file.path(source, "man", "api.Rd"))
        result <- callr::rcmd("INSTALL", cmdargs = c(paste0("--library=", lib), source),
            stdout = "|", stderr = "|", system_profile = FALSE, user_profile = FALSE)
        expect_identical(result$status, 0L)
    }
    first_lib <- file.path(root, "first")
    second_lib <- file.path(root, "second")
    install("old", first_lib)
    install("new", second_lib)
    .libPaths(c(first_lib, original))
    first <- member_prepare_package(package)
    expect_identical(names(first$roots$api$fields), "old")
    namespace <- PackageNamespace$new(package)
    expect_match(namespace$get_documentation("api")$title, "old documentation", fixed = TRUE)
    expect_identical(namespace$get_documentation("late_topic"), list())
    expect_false(is.null(package_metadata_help_identity(character(), package)))
    first_help <- utils::help("api", (package), lib.loc = first_lib)
    second_help <- utils::help("api", (package), lib.loc = second_lib)
    expect_false(identical(package_metadata_help_identity(first_help), package_metadata_help_identity(second_help)))
    expect_match(get_help(second_help, "text"), "new description", fixed = TRUE)
    workspace <- Workspace$new(NULL)
    expect_match(workspace$get_help("api", package), "old documentation", fixed = TRUE)
    # Replace the installed help database without changing the version or
    # loaded namespace. R's own lazy-load database cache must refresh too.
    files <- list.files(file.path(second_lib, package, "help"), full.names = TRUE)
    for (source in files) {
        destination <- file.path(first_lib, package, "help", basename(source))
        stamp <- file.info(destination)$mtime
        expect_true(file.copy(source, destination, overwrite = TRUE))
        Sys.setFileTime(destination, stamp)
    }
    expect_true(file.copy(file.path(second_lib, package, "Meta/Rd.rds"),
            file.path(first_lib, package, "Meta/Rd.rds"), overwrite = TRUE))
    expect_match(namespace$get_documentation("api")$title, "new documentation", fixed = TRUE)
    expect_match(namespace$get_documentation("late_topic")$title, "new documentation", fixed = TRUE)
    expect_match(workspace$get_help("api", package), "new documentation", fixed = TRUE)
    expect_match(get_help(first_help, "text"), "new description", fixed = TRUE)
    # A same-version, same-size DESCRIPTION replacement changes the general
    # package identity even when its modification time is preserved.
    before <- package_metadata_identity(package)
    description <- file.path(first_lib, package, "DESCRIPTION")
    stamp <- file.info(description)
    text <- readLines(description)
    writeLines(sub("Cache Test Fixture", "Cache Best Fixture", text, fixed = TRUE), description)
    Sys.setFileTime(description, stamp$mtime)
    expect_identical(file.info(description)$size, stamp$size)
    expect_false(identical(package_metadata_identity(package), before))
    second <- member_prepare_package(package, c(second_lib, original))
    expect_identical(names(second$roots$api$fields), "old")
    unloadNamespace(package)
    third <- member_prepare_package(package, c(second_lib, original))
    expect_identical(names(third$roots$api$fields), "new")
    expect_false(identical(second$identity$namespace, third$identity$namespace))
    ns <- asNamespace(package)
    withr::local_options(scipen = 0)
    signature <- namespace$get_signature("target")
    withr::local_options(scipen = 999)
    expect_false(identical(namespace$get_signature("target"), signature))
    expect_identical(names(formals(namespace$get_diagnostic_stub("target"))), "value")
    replacement <- function(revised = TRUE) revised
    environment(replacement) <- ns
    unlockBinding("target", ns)
    assign("target", replacement, ns)
    lockBinding("target", ns)
    expect_identical(names(namespace$get_formals("target")), "revised")
    expect_identical(names(formals(namespace$get_diagnostic_stub("target"))), "revised")
    unlockBinding("api", ns)
    assign("api", list(runtime = TRUE), ns)
    lockBinding("api", ns)
    changed <- member_prepare_package(package, c(second_lib, original))
    expect_identical(names(changed$roots$api$fields), "runtime")
    expect_false(identical(changed$identity$namespace, third$identity$namespace))
    registered <- ns$adapt.default
    bindings <- s3_metadata_context(package)
    expect_false(is.null(bindings))
    foreign <- registered
    environment(foreign) <- new.env(parent = ns)
    expect_identical(member_function_syntax(registered), member_function_syntax(foreign))
    registerS3method("adapt", "default", foreign, envir = ns)
    expect_false(identical(s3_metadata_context(package), bindings))
    changed <- member_prepare_package(package, c(second_lib, original))
    expect_identical(changed$generic_external, "adapt")
    other <- registered
    environment(other) <- new.env(parent = ns)
    registerS3method("adapt", "default", other, envir = ns)
    expect_identical(member_prepare_package(package, c(second_lib, original))$generic_external, "adapt")
})

test_that("S3 catalogs reuse cached metadata and detect registry changes", {
    withr::local_options(languageserver.package_cache_dir = withr::local_tempdir())
    prepared <- character()
    prepare <- s3_prepare_namespace
    testthat::local_mocked_bindings(s3_prepare_namespace = function(package) {
        prepared <<- c(prepared, package)
        prepare(package)
    }, .package = "languageserver")
    first <- s3_prepare_packages("datasets")
    initial_preparations <- length(prepared)
    prepared <- character()
    second <- s3_prepare_packages("datasets")
    expect_true(initial_preparations > 0L)
    expect_length(prepared, 0L)
    expect_identical(second, first)
    table <- get(".__S3MethodsTable__.", asNamespace("base"))
    method <- "print.persistent_cache_fixture"
    withr::defer(rm(list = method, envir = table))
    assign(method, function(x, ...) NULL, table)
    changed <- s3_prepare_packages("datasets")
    expect_true(length(prepared) > 0L)
    expect_false(identical(changed$base$generation, first$base$generation))
    expect_true(paste0("base::", method) %in% names(changed$base$methods))
})

test_that("Documentation and rendered help reuse the shared package cache", {
    withr::local_options(languageserver.package_cache_dir = withr::local_tempdir())
    parsed <- 0L
    parse_help <- get_help_rd
    testthat::local_mocked_bindings(get_help_rd = function(...) {
        parsed <<- parsed + 1L
        parse_help(...)
    }, .package = "languageserver")
    first <- PackageNamespace$new("base")$get_documentation("sum")
    expect_identical(parsed, 1L)
    second <- PackageNamespace$new("base")$get_documentation("sum")
    expect_identical(second, first)
    expect_identical(parsed, 1L)
    help <- utils::help("sum", "base")
    text <- get_help(help, "text")
    expect_identical(parsed, 2L)
    expect_identical(get_help(help, "text"), text)
    expect_identical(parsed, 2L)
})
