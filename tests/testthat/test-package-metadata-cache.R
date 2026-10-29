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
        writeLines(c("Package: lscachefixture", "Version: 0.0.1", "Title: Cache Test Fixture",
                "Description: Test package metadata cache invalidation.", "License: MIT",
                "Author: Test", "Maintainer: Test <test@example.com>"), file.path(source, "DESCRIPTION"))
        writeLines("export(api)", file.path(source, "NAMESPACE"))
        writeLines(paste0("api <- list(", field, " = TRUE)"), file.path(source, "R", "api.R"))
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
    second <- member_prepare_package(package, c(second_lib, original))
    expect_identical(names(second$roots$api$fields), "old")
    unloadNamespace(package)
    third <- member_prepare_package(package, c(second_lib, original))
    expect_identical(names(third$roots$api$fields), "new")
    expect_false(identical(second$identity$namespace, third$identity$namespace))
    ns <- asNamespace(package)
    unlockBinding("api", ns)
    assign("api", list(runtime = TRUE), ns)
    lockBinding("api", ns)
    changed <- member_prepare_package(package, c(second_lib, original))
    expect_identical(names(changed$roots$api$fields), "runtime")
    expect_false(identical(changed$identity$namespace, third$identity$namespace))
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
