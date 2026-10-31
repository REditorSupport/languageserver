s3_startup_catalogs <- function(packages = startup_packages) {
    stats::setNames(lapply(packages, function(package) {
        list(schema = 1L, package = package, generation = "fixture", dependencies = character(),
            functions = list(), methods = list(), objects = list())
    }), packages)
}

test_that("S3 startup retries worker failures after a bounded cooldown", {
    now <- 0
    calls <- 0L
    catalogs <- s3_startup_catalogs()
    testthat::local_mocked_bindings(workspace_startup_packages = function() startup_packages,
        .package = "languageserver")
    load <- workspace_startup_s3_cache(prepare = function(requested) {
        calls <<- calls + 1L
        now <<- now + 8
        if (calls < 4L) stop("startup worker timed out")
        catalogs
    }, clock = function() now)

    expect_identical(load(), list())
    expect_identical(calls, 1L)
    now <- 37.9
    for (i in seq_len(10L)) expect_identical(load(), list())
    expect_identical(calls, 1L)
    now <- 38
    expect_identical(load(), list())
    expect_identical(calls, 2L)
    now <- 105.9
    expect_identical(load(), list())
    expect_identical(calls, 2L)
    now <- 106
    expect_identical(load(), list())
    expect_identical(calls, 3L)
    now <- 173.9
    expect_identical(load(), list())
    expect_identical(calls, 3L)
    now <- 174
    expect_identical(load(), catalogs)
    expect_identical(calls, 4L)
    now <- 1000
    expect_identical(load(), catalogs)
    expect_identical(calls, 4L)
})

test_that("S3 startup retains partial catalogs and retries incomplete results", {
    now <- 0
    calls <- 0L
    catalogs <- s3_startup_catalogs()
    partial <- catalogs["base"]
    broken <- catalogs
    broken$stats$error <- "namespace preparation failed"
    incompatible <- catalogs
    incompatible$stats$schema <- 0L
    dependency <- catalogs
    dependency$stats$dependencies <- "missingfixture"
    testthat::local_mocked_bindings(workspace_startup_packages = function() character(),
        .package = "languageserver")
    results <- list(partial, broken, incompatible, dependency, catalogs)
    load <- workspace_startup_s3_cache(prepare = function(requested) {
        calls <<- calls + 1L
        results[[calls]]
    }, clock = function() now)

    expect_identical(load(), partial)
    expect_identical(load(), partial)
    expect_identical(calls, 1L)
    now <- 30
    expect_identical(load(), broken)
    now <- 89
    expect_identical(load(), broken)
    expect_identical(calls, 2L)
    now <- 90
    expect_identical(load(), incompatible)
    now <- 150
    expect_identical(load(), dependency)
    now <- 210
    expect_identical(load(), catalogs)
    now <- 1000
    expect_identical(load(), catalogs)
    expect_identical(calls, 5L)
})

test_that("S3 startup worker errors preserve earlier partial catalogs", {
    now <- 0
    calls <- 0L
    catalogs <- s3_startup_catalogs()
    partial <- catalogs["base"]
    testthat::local_mocked_bindings(workspace_startup_packages = function() startup_packages,
        .package = "languageserver")
    load <- workspace_startup_s3_cache(prepare = function(requested) {
        calls <<- calls + 1L
        if (calls == 1L) return(partial)
        if (calls == 2L) stop("startup worker failed")
        catalogs
    }, clock = function() now)

    expect_identical(load(), partial)
    now <- 30
    expect_identical(load(), partial)
    now <- 89
    expect_identical(load(), partial)
    expect_identical(calls, 2L)
    now <- 90
    expect_identical(load(), catalogs)
})

test_that("S3 startup invalidates successful catalogs when worker inputs change", {
    calls <- 0L
    packages <- startup_packages
    requests <- list()
    testthat::local_mocked_bindings(workspace_startup_packages = function() packages,
        .package = "languageserver")
    withr::local_options(languageserver.package_cache_dir = FALSE)
    load <- workspace_startup_s3_cache(prepare = function(requested) {
        calls <<- calls + 1L
        requests[[calls]] <<- requested
        s3_startup_catalogs(unique(c(startup_packages, requested$packages)))
    }, clock = function() 0)

    expect_named(load(), startup_packages)
    expect_named(load(), startup_packages)
    expect_identical(calls, 1L)
    packages <- c(startup_packages, "packagefixture")
    expect_named(load(), packages)
    expect_identical(calls, 2L)
    expect_identical(requests[[2L]]$packages, packages)
    library <- withr::local_tempdir()
    withr::local_libpaths(library)
    expect_named(load(), packages)
    expect_identical(calls, 3L)
    expect_identical(requests[[3L]]$lib_paths, .libPaths())
    options(languageserver.package_cache_dir = library)
    expect_named(load(), packages)
    expect_identical(calls, 4L)
    expect_identical(requests[[4L]]$cache_dir, library)
    expect_named(load(), packages)
    expect_identical(calls, 4L)
})

test_that("S3 startup cooldowns do not suppress preparation for changed inputs", {
    calls <- 0L
    packages <- startup_packages
    testthat::local_mocked_bindings(workspace_startup_packages = function() packages,
        .package = "languageserver")
    load <- workspace_startup_s3_cache(prepare = function(requested) {
        calls <<- calls + 1L
        if (calls == 1L) stop("startup worker failed")
        s3_startup_catalogs(requested$packages)
    }, clock = function() 0)

    expect_identical(load(), list())
    expect_identical(load(), list())
    expect_identical(calls, 1L)
    packages <- c(startup_packages, "packagefixture")
    expect_named(load(), packages)
    expect_identical(calls, 2L)
})

test_that("Workspace construction reads startup S3 catalogs once", {
    calls <- 0L
    catalogs <- s3_startup_catalogs()
    old <- lsp_settings$get("s3_dispatch")
    withr::defer(lsp_settings$set("s3_dispatch", old))
    lsp_settings$set("s3_dispatch", TRUE)
    testthat::local_mocked_bindings(workspace_startup_packages = function() startup_packages,
        workspace_startup_members = function() list(), workspace_startup_s3 = function() {
            calls <<- calls + 1L
            catalogs
        }, .package = "languageserver")

    workspace <- Workspace$new(NULL)
    expect_identical(calls, 1L)
    expect_setequal(workspace$s3_metadata$keys(), startup_packages)
})
