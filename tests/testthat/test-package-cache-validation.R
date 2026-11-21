# Each database has one record. Offsets in .rdx are zero-based byte offsets.
cache_record_fixture <- function(bytes, compressed = FALSE, references = NULL,
    variables = list(value = c(0L, length(bytes)))) {
    directory <- withr::local_tempdir(.local_envir = parent.frame())
    path <- file.path(directory, "values.rdx")
    writeBin(bytes, file.path(directory, "values.rdb"))
    saveRDS(list(variables = variables, references = references, compressed = compressed), path)
    path
}

test_that("Both lazy-load scanners accept serialized records in supported encodings", {
    for (version in c(2L, 3L)) {
        value <- serialize(list(columns = c("a", "b")), NULL, version = version)
        size <- writeBin(length(value), raw(), endian = "big")
        formats <- list(
            list(FALSE, value),
            list(TRUE, c(size, memCompress(value, "gzip"))),
            list(2L, c(size, charToRaw("0"), value)),
            list(2L, c(size, charToRaw("1"), memCompress(value, "gzip"))),
            list(2L, c(size, charToRaw("2"), memCompress(value, "bzip2"))),
            list(3L, c(size, charToRaw("Z"), memCompress(value, "xz")))
        )
        for (format in formats) {
            path <- cache_record_fixture(format[[2L]], format[[1L]])
            expect_false(package_metadata_rdb_scan_headers(path))
            expect_identical(unname(package_metadata_rdb_scan(path)), FALSE)
        }
    }
})

test_that("Unreadable compressed records prevent reuse by either scanner", {
    size <- writeBin(10L, raw(), endian = "big")
    bad_records <- list(
        list(TRUE, raw(3L)),
        list(TRUE, writeBin(-1L, raw(), endian = "big")),
        list(TRUE, c(size, memCompress(raw(4L), "gzip"))),
        list(2L, size),
        list(2L, c(size, as.raw(0L))),
        list(2L, c(size, charToRaw("?"))),
        list(2L, c(size, charToRaw("0"), raw(3L)))
    )
    for (record in bad_records) {
        path <- cache_record_fixture(record[[2L]], record[[1L]])
        expect_true(package_metadata_rdb_scan_headers(path))
        expect_identical(unname(package_metadata_rdb_scan(path)), TRUE)
    }
})

test_that("Header uncertainty defers to the worker and corrupt offsets reject reuse", {
    ascii <- serialize(123L, NULL, ascii = TRUE, version = 2L)
    path <- cache_record_fixture(ascii)
    expect_identical(package_metadata_rdb_scan_headers(path), NA)
    expect_identical(unname(package_metadata_rdb_scan(path)), FALSE)

    xdr <- serialize(123L, NULL, version = 3L)
    unknown_version <- negative_encoding <- xdr
    unknown_version[3:6] <- writeBin(4L, raw(), endian = "big")
    negative_encoding[15:18] <- writeBin(-1L, raw(), endian = "big")
    for (record in list(raw(), unknown_version, negative_encoding, head(xdr, 18L))) {
        path <- cache_record_fixture(record)
        expect_identical(package_metadata_rdb_scan_headers(path), NA)
        expect_identical(unname(package_metadata_rdb_scan(path)), TRUE)
    }

    path <- cache_record_fixture(xdr, variables = list(value = c(1L, length(xdr))))
    expect_true(package_metadata_rdb_scan_headers(path))
    expect_identical(unname(package_metadata_rdb_scan(path)), TRUE)
    unlink(sub("[.]rdx$", ".rdb", path))
    expect_true(package_metadata_rdb_scan_headers(path))
    expect_identical(unname(package_metadata_rdb_scan(path)), TRUE)
})

test_that("Both scanners inspect ordinary and lazy environment reference records", {
    eager <- serialize(list(bindings = list(number = 1L)), NULL, version = 2L)
    key <- c(0L, length(eager))
    path <- cache_record_fixture(eager, references = list(env = key), variables = list())
    expect_false(package_metadata_rdb_scan_headers(path))
    expect_identical(unname(package_metadata_rdb_scan(path)), FALSE)

    for (bad in list(c(-1L, 1L), c(0L, length(eager) + 1L))) {
        for (ref in list(bad, list(eagerKey = key, lazyKeys = list(bad)))) {
            path <- cache_record_fixture(eager, references = list(env = ref), variables = list())
            expect_true(package_metadata_rdb_scan_headers(path))
            expect_identical(unname(package_metadata_rdb_scan(path)), TRUE)
        }
    }

    ascii <- serialize(123L, NULL, ascii = TRUE, version = 2L)
    ref <- list(eagerKey = key, lazyKeys = list(c(length(eager), length(ascii))))
    path <- cache_record_fixture(c(eager, ascii), references = list(env = ref), variables = list())
    expect_identical(package_metadata_rdb_scan_headers(path), NA)
    expect_identical(unname(package_metadata_rdb_scan(path)), FALSE)

    path <- cache_record_fixture(raw(4L), references = list(env = c(0L, 4L)), variables = list())
    expect_true(package_metadata_rdb_scan_headers(path))
    expect_identical(unname(package_metadata_rdb_scan(path)), TRUE)
})

test_that("Startup manifests require matching named worker identities", {
    identity <- package_metadata_startup_identity("utils")
    manifest <- list(utils = identity)
    catalog <- list(identity = identity)
    catalog$identity$context <- list(packages = manifest)
    valid <- list(utils = catalog)
    expect_true(package_metadata_startup_s3_identities_valid(valid, manifest))
    expect_true(package_metadata_startup_members_identities_valid(valid, manifest, manifest))
    for (bad in list("utils", unname(valid), list(stats = catalog))) {
        expect_false(package_metadata_startup_s3_identities_valid(bad, manifest))
        expect_false(package_metadata_startup_members_identities_valid(bad, manifest, manifest))
    }
    for (ids in list(list(), unname(manifest), list(stats = identity))) {
        expect_false(package_metadata_startup_s3_identities_valid(structure(valid, identities = ids), manifest))
        expect_false(package_metadata_startup_members_identities_valid(valid, manifest, ids))
    }
    expect_false(package_metadata_context_matches_manifest(list(packages = list(list(package = ""))), manifest))
    expect_false(package_metadata_identity_matches_manifest(list(error = "unloaded"), identity))
    changed <- identity
    changed$startup_files <- "changed"
    expect_false(package_metadata_identity_matches_manifest(changed, identity))
    expect_true(package_metadata_bundle_key_matches_manifest(list(identity = list(packages = manifest)), manifest))
    expect_false(package_metadata_bundle_key_matches_manifest(list(manifest = list(stats = identity)), manifest))
})

test_that("Malformed worker results and incomplete catalogs cannot be cached", {
    for (bad in list("failed", structure(list(), class = "failed_worker"))) {
        unpacked <- package_metadata_startup_members_unpack(bad, "utils")
        expect_false(unpacked$valid)
        expect_false(unpacked$complete)
        expect_null(unpacked$snapshots)
        expect_false(package_metadata_startup_s3_complete(bad, "utils"))
        expect_false(package_metadata_manifest_valid(bad))
    }
    expect_false(package_metadata_startup_s3_complete(list(list(schema = 1L)), "utils"))
    expect_false(package_metadata_manifest_valid(list(list(package = "utils"))))

    key <- list(path = withr::local_tempfile())
    result <- list(.startup_members_result = TRUE, inspected = 1L, snapshots = list(list()), complete = TRUE)
    unpacked <- package_metadata_startup_members_unpack(result, "utils")
    expect_identical(unpacked$inspected, character())
    expect_false(unpacked$valid)
    expect_false(unpacked$complete)
    expect_null(unpacked$snapshots)
    package_metadata_startup_members_write(key, "utils", result)
    package_metadata_startup_s3_write(key, "utils", list())
    expect_false(file.exists(key$path))
})

test_that("Runtime stability tracks incomplete database pairs and hook-bearing indexes", {
    lib <- withr::local_tempdir()
    name <- "lsruntimefixture"
    package <- file.path(lib, name)
    meta <- file.path(package, "Meta")
    dir.create(meta, recursive = TRUE)
    writeLines(c(paste("Package:", name), "Version: 1.0"), file.path(package, "DESCRIPTION"))
    description <- c(Package = name, Version = "1.0")
    built <- readRDS(file.path(find.package("utils"), "Meta", "package.rds"))$Built
    saveRDS(list(DESCRIPTION = description, Built = built), file.path(meta, "package.rds"))
    ns <- file.path(meta, "nsInfo.rds")
    stable <- function() package_metadata_package_runtime_stable(name, lib)
    saveRDS("bad metadata", ns)
    expect_false(stable())
    saveRDS(list(), ns)
    expect_true(stable())

    code <- file.path(package, "R")
    data <- file.path(package, "data")
    dir.create(code)
    dir.create(data)
    loader <- file.path(code, name)
    writeLines(package_metadata_nspackloader_lines(), loader)
    expect_false(stable())
    unlink(loader)
    for (prefix in c(file.path(code, name), file.path(code, "sysdata"), file.path(data, "Rdata"))) {
        rdx <- paste0(prefix, ".rdx")
        rdb <- paste0(prefix, ".rdb")
        saveRDS(list(variables = list()), rdx)
        expect_false(stable())
        file.create(rdb)
        if (identical(basename(prefix), "sysdata")) {
            # R/sysdata is allowed alongside a package code database.
            saveRDS(list(variables = list()), paste0(loader, ".rdx"))
            file.create(paste0(loader, ".rdb"))
        }
        expect_true(stable())
        for (bad_index in list("not an index", list(variables = list(1L)), list(variables = list(.onLoad = 1L)))) {
            saveRDS(bad_index, rdx)
            expect_false(stable())
        }
        unlink(c(rdx, rdb))
    }
})

test_that("Namespace imports combine installed metadata with declarative source directives", {
    pkg <- withr::local_tempdir()
    dir.create(file.path(pkg, "Meta"))
    saveRDS(list(imports = list("plain", list("from", "fun"), integer()),
            importClasses = list(list("classes", "Class"), NULL),
            importMethods = list(list("methodsPkg", "method"), NULL)),
        file.path(pkg, "Meta", "nsInfo.rds"))
    expect_setequal(package_metadata_namespace_imports(pkg), c("plain", "from", "classes", "methodsPkg"))
    ns <- file.path(pkg, "NAMESPACE")
    writeLines(c('import("direct", except = hidden)', "importFrom(123, ignored)",
            "if (flag) { importFrom(conditional, fun) } else NULL"), ns)
    expect_setequal(package_metadata_namespace_imports(pkg), c("plain", "from", "classes", "methodsPkg",
            "direct", "conditional"))
    writeLines("importFrom(broken,", ns)
    expect_identical(attr(package_metadata_namespace_imports(pkg), "complete"), FALSE)
})

test_that("Startup discovery distinguishes absent packages from an empty request", {
    absent <- "languageserver.fixture.does.not.exist"
    expect_identical(package_metadata_manifest(character()), list())
    expect_null(package_metadata_manifest(absent))
    expect_true(package_metadata_runtime_stable(character()))
    expect_false(package_metadata_runtime_stable(absent))
    expect_identical(package_metadata_lazyload_files(absent), character())
    expect_true(package_metadata_missing_absent(NULL))
    expect_false(package_metadata_transitive_packages("utils", limit = -1L)$complete)
    expect_null(package_metadata_help_identity(character()))
    expect_identical(package_metadata_help_file(character()), character())
    expect_null(package_metadata_startup_members_read(NULL, "utils"))
    expect_null(package_metadata_startup_s3_read(NULL, "utils"))
})

test_that("Failed database fingerprints are never treated as reusable verdicts", {
    ok <- serialize(42L, NULL, version = 2L)
    path <- cache_record_fixture(ok)
    checksum <- package_metadata_file_checksum
    readable <- FALSE
    testthat::local_mocked_bindings(
        package_metadata_lazyload_files = function(...) path,
        package_metadata_file_checksum = function(files) {
            if (readable) checksum(files) else rep(NA_character_, length(files))
        },
        .package = "languageserver"
    )
    expect_false(package_metadata_promises_absent("fixture"))
    readable <- TRUE
    expect_true(package_metadata_promises_absent("fixture"))
})

test_that("Disk budget eviction preserves the newest valid cache record", {
    directory <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = directory)
    testthat::local_mocked_bindings(package_metadata_cache_max_bytes = 4096L,
        package_metadata_cache_server = function() "test", .package = "languageserver")
    old <- file.path(directory, "old.rds")
    new <- file.path(directory, "new.rds")
    writeBin(raw(4096L), old)
    Sys.setFileTime(old, Sys.time() - 3600)
    identity <- list(package = "fixture")
    package_metadata_cache_write(new, identity, list(field = 42L))
    expect_false(file.exists(old))
    expect_identical(package_metadata_cache_read(new, identity), list(field = 42L))
})
