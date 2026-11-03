resolution_identity_package <- function(library, package, depends = NULL, imports = NULL) {
    path <- file.path(library, package)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    description <- c(paste("Package:", package), "Version: 1.0.0", "Title: Resolution Identity Fixture",
        "Description: Inert installed-package identity fixture.")
    if (!is.null(depends)) description <- c(description, paste("Depends:", depends))
    if (!is.null(imports)) description <- c(description, paste("Imports:", imports))
    writeLines(description, file.path(path, "DESCRIPTION"))
    writeLines(character(), file.path(path, "NAMESPACE"))
    dir.create(file.path(path, "R"), showWarnings = FALSE)
    # Merely finding and fingerprinting the installation must never execute this.
    writeLines(".onLoad <- function(...) stop('package hook was evaluated')", file.path(path, "R", "hooks.R"))
    path
}

resolution_identity_replace <- function(path, bytes) {
    time <- file.info(path)$mtime
    replacement <- tempfile("identity-replacement-", tmpdir = dirname(path))
    writeBin(charToRaw(bytes), replacement)
    Sys.setFileTime(replacement, time)
    unlink(path)
    stopifnot(file.rename(replacement, path))
}

test_that("Resolution identities follow dependencies without loading namespaces", {
    library <- withr::local_tempdir()
    resolution_identity_package(library, "resolutionchild")
    resolution_identity_package(library, "resolutionlink")
    parent <- resolution_identity_package(library, "resolutionparent",
        depends = "R (>= 4.0.0), resolutionchild (>= 1.0.0)", imports = "resolutionlink")
    first <- package_resolution_installations("resolutionparent", library)
    expect_true(first$complete)
    expect_setequal(first$packages, c("resolutionparent", "resolutionchild", "resolutionlink"))
    expect_identical(package_resolution_installations("resolutionparent", library), first)
    expect_false(any(c("resolutionparent", "resolutionchild", "resolutionlink", "resolutionnew") %in% loadedNamespaces()))

    resolution_identity_package(library, "resolutionnew")
    writeLines(c(readLines(file.path(parent, "DESCRIPTION")), "LinkingTo: resolutionnew"),
        file.path(parent, "DESCRIPTION"))
    changed <- package_resolution_installations("resolutionparent", library)
    expect_true(changed$complete)
    expect_true("resolutionnew" %in% changed$packages)
    expect_false(identical(changed$key, first$key))
    expect_false(any(c("resolutionparent", "resolutionchild", "resolutionlink", "resolutionnew") %in% loadedNamespaces()))
})

test_that("Same-version databases and binaries invalidate resolution identities", {
    library <- withr::local_tempdir()
    path <- resolution_identity_package(library, "resolutionbinary")
    dir.create(file.path(path, "libs"))
    dir.create(file.path(path, "Meta"))
    files <- file.path(path, c("R/resolutionbinary.rdb", "libs/resolutionbinary.so", "Meta/Rd.rds"))
    for (file in files) writeBin(charToRaw("before"), file)
    previous <- package_resolution_installations("resolutionbinary", library)
    for (file in files) {
        # Same byte count and restored mtime: file identity/change time still differs.
        resolution_identity_replace(file, "after!")
        current <- package_resolution_installations("resolutionbinary", library)
        expect_true(current$complete)
        expect_false(identical(current$key, previous$key), info = file)
        expect_identical(unname(read.dcf(file.path(path, "DESCRIPTION"), fields = "Version")[[1L]]), "1.0.0")
        previous <- current
    }
})

test_that("Resolution identities notice new earlier-library installations", {
    first_library <- withr::local_tempdir()
    second_library <- withr::local_tempdir()
    resolution_identity_package(second_library, "resolutionprecedence")
    libraries <- c(first_library, second_library)
    original <- package_resolution_installations("resolutionprecedence", libraries)
    expect_true(original$complete)
    path <- resolution_identity_package(first_library, "resolutionprecedence")
    writeLines("export(new_binding)", file.path(path, "NAMESPACE"))
    earlier <- package_resolution_installations("resolutionprecedence", libraries)
    expect_true(earlier$complete)
    expect_false(identical(earlier$key, original$key))
    unlink(path, recursive = TRUE)
    restored <- package_resolution_installations("resolutionprecedence", libraries)
    expect_true(restored$complete)
    expect_false(identical(restored$key, earlier$key))
})

test_that("Missing packages and dependencies do not establish a reusable identity", {
    library <- withr::local_tempdir()
    resolution_identity_package(library, "resolutionmissingparent", imports = "resolutionmissingchild")
    incomplete <- package_resolution_installations("resolutionmissingparent", library)
    expect_false(incomplete$complete)
    resolution_identity_package(library, "resolutionmissingchild")
    available <- package_resolution_installations("resolutionmissingparent", library)
    expect_true(available$complete)
    expect_true("resolutionmissingchild" %in% available$packages)
    unlink(file.path(library, "resolutionmissingchild"), recursive = TRUE)
    expect_false(package_resolution_installations("resolutionmissingparent", library)$complete)
    expect_false(package_resolution_installations("resolutionabsent", library)$complete)
    resolution_identity_package(library, "resolutionabsent")
    expect_true(package_resolution_installations("resolutionabsent", library)$complete)
})

test_that("Malformed dependency descriptions cannot be shared", {
    library <- withr::local_tempdir()
    path <- resolution_identity_package(library, "resolutionmalformed")
    writeLines("this is not a DESCRIPTION field", file.path(path, "DESCRIPTION"))
    expect_false(package_resolution_installations("resolutionmalformed", library)$complete)
})

test_that("A single oversized installation does not escape the file budget", {
    library <- withr::local_tempdir()
    path <- resolution_identity_package(library, "resolutionoversized")
    files <- file.path(path, "R", sprintf("part-%04d.rds", seq_len(4100L)))
    expect_true(all(file.create(files)))
    expect_false(package_resolution_installations("resolutionoversized", library)$complete)
})


test_that("macOS debug-symbol bundles do not disable otherwise reusable packages", {
    library <- withr::local_tempdir()
    path <- resolution_identity_package(library, "resolutiondebug")
    libs <- file.path(path, "libs")
    debug <- file.path(libs, "resolutiondebug.so.dSYM", "Contents", "Resources", "DWARF")
    dir.create(debug, recursive = TRUE)
    writeBin(charToRaw("runtime"), file.path(libs, "resolutiondebug.so"))
    writeBin(charToRaw("symbols"), file.path(debug, "resolutiondebug.so"))
    identity <- package_resolution_installations("resolutiondebug", library)
    expect_true(identity$complete)
    expect_identical(package_resolution_installations("resolutiondebug", library), identity)
})


test_that("Architecture-specific binaries can coexist with macOS debug symbols", {
    library <- withr::local_tempdir()
    path <- resolution_identity_package(library, "resolutionarchdebug")
    arch <- file.path(path, "libs", "x64")
    debug <- file.path(arch, "resolutionarchdebug.so.dSYM", "Contents", "Resources", "DWARF")
    dir.create(debug, recursive = TRUE)
    writeBin(charToRaw("runtime"), file.path(arch, "resolutionarchdebug.so"))
    writeBin(charToRaw("symbols"), file.path(debug, "resolutionarchdebug.so"))
    identity <- package_resolution_installations("resolutionarchdebug", library)
    expect_true(identity$complete)
    expect_identical(package_resolution_installations("resolutionarchdebug", library), identity)
})

test_that("In-place executable loader changes invalidate the installation", {
    library <- withr::local_tempdir()
    path <- resolution_identity_package(library, "resolutionloader")
    loader <- file.path(path, "R", "resolutionloader")
    writeLines("invisible(1L)", loader)
    original <- package_resolution_installations("resolutionloader", library)
    time <- file.info(loader)$mtime
    writeLines("invisible(2L)", loader)
    Sys.setFileTime(loader, time)
    changed <- package_resolution_installations("resolutionloader", library)
    expect_true(changed$complete)
    expect_false(identical(original$key, changed$key))
})

test_that("Resolution identities follow symlinked database targets", {
    skip_on_os("windows")
    library <- withr::local_tempdir()
    path <- resolution_identity_package(library, "resolutionsymlink")
    target <- withr::local_tempfile()
    writeBin(charToRaw("before"), target)
    link <- file.path(path, "R", "resolutionsymlink.rdb")
    expect_true(file.symlink(target, link))
    original <- package_resolution_installations("resolutionsymlink", library)
    expect_true(original$complete)
    time <- file.info(target)$mtime
    writeBin(charToRaw("after!"), target)
    Sys.setFileTime(target, time)
    changed <- package_resolution_installations("resolutionsymlink", library)
    expect_true(changed$complete)
    expect_false(identical(changed$key, original$key))
})
