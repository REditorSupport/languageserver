lazydata_fixture <- function(code, snapshots, startup = character(), point = NULL) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    document <- Document$new("file:///lazydata-members.R", "r", 1L, content)
    data <- parse_document(document$uri, content)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- MemberMetadataCache$new(32 * 1024^2)
    for (package in names(snapshots)) metadata$set(package, snapshots[[package]])
    if (is.null(point)) point <- list(row = length(content) - 1L, col = nchar(tail(content, 1L)))
    list(document = document, point = point,
        workspace = list(member_metadata = metadata, startup_packages = startup))
}

lazydata_labels <- function(code, snapshots, startup = character(), point = NULL) {
    fixture <- lazydata_fixture(code, snapshots, startup, point)
    items <- member_completion(fixture$document$uri, fixture$workspace, fixture$document, fixture$point, FALSE, 200L)
    vapply(items, `[[`, character(1L), "label")
}

lazydata_test_package <- function(.local_envir = parent.frame()) {
    root <- withr::local_tempdir(.local_envir = .local_envir)
    package <- "lazycolumnfixture"
    path <- file.path(root, package)
    library <- file.path(root, "library")
    marker <- file.path(root, "getter-called")
    dir.create(file.path(path, "R"), recursive = TRUE)
    dir.create(file.path(path, "data"))
    dir.create(library)
    writeLines(c(
        "Package: lazycolumnfixture", "Version: 0.0.1", "Title: Lazy Dataset Fixture",
        "Description: Test lazy dataset completion without external packages.", "License: MIT",
        "Author: Test Fixture", "Maintainer: Test Fixture <fixture@example.org>", "LazyData: true"
    ), file.path(path, "DESCRIPTION"))
    writeLines('S3method("$", lazy_fixture_frame)', file.path(path, "NAMESPACE"))
    writeLines(c(
        "`$.lazy_fixture_frame` <- function(x, name) {",
        paste0("writeLines(\"getter\", ", deparse(marker), ")"),
        ".subset2(x, name)", "}"
    ), file.path(path, "R", "methods.R"))
    columns <- c("alpha", "beta", "a b")
    observations <- data.frame(alpha = seq_len(150L), beta = rep("value", 150L),
        `a b` = rep(TRUE, 150L), check.names = FALSE)
    class(observations) <- c("lazy_fixture_frame", "lazy_fixture_table", "data.frame")
    save(observations, file = file.path(path, "data", "observations.rda"), version = 2)
    install <- system2(file.path(R.home("bin"), "R"),
        c("CMD", "INSTALL", paste0("--library=", shQuote(library)), shQuote(path)),
        stdout = TRUE, stderr = TRUE)
    status <- attr(install, "status")
    if (is.null(status)) status <- 0L
    expect_identical(status, 0L, info = paste(install, collapse = "\n"))
    if (status != 0L) stop("Lazy dataset fixture installation failed")
    withr::defer(if (package %in% loadedNamespaces()) unloadNamespace(package), envir = .local_envir)
    list(root = root, package = package, library = library, columns = columns, marker = marker)
}

test_that("Lazy datasets retain public visibility without exposing private bindings", {
    state <- new.env(parent = emptyenv())
    state$loads <- 0L
    marker <- withr::local_tempfile()
    ns <- new.env(parent = baseenv())
    ns$shared <- list(private = TRUE)
    ns$exported <- list(namespace = TRUE)
    data <- new.env(parent = emptyenv())
    delayedAssign("shared", {
        state$loads <- state$loads + 1L
        data.frame(alpha = 1L, beta = 2L)
    }, assign.env = data)
    delayedAssign("exported", stop("shadowed dataset must not load"), assign.env = data)
    delayedAssign("broken", stop("bad dataset"), assign.env = data)
    makeActiveBinding("active", function() {
        writeLines("getter", marker)
        stop("getter")
    }, data)
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "lazyfixture", exports = "exported", lazydata = data
    )))
    snapshots <- list(lazyfixture = snapshot)
    expect_identical(state$loads, 1L)
    expect_identical(lazydata_labels("lazyfixture::shared$", snapshots), c("alpha", "beta"))
    expect_identical(lazydata_labels("library(lazyfixture)\nshared$", snapshots), c("alpha", "beta"))
    expect_identical(lazydata_labels("copy <- lazyfixture::shared\ncopy$", snapshots), c("alpha", "beta"))
    expect_identical(lazydata_labels("lazyfixture:::shared$", snapshots), "private")
    expect_identical(lazydata_labels("lazyfixture::exported$", snapshots), "namespace")
    expect_length(lazydata_labels("lazyfixture::broken$", snapshots), 0L)
    expect_length(lazydata_labels("lazyfixture::active$", snapshots), 0L)
    expect_length(lazydata_labels("shared$", snapshots), 0L)
    expect_identical(state$loads, 1L)
    expect_false(file.exists(marker))
})

lazydata_shared_snapshot <- function(package, column) {
    ns <- new.env(parent = baseenv())
    data <- new.env(parent = emptyenv())
    data$shared <- stats::setNames(data.frame(1L), column)
    member_index_freeze(member_package_index(member_namespace_input(
        ns, package = package, exports = character(), lazydata = data
    )))
}

test_that("Implicit startup datasets follow R masking and lexical shadowing", {
    snapshots <- list(earlier = lazydata_shared_snapshot("earlier", "first"),
        later = lazydata_shared_snapshot("later", "second"), explicit = lazydata_shared_snapshot("explicit", "third"))
    expect_identical(lazydata_labels("shared$", snapshots, c("earlier", "later")), "second")
    expect_identical(lazydata_labels("library(explicit)\nshared$", snapshots, c("earlier", "later")), "third")
    expect_identical(lazydata_labels("shared <- list(local = TRUE)\nshared$", snapshots, c("earlier", "later")), "local")
    expect_length(lazydata_labels("shared$", snapshots), 0L)
    expect_length(lazydata_labels("f <- function(shared) { shared$\n}", snapshots, c("earlier", "later"),
            point = list(row = 0L, col = nchar("f <- function(shared) { shared$"))), 0L)
})

test_that("Suppressed package attachments expose dataset columns in source order", {
    snapshots <- list(earlier = lazydata_shared_snapshot("earlier", "first"),
        later = lazydata_shared_snapshot("later", "second"))
    lines <- c("suppressPackageStartupMessages({", "  shared$first", "  library(earlier)",
        "  shared$first", "  require('later')", "})", "shared$second")
    code <- paste(lines, collapse = "\n")
    expect_length(lazydata_labels(code, snapshots, point = list(row = 1L, col = 9L)), 0L)
    expect_identical(lazydata_labels(code, snapshots, point = list(row = 3L, col = 9L)), "first")
    expect_identical(lazydata_labels(code, snapshots, point = list(row = 6L, col = 7L)), "second")

    for (scope in c(
        "suppressPackageStartupMessages <- function(expr) NULL\nsuppressPackageStartupMessages(library(earlier))",
        "suppressPackageStartupMessages({ library <- function(...) NULL; library(earlier) })",
        "suppressPackageStartupMessages(quote(library(earlier)))",
        "suppressPackageStartupMessages(if (flag) library(earlier))")) {
        expect_length(lazydata_labels(paste(scope, "shared$", sep = "\n"), snapshots), 0L)
    }
    qualified <- paste("suppressPackageStartupMessages <- function(expr) NULL", "library <- function(...) NULL",
        "base::suppressPackageStartupMessages(expr = base::library(earlier))", "shared$", sep = "\n")
    expect_identical(lazydata_labels(qualified, snapshots), "first")
})

test_that("Base wrappers expose datasets from their evaluated arguments", {
    snapshots <- list(earlier = lazydata_shared_snapshot("earlier", "first"),
        later = lazydata_shared_snapshot("later", "second"))
    columns <- function(scope) lazydata_labels(paste(scope, "shared$", sep = "\n"), snapshots)
    for (scope in c(
        "try(library(earlier))",
        "invisible(identity(force(library(earlier))))",
        "try(silent = TRUE, expr = suppressPackageStartupMessages({ library(earlier) }))",
        "tryCatch(error = function(e) NULL, expr = { require('earlier') })",
        "try <- function(expr) NULL\nsuppressPackageStartupMessages(base::try(library(earlier)))")) {
        expect_identical(columns(scope), "first", info = scope)
    }
    expect_identical(columns("tryCatch(finally = library(later), expr = library(earlier))"), "second")
    code <- paste(c("library(earlier)", "tryCatch(TRUE, finally = {",
            "  shared$first", "  library(later)", "  shared$second", "}, error = function(e) NULL)"), collapse = "\n")
    expect_identical(lazydata_labels(code, snapshots, point = list(row = 2L, col = 9L)), "first")
    expect_identical(lazydata_labels(code, snapshots, point = list(row = 4L, col = 9L)), "second")
    code <- paste(c("library(earlier)", "tryCatch(finally = { library(later) }, {",
            "  shared$first", "})"), collapse = "\n")
    expect_identical(lazydata_labels(code, snapshots, point = list(row = 2L, col = 9L)), "first")
    incomplete <- sub("shared$first", "shared$", code, fixed = TRUE)
    expect_identical(lazydata_labels(incomplete, snapshots, point = list(row = 2L, col = 9L)), "first")
    for (scope in c(
        "tryCatch(TRUE, error = function(e) library(earlier))",
        "try(silent = { library(earlier); TRUE }, expr = TRUE)",
        "try <- function(expr) NULL\nsuppressPackageStartupMessages(try(library(earlier)))",
        "tryCatch <- function(...) NULL\ntryCatch(library(earlier))")) {
        expect_identical(columns(scope), character(), info = scope)
    }
})

lazydata_labels_at_access <- function(lines, row, snapshots) {
    column <- regexpr("$", lines[[row]], fixed = TRUE)[[1L]]
    lazydata_labels(paste(lines, collapse = "\n"), snapshots, point = list(row = row - 1L, col = column))
}

test_that("A finally block written first sees attachments in execution order", {
    snapshots <- list(earlier = lazydata_shared_snapshot("earlier", "first"),
        later = lazydata_shared_snapshot("later", "second"))
    lines <- c("tryCatch(", "  finally = {", "    library(later)", "    shared$", "  },", "  expr = TRUE", ")")
    expect_identical(lazydata_labels_at_access(lines, 4L, snapshots), "second")
    lines <- c("tryCatch(finally = {", "  shared$", "}, expr = { library(earlier) })")
    expect_identical(lazydata_labels_at_access(lines, 2L, snapshots), "first")

    lines <- c("tryCatch(finally = {", "  shared$first", "  library(later)", "  shared$second",
        "}, expr = {", "  library(earlier)", "  shared$first", "})", "shared$second")
    expect_identical(lazydata_labels_at_access(lines, 2L, snapshots), "first")
    expect_identical(lazydata_labels_at_access(lines, 4L, snapshots), "second")
    expect_identical(lazydata_labels_at_access(lines, 7L, snapshots), "first")
    expect_identical(lazydata_labels_at_access(lines, 9L, snapshots), "second")
})

test_that("An assignment keeps the package object visible when its RHS was evaluated", {
    snapshots <- list(earlier = lazydata_shared_snapshot("earlier", "first"),
        later = lazydata_shared_snapshot("later", "second"))
    lines <- c("library(earlier)", "suppressPackageStartupMessages({", "  x <- shared",
        "  library(later)", "  x$", "})")
    expect_identical(lazydata_labels_at_access(lines, 5L, snapshots), "first")

    lines <- c("library(earlier)", "suppressPackageStartupMessages({", "  x <- shared",
        "  library(later)", "  copy <- x", "  copy$first", "  shared$second", "})")
    expect_identical(lazydata_labels_at_access(lines, 6L, snapshots), "first")
    expect_identical(lazydata_labels_at_access(lines, 7L, snapshots), "second")
    lines <- c("library(earlier)", "x <- shared", "library(later)", "x$")
    expect_identical(lazydata_labels_at_access(lines, 4L, snapshots), "first")
    lines <- c("library(earlier)", "suppressPackageStartupMessages({", "  f <- function() shared",
        "  library(later)", "  f()$", "})")
    expect_identical(lazydata_labels_at_access(lines, 5L, snapshots), "second")
})

test_that("Wrapped attachments in a sourced file take effect at the source call", {
    root <- index_normalize_path(withr::local_tempdir())
    workspace <- Workspace$new(root)
    workspace$member_metadata$set("earlier", lazydata_shared_snapshot("earlier", "first"))
    workspace$member_metadata$set("later", lazydata_shared_snapshot("later", "second"))
    open <- function(name, lines) {
        uri <- path_to_uri(file.path(root, name))
        doc <- Document$new(uri, "r", 1L, lines)
        doc$did_open()
        workspace$documents$set(uri, doc)
        parsed <- parse_document(uri, lines)
        parsed$version <- doc$version
        workspace$update_parse_data(uri, parsed)
        doc
    }
    open("packages.R", c("suppressPackageStartupMessages({", "  library(later)", "})"))
    doc <- open("main.R", c("library(earlier)", "shared$first", 'source("packages.R")', "shared$second"))
    columns <- function(row) {
        items <- member_completion(doc$uri, workspace, doc, list(row = row, col = 7L), FALSE, 200L)
        vapply(items, `[[`, character(1L), "label")
    }
    expect_identical(columns(1L), "first")
    expect_identical(columns(3L), "second")
})

test_that("Data-frame subclasses propose stored columns without invoking dollar methods", {
    marker <- withr::local_tempfile()
    ns <- new.env(parent = baseenv())
    ns$table <- structure(list(alpha = list(raw = TRUE), beta = 1L), class = c("lazy_table", "data.frame"))
    ns$`$.lazy_table` <- function(x, name) {
        writeLines("method", marker)
        list(virtual = TRUE)
    }
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "lazyfixture", exports = "table"
    )))
    snapshots <- list(lazyfixture = snapshot)
    expect_identical(lazydata_labels("lazyfixture::table$", snapshots), c("alpha", "beta"))
    expect_length(lazydata_labels("lazyfixture::table$alpha$", snapshots), 0L)
    expect_identical(lazydata_labels('structure(lazyfixture::table, class = "list")$alpha$', snapshots), "raw")
    expect_false(file.exists(marker))
})

test_that("Installed startup datasets supply mtcars columns", {
    snapshots <- member_prepare_startup_data("datasets")
    expect_true(isTRUE(snapshots$datasets$dataset_only))
    expect_identical(lazydata_labels("mtcars$", snapshots, "datasets"), sort(names(datasets::mtcars)))
    expect_identical(lazydata_labels("datasets::mtcars$", snapshots), sort(names(datasets::mtcars)))
    expect_length(lazydata_labels("datasets:::mtcars$", snapshots), 0L)
})

test_that("Installed lazy dataset subclasses supply columns without invoking getters", {
    # Starts subprocesses; too slow and timing-sensitive for CRAN machines.
    skip_on_cran()
    fixture <- lazydata_test_package()
    withr::local_libpaths(c(fixture$library, .libPaths()))
    ns <- asNamespace(fixture$package)
    expect_false(exists("observations", ns, inherits = FALSE))
    expect_false("observations" %in% getNamespaceExports(ns))
    snapshots <- list(lazycolumnfixture = member_prepare_package(fixture$package))
    for (code in c("lazycolumnfixture::observations$", "library(lazycolumnfixture)\nobservations$",
            "copy <- lazycolumnfixture::observations\ncopy$")) {
        expect_identical(lazydata_labels(code, snapshots), sort(fixture$columns), info = code)
    }
    expect_length(lazydata_labels("lazycolumnfixture:::observations$", snapshots), 0L)
    completion <- lazydata_fixture("lazycolumnfixture::observations$`a", snapshots)
    items <- member_completion(completion$document$uri, completion$workspace, completion$document,
        completion$point, FALSE, 200L)
    item <- items[[which(vapply(items, `[[`, character(1L), "label") == "a b")]]
    expect_identical(item$textEdit$newText, "`a b`")
    expect_false(file.exists(fixture$marker))
})

test_that("Startup dataset models can be enabled later and stay protected in the cache", {
    snapshots <- member_prepare_startup_data("datasets")
    old <- lsp_settings$get("member_completion")
    withr::defer(lsp_settings$set("member_completion", old))
    lsp_settings$set("member_completion", FALSE)
    testthat::local_mocked_bindings(workspace_startup_packages = function() "datasets",
        workspace_startup_members = function() snapshots, workspace_startup_s3 = function() list(),
        .package = "languageserver")
    workspace <- Workspace$new(withr::local_tempdir())
    expect_length(workspace$startup_member_packages, 0L)
    lsp_settings$set("member_completion", TRUE)
    workspace$load_startup_members()
    expect_identical(workspace$startup_member_packages, "datasets")
    expect_true(workspace$member_metadata$has("datasets"))
    metadata <- MemberMetadataCache$new(32 * 1024^2, max_entries = 2L)
    metadata$set("datasets", snapshots$datasets)
    for (package in c("first", "second")) {
        index <- member_generic_index("")
        index$package <- package
        metadata$set(package, member_index_freeze(index), protect = workspace$startup_member_packages)
    }
    expect_true(metadata$has("datasets"))
    expect_true("mtcars" %in% names(metadata$get("datasets")$roots))
})

test_that("Dataset completion works over LSP from an initially incomplete buffer", {
    skip_on_cran()
    fixture <- lazydata_test_package()
    # Add only the fixture directory, retaining the standalone server's normal
    # library paths instead of forwarding check-worker proxy libraries.
    columns <- function(client, code) {
        path <- tempfile(tmpdir = fixture$root, fileext = ".R")
        did_open(client, path, text = code)
        timeout <- if (identical(Sys.getenv("R_COVR"), "true")) 90 else 45
        point <- c(length(code) - 1L, nchar(tail(code, 1L)))
        result <- respond_completion(client, path, point, timeout = timeout, retry = FALSE)
        sort(vapply(result$items, `[[`, character(1L), "label"))
    }
    client <- language_client(working_dir = fixture$root, lib_paths = fixture$library)
    expect_identical(columns(client, "mtcars$"), sort(names(datasets::mtcars)))
    expect_identical(columns(client, "lazycolumnfixture::observations$"), sort(fixture$columns))
    cold_columns <- function(code) {
        cold_client <- language_client(working_dir = fixture$root, lib_paths = fixture$library)
        columns(cold_client, code)
    }
    wrapped <- c("suppressPackageStartupMessages({", "  library(lazycolumnfixture)", "})", "observations$")
    expect_identical(cold_columns(wrapped), sort(fixture$columns))
    handled <- c("tryCatch({", "  try(library(lazycolumnfixture))", "}, error = function(e) NULL)", "observations$")
    expect_identical(cold_columns(handled), sort(fixture$columns))
    expect_false(file.exists(fixture$marker))
})
