external_subset_fixture <- function(code, snapshots) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    document <- Document$new("file:///external-subset.R", "r", 1L, content)
    parsed <- parse_document(document$uri, content)
    parsed$version <- 1L
    document$update_parse_data(parsed)
    metadata <- MemberMetadataCache$new(32 * 1024^2)
    for (package in names(snapshots)) metadata$set(package, snapshots[[package]])
    items <- member_completion(document$uri, list(member_metadata = metadata), document,
        list(row = length(content) - 1L, col = nchar(tail(content, 1L))), FALSE, 200L)
    vapply(items, `[[`, character(1L), "label")
}

external_subset_snapshot <- function(ns, package = "subsetowner", exports = "record") {
    member_index_freeze(member_package_index(member_namespace_input(ns,
                package = package, exports = exports)))
}

external_subset_registration <- function(operator, class, fn, .local_envir = parent.frame()) {
    table <- get(".__S3MethodsTable__.", asNamespace("base"))
    key <- paste0(operator, ".", class)
    old <- if (exists(key, table, inherits = FALSE)) get(key, table, inherits = FALSE) else NULL
    registerS3method(operator, class, fn, envir = asNamespace("base"))
    withr::defer({
        if (is.null(old)) rm(list = key, envir = table) else assign(key, old, table)
    }, envir = .local_envir)
}

test_that("Externally registered subset methods never fall back to stored list fields", {
    marker <- withr::local_tempfile()
    ns <- new.env(parent = baseenv())
    ns$record <- structure(list(raw = list(wrong = TRUE)), class = "member_external_subset")
    method <- function(x, i, ...) {
        writeLines("called", marker)
        list(virtual = TRUE)
    }
    for (operator in c("[", "[[")) external_subset_registration(operator, "member_external_subset", method)
    expect_identical(names(ns$record[[1L]]), "virtual")
    unlink(marker)
    snapshots <- list(subsetowner = external_subset_snapshot(ns))
    for (code in c("subsetowner::record[[1L]]$", "subsetowner::record[1L]$")) {
        expect_length(external_subset_fixture(code, snapshots), 0L)
    }
    expect_false(file.exists(marker))
})

test_that("Subset registrations from another installed namespace are recognized", {
    # Resolve the trusted built-in lazy method before taking the snapshot.
    get("[[.person", asNamespace("utils"))
    get("[[.person", get(".__S3MethodsTable__.", asNamespace("base")))
    ns <- new.env(parent = baseenv())
    ns$record <- structure(list(raw = list(wrong = TRUE)), class = "person")
    snapshot <- external_subset_snapshot(ns)
    expect_match(snapshot$subset_registry[["[[.person"]], "namespace:utils:", fixed = TRUE)
    expect_length(external_subset_fixture("subsetowner::record[[1L]]$", list(subsetowner = snapshot)), 0L)
})

test_that("Registered subset precedence respects the class chain and local overrides", {
    marker <- withr::local_tempfile()
    ns <- new.env(parent = baseenv())
    ns$record <- structure(list(raw = list(wrong = TRUE)), class = c("member_external_first", "member_owned_second"))
    ns$`[[.member_owned_second` <- function(x, i) list(owned = TRUE)
    external_subset_registration("[[", "member_external_first", function(x, i) {
        writeLines("called", marker)
        list(external = TRUE)
    })
    snapshots <- list(subsetowner = external_subset_snapshot(ns))
    expect_length(external_subset_fixture("subsetowner::record[[1L]]$", snapshots), 0L)
    expect_identical(external_subset_fixture(paste0(
        "`[[.member_external_first` <- function(x, i) list(local = TRUE)\n",
        "subsetowner::record[[1L]]$"
    ), snapshots), "local")
    expect_false(file.exists(marker))

    ns$record <- structure(list(raw = list(wrong = TRUE)), class = c("member_owned_first", "member_external_first"))
    ns$`[[.member_owned_first` <- function(x, i) list(owned = TRUE)
    external_subset_registration("[[", "member_owned_first", ns$`[[.member_owned_first`)
    snapshots <- list(subsetowner = external_subset_snapshot(ns))
    expect_identical(external_subset_fixture("subsetowner::record[[1L]]$", snapshots), "owned")
})

test_that("Later namespace snapshots invalidate stale default subset assumptions", {
    ns <- new.env(parent = baseenv())
    ns$record <- structure(list(raw = list(wrong = TRUE)), class = "member_later_subset")
    snapshots <- list(subsetowner = external_subset_snapshot(ns))
    expect_identical(external_subset_fixture("subsetowner::record[[1L]]$", snapshots), "wrong")
    marker <- withr::local_tempfile()
    external_subset_registration("[[", "member_later_subset", function(x, i) {
        writeLines("called", marker)
        list(external = TRUE)
    })
    other <- new.env(parent = baseenv())
    snapshots$subsetprovider <- external_subset_snapshot(other, "subsetprovider", character())
    expect_length(external_subset_fixture("subsetowner::record[[1L]]$", snapshots), 0L)
    expect_false(file.exists(marker))
})

test_that("Standard data-frame subset methods retain known column structure", {
    ns <- new.env(parent = baseenv())
    ns$record <- data.frame(row = 1L)
    ns$record$items <- list(list(done = TRUE))
    snapshots <- list(subsetowner = external_subset_snapshot(ns))
    expect_identical(external_subset_fixture("subsetowner::record[[2L]][[1L]]$", snapshots), "done")
    expect_identical(external_subset_fixture("subsetowner::record[[\"items\"]][[1L]]$", snapshots), "done")
})

test_that("A fresh isolated worker recognizes standard dataframe subset methods", {
    registry <- callr::r(function(prepare) {
        environment(prepare) <- asNamespace("languageserver")
        ns <- new.env(parent = baseenv())
        ns$record <- data.frame(row = 1L)
        ns$record$items <- list(list(done = TRUE))
        prepare(ns, "coldowner")
    }, args = list(member_subset_registry), system_profile = FALSE, user_profile = FALSE)
    expect_true(registry$complete)
    expect_identical(unname(registry$methods[c("[.data.frame", "[[.data.frame")]), c("base", "base"))
})

test_that("Uncertain registered method bindings are not forced during preparation", {
    marker <- withr::local_tempfile()
    table <- get(".__S3MethodsTable__.", asNamespace("base"))
    key <- "[[.member_active_subset"
    makeActiveBinding(key, function() {
        writeLines("getter", marker)
        stop("must not invoke method lookup")
    }, table)
    withr::defer(rm(list = key, envir = table))
    ns <- new.env(parent = baseenv())
    ns$record <- structure(list(raw = list(wrong = TRUE)), class = "member_active_subset")
    snapshots <- list(subsetowner = external_subset_snapshot(ns))
    expect_length(external_subset_fixture("subsetowner::record[[1L]]$", snapshots), 0L)
    expect_false(file.exists(marker))
})
