test_that("Source package metadata respects file order, overrides, exports and intrinsic masking", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines(c("Package: memberfixture", "Version: 1.0"), file.path(root, "DESCRIPTION"))
    writeLines(c("export(make, inherited)", "import(rlang)", "importFrom(methods, new, slot, unrelated)"),
        file.path(root, "NAMESPACE"))
    writeLines(c("make <- function() list(old = TRUE)", "inherited <- make", "abort <- function() FALSE"),
        file.path(root, "R", "a.R"))
    writeLines(c("make <- function() list(updated = TRUE)", "register(make)", "make$attr <- 1"),
        file.path(root, "R", "z.R"))

    input <- member_source_input(root)
    expect_identical(input$package, "memberfixture")
    expect_identical(input$exports, c("make", "inherited"))
    expect_identical(input$locations$make, "z.R")
    expect_identical(input$locations$inherited, "a.R")
    expect_identical(input$definitions$make[[3L]], quote(list(updated = TRUE)))
    expect_true(all(c("list2", "new", "slot") %in% input$intrinsics))
    expect_false(any(c("unrelated", "abort") %in% input$intrinsics))
    expect_identical(input$expressions[[2L]][[2L]], quote(register(make)))
})

test_that("Snapshot budgets degrade individual fields and skip active bindings", {
    root <- new.env(parent = emptyenv())
    root$nested <- new.env(parent = emptyenv())
    root$nested$value <- 42L
    root$wide <- list(left = 1L, right = 2L)
    seen <- FALSE
    makeActiveBinding("active", function(...) {
        seen <<- TRUE
        stop("active binding was forced")
    }, root)
    snap <- member_snapshot(root, max_depth = 0L)
    shape <- member_snapshot_shape(snap)
    expect_identical(snap$fields$active$kind, "active")
    expect_false(snap$fields$nested$complete)
    expect_identical(shape$fields$nested$reason, "cycle_or_budget")
    expect_false(seen)
    snap <- member_snapshot(root, max_bindings = 4L)
    expect_identical(snap$fields$nested$fields$value$value, 42L)
    expect_identical(snap$fields$wide$reason, "snapshot_budget")
    expect_false(seen)

    two <- list(a = 1L, b = 2L)
    opaque <- member_snapshot(two, dollar_context = list(scopes = list()))
    expect_false(opaque$default_dollar)
    expect_identical(names(member_snapshot_shape(opaque)$fields), names(two))
    class(two) <- c("fixture", NA_character_)
    expect_false(member_snapshot(two)$default_dollar)
})

test_that("Snapshots retain small captures and discard oversized lexical scopes", {
    env <- new.env(parent = emptyenv())
    env$field <- 3L
    env$method <- function() field
    environment(env$method) <- env
    expect_identical(member_snapshot(env$method)$captures, list(field = 3L))
    for (i in seq_len(101L)) env[[paste0("extra", i)]] <- i
    expect_identical(member_snapshot(env$method)$captures, list())
    expect_identical(member_snapshot(list(old = pairlist(a = 1L)))$elements$old$type, "pairlist")
})

test_that("Oversized ordinary namespace values leave unrelated public data available", {
    ns <- new.env(parent = emptyenv())
    ns$large <- vector("list", 50001L)
    ns$small <- list(field = TRUE)
    input <- member_namespace_input(ns, package = "budgetfixture", exports = c("large", "small"))
    expect_identical(input$snapshots$large$reason, "snapshot_budget")
    expect_identical(input$registries$large, character())
    expect_identical(input$snapshots$small$elements$field$value, TRUE)
})

test_that("Namespace budget fallback retains public lazy data and reports its cause", {
    data <- list2env(list(table = list(column = 1L)), parent = emptyenv())
    read_input <- member_namespace_input
    failure <- "Namespace exceeds metadata budget"
    testthat::local_mocked_bindings(
        member_namespace_lazydata = function(ns) data,
        member_namespace_input = function(ns, lazydata = NULL, lazydata_only = FALSE) {
            if (!lazydata_only) stop(failure)
            read_input(ns, lazydata = lazydata, lazydata_only = TRUE)
        },
        .package = "languageserver"
    )
    withr::local_options(languageserver.package_cache_dir = FALSE)
    withr::local_libpaths(.libPaths())
    snapshot <- member_prepare_package("utils", identity = list(package = "utils"))
    expect_true(snapshot$dataset_only)
    expect_true(snapshot$namespace_attempted)
    expect_identical(snapshot$preparation_error, failure)
    expect_identical(names(snapshot$roots$table$fields), "column")
    failure <- "invalid metadata"
    expect_error(member_prepare_package("utils", identity = list(package = "utils")), "invalid metadata")
    failure <- "Namespace exceeds registry budget"
    rm("table", envir = data)
    expect_error(member_prepare_package("utils", identity = list(package = "utils")), failure)
})

test_that("Startup data manifests include transitive slot dependencies reported by workers", {
    data <- list2env(list(table = list(column = 1L)), parent = emptyenv())
    dependency <- "slotfixture"
    complete <- TRUE
    available <- TRUE
    testthat::local_mocked_bindings(
        member_namespace_lazydata = function(ns) data,
        package_metadata_live_identity = function(package, ...) list(package = package),
        package_metadata_cache_read = function(...) {
            list(dataset_only = TRUE, identity = list(package = "utils"), s4_dependencies = dependency)
        },
        package_metadata_transitive_packages = function(packages, ...) {
            if (dependency %in% packages) list(complete = complete, packages = c(dependency, "parentfixture"))
            else list(complete = TRUE, packages = packages)
        },
        package_metadata_manifest = function(packages, ...) {
            if (!available && dependency %in% packages) return(NULL)
            stats::setNames(lapply(packages, function(p) list(package = p)), packages)
        },
        .package = "languageserver"
    )
    withr::local_libpaths(.libPaths())
    prepare <- function() member_prepare_startup_data("utils", cache_dir = FALSE, include_status = TRUE)
    result <- prepare()
    expect_true(result$complete)
    expect_true(all(c(dependency, "parentfixture") %in% names(result$identities)))
    expect_identical(result$snapshots$utils$identity$context$packages, unname(result$identities))
    complete <- FALSE
    expect_false(prepare()$complete)
    complete <- TRUE
    available <- FALSE
    result <- prepare()
    expect_false(result$complete)
    expect_null(result$identities)
    expect_identical(result$inspected, "utils")
})

test_that("Generator pruning follows nested method results, metadata and property links", {
    shape <- function(type) list(type = type)
    base_class <- function(name) list(kind = "base", name = name)
    index <- list(
        constructor_inputs = list(make = list(arg = list(slots = list(a = shape("Slot"))))),
        registration_rules = list(attach = list(owners = "Registered")),
        properties = list(Registered = list(next_type = "Transitive"), Exported = list(item = shape("Property"))),
        method_results = list(method = list(possible_returns = list(shape("Returned")))),
        generic_returns = list(generic = list(class = list(binding_env = list(arg = shape("Binding"))))),
        package_roots = list(api = list(
            s7_generator = list(parent = base_class("Parent"), classes = list(base_class("Union"))),
            s7_property = list(class = base_class("PropertyClass")),
            r6_layers = list(list(public = list(a = shape("Public")), private = list(b = shape("Private"))))
        )),
        subset_registry = c("[[.Dispatched" = "opaque")
    )
    input <- list(lazydata_roots = list(dataset = shape("Dataset")))
    keep <- c("Slot", "Registered", "Transitive", "Property", "Returned", "Binding", "Parent", "Union",
        "PropertyClass", "Public", "Private", "Dataset", "Dispatched")
    expect_identical(member_prunable_r6_generators(index, input, c(keep, "Unused")), "Unused")
    expect_identical(member_prunable_r6_generators(index, input, character()), character())
})

test_that("S7 snapshots retain stored properties without invoking computed getters", {
    skip_if_not_installed("S7")
    cls <- S7::new_class("SnapshotPath", properties = list(label = S7::class_character,
            computed = S7::new_property(S7::class_character, getter = function(self) stop("getter called"))))
    x <- cls(label = "safe")
    snapshot <- member_snapshot(x)
    expect_identical(snapshot$kind, "s7_instance")
    expect_identical(names(snapshot$slots), "label")
    expect_identical(member_snapshot_shape(snapshot)$slots$label$literal, "safe")
    expect_identical(member_snapshot(list(x), max_depth = 0L)$elements[[1L]]$reason, "snapshot_depth")
})

test_that("S4 snapshots reserve space for all slots before exposing any of them", {
    scope <- new.env(parent = globalenv())
    constructor <- methods::setClass("LSMemberPathSlot", slots = c(value = "integer"), where = scope)
    withr::defer(methods::removeClass("LSMemberPathSlot", where = scope))
    object <- constructor(value = 42L)
    expect_identical(member_snapshot(object, max_bindings = 0L, lookup_scope = scope)$reason, "snapshot_budget")
    enough <- member_snapshot(object, max_bindings = 1L, lookup_scope = scope)
    expect_identical(enough$slots$value$value, 42L)
})

test_that("Runtime subset metadata distinguishes opaque registrations from local definitions", {
    ns <- new.env(parent = emptyenv())
    table <- new.env(parent = emptyenv())
    ns[["[.registered"]] <- function(x, ...) x
    table[["[.registered"]] <- ns[["[.registered"]]
    table[["[[.anonymous"]] <- eval(quote(function(x, ...) x), new.env(parent = baseenv()))
    read_binding <- member_binding
    testthat::local_mocked_bindings(member_binding = function(env, name, ...) {
        if (identical(env, asNamespace("base")) && name == ".__S3MethodsTable__.") return(list("value", table))
        read_binding(env, name, ...)
    }, .package = "languageserver")
    snapshot <- member_subset_registry(ns, "fixture")
    expect_true(snapshot$complete)
    expect_identical(unname(snapshot$methods["[[.anonymous"]), "opaque")
    expect_match(unname(snapshot$methods["[.registered"]), "^namespace:fixture:")
    table <- NULL
    expect_false(member_subset_registry(ns, "fixture")$complete)
    expect_null(member_snapshot_dollar_context(ns)$registered)
    expect_false(member_snapshot(list(field = 1L), lookup_scope = ns)$default_dollar)
})
