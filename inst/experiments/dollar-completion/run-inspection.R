# Rscript inst/experiments/dollar-completion/run-inspection.R
# Executes only this trusted fixture and the inspector, never fixture methods,
# active getters, lazy expressions, or user document expressions.
source("inst/experiments/dollar-completion/inference.R")
source("inst/experiments/dollar-completion/runtime-inspection.R")
checks <- 0L
check <- function(ok, description) {
    if (!isTRUE(ok)) stop(description, call. = FALSE)
    checks <<- checks + 1L
}
marker <- tempfile("inspection-must-not-execute-")
fixture <- new.env(parent = emptyenv())
registry <- new.env(parent = emptyenv())
class(registry) <- "fixture_registry"
fixture$registry <- registry
registry$factory <- function(x = {
    writeLines("default executed", marker)
    stop("default forced")
}) list(filter = function(p) list(collect = function() x), value = x)
registry$flag <- "current"
makeActiveBinding("active", function() {
    writeLines("getter executed", marker)
    stop("getter called")
}, registry)
delayedAssign("deferred", {
    writeLines("promise executed", marker)
    stop("promise forced")
}, assign.env = registry)
registry$cycle <- fixture
makeActiveBinding("$", function() stop("ordinary get must bypass dollar dispatch"), fixture)

snapshot <- inspection_snapshot(fixture)
fields <- snapshot$fields$registry$fields
check(identical(snapshot$fields$registry$classes, "fixture_registry"), "class metadata")
check(all(c("factory", "flag", "active", "deferred", "cycle") %in% names(fields)), "binding names")
check(identical(fields$factory$kind, "function"), "ordinary function syntax")
check(static_head(fields$factory$syntax, "function"), "function AST")
check(length(fields$factory$syntax[[2L]]) == 1L, "formals available without forcing defaults")
check(identical(fields$active$kind, "active"), "active binding descriptor")
check(static_head(fields$active$syntax, "function"), "active getter body available without calling it")
check(identical(fields$deferred$kind, "deferred"), "promise skipped")
check(is.null(fields$deferred$syntax), "no promise materialization")
check(identical(fields$flag$value, "current"), "ordinary literal metadata")
check(identical(fields$cycle$kind, "reference"), "cycles bounded")
check(!file.exists(marker), "no getters, promises, methods, or defaults executed")
check(isTRUE(rlang::env_binding_are_lazy(registry, "deferred")[[1L]]), "promise remains unforced")

# Pass extracted syntax into the same generic shape engine used for source.
index <- static_generic_index("")
index$definitions$factory <- fields$factory$syntax
result <- static_complete("factory(unresolved)$filter(predicate)$", index)
check(identical(result$labels, "collect"), "chained shape from runtime-extracted body")
check(!file.exists(marker), "inference does not invoke extracted functions")

initial_hash <- inspection_fingerprint(snapshot)
check(identical(initial_hash, inspection_fingerprint(inspection_snapshot(fixture))), "stable unchanged fingerprint")
registry$new_method <- function() list(added = 1)
added <- inspection_snapshot(fixture)
check("new_method" %in% names(added$fields$registry$fields), "new methods discovered without name table")
check(!identical(initial_hash, inspection_fingerprint(added)), "addition invalidates metadata")
registry$factory <- function(x) list(changed = function() list(finish = x))
changed <- inspection_snapshot(fixture)
check(!identical(inspection_fingerprint(added), inspection_fingerprint(changed)), "body replacement invalidates metadata")
index$definitions$factory <- changed$fields$registry$fields$factory$syntax
result <- static_complete("factory(unresolved)$changed()$", index)
check(identical(result$labels, "finish"), "return shape follows replaced function body")
rm("new_method", envir = registry)
removed <- inspection_snapshot(fixture)
check(!"new_method" %in% names(removed$fields$registry$fields), "removals discovered")
check(!identical(inspection_fingerprint(changed), inspection_fingerprint(removed)), "removal invalidates metadata")
check(!isTRUE(inspection_snapshot(fixture, max_bindings = 1L)$complete), "binding budget")
check(!isTRUE(inspection_snapshot(fixture, max_depth = 0L)$fields$registry$complete), "depth budget")
check(!file.exists(marker) && !"polars" %in% loadedNamespaces(), "canaries intact; Polars not loaded")
cat("Runtime binding-inspection checks:", checks, "passed\n")
cat("Active getter syntax read; lazy promise skipped; function bodies inferred without calls.\n")
cat("Additions, removals, and return-body replacements detected by metadata fingerprints.\n")
