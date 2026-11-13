# Acceptance tests for the R6 snapshot-size root cause ("Namespace exceeds
# snapshot budget", raised by member_index_freeze() above 32 MB of
# utils::object.size()).
#
# These are opaque-box checks: they only look at sizes, at success versus
# error, and at completion labels. They never inspect how a shape stores its
# `private` or `super` context, so the implementation is free to change that.
#
# Three kinds of expectation are used:
#   * size bounds, derived from the 32 MB budget (never from one measured
#     implementation);
#   * growth laws (ratios between sizes), which separate "linear in the number
#     of distinct methods" from "multiplied per method or per inheritance
#     level";
#   * completion labels, which pin the behaviour from before this change.
#     Where that behaviour is known to disagree with what R does at runtime the
#     test says so; pinning it is not an endorsement.

r6_budget_limit <- 32 * 1024^2
# Bound used for everything that should be comfortably small: half the budget.
# The original CI failure was processx at 31.86 MB here and over 32 MB on
# another R build, a margin of 0.44 %, so "just under 32 MB" is not acceptable.
r6_budget_headroom <- r6_budget_limit / 2

r6_budget_size <- function(x) as.numeric(utils::object.size(x))

r6_budget_code <- function(...) paste(c(...), collapse = "\n")

# Completion labels at the `@@` marker, or at the end of `code` without one.
# `snapshots` is a named list of frozen package snapshots.
r6_budget_labels <- function(code, snapshots = list()) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    marker <- grep("@@", content, fixed = TRUE)
    if (length(marker)) {
        row <- marker[[1L]]
        col <- regexpr("@@", content[[row]], fixed = TRUE)[[1L]] - 1L
        content[[row]] <- sub("@@", "", content[[row]], fixed = TRUE)
        point <- list(row = row - 1L, col = col)
    } else {
        point <- list(row = length(content) - 1L, col = nchar(content[[length(content)]]))
    }
    uri <- "file:///r6-snapshot-budget.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    data <- parse_document(uri, content)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    for (package in names(snapshots)) metadata$set(package, member_index_thaw(snapshots[[package]]))
    workspace <- list(member_metadata = metadata)
    items <- member_completion(uri, workspace, document, point, TRUE, 200L)
    vapply(items, `[[`, character(1L), "label")
}

# Snapshot of a synthetic namespace, the route the existing stored-instance
# tests use (see test-member-r6-review.R).
r6_budget_namespace_snapshot <- function(objects) {
    ns <- new.env(parent = baseenv())
    for (name in names(objects)) assign(name, objects[[name]], envir = ns)
    member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "r6budgetfixture", exports = names(objects)
    )))
}

# member_prepare_package() with the package metadata cache redirected to a
# temporary directory, so no test writes into the user's real cache. The other
# tests in this suite do not isolate the cache; this file does.
r6_budget_prepare <- function(package, library = NULL, .local_envir = parent.frame()) {
    cache <- withr::local_tempdir()
    withr::local_options(languageserver.package_cache_dir = cache)
    if (!is.null(library)) {
        withr::local_libpaths(c(library, .libPaths()))
        withr::defer(if (package %in% loadedNamespaces()) unloadNamespace(package), envir = .local_envir)
    }
    member_prepare_package(package)
}

# ---------------------------------------------------------------------------
# Synthetic classes
# ---------------------------------------------------------------------------

# Source of an inheritance chain Chain1 <- ... <- Chain<depth>. Every level
# defines the same `methods` public methods; from level 2 on each one either
# calls `super$<same method>()` or returns its own list.
r6_budget_chain_code <- function(depth, methods = 8L, use_super = TRUE) {
    vapply(seq_len(depth), function(level) {
        members <- vapply(seq_len(methods), function(i) {
            if (use_super && level > 1L) {
                sprintf("m%d = function() super$m%d()", i, i)
            } else {
                sprintf("m%d = function() list(level%d = %dL)", i, level, i)
            }
        }, character(1L))
        sprintf("Chain%d <- R6::R6Class(\"Chain%d\", %spublic = list(%s))", level, level,
            if (level > 1L) sprintf("inherit = Chain%d, ", level - 1L) else "",
            paste(members, collapse = ", "))
    }, character(1L))
}

# The deepest live generator of that chain.
r6_budget_chain <- function(depth, methods = 8L, use_super = TRUE) {
    env <- new.env(parent = baseenv())
    eval(parse(text = r6_budget_chain_code(depth, methods, use_super), keep.source = FALSE), env)
    get(paste0("Chain", depth), envir = env)
}

# Source of a class with `n_public` public methods and `n_private` private
# fields. Every method reads one private field.
r6_budget_wide_code <- function(n_public = 100L, n_private = 100L) {
    methods <- sprintf("m%d = function() list(value%d = private$f%d)", seq_len(n_public), seq_len(n_public),
        (seq_len(n_public) - 1L) %% n_private + 1L)
    fields <- sprintf("f%d = %dL", seq_len(n_private), seq_len(n_private))
    c("Wide <- R6::R6Class(\"Wide\",",
        sprintf("    public = list(%s),", paste(methods, collapse = ", ")),
        sprintf("    private = list(%s))", paste(fields, collapse = ", ")))
}

r6_budget_wide <- function(n_public = 100L, n_private = 100L) {
    env <- new.env(parent = baseenv())
    eval(parse(text = r6_budget_wide_code(n_public, n_private), keep.source = FALSE), env)
    env$Wide
}

# A live instance of a class with `n` public and `n` private methods of
# ordinary size: public method i calls private method i.
r6_budget_stored_instance <- function(n) {
    public <- sprintf(
        paste0("m%d = function(x = NULL, ...) {\n    value <- private$p%d(x)\n",
            "    if (is.null(value)) return(invisible(self))\n    value\n}"),
        seq_len(n), seq_len(n))
    private <- sprintf(
        paste0("p%d = function(x = NULL) {\n    if (!is.null(x)) private$state <- x\n",
            "    list(value%d = TRUE)\n}"),
        seq_len(n), seq_len(n))
    code <- sprintf("R6::R6Class(\"Stored\", public = list(%s), private = list(state = NULL, %s))",
        paste(public, collapse = ",\n"), paste(private, collapse = ",\n"))
    eval(parse(text = code, keep.source = FALSE), new.env(parent = baseenv()))$new()
}

# Three levels. Covers `super$m()` over two and three levels, `super$m()`
# reaching a method the parent only inherits, and a parent method that uses
# `private` reached through `super` while the child overrides that private
# method.
r6_budget_super_code <- r6_budget_code(
    "Grand <- R6::R6Class(\"Grand\",",
    "    public = list(",
    "        run = function() list(grand = TRUE),",
    "        keep = function() list(grand_keep = TRUE),",
    "        read = function() private$helper()",
    "    ),",
    "    private = list(helper = function() list(grand_helper = TRUE))",
    ")",
    "Parent <- R6::R6Class(\"Parent\", inherit = Grand,",
    "    public = list(",
    "        run = function() super$run(),",
    "        read = function() super$read()",
    "    )",
    ")",
    "Child <- R6::R6Class(\"Child\", inherit = Parent,",
    "    public = list(",
    "        run = function() super$run(),",
    "        keep_via_super = function() super$keep()",
    "    ),",
    "    private = list(helper = function() list(child_helper = TRUE))",
    ")"
)

# Two levels. Covers public -> private -> private, private -> public ->
# private, and `initialize` calling `super$initialize()`.
r6_budget_private_code <- r6_budget_code(
    "Base <- R6::R6Class(\"Base\",",
    "    public = list(",
    "        config = NULL,",
    "        initialize = function() {",
    "            self$config <- list(base_ready = TRUE)",
    "        },",
    "        via_private = function() private$first(),",
    "        chain = function() invisible(self)",
    "    ),",
    "    private = list(",
    "        first = function() private$second(),",
    "        second = function() list(done = TRUE)",
    "    )",
    ")",
    "Derived <- R6::R6Class(\"Derived\", inherit = Base,",
    "    public = list(",
    "        extra = NULL,",
    "        initialize = function() {",
    "            super$initialize()",
    "            self$extra <- list(child_ready = TRUE)",
    "        },",
    "        to_private = function() private$hop()",
    "    ),",
    "    private = list(hop = function() self$via_private())",
    ")"
)

# Fields whose value is a quoted function expression.
r6_budget_language_code <- r6_budget_code(
    "Q <- R6::R6Class(\"Q\", public = list(",
    "    out = NULL,",
    "    initialize = function() {",
    "        private$expr <- list(real = TRUE)",
    "        self$out <- private$expr",
    "    }",
    "), private = list(expr = quote(function() NULL)))",
    "QF <- R6::R6Class(\"QF\", public = list(expr = quote(function() list(fake = TRUE))))"
)

# A larger document-only family used for cursor positions inside methods.
# `child_probe` / `parent_probe` are the body of an extra `probe` method.
r6_budget_family_code <- function(child_probe = NULL, parent_probe = NULL) {
    probe <- function(body) {
        if (is.null(body)) return(NULL)
        c("        probe = function() {", paste0("            ", body), "        },")
    }
    r6_budget_code(
        "Grand <- R6::R6Class(\"Grand\",",
        "    public = list(",
        "        config = NULL,",
        "        initialize = function() {",
        "            self$config <- list(base_ready = TRUE)",
        "            private$state <- list(from_initialize = TRUE)",
        "        },",
        "        run = function() list(grand = TRUE),",
        "        keep = function() list(grand_keep = TRUE),",
        "        via_private = function() private$first(),",
        "        read = function() private$helper(),",
        "        state_of = function() private$state,",
        "        chain = function() invisible(self)",
        "    ),",
        "    private = list(",
        "        state = NULL,",
        "        payload = list(done = TRUE),",
        "        first = function() private$second(),",
        "        second = function() private$payload,",
        "        helper = function() list(grand_helper = TRUE)",
        "    ),",
        "    active = list(current = function() private$payload)",
        ")",
        "Parent <- R6::R6Class(\"Parent\", inherit = Grand,",
        "    public = list(",
        probe(parent_probe),
        "        run = function() super$run(),",
        "        read = function() super$read(),",
        "        parent_only = function() private$parent_secret()",
        "    ),",
        "    private = list(parent_secret = function() list(parent_secret = TRUE))",
        ")",
        "Child <- R6::R6Class(\"Child\", inherit = Parent,",
        "    public = list(",
        probe(child_probe),
        "        extra = NULL,",
        "        initialize = function() {",
        "            super$initialize()",
        "            self$extra <- list(child_ready = TRUE)",
        "        },",
        "        run = function() super$run(),",
        "        keep_via_super = function() super$keep(),",
        "        parent_via_super = function() super$parent_only(),",
        "        to_private = function() private$hop()",
        "    ),",
        "    private = list(",
        "        hop = function() self$via_private(),",
        "        helper = function() list(child_helper = TRUE)",
        "    )",
        ")"
    )
}

# Classes shared by the non-portable bare-reference examples below.
r6_budget_inner_other <- c(
    "Inner <- R6::R6Class(\"Inner\", public = list(a = NULL, b = list(old_b = TRUE)))",
    "Other <- R6::R6Class(\"Other\", public = list(a = NULL, b = list(new_b = TRUE)))"
)

# ---------------------------------------------------------------------------
# Installed fixture packages
# ---------------------------------------------------------------------------

# A generator only becomes a runtime class shape inside
# member_prepare_package(), so runtime classes are tested through real
# packages installed into a temporary library (the pattern of
# test-member-lazydata.R and test-package-metadata-cache.R). All fixture
# packages are installed by one R CMD INSTALL, once per test process.
r6_budget_library <- local({
    cached <- NULL
    function() {
        if (!is.null(cached)) return(cached)
        sources <- list(
            r6budgetchain = r6_budget_chain_code(4L, 8L, TRUE),
            r6budgetwide = r6_budget_wide_code(100L, 100L),
            r6budgetsuper = r6_budget_super_code,
            r6budgetprivate = r6_budget_private_code,
            r6budgetlanguage = r6_budget_language_code
        )
        root <- tempfile("r6-budget-fixture-")
        library <- file.path(root, "library")
        dir.create(library, recursive = TRUE)
        withr::defer(unlink(root, recursive = TRUE), testthat::teardown_env())
        paths <- vapply(names(sources), function(package) {
            path <- file.path(root, package)
            dir.create(file.path(path, "R"), recursive = TRUE)
            writeLines(c(
                paste("Package:", package), "Version: 0.0.1", "Title: R6 Snapshot Budget Fixture",
                "Description: Synthetic R6 classes for snapshot budget tests.", "License: MIT",
                "Author: Test Fixture", "Maintainer: Test Fixture <fixture@example.org>", "Imports: R6"
            ), file.path(path, "DESCRIPTION"))
            writeLines("exportPattern(\"^[^.]\")", file.path(path, "NAMESPACE"))
            writeLines(sources[[package]], file.path(path, "R", "classes.R"))
            path
        }, character(1L))
        result <- callr::rcmd("INSTALL",
            cmdargs = c("--no-docs", "--no-multiarch", "--no-test-load", paste0("--library=", library), paths),
            stdout = "|", stderr = "|", system_profile = FALSE, user_profile = FALSE)
        if (!identical(result$status, 0L)) {
            stop("R6 snapshot budget fixture installation failed:\n", result$stderr)
        }
        cached <<- library
        library
    }
})

# Public method names R6 itself reports for a generator: the oracle for "the
# snapshot still describes this class".
r6_budget_public_methods <- function(package, class) {
    generator <- get(class, envir = asNamespace(package))
    grep("^[A-Za-z]", names(generator$public_methods), value = TRUE)
}

expect_r6_package_within <- function(package, class, limit) {
    snapshot <- r6_budget_prepare(package)
    expect_lt(r6_budget_size(snapshot), limit)
    methods <- r6_budget_public_methods(package, class)
    expect_gt(length(methods), 0L)
    labels <- r6_budget_labels(sprintf("obj <- %s::%s$new()\nobj$", package, class),
        stats::setNames(list(snapshot), package))
    expect_identical(setdiff(methods, labels), character())
    invisible(snapshot)
}

# ---------------------------------------------------------------------------
# T1. Real packages prepare within the budget
# ---------------------------------------------------------------------------

test_that("processx prepares with at least half of the snapshot budget to spare", {
    # Bound: half the budget (see r6_budget_headroom). Before this change processx
    # 3.9.0 measured 31.86 MB, which was the reported failure at its threshold.
    snapshot <- expect_r6_package_within("processx", "process", r6_budget_headroom)
    labels <- r6_budget_labels("proc <- processx::process$new(\"must-not-launch\")\nproc$",
        list(processx = snapshot))
    expect_true(all(c("is_alive", "wait", "get_exit_status") %in% labels))
})

test_that("callr prepares with at least half of the snapshot budget to spare", {
    # Bound: half the budget. Before this change callr 3.7.6 measured 16.02 MB.
    expect_r6_package_within("callr", "r_session", r6_budget_headroom)
})

test_that("desc prepares with at least half of the snapshot budget to spare", {
    skip_if_not_installed("desc")
    # Bound: half the budget. Already true before this change (6.10 MB); kept so a
    # change that makes it worse is noticed.
    expect_r6_package_within("desc", "description", r6_budget_headroom)
})

test_that("httr prepares with at least half of the snapshot budget to spare", {
    skip_if_not_installed("httr")
    # Bound: half the budget. Before this change httr 1.4.7 failed with "Namespace
    # exceeds snapshot budget" (71 MB before the check): one level of
    # inheritance over 12 methods.
    expect_r6_package_within("httr", "Token2.0", r6_budget_headroom)
})

test_that("testthat reporter classes have small runtime shapes", {
    skip_if_not_installed("testthat")
    # Whole-package preparation of testthat is deliberately not asserted: with
    # the R6 part fixed, testthat still exceeds the 32 MB check because of its
    # non-R6 tables and of roots being stored three times, the same limitation
    # that fails base, methods, Matrix and mgcv. That is outside
    # the R6 size fix. What the fix owns is the size of the R6 shapes: testthat
    # has 29 reporter generators with inheritance depth >= 2, and before this change
    # single shapes reached hundreds of MB.
    ns <- asNamespace("testthat")
    generators <- Filter(function(name) inherits(get(name, envir = ns), "R6ClassGenerator"), ls(ns))
    expect_gt(length(generators), 0L)
    index <- member_generic_index("")
    sizes <- vapply(generators, function(name) {
        r6_budget_size(member_r6_runtime_shape(get(name, envir = ns), index))
    }, numeric(1L))
    # Bound per class: an eighth of the budget, because every root may be held
    # three times in a snapshot and a package has more than one class.
    largest <- names(sizes)[[which.max(sizes)]]
    expect_lt(max(sizes), r6_budget_limit / 8,
        label = sprintf("largest testthat R6 shape (%s, %.1f MB)", largest, max(sizes) / 1024^2))
})

# ---------------------------------------------------------------------------
# T2. Growth law along an inheritance chain
# ---------------------------------------------------------------------------

# With M methods per level, storing ancestor contexts by value multiplies the
# size by about M + 1 per level (previously 9 for M = 8). A design that is
# linear in the number of distinct methods gives size(n + 1) / size(n) of
# about (n + 1) / n: 2 at the first step, 1.2 at the last. The thresholds sit
# between the two laws, not at a measured value.
expect_linear_chain_growth <- function(sizes) {
    depths <- seq_along(sizes)
    ratios <- sizes[-1L] / sizes[-length(sizes)]
    info <- paste0("sizes (KB): ", paste(round(sizes / 1024), collapse = ", "),
        "; ratios: ", paste(round(ratios, 2), collapse = ", "))
    # Every step: far below the per-level method count.
    for (n in seq_along(ratios)) expect_lt(ratios[[n]], 4, label = sprintf("size ratio depth %d -> %d", n, n + 1L))
    # Last two steps: a linear law gives at most 5/4; a geometric one never drops.
    for (n in tail(seq_along(ratios), 2L)) {
        expect_lt(ratios[[n]], 1.5, label = sprintf("size ratio depth %d -> %d", n, n + 1L))
    }
    # The deepest class: an eighth of the budget, because every root may be held
    # three times in a snapshot and a package has more than one class.
    expect_lt(sizes[[length(sizes)]], r6_budget_limit / 8, label = paste0("deepest size; ", info))
}

test_that("Runtime R6 chain size is linear in depth when overrides call super", {
    index <- member_generic_index("")
    sizes <- vapply(1:6, function(depth) {
        r6_budget_size(member_r6_runtime_shape(r6_budget_chain(depth, 8L, TRUE), index))
    }, numeric(1L))
    expect_linear_chain_growth(sizes)
})

test_that("Runtime R6 chain size is linear in depth when overrides do not mention super", {
    index <- member_generic_index("")
    sizes <- vapply(1:6, function(depth) {
        r6_budget_size(member_r6_runtime_shape(r6_budget_chain(depth, 8L, FALSE), index))
    }, numeric(1L))
    expect_linear_chain_growth(sizes)
})

test_that("Runtime R6 chain size is linear in the number of methods per level", {
    index <- member_generic_index("")
    sizes <- vapply(c(4L, 8L, 16L), function(methods) {
        r6_budget_size(member_r6_runtime_shape(r6_budget_chain(3L, methods, TRUE), index))
    }, numeric(1L))
    # Doubling the methods of all three levels at most doubles a linear size.
    # By-value contexts give about 2 * ((2M + 1) / (M + 1))^2, i.e. more than 6.
    expect_lt(sizes[[2L]] / sizes[[1L]], 2.5)
    expect_lt(sizes[[3L]] / sizes[[2L]], 2.5)
})

test_that("Document R6 chain size is linear in depth", {
    # The same shape builder runs for classes written in a document.
    for (use_super in c(TRUE, FALSE)) {
        sizes <- vapply(1:6, function(depth) {
            source <- paste(r6_budget_chain_code(depth, 8L, use_super), collapse = "\n")
            block <- str2lang(paste0("{\n", source, "\nChain", depth, "\n}"))
            r6_budget_size(member_infer(block, member_generic_index("")))
        }, numeric(1L))
        expect_linear_chain_growth(sizes)
    }
    code <- r6_budget_code(r6_budget_chain_code(5L, 8L, TRUE), "Chain5$new()$")
    expect_identical(r6_budget_labels(code), paste0("m", 1:8))
    expect_identical(r6_budget_labels(paste0(code, "m3()$")), "level1")
})

test_that("An installed package with a four-level super chain prepares within the budget", {
    # The previous code failed here with "Namespace exceeds snapshot budget".
    snapshot <- r6_budget_prepare("r6budgetchain", r6_budget_library())
    expect_lt(r6_budget_size(snapshot), r6_budget_headroom)
    snapshots <- list(r6budgetchain = snapshot)
    expect_identical(r6_budget_labels("r6budgetchain::Chain4$new()$", snapshots), c("clone", paste0("m", 1:8)))
    expect_identical(r6_budget_labels("r6budgetchain::Chain4$new()$m3()$", snapshots), "level1")
    expect_identical(r6_budget_labels("r6budgetchain::Chain2$new()$m8()$", snapshots), "level1")
})

# ---------------------------------------------------------------------------
# T3. Wide classes and stored instances
# ---------------------------------------------------------------------------

test_that("An installed class with 100 methods and 100 private fields prepares within the budget", {
    # The previous code failed here with "Namespace exceeds snapshot budget".
    snapshot <- r6_budget_prepare("r6budgetwide", r6_budget_library())
    expect_lt(r6_budget_size(snapshot), r6_budget_headroom)
    snapshots <- list(r6budgetwide = snapshot)
    expect_identical(sort(r6_budget_labels("r6budgetwide::Wide$new()$", snapshots)),
        sort(c("clone", paste0("m", 1:100))))
    expect_identical(r6_budget_labels("r6budgetwide::Wide$new()$m7()$", snapshots), "value7")
    expect_identical(r6_budget_labels("r6budgetwide::Wide$new()$m100()$", snapshots), "value100")
})

test_that("Runtime R6 class size does not multiply methods by private fields", {
    index <- member_generic_index("")
    small <- r6_budget_size(member_r6_runtime_shape(r6_budget_wide(50L, 50L), index))
    large <- r6_budget_size(member_r6_runtime_shape(r6_budget_wide(100L, 100L), index))
    # Doubling both counts doubles a linear size and quadruples a
    # methods-times-private-table size (previously 3.8).
    expect_lt(large / small, 3)
    # One class must fit three times (package_roots, roots, namespace_roots)
    # with room for the rest of its package: an eighth of the budget.
    expect_lt(large, r6_budget_limit / 8)
})

test_that("A namespace holding an R6 instance with 40 + 40 methods fits the budget", {
    counts <- c(10L, 20L, 40L, 80L)
    snapshots <- lapply(counts, function(n) {
        tryCatch(r6_budget_namespace_snapshot(list(object = r6_budget_stored_instance(n))),
            error = function(e) e)
    })
    names(snapshots) <- counts
    for (n in counts) {
        snapshot <- snapshots[[as.character(n)]]
        expect_false(inherits(snapshot, "error"),
            label = sprintf("whether the snapshot of a stored instance with %d + %d methods raised an error (%s)", n, n,
                if (inherits(snapshot, "error")) conditionMessage(snapshot) else "none"))
        if (inherits(snapshot, "error")) next
        stored <- list(r6budgetfixture = snapshot)
        expect_identical(sort(r6_budget_labels("r6budgetfixture::object$", stored)),
            sort(c("clone", paste0("m", seq_len(n)))))
        expect_identical(r6_budget_labels("r6budgetfixture::object$m7()$", stored), "value7")
    }
    # The failures above are the result; sizes need every snapshot.
    if (any(vapply(snapshots, inherits, logical(1L), "error"))) return(invisible())
    sizes <- vapply(snapshots, r6_budget_size, numeric(1L))
    # The previous code failed at 40 + 40 with "Namespace exceeds snapshot budget".
    expect_lt(sizes[["40"]], r6_budget_headroom)
    # Doubling the method count at most doubles a linear size; expanding the
    # contexts into every bound method gives about 3.3 (previously, 10 -> 20).
    ratios <- sizes[-1L] / sizes[-length(sizes)]
    for (i in seq_along(ratios)) {
        expect_lt(ratios[[i]], 2.5, label = sprintf("size ratio %d -> %d methods", counts[[i]], counts[[i + 1L]]))
    }
})

# ---------------------------------------------------------------------------
# Behaviour that must survive the removal of per-method context copies
# ---------------------------------------------------------------------------

# Queries answered identically for the document-defined and the installed
# class. Names are the code after the class prefix.
r6_budget_super_expected <- list(
    "Parent$new()$run()$" = "grand",
    "Child$new()$run()$" = "grand",
    "Child$new()$keep()$" = "grand_keep",
    "Child$new()$keep_via_super()$" = "grand_keep",
    "Grand$new()$read()$" = "grand_helper",
    "Parent$new()$read()$" = "grand_helper",
    "Child$new()$read()$" = "child_helper"
)
r6_budget_super_extracted <- list(
    "obj <- %sChild$new()\nfn <- obj$run\nfn()$" = "grand",
    "obj <- %sChild$new()\nfn <- obj$read\nfn()$" = "child_helper",
    "obj <- %sChild$new()\nfn <- obj$keep_via_super\nfn()$" = "grand_keep",
    "fn <- %sChild$new()$keep\nfn()$" = "grand_keep"
)

test_that("Document R6 super chains resolve over two and three levels", {
    for (query in names(r6_budget_super_expected)) {
        expect_identical(r6_budget_labels(r6_budget_code(r6_budget_super_code, query)),
            r6_budget_super_expected[[query]], info = query)
    }
    for (query in names(r6_budget_super_extracted)) {
        expect_identical(r6_budget_labels(r6_budget_code(r6_budget_super_code, sprintf(query, ""))),
            r6_budget_super_extracted[[query]], info = query)
    }
    expect_identical(r6_budget_labels(r6_budget_code(r6_budget_super_code, "Child$new()$")),
        c("keep", "keep_via_super", "read", "run"))
})

test_that("Installed R6 super chains resolve over two and three levels", {
    # Before this change this nine-method package took 29.8 MB of the budget.
    snapshot <- r6_budget_prepare("r6budgetsuper", r6_budget_library())
    expect_lt(r6_budget_size(snapshot), r6_budget_limit)
    snapshots <- list(r6budgetsuper = snapshot)
    for (query in names(r6_budget_super_expected)) {
        expect_identical(r6_budget_labels(paste0("r6budgetsuper::", query), snapshots),
            r6_budget_super_expected[[query]], info = query)
    }
    for (query in names(r6_budget_super_extracted)) {
        expect_identical(r6_budget_labels(sprintf(query, "r6budgetsuper::"), snapshots),
            r6_budget_super_extracted[[query]], info = query)
    }
    expect_identical(r6_budget_labels("r6budgetsuper::Child$new()$", snapshots),
        c("clone", "keep", "keep_via_super", "read", "run"))
    expect_identical(r6_budget_labels("r6budgetsuper::Parent$new()$", snapshots),
        c("clone", "keep", "read", "run"))
})

test_that("A document class inheriting an installed R6 class keeps the whole super chain", {
    snapshots <- list(r6budgetsuper = r6_budget_prepare("r6budgetsuper", r6_budget_library()))
    code <- r6_budget_code(
        "Local <- R6::R6Class(\"Local\", inherit = r6budgetsuper::Child, public = list(",
        "    run = function() super$run(),",
        "    mine = function() private$helper()",
        "))"
    )
    expected <- list(
        "Local$new()$run()$" = "grand",
        "Local$new()$mine()$" = "child_helper",
        "Local$new()$keep_via_super()$" = "grand_keep",
        "Local$new()$read()$" = "child_helper",
        "Local$new()$keep()$" = "grand_keep",
        "Local$new()$" = c("clone", "keep", "keep_via_super", "mine", "read", "run")
    )
    for (query in names(expected)) {
        expect_identical(r6_budget_labels(r6_budget_code(code, query), snapshots), expected[[query]], info = query)
    }
})

r6_budget_private_expected <- list(
    "Base$new()$via_private()$" = "done",
    "Derived$new()$via_private()$" = "done",
    "Derived$new()$to_private()$" = "done",
    "Derived$new()$chain()$to_private()$" = "done",
    "Base$new()$config$" = "base_ready",
    "Derived$new()$extra$" = "child_ready",
    # Pre-existing base behaviour, unchanged by the size fix: at runtime
    # `super$initialize()` sets `config` (the answer would be `base_ready`),
    # but completion does not follow the write made by the parent initializer.
    "Derived$new()$config$" = character()
)
r6_budget_private_extracted <- list(
    "obj <- %sDerived$new()\nfn <- obj$via_private\nfn()$" = "done",
    "obj <- %sDerived$new()\nfn <- obj$to_private\nfn()$" = "done",
    "f <- function() {\nobj <- %sDerived$new()\nfn <- obj$to_private\nfn()$" = "done"
)

test_that("Document R6 methods reach private methods and parent initializers", {
    for (query in names(r6_budget_private_expected)) {
        expect_identical(r6_budget_labels(r6_budget_code(r6_budget_private_code, query)),
            r6_budget_private_expected[[query]], info = query)
    }
    for (query in names(r6_budget_private_extracted)) {
        expect_identical(r6_budget_labels(r6_budget_code(r6_budget_private_code, sprintf(query, ""))),
            r6_budget_private_extracted[[query]], info = query)
    }
    expect_identical(r6_budget_labels(r6_budget_code(r6_budget_private_code, "Derived$new()$")),
        c("chain", "config", "extra", "initialize", "to_private", "via_private"))
})

test_that("Installed R6 methods reach private methods and parent initializers", {
    snapshot <- r6_budget_prepare("r6budgetprivate", r6_budget_library())
    expect_lt(r6_budget_size(snapshot), r6_budget_headroom)
    snapshots <- list(r6budgetprivate = snapshot)
    for (query in names(r6_budget_private_expected)) {
        expect_identical(r6_budget_labels(paste0("r6budgetprivate::", query), snapshots),
            r6_budget_private_expected[[query]], info = query)
    }
    for (query in names(r6_budget_private_extracted)) {
        expect_identical(r6_budget_labels(sprintf(query, "r6budgetprivate::"), snapshots),
            r6_budget_private_extracted[[query]], info = query)
    }
    expect_identical(r6_budget_labels("r6budgetprivate::Derived$new()$", snapshots),
        c("chain", "clone", "config", "extra", "initialize", "to_private", "via_private"))
})

test_that("Stored R6 instances keep super, private and initializer results", {
    env <- new.env(parent = baseenv())
    eval(parse(text = c(r6_budget_super_code, r6_budget_private_code), keep.source = FALSE), env)
    snapshot <- r6_budget_namespace_snapshot(list(child = env$Child$new(), derived = env$Derived$new()))
    expect_lt(r6_budget_size(snapshot), r6_budget_headroom)
    snapshots <- list(r6budgetfixture = snapshot)
    expected <- list(
        "r6budgetfixture::child$run()$" = "grand",
        "r6budgetfixture::child$keep()$" = "grand_keep",
        "r6budgetfixture::child$keep_via_super()$" = "grand_keep",
        "r6budgetfixture::child$read()$" = "child_helper",
        "r6budgetfixture::child$" = c("clone", "keep", "keep_via_super", "read", "run"),
        "r6budgetfixture::derived$via_private()$" = "done",
        "r6budgetfixture::derived$to_private()$" = "done",
        "r6budgetfixture::derived$chain()$to_private()$" = "done",
        # A stored instance has really run its initializers.
        "r6budgetfixture::derived$config$" = "base_ready",
        "r6budgetfixture::derived$extra$" = "child_ready",
        "r6budgetfixture::derived$" = c("chain", "clone", "config", "extra", "initialize", "to_private", "via_private"),
        "fn <- r6budgetfixture::child$run\nfn()$" = "grand",
        "fn <- r6budgetfixture::child$read\nfn()$" = "child_helper",
        "fn <- r6budgetfixture::derived$to_private\nfn()$" = "done"
    )
    for (query in names(expected)) {
        expect_identical(r6_budget_labels(query, snapshots), expected[[query]], info = query)
    }
})

test_that("Document R6 family resolves through self, private and super from outside", {
    family <- r6_budget_family_code()
    expected <- list(
        "Grand$new()$via_private()$" = "done",
        "Child$new()$via_private()$" = "done",
        "Child$new()$to_private()$" = "done",
        "Parent$new()$run()$" = "grand",
        "Child$new()$run()$" = "grand",
        "Child$new()$keep()$" = "grand_keep",
        "Child$new()$keep_via_super()$" = "grand_keep",
        "Grand$new()$read()$" = "grand_helper",
        "Parent$new()$read()$" = "grand_helper",
        "Child$new()$read()$" = "child_helper",
        "Child$new()$parent_via_super()$" = "parent_secret",
        "Child$new()$parent_only()$" = "parent_secret",
        "Grand$new()$config$" = "base_ready",
        "Child$new()$extra$" = "child_ready",
        "Child$new()$chain()$run()$" = "grand",
        "Child$new()$chain()$via_private()$" = "done",
        "Child$" = "new",
        "Child$new()$" = c("chain", "config", "current", "extra", "initialize", "keep", "keep_via_super",
            "parent_only", "parent_via_super", "read", "run", "state_of", "to_private", "via_private"),
        "obj <- Child$new()\nfn <- obj$run\nfn()$" = "grand",
        "obj <- Child$new()\nfn <- obj$via_private\nfn()$" = "done",
        "obj <- Child$new()\nfn <- obj$read\nfn()$" = "child_helper",
        "obj <- Child$new()\nfn <- obj$keep\nfn()$" = "grand_keep",
        "obj <- Child$new()\nfn <- obj$keep_via_super\nfn()$" = "grand_keep",
        "f <- function() {\nobj <- Child$new()\nfn <- obj$read\nfn()$" = "child_helper",
        "f <- function() {\nobj <- Child$new()\nfn <- obj$keep_via_super\nfn()$" = "grand_keep"
    )
    for (query in names(expected)) {
        expect_identical(r6_budget_labels(r6_budget_code(family, query)), expected[[query]], info = query)
    }
    # Pre-existing base behaviour, unchanged by the size fix: completion gives no
    # suggestion for these three although the runtime values are known
    # (`base_ready`, `from_initialize`, `done`).
    for (query in c("Child$new()$config$", "Grand$new()$state_of()$", "Child$new()$current$")) {
        expect_length(r6_budget_labels(r6_budget_code(family, query)), 0L)
    }
})

test_that("Document R6 family resolves self, private and super at a cursor inside a method", {
    child <- list(
        "private$@@" = c("first", "helper", "hop", "parent_secret", "payload", "second", "state"),
        "super$@@" = c("chain", "current", "first", "helper", "initialize", "keep", "parent_only",
            "parent_secret", "read", "run", "second", "state_of", "via_private"),
        "self$@@" = c("chain", "config", "current", "extra", "initialize", "keep", "keep_via_super",
            "parent_only", "parent_via_super", "probe", "read", "run", "state_of", "to_private", "via_private"),
        "super$run()$@@" = "grand",
        "super$keep()$@@" = "grand_keep",
        "super$read()$@@" = "child_helper",
        "super$parent_only()$@@" = "parent_secret",
        "self$read()$@@" = "child_helper",
        "self$via_private()$@@" = "done",
        "private$hop()$@@" = "done",
        "private$first()$@@" = "done",
        "private$helper()$@@" = "child_helper",
        "private$payload$@@" = "done"
    )
    for (body in names(child)) {
        expect_identical(r6_budget_labels(r6_budget_family_code(child_probe = body)), child[[body]], info = body)
    }
    parent <- list(
        "private$@@" = c("first", "helper", "parent_secret", "payload", "second", "state"),
        "super$@@" = c("chain", "current", "first", "helper", "initialize", "keep", "read", "run",
            "second", "state_of", "via_private"),
        "self$@@" = c("chain", "config", "current", "initialize", "keep", "parent_only", "probe", "read",
            "run", "state_of", "via_private"),
        "super$run()$@@" = "grand",
        "super$read()$@@" = "grand_helper",
        "private$parent_secret()$@@" = "parent_secret",
        "private$first()$@@" = "done"
    )
    for (body in names(parent)) {
        expect_identical(r6_budget_labels(r6_budget_family_code(parent_probe = body)), parent[[body]], info = body)
    }
})

# ---------------------------------------------------------------------------
# T5. Reviewer examples from PR 853 keep their previous behaviour
# ---------------------------------------------------------------------------

# These are the examples raised in the PR 853 review. The size fix must not
# change what completion answers for any of them, and none may raise an error.
# None of these depends on polars.

test_that("non-portable bare reads after a field write stay without suggestion", {
    # Pre-existing base behaviour, unchanged by the size fix: the runtime value
    # is `fresh`; completion conservatively suggests nothing (and never `stale`).
    cases <- list(
        self_write = c(
            "C <- R6::R6Class(\"NP\", portable = FALSE, public = list(",
            "  x = list(stale = TRUE),",
            "  read = function() { self$x <- list(fresh = TRUE); x }",
            "))"),
        private_write = c(
            "C <- R6::R6Class(\"NP\", portable = FALSE, public = list(",
            "  read = function() { private$x <- list(fresh = TRUE); x }",
            "), private = list(x = list(stale = TRUE)))"),
        sibling_helper = c(
            "C <- R6::R6Class(\"NP\", portable = FALSE, public = list(",
            "  x = list(stale = TRUE),",
            "  read = function() { self$x <- list(fresh = TRUE); helper() },",
            "  helper = function() x",
            "))"),
        private_helper = c(
            "C <- R6::R6Class(\"NP\", portable = FALSE, public = list(",
            "  x = list(stale = TRUE),",
            "  read = function() { self$x <- list(fresh = TRUE); helper() }",
            "), private = list(helper = function() x))")
    )
    for (name in names(cases)) {
        expect_length(r6_budget_labels(r6_budget_code(cases[[name]], "C$new()$read()$")), 0L)
    }
})

test_that("language-valued R6 fields stay data in installed and document classes", {
    snapshots <- list(r6budgetlanguage = r6_budget_prepare("r6budgetlanguage", r6_budget_library()))
    expect_identical(r6_budget_labels("r6budgetlanguage::Q$new()$out$", snapshots), "real")
    # The call fails at runtime with "attempt to apply non-function".
    expect_length(r6_budget_labels("r6budgetlanguage::QF$new()$expr()$", snapshots), 0L)
    expect_identical(r6_budget_labels("r6budgetlanguage::Q$new()$", snapshots), c("clone", "initialize", "out"))
    expect_identical(r6_budget_labels("r6budgetlanguage::QF$new()$", snapshots), c("clone", "expr"))
    expect_identical(r6_budget_labels(r6_budget_code(r6_budget_language_code, "Q$new()$out$")), "real")
    expect_length(r6_budget_labels(r6_budget_code(r6_budget_language_code, "QF$new()$expr()$")), 0L)
})

test_that("a local assigned in a compound statement is not replaced by the field", {
    # Pre-existing base behaviour, unchanged by the size fix: the runtime value
    # is `local`; completion suggests nothing (and never `field`).
    expect_length(r6_budget_labels(r6_budget_code(
        "C <- R6::R6Class(\"C\", portable = FALSE, public = list(",
        "  x = list(field = TRUE), y = NULL,",
        "  probe = function() {",
        "    { x <- list(local = TRUE); self$y <- list(updated = TRUE) }",
        "    x$@@",
        "  }",
        "))"
    )), 0L)
})

test_that("a method extracted before a receiver-mutating branch stays callable", {
    for (portable in c("", "portable = FALSE, ")) {
        expect_identical(r6_budget_labels(r6_budget_code(
            paste0("C <- R6::R6Class(\"C\", ", portable, "public = list("),
            "  x = 0L,",
            "  helper = function() list(ok = TRUE),",
            "  read = function(flag) {",
            "    fn <- self$helper",
            "    if (flag) self$x <- 1L else self$x <- 2L",
            "    fn()",
            "  }",
            "))",
            "C$new()$read(unknown_flag)$"
        )), "ok", info = portable)
    }
})

test_that("new fields on unlocked receivers are kept and locked receivers reject them", {
    probe <- function(options) {
        r6_budget_labels(r6_budget_code(
            paste0("C <- R6::R6Class(\"C\", ", options, "public = list("),
            "  probe = function() {",
            "    self$added <- list(x = TRUE)",
            "    self$@@",
            "  }",
            "))"
        ))
    }
    expect_identical(probe("lock_objects = FALSE, "), c("added", "probe"))
    expect_identical(probe("lock_objects = FALSE, portable = FALSE, "), c("added", "probe"))
    expect_identical(probe(""), "probe")
})

test_that("non-portable bare references to a replaced object keep their previous behaviour", {
    outer <- function(...) {
        r6_budget_labels(r6_budget_code(
            r6_budget_inner_other,
            "Outer <- R6::R6Class(\"Outer\", portable = FALSE, public = list(",
            "  x = Inner$new(),",
            ...,
            "))"
        ))
    }
    # Object replaced in one branch of an if/else. Pre-existing base behaviour, unchanged by the size fix: the runtime
    # reads the replacement object (`new_b`); completion suggests `old_b`.
    expect_identical(outer(
        "  probe = function() {",
        "    { self$x <- Other$new(); x$a <- list(updated = TRUE) }",
        "    x$b$@@",
        "  }"
    ), "old_b")
    # Control for the if/else case, reversed order: `old_b` is also the runtime answer.
    expect_identical(outer(
        "  probe = function() {",
        "    { x$a <- list(updated = TRUE); self$x <- Other$new() }",
        "    x$b$@@",
        "  }"
    ), "old_b")
    # Replacement later in a loop, unknown condition. Pre-existing base behaviour, unchanged by the size
    # fix: the else path returns `new_b` at runtime; completion suggests `old_b`.
    expect_identical(outer(
        "  probe = function(flag) {",
        "    if (flag) {",
        "      x$a <- 1",
        "    } else {",
        "      self$x <- Other$new()",
        "      x$a <- 2",
        "    }",
        "    x$b$@@",
        "  }"
    ), "old_b")
    # Replacement later in a loop, literal FALSE. Pre-existing base behaviour, unchanged by the size
    # fix: the runtime answer is `new_b`; completion suggests `old_b`.
    expect_identical(outer(
        "  probe = function() {",
        "    if (FALSE) {",
        "      x$a <- 1",
        "    } else {",
        "      self$x <- Other$new()",
        "      x$a <- 2",
        "    }",
        "    x$b$@@",
        "  }"
    ), "old_b")
    # Early exit before the replacement, one-line form. Pre-existing base behaviour, unchanged by the size
    # fix: the runtime answer is `new_b`; completion suggests `old_b`.
    expect_identical(outer(
        "  probe = function() {",
        "    if (FALSE) x$a <- 1 else {",
        "      self$x <- Other$new()",
        "      x$a <- 2",
        "    }",
        "    x$b$@@",
        "  }"
    ), "old_b")
    # Early exit through a locally shadowed stop(). Pre-existing base behaviour, unchanged by
    # the size fix: probe(TRUE) returns `new_b` at runtime; completion suggests
    # `old_b`.
    expect_identical(outer(
        "  probe = function(flag) {",
        "    stop <- function() NULL",
        "    if (flag) { self$x <- Other$new(); stop() }",
        "    x$b$@@",
        "  }"
    ), "old_b")
})

test_that("empty switch arms do not raise and plain control flow keeps its values", {
    value <- member_infer(
        str2lang("{ switch(key, a = , b = { y <- 1L }); list(ok = TRUE) }"),
        member_generic_index("")
    )
    expect_identical(names(value$fields), "ok")
    value <- member_infer(
        str2lang("{ switch('a', a = , b = { y <- 1L }); list(ok = TRUE) }"),
        member_generic_index("")
    )
    expect_identical(names(value$fields), "ok")
    # Plain functions, no R6 anywhere.
    expect_identical(r6_budget_labels(r6_budget_code(
        "f <- function(k = NULL, flag = NULL) {",
        "  switch(k, a = , b = { y <- 1L })",
        "  list(ok = TRUE)",
        "}",
        "f()$"
    )), "ok")
    expect_identical(r6_budget_labels(r6_budget_code(
        "f <- function() {",
        "  switch(\"a\", a = , b = { y <- 1L })",
        "  list(ok = TRUE)",
        "}",
        "f()$"
    )), "ok")
    expect_identical(r6_budget_labels(r6_budget_code(
        "f <- function(k) {",
        "  switch(k, a = , b = { y <- 1L; list(ok = TRUE) }, list(ok = TRUE))",
        "}",
        "f(\"a\")$"
    )), "ok")
    expect_identical(r6_budget_labels(r6_budget_code(
        "switch(key, a = , b = { y <- 1L })",
        "z <- list(ok = TRUE)",
        "z$"
    )), "ok")
    # Inside an R6 method, with a field write in the arm.
    expect_identical(r6_budget_labels(r6_budget_code(
        "C <- R6::R6Class(\"C\", public = list(",
        "  x = NULL,",
        "  read = function(key) {",
        "    switch(key, a = , b = { self$x <- 1L })",
        "    list(ok = TRUE)",
        "  }",
        "))",
        "C$new()$read(k)$"
    )), "ok")
    # A switch with a default arm always yields one of its arms.
    expect_identical(r6_budget_labels(r6_budget_code(
        "f <- function(k = NULL, flag = NULL) {",
        "  switch(k, a = { y <- 1L; list(p = 1) }, list(p = 2))",
        "}",
        "f()$"
    )), "p")
    # A repeat loop keeps the fields known before it. (The runtime value also
    # has `b`; completion reports `a` only.)
    expect_identical(r6_budget_labels(r6_budget_code(
        "f <- function() {",
        "  x <- list(a = 1)",
        "  repeat { x$b <- 2; break }",
        "  x",
        "}",
        "f()$"
    )), "a")
})

test_that("callbacks that are only stored do not invalidate the fields they capture", {
    statements <- c(
        "callbacks <- list(function() self$x <- list(changed = TRUE))",
        "callbacks <- pairlist(function() self$x <- list(changed = TRUE))",
        "callbacks <- structure(list(function() self$x <- list(changed = TRUE)), class = \"cbs\")",
        "callbacks <- c(function() self$x <- list(changed = TRUE))",
        "reg.finalizer(environment(), function(e) self$x <- list(changed = TRUE))"
    )
    for (portable in c("", "portable = FALSE, ")) {
        for (statement in statements) {
            expect_identical(r6_budget_labels(r6_budget_code(
                paste0("C <- R6::R6Class(\"Callbacks\", ", portable, "public = list("),
                "  x = list(kept = TRUE),",
                "  read = function() {",
                paste0("    ", statement),
                "    self$x",
                "  }",
                "))",
                "C$new()$read()$"
            )), "kept", info = paste0(portable, statement))
        }
    }
})

test_that("switch with a logical or fractional selector gives no stale suggestion", {
    # Pre-existing base behaviour, unchanged by the size fix: the runtime value
    # is `fresh`; completion suggests nothing (and never `stale`).
    for (selector in c("TRUE", "1.5")) {
        expect_identical(r6_budget_labels(r6_budget_code(
            "C <- R6::R6Class(\"LogicalSwitch\", public = list(",
            "  x = list(stale = TRUE),",
            "  probe = function() {",
            paste0("    switch(", selector, ", self$x <- list(fresh = TRUE))"),
            "    self$x$@@",
            "  }",
            "))"
        )), character(), info = selector)
    }
    # Same for a shadowed stop() in a portable class: `fresh` when `flag` is
    # true at runtime, no suggestion is made.
    expect_length(r6_budget_labels(r6_budget_code(
        "C <- R6::R6Class(\"C\", public = list(",
        "  x = list(stale = TRUE),",
        "  probe = function(flag) {",
        "    stop <- function(...) NULL",
        "    if (flag) { self$x <- list(fresh = TRUE); stop() }",
        "    self$x$@@",
        "  }",
        "))"
    )), 0L)
})

test_that("Non-portable bare method calls stay without suggestion", {
    # Pre-existing base behaviour, unchanged by the size fix: the runtime value
    # is `done` in each case; completion does not model bare member calls when it
    # infers a method's result and suggests nothing.
    class <- function(...) {
        c("C <- R6::R6Class(\"NPChain\", portable = FALSE, public = list(",
            "  read = function() helper(),",
            ...,
            "  helper = function() secret()",
            "), private = list(secret = function() payload, payload = list(done = TRUE)))")
    }
    expect_length(r6_budget_labels(r6_budget_code(class(), "C$new()$read()$")), 0L)
    expect_length(r6_budget_labels(r6_budget_code(class(
        "  probe = function() {", "    secret()$@@", "  },"))), 0L)
    expect_length(r6_budget_labels(r6_budget_code(class(
        "  probe = function() {", "    read()$@@", "  },"))), 0L)
})
