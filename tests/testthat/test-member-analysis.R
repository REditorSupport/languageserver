test_that("Ordinary symbol providers and disabled scopes skip member inference", {
    fixture <- provider_fixture(c("probe <- function(argument) {", "value <- list(a = 1)", "sum(argument, 1)", "}"))
    fixture$workspace$get_documentation <- function(...) NULL
    fixture$workspace$get_signature <- function(...) "sum(..., na.rm = FALSE)"
    fixture$workspace$get_help <- function(...) NULL
    fixture$workspace$guess_namespace <- function(...) "base"
    testthat::local_mocked_bindings(member_resolve_cursor = function(...) stop("Unnecessary member inference"),
        .package = "languageserver")
    expect_length(signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 2L, col = 14L))$result$signatures, 1L)
    expect_no_error(hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 2L, col = 2L)))
    expect_null(member_constructor_arguments(fixture$uri, fixture$workspace, fixture$document,
            list(row = 2L, col = 14L), ""))
    old <- lsp_settings$get("member_completion")
    withr::defer(lsp_settings$set("member_completion", old))
    lsp_settings$set("member_completion", FALSE)
    expect_identical(context_scope_completion(fixture$uri, fixture$workspace, "", list(row = 2L, col = 0L),
            FALSE, 200L, character()), list())
})

test_that("Request capability hints ignore unrelated cached packages and include defaults", {
    fixture <- provider_fixture(c("probe <- function() {", "value <- 1", "}"))
    fixture$workspace$member_metadata <- MemberMetadataCache$new(1024^2)
    classes <- member_generic_index('factory <- function() R6::R6Class("C")')
    fixture$workspace$member_metadata$set("classes", member_index_freeze(classes))
    expect_false(member_scope_required(fixture$workspace, fixture$document, list(row = 1L, col = 1L)))
    expect_false(member_symbol_required(fixture$workspace, fixture$document))
    expect_true("new_class" %in% member_syntax_names(quote(function() {
        nested <- function(C = S7::new_class("C")) C
    })))
    expect_true(parse_document("file:///defaults.R",
            'outer <- function() {inner <- function(C = S7::new_class("C")) C}')$member_data$symbol_scope)
    # Missing hints in older metadata remain conservative when referenced.
    fixture <- provider_fixture(c("library(classes)", "probe <- function() NULL"))
    fixture$workspace$member_metadata <- list(keys = function() "classes", catalog = function(...) list())
    expect_true(member_symbol_required(fixture$workspace, fixture$document))
})

test_that("Argument-less package calls preserve ordinary and member providers", {
    for (call in c("library()", "require()")) {
        fixture <- provider_fixture(c(call, "outer <- function() {",
                "probe <- function(argument = 1) NULL", "probe(argument = 2)", "}"))
        fixture$workspace$member_metadata <- MemberMetadataCache$new(1024^2)
        expect_false(member_symbol_required(fixture$workspace, fixture$document))
        expect_false(member_scope_required(fixture$workspace, fixture$document, list(row = 3L, col = 3L)))
        expect_identical(signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
                list(row = 3L, col = 18L))$result$signatures[[1L]]$label, "probe(argument = 1)")
        expect_match(hover_reply(1L, fixture$uri, fixture$workspace, fixture$document,
                list(row = 3L, col = 3L))$result$contents[[1L]], "probe\\(argument = 1\\)")
        fixture$workspace$loaded_packages <- character()
        fixture$workspace$imported_objects <- collections::dict()
        fixture$workspace$get_namespace <- function(...) NULL
        items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = 3L, col = 0L), list())$result$items
        expect_true("probe" %in% vapply(items, `[[`, character(1L), "label"))

        fixture <- provider_fixture(c(call, "x <- list(run = function(argument = 1) NULL)", "x$run(argument = 2)"))
        fixture$workspace$member_metadata <- MemberMetadataCache$new(1024^2)
        items <- member_completion(fixture$uri, fixture$workspace, fixture$document,
            list(row = 2L, col = 2L), TRUE, 200L)
        expect_identical(vapply(items, `[[`, character(1L), "label"), "run")
        expect_identical(signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
                list(row = 2L, col = 18L))$result$signatures[[1L]]$label, "run(argument = 1)")
        expect_identical(hover_reply(1L, fixture$uri, fixture$workspace, fixture$document,
                list(row = 2L, col = 4L))$result$contents, "```r\nrun(argument = 1)\n```")
    }
})

test_that("Lazy lexical locals evaluate only dependencies and respect alias history", {
    fixture <- provider_fixture(c("probe <- function() {",
            sprintf("unused%d <- expensive(value)", seq_len(1000L)),
            "value <- list(first = 1)", "alias <- value", "value <- list(second = 2)", "alias$first", "}"))
    resolved <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document,
        list(row = 1004L, col = 6L), member_cursor(fixture$document, list(row = 1004L, col = 6L)))
    expect_identical(names(resolved$value$fields), "first")
    expect_lt(20000L - resolved$budget$remaining, 20L)
    expect_false(resolved$budget$exhausted)
    deep <- member_value(type = "list", fields = list(a = member_literal(1)))
    for (i in seq_len(100L)) deep <- member_local_binding(quote(x), list(x = deep))
    budget <- member_request_budget()
    expect_identical(member_local_value(deep, member_generic_index(""), budget)$reason, "budget")
    expect_true(budget$exhausted)
    # Object-system lazy values have no lexical memo environment.
    expect_identical(member_local_value(member_value(binding_expr = quote(1)),
            member_generic_index(""), member_request_budget())$literal, 1)
})

test_that("Hashed dependency captures keep preceding values and internal state", {
    index <- member_generic_index("")
    bindings <- list(x = member_literal(1), unrelated = member_literal(9),
        .__member_position__ = c(3L, 0L), .__s4_classes__ = list(Leaf = list(name = "Leaf")))
    lookup <- list2env(bindings, hash = TRUE, parent = emptyenv())
    captured <- member_local_binding(quote(list(value = x, missing = unbound)), bindings, lookup = lookup)
    expect_equal(captured$binding_env, bindings[c("x", ".__member_position__", ".__s4_classes__")])
    expect_false(any(vapply(captured$binding_env, identical, logical(1L), lookup)))
    lookup$x <- member_literal(2)
    lookup$.__s4_classes__ <- list(Other = list(name = "Other"))
    value <- member_local_value(captured, index, member_request_budget())
    expect_identical(value$fields$value$literal, 1)
    expect_identical(names(captured$binding_env$.__s4_classes__), "Leaf")
})

test_that("Completion outlines leave list elements deferred for member access", {
    index <- member_generic_index("")
    value <- member_local_binding(quote(list(run = function(argument = 1) NULL)), list())
    budget <- member_request_budget()
    outline <- member_completion_value(value, index, budget)
    expect_identical(outline$type, "list")
    expect_null(outline$fields)
    expect_false(exists("value", value$binding_cache, inherits = FALSE))
    expect_identical(budget$remaining, 20000L)
    full <- member_local_value(value, index, budget)
    expect_identical(full$fields$run$function_expr[[2L]]$argument, 1)
    expect_identical(member_local_value(value, index, budget), full)
    index$document_bindings <- list(list = list(list(expr = quote(function(...) function(value) value),
                start = c(0L, 0L), end = c(0L, 40L))))
    shadowed <- member_local_binding(quote(list()), list(.__member_position__ = c(1L, 0L)))
    expect_true(!is.null(member_completion_value(shadowed, index, member_request_budget())$function_expr))
    index$document_bindings <- NULL

    for (shadow in list(
        member_value(function_expr = quote(function(...) R6::R6Class("Widget")$new())),
        member_value(function_expr = quote(function(...) function(value) value)))) {
        value <- member_local_binding(quote(list()), list(list = shadow))
        outline <- member_completion_value(value, index, member_request_budget())
        expect_true(!is.null(outline$r6_class) || !is.null(outline$function_expr))
    }
    index <- member_generic_index('list <- function(...) R6::R6Class("Widget")$new()')
    value <- member_local_binding(quote(list()), list())
    expect_identical(member_completion_value(value, index, member_request_budget())$r6_class$name, "Widget")
})

test_that("Member providers reuse receivers and invalidate on edits and metadata changes", {
    fixture <- provider_fixture(c("value <- list(run = function(argument = 1) NULL)", "value$run(argument = 2)"))
    fixture$workspace$member_metadata <- MemberMetadataCache$new(1024^2)
    point <- list(row = 1L, col = 6L)
    cursor <- member_cursor(fixture$document, point)
    first <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor)
    expect_equal(fixture$document$member_receivers$size(), 1L)
    location <- member_hover_location(fixture$document, list(row = 1L, col = 8L))
    testthat::local_mocked_bindings(member_recover_scope = function(...) stop("Repeated recovery"),
        .package = "languageserver")
    expect_identical(member_symbol(fixture$uri, fixture$workspace, fixture$document, location)$signature,
        "run(argument = 1)")
    expect_identical(first$value$fields$run$function_expr[[2L]]$argument, 1)
    fixture$workspace$member_metadata$set("package", member_index_freeze(member_generic_index("")))
    expect_error(member_symbol(fixture$uri, fixture$workspace, fixture$document, location), "Repeated recovery")
    fixture$document$set_content(2L, fixture$document$content)
    expect_equal(fixture$document$member_receivers$size(), 0L)
    expect_null(member_symbol(fixture$uri, fixture$workspace, fixture$document, location))
})

test_that("Metadata revisions reflect semantic changes rather than cache hits", {
    cache <- MemberMetadataCache$new(1024^2)
    snapshot <- member_index_freeze(member_generic_index(""))
    cache$set("package", snapshot)
    revision <- cache$revision()
    cache$catalog("package")
    cache$get("package")
    cache$.__enclos_env__$private$indexes$clear()
    cache$get("package")
    expect_identical(cache$revision(), revision)
    cache$remove("package")
    expect_gt(cache$revision(), revision)
})

test_that("Dependency slicing retains opaque writes, closures and S4 declarations", {
    for (lines in list(
        c("x <- list(old = 1)", "read <- function() x", "x <- list(new = 2)", "read()$old"),
        c("x <- list(old = 1)", "if (flag) x <- list(new = 2)", "x$old"),
        c('Leaf <- methods::setClass("Leaf", slots = c(value = "numeric"))',
            'x <- methods::new("Leaf")', "x@value")
    )) {
        fixture <- provider_fixture(c("probe <- function() {", lines, "}"))
        row <- length(lines)
        line <- fixture$document$line0(row)
        point <- list(row = row, col = regexpr("[$@]", line)[[1L]])
        result <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document,
            point, member_cursor(fixture$document, point))
        if (any(grepl("if (flag)", lines, fixed = TRUE))) {
            expect_length(result$value$type, 0L)
        } else if (any(grepl("setClass", lines, fixed = TRUE))) {
            expect_true("value" %in% names(result$value$slot_types))
        } else {
            expect_identical(names(result$value$fields), "old")
        }
    }
})

test_that("Early invocation summaries retain argument matching and argument shapes", {
    index <- member_generic_index("run <- function(value = 1) list(done = value)")
    expect_identical(member_infer(quote(run()$done), index)$literal, 1)
    expect_identical(member_infer(quote(run(2)$done), index)$literal, 2)
    expect_identical(member_infer(quote(run(other = 2)), index)$reason, "argument_matching")
    expect_identical(member_infer(quote(run(value = 2, value = 3)), index)$reason, "argument_matching")
    expect_identical(member_infer(quote(run(2)$done), index)$literal, 2)
})

test_that("Receiver caches expire when a metadata store is replaced", {
    fixture <- provider_fixture(c("x <- list(a = 1)", "x$a"))
    fixture$workspace$member_metadata <- MemberMetadataCache$new(1024^2)
    point <- list(row = 1L, col = 2L)
    cursor <- member_cursor(fixture$document, point)
    member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor)
    fixture$workspace$member_metadata <- MemberMetadataCache$new(1024^2)
    testthat::local_mocked_bindings(member_recover_scope = function(...) stop("Fresh metadata store"),
        .package = "languageserver")
    expect_error(member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor),
        "Fresh metadata store")
})

test_that("Recursive arguments cache only a proven native class surface", {
    index <- member_generic_index("")
    index$members$Frame <- c(run = "native_run")
    index$native_factories <- "native_run"
    budget <- member_request_budget()
    budget$transient <- TRUE
    value <- member_value(type = "Frame")
    expect_true(member_receiver_cacheable(value, index, budget))
    value$fields <- list(partial = member_value(reason = "recursion"))
    expect_false(member_receiver_cacheable(value, index, budget))
    budget$exhausted <- TRUE
    expect_false(member_receiver_cacheable(member_value(type = "Frame"), index, budget))
    expect_false(member_receiver_cacheable(member_value(reason = "recursion"), index, budget))
})

test_that("Only standalone final ordinary identifier edits bypass parse waits", {
    fixture <- provider_fixture(c("x <- list(a = 1)", ""))
    edit <- function(doc, text) {
        doc$apply_content_changes(doc$version + 1L,
            list(list(range = range(position(1L, 0L), position(1L, nchar(doc$line0(1L)))), text = text)))
    }
    edit(fixture$document, "x")
    expect_true(member_ordinary_request(fixture$workspace, fixture$document))
    expect_false(member_scope_required(fixture$workspace, fixture$document, list(row = 1L, col = 1L)))
    edit(fixture$document, "xy")
    expect_true(member_ordinary_request(fixture$workspace, fixture$document))
    edit(fixture$document, "xy <- list(a = 1)")
    expect_false(member_ordinary_request(fixture$workspace, fixture$document))
    for (content in list(c("f(a =", "x", ")"), c("f(", "x)"),
            c("f <- function(", "x) x"), c("f <- function() {", "x", "}"))) {
        fixture <- provider_fixture(content)
        edit(fixture$document, "y")
        expect_false(member_ordinary_request(fixture$workspace, fixture$document))
    }
})

test_that("Identical worker metadata does not invalidate receiver summaries", {
    fixture <- provider_fixture("x <- 1")
    metadata <- MemberMetadataCache$new(1024^2)
    fixture$workspace$member_metadata <- metadata
    fixture$workspace$load_packages <- fixture$workspace$update_loaded_packages <- function(...) NULL
    server <- list(get_workspace = function(...) fixture$workspace)
    snapshot <- member_index_freeze(member_generic_index(""))
    payload <- list(members = list(package = snapshot), packages = character())
    resolve_callback(server, fixture$uri, fixture$document$version, payload)
    revision <- metadata$revision()
    resolve_callback(server, fixture$uri, fixture$document$version, payload)
    expect_identical(metadata$revision(), revision)
})
