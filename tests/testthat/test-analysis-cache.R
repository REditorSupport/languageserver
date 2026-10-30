analysis_s3_fixture <- local({
    state <- new.env(parent = emptyenv())
    function(content) {
        if (is.null(state$catalogs)) state$catalogs <- stats::setNames(lapply(startup_packages, s3_prepare_namespace), startup_packages)
        fixture <- provider_fixture(content)
        fixture$workspace$s3_metadata <- S3MetadataCache$new()
        for (package in names(state$catalogs)) fixture$workspace$s3_metadata$set(package, state$catalogs[[package]])
        fixture$workspace$get_documentation <- function(...) NULL
        fixture
    }
})

analysis_cache_update <- function(fixture, content) {
    document <- fixture$document
    document$set_content(document$version + 1L, content)
    parsed <- parse_document(document$uri, content)
    parsed$version <- document$version
    document$update_parse_data(parsed)
}

test_that("Analysis domains isolate contexts, bound storage and clone cached values", {
    owner <- new.env(parent = emptyenv())
    first <- analysis_cache(owner, "first", "context", max_entries = 2L)
    second <- analysis_cache(owner, "second", "context")
    analysis_cache_set(first, "a", list(value = 1))
    analysis_cache_set(second, "a", list(value = 2))
    value <- analysis_cache_get(first, "a")
    value$value$value <- 3
    expect_identical(analysis_cache_get(first, "a")$value$value, 1)
    expect_identical(analysis_cache_get(second, "a")$value$value, 2)
    analysis_cache_set(first, "null", NULL)
    expect_true(analysis_cache_get(first, "null")$hit)
    expect_null(analysis_cache_get(first, "null")$value)
    analysis_cache_set(first, "new", 4)
    expect_lte(first$size(), 2L)
    expect_false(analysis_cache_get(first, "a")$hit)
    first <- analysis_cache(owner, "first", "changed")
    expect_identical(first$size(), 0L)
    expect_true(analysis_cache_get(second, "a")$hit)
    bounded <- analysis_cache(owner, "bounded", max_bytes = 256L)
    analysis_cache_set(bounded, "large", raw(1000L))
    expect_false(analysis_cache_get(bounded, "large")$hit)
})

test_that("Old handles, incompatible formats and changed limits cannot poison a new domain", {
    owner <- new.env(parent = emptyenv())
    old <- analysis_cache(owner, "fixture", "old", max_entries = 3L)
    analysis_cache_set(old, "value", "old")
    current <- analysis_cache(owner, "fixture", "current", max_entries = 3L)
    analysis_cache_set(current, "value", "current")
    analysis_cache_set(old, "value", "stale writer")
    expect_identical(analysis_cache_get(current, "value")$value, "current")
    for (i in 1:3) analysis_cache_set(current, as.character(i), i)
    smaller <- analysis_cache(owner, "fixture", "current", max_entries = 1L)
    expect_identical(smaller$size(), 0L)
    for (i in 1:3) analysis_cache_set(smaller, as.character(i), i)
    expect_identical(smaller$size(), 1L)
    smaller$set("bad", as.raw(c(1, 2, 3)))
    expect_false(analysis_cache_get(smaller, "bad")$hit)
    analysis_cache_set(smaller, "bad", list(value = 1))
    record <- smaller$get("bad")
    record$payload <- serialize(list(value = 2), NULL)
    smaller$set("bad", record)
    expect_false(analysis_cache_get(smaller, "bad")$hit)
    analysis_cache_set(smaller, "value", "old format")
    state <- owner$analysis_caches$fixture
    state$runtime$format <- -1L
    owner$analysis_caches$fixture <- state
    expect_identical(analysis_cache(owner, "fixture", "current", max_entries = 1L)$size(), 0L)
})

test_that("Source signatures account for current formatting options", {
    document <- Document$new("file:///format-options.R", language = "r")
    withr::local_options(scipen = 0)
    original <- analysis_function_header(document, "function(value = 1e5)", "f")$signature
    withr::local_options(scipen = 999)
    current <- analysis_function_header(document, "function(value = 1e5)", "f")$signature
    expected <- get_signature("f", str2expression(c("function(value = 1e5)", "NULL"))[[1L]])
    expect_identical(current, expected)
    expect_false(identical(original, current))
})

test_that("Source signatures and documentation survive reparses and retain current inputs", {
    lines <- c("#' Describe a source function.", "#' @param argument The value.",
        "target <- function(argument, option = 1) argument", "target(1, 2)")
    fixture <- provider_fixture(lines)
    document <- fixture$document
    point <- list(row = 3L, col = 7L)
    signature <- signature_reply(1L, fixture$uri, fixture$workspace, document, point)$result
    expect_identical(signature$signatures[[1L]]$label, "target(argument, option = 1)")
    expect_match(signature$signatures[[1L]]$documentation$value, "Describe a source function", fixed = TRUE)
    lines[[4L]] <- "target(3, 4)"
    analysis_cache_update(fixture, lines)
    old_header <- analysis_function_header
    old_docs <- convert_comment_to_documentation
    header <- old_header
    environment(header) <- list2env(list(str2expression = function(...) stop("Reparsed unchanged formals")),
        parent = environment(header))
    testthat::local_mocked_bindings(
        analysis_function_header = header,
        convert_comment_to_documentation = function(...) stop("Reparsed unchanged documentation"),
        .package = "languageserver")
    signature <- signature_reply(1L, fixture$uri, fixture$workspace, document, point)$result
    expect_identical(signature$signatures[[1L]]$label, "target(argument, option = 1)")
    hover <- hover_reply(1L, fixture$uri, fixture$workspace, document, list(row = 3L, col = 3L))$result
    expect_match(paste(hover$contents, collapse = "\n"), "Describe a source function", fixed = TRUE)
    # Direct input checks do not reuse an old source signature or description.
    testthat::local_mocked_bindings(analysis_function_header = old_header, convert_comment_to_documentation = old_docs,
        .package = "languageserver")
    lines[[1L]] <- "#' The updated description."
    lines[[3L]] <- "target <- function(argument, revised = TRUE) argument"
    analysis_cache_update(fixture, lines)
    signature <- signature_reply(1L, fixture$uri, fixture$workspace, document, point)$result
    expect_identical(signature$signatures[[1L]]$label, "target(argument, revised = TRUE)")
    expect_match(signature$signatures[[1L]]$documentation$value, "updated description", fixed = TRUE)
})

test_that("Parse workers reuse unchanged documentation across function and call edits", {
    content <- c("#' Worker cache documentation fixture.", "#' @param value The input.",
        "target <- function(value, extra = 1) value", "target(1)")
    first <- parse_document("file:///worker-documentation.R", content)
    expect_match(first$documentation$target$description, "Worker cache", fixed = TRUE)
    content[[3L]] <- "target <- function(value, extra = 2) value"
    content[[4L]] <- "target(2)"
    testthat::local_mocked_bindings(convert_comment_to_documentation = function(...) {
        stop("Repeated worker documentation conversion")
    }, .package = "languageserver")
    second <- parse_document("file:///worker-documentation.R", content)
    expect_identical(second$documentation$target, first$documentation$target)
})

test_that("S3 binding inference reuses complete summaries across edits and cursor positions", {
    content <- c("factory <- function(input) data.frame(value = input)", "item <- factory(1)",
        rep("item <- factory(item)", 8L), "print(item, digits = 1)")
    fixture <- analysis_s3_fixture(content)
    resolve <- function() {
        context <- s3_context(fixture$uri, fixture$workspace, fixture$document,
            c(fixture$document$nline - 1L, 0L))
        value <- s3_source_value("item", context)
        list(value = value, nodes = context$nodes)
    }
    cold <- resolve()
    expect_identical(cold$value$classes, "data.frame")
    expect_gt(cold$nodes, 20L)
    content[[length(content)]] <- "print(item, digits = 2)"
    analysis_cache_update(fixture, content)
    warm <- resolve()
    expect_identical(warm$value, cold$value)
    expect_identical(warm$nodes, 0L)
    analysis_cache_update(fixture, c(content, "print(item, digits = 3)"))
    expect_lte(resolve()$nodes, 3L)
    content[[1L]] <- "factory <- function(input) input"
    analysis_cache_update(fixture, content)
    expect_identical(resolve()$value$classes, .class2(1))
})

test_that("S3 reuse validates peer contents and metadata identity", {
    fixture <- analysis_s3_fixture(c("item <- factory(1)", "print(item)"))
    peer <- Document$new("file:///peer-source.R", language = "r", version = 1L,
        content = "factory <- function(input) data.frame(value = input)")
    data <- parse_document(peer$uri, peer$content)
    data$version <- peer$version
    peer$update_parse_data(data)
    fixture$workspace$documents$set(peer$uri, peer)
    resolve <- function() {
        context <- s3_context(fixture$uri, fixture$workspace, fixture$document, c(1L, 0L))
        list(value = s3_source_value("item", context), context = context)
    }
    expect_identical(resolve()$value$classes, "data.frame")
    expect_identical(resolve()$context$nodes, 0L)
    peer$set_content(1L, "factory <- function(input) input")
    data <- parse_document(peer$uri, peer$content)
    data$version <- 1L
    peer$update_parse_data(data)
    expect_identical(resolve()$value$classes, .class2(1))
    old <- fixture$workspace$s3_metadata
    fixture$workspace$s3_metadata <- S3MetadataCache$new()
    for (package in old$keys()) fixture$workspace$s3_metadata$set(package, old$get(package))
    expect_gt(resolve()$context$nodes, 0L)
})

test_that("Indexed source summaries participate in S3 reuse without version-only validation", {
    fixture <- analysis_s3_fixture(c("item <- factory(1)", "print(item)"))
    peer_uri <- "file:///indexed-source.R"
    summaries <- collections::dict()
    fixture$workspace$index <- list(enabled = TRUE, package_root_for_uri = function(...) NULL,
        source_closure = function(...) c(fixture$uri, peer_uri), summaries = summaries)
    update <- function(text) {
        parsed <- parse(text = text, keep.source = TRUE)
        summaries$set(peer_uri, list(s3 = s3_index_summary(text, peer_uri, expressions = parsed)))
    }
    update("factory <- function(input) data.frame(value = input)")
    resolve <- function() {
        context <- s3_context(fixture$uri, fixture$workspace, fixture$document, c(1L, 0L))
        value <- s3_source_value("item", context)
        list(value = value, context = context)
    }
    expect_identical(resolve()$value$classes, "data.frame")
    expect_identical(resolve()$context$nodes, 0L)
    update("factory <- function(input) input")
    expect_identical(resolve()$value$classes, .class2(1))
})

test_that("Partial S3 results and scratch-state effects are excluded from reuse", {
    expect_false(s3_binding_cacheable(s3_value("data.frame", fields = list(partial = s3_unknown("budget")))))
    expect_false(s3_binding_cacheable(s3_value(NULL, fn = quote(function() value), closure = new.env())))
    fixture <- analysis_s3_fixture(c("item <- {scratch <- 1; data.frame(value = scratch)}", "print(item)"))
    resolve <- function() {
        context <- s3_context(fixture$uri, fixture$workspace, fixture$document, c(1L, 0L))
        value <- s3_source_value("item", context)
        list(context = context, value = value)
    }
    expect_identical(resolve()$value$classes, "data.frame")
    expect_gt(resolve()$context$nodes, 0L)
    context <- s3_context(fixture$uri, fixture$workspace, fixture$document, c(1L, 0L))
    context$deadline <- -1
    s3_source_value("item", context)
    expect_true(context$exhausted)
    expect_identical(context$binding_cache$size(), 0L)
})

test_that("An exhausted S3 request cannot pin an incomplete result", {
    fixture <- analysis_s3_fixture(c("render <- function(x, ...) UseMethod('render')",
            "render.numeric <- function(x, digits = 1, ...) NULL", "render(1, digits = 2)"))
    build_context <- s3_context
    testthat::local_mocked_bindings(s3_context = function(...) {
        context <- build_context(...)
        context$deadline <- -1
        context
    }, .package = "languageserver")
    resolve <- function() resolve_s3_call(fixture$uri, fixture$workspace, fixture$document, list(row = 2L, col = 20L))
    expect_null(resolve())
    expect_identical(fixture$document$s3_calls$size(), 0L)
    testthat::local_mocked_bindings(s3_context = build_context, .package = "languageserver")
    expect_identical(resolve()$method$name, "render.numeric")
})

test_that("NAMESPACE declarations reuse parsed facts while checking exact current contents", {
    root <- tempfile()
    dir.create(root)
    withr::defer(unlink(root, recursive = TRUE))
    path <- file.path(root, "NAMESPACE")
    writeLines("S3method(render, first)", path)
    owner <- new.env(parent = emptyenv())
    first <- s3_namespace_snapshot(root, owner)
    expect_identical(first$records[[1L]]$class, "first")
    snapshot <- s3_namespace_snapshot
    environment(snapshot) <- list2env(list(parse = function(...) stop("Reparsed NAMESPACE")),
        parent = environment(snapshot))
    expect_identical(snapshot(root, owner), first)
    stamp <- file.info(path)$mtime
    writeLines("S3method(render, other)", path)
    Sys.setFileTime(path, stamp)
    second <- s3_namespace_snapshot(root, owner)
    expect_identical(second$records[[1L]]$class, "other")
    expect_false(identical(first$key, second$key))
    writeLines("S3method(", path)
    expect_false(s3_namespace_snapshot(root, owner)$valid)
})
