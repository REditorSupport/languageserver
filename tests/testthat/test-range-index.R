test_that("indexed viewport providers agree with XML for nested and Unicode calls", {
    content <- c(
        'label <- "\U0001f600"; target(éclair, βeta)',
        "target(one, beta = two, three)",
        "target(one, be = two, three)",
        "target(, two, )",
        "target( # first argument",
        "  one, # second argument",
        "  inner(two, three))",
        "pkg::target(one, two)",
        "object$target(one, two)",
        "target(alpha, beta)",
        'target("alpha" = one, two)',
        "target((alpha), beta)",
        "target(function(x) x, two)",
        "target({ alpha }, beta)"
    )
    fixture <- provider_fixture(content, function(...) formals(function(alpha, beta, gamma) NULL))
    indexed <- unserialize(serialize(fixture$document$parse_data$range_data, NULL))
    expect_identical(indexed, fixture$document$parse_data$range_data)

    requests <- list(
        range(position(0L, 0L), position(length(content), 0L)),
        range(position(0L, 28L), position(3L, 5L)),
        range(position(5L, 0L), position(7L, 0L)),
        range(position(20L, 0L), position(30L, 0L))
    )
    for (request in requests) {
        for (provider in list(inlay_hint_reply, inline_value_reply)) {
            fixture$document$parse_data$range_data <- indexed
            actual <- provider(1L, fixture$uri, fixture$workspace,
                fixture$document, request)$result
            fixture$document$parse_data$range_data <- NULL
            expected <- provider(1L, fixture$uri, fixture$workspace,
                fixture$document, request)$result
            expect_equal(actual, expected)
        }
    }
})

test_that("inlay indexes keep workspace formals and settings live", {
    current_formals <- formals(function(first, second) NULL)
    fixture <- provider_fixture("target(one, two)", function(...) current_formals)
    request <- range(position(0L, 0L), position(1L, 0L))
    labels <- function() {
        vapply(inlay_hint_reply(1L, fixture$uri,
                fixture$workspace, fixture$document, request)$result, `[[`, character(1L), "label")
    }
    expect_equal(labels(), c("first =", "second ="))
    current_formals <- formals(function(changed, updated) NULL)
    expect_equal(labels(), c("changed =", "updated ="))
    old_minimum <- lsp_settings$get("inlay_hints_minimum_arguments")
    withr::defer(lsp_settings$set("inlay_hints_minimum_arguments", old_minimum))
    lsp_settings$set("inlay_hints_minimum_arguments", 3L)
    expect_length(labels(), 0L)
})

test_that("range indexes handle empty, incomplete and stale documents", {
    empty <- range_provider_parse_data(NULL, character())
    expect_length(empty$argument_lines, 0L)
    expect_length(empty$variables$line, 0L)
    fixture <- provider_fixture("target(")
    request <- range(position(0L, 0L), position(1L, 0L))
    for (provider in list(inlay_hint_reply, inline_value_reply)) {
        expect_length(provider(1L, fixture$uri, fixture$workspace,
                fixture$document, request)$result, 0L)
    }
    fixture$document$version <- 2L
    expect_null(inline_value_reply(1L, fixture$uri, fixture$workspace,
            fixture$document, request))
    expect_null(inlay_hint_reply(1L, fixture$uri, fixture$workspace,
            fixture$document, request))
})

test_that("viewport indexes retain literate R regions and tab positions", {
    uri <- "file:///range-index.qmd"
    content <- c("# Heading", "```{python}", "target(one, two)", "```",
        "```{r}", '\tlabel <- "\U0001f600"; target(one, two)', "```")
    document <- Document$new(uri, version = 1L, content = content)
    parsed <- parse_document(uri, content, is_rmarkdown = TRUE)
    parsed$version <- 1L
    parsed$xml_doc <- xml2::read_xml(parsed$xml_data)
    document$update_parse_data(parsed)
    workspace <- list(get_parse_data = function(...) parsed,
        get_formals = function(...) formals(function(first, second) NULL))
    request <- range(position(0L, 0L), position(length(content), 0L))
    indexed <- parsed$range_data
    for (provider in list(inlay_hint_reply, inline_value_reply)) {
        parsed$range_data <- indexed
        actual <- provider(1L, uri, workspace, document, request)$result
        expect_gt(length(actual), 0L)
        parsed$range_data <- NULL
        expected <- provider(1L, uri, workspace, document, request)$result
        expect_equal(actual, expected)
    }
    expect_true(all(indexed$variables$line == 5L))
    expect_true(all(indexed$argument_lines == 5L))
})

test_that("folding caches use original literate content", {
    uri <- "file:///folding-cache.Rmd"
    first <- c("# Heading", "", "```{r}", "target(one, two)", "```")
    second <- c("Some prose", "", "```{r}", "target(one, two)", "```")
    document <- Document$new(uri, version = 1L, content = first)
    parsed <- parse_document(uri, first, is_rmarkdown = TRUE)
    parsed$version <- 1L
    parsed$xml_doc <- xml2::read_xml(parsed$xml_data)
    document$update_parse_data(parsed)
    workspace <- list(get_parse_data = function(...) parsed)
    first_result <- document_folding_range_reply(1L, uri, workspace, document)$result
    expect_identical(document_folding_range_reply(2L, uri, workspace, document)$result,
        first_result)
    document$set_content(1L, second)
    second_result <- document_folding_range_reply(3L, uri, workspace, document)$result
    expect_false(identical(first_result, second_result))
    parsed$folding_ranges_cache <- NULL
    expect_identical(document_folding_range_reply(4L, uri, workspace, document)$result,
        second_result)
})

test_that("comment folds retain contiguous indentation and section boundaries", {
    content <- c("# one", "# two", "  # three", "  # four", "", "  # five",
        "  # six", "# section ----", "# seven", "# eight", "# %%", "# nine")
    fixture <- provider_fixture(content)
    folds <- get_comment_folding_ranges(fixture$document$parse_data$xml_doc)
    expect_equal(vapply(folds, `[[`, integer(1L), "startLine"), c(0L, 2L, 5L, 8L))
    expect_equal(vapply(folds, `[[`, integer(1L), "endLine"), c(1L, 3L, 6L, 9L))
})

test_that("linked editing caches ranges without losing cursor boundaries", {
    fixture <- provider_fixture(c("#' @param first,second docs",
            "target <- function(first, second) first + second", "target(1, 2)"))
    first <- linked_editing_range_reply(1L, fixture$uri, fixture$workspace,
        fixture$document, position(0L, 11L))$result
    expect_length(first$ranges, 2L)
    for (point in list(first$ranges[[1L]]$start, first$ranges[[1L]]$end,
            first$ranges[[2L]]$start, first$ranges[[2L]]$end)) {
        expect_identical(linked_editing_range_reply(2L, fixture$uri,
                fixture$workspace, fixture$document, point)$result, first)
    }
    expect_null(linked_editing_range_reply(3L, fixture$uri, fixture$workspace,
            fixture$document, position(2L, 2L))$result)
    cached <- fixture$document$parse_data$linked_editing_cache
    restored <- unserialize(serialize(cached, NULL))
    expect_identical(as.list(restored$ranges), as.list(cached$ranges))
})

test_that("document links observe files appearing and disappearing without edits", {
    root <- withr::local_tempdir()
    path <- file.path(root, "target.R")
    fixture <- provider_fixture(c('source("target.R")', 'source("target.R")'))
    links <- function() {
        document_link_reply(1L, fixture$uri, fixture$workspace,
            fixture$document, root)$result
    }
    expect_length(links(), 0L)
    writeLines("value <- 1", path)
    expect_length(links(), 2L)
    expect_equal(vapply(links(), function(link) link$data$path, character(1L)),
        rep(as.character(fs::path_abs(path)), 2L))
    unlink(path)
    expect_length(links(), 0L)
    dir.create(path)
    expect_length(links(), 0L)
})

test_that("native semantic deltas reconstruct edits across long common prefixes", {
    previous <- rep(c(1L, 0L, 1L, 8L, 0L), 10000L)
    for (current in list(integer(), previous, c(previous, 1L, 0L, 2L, 8L, 0L),
            c(previous[seq_len(20000L)], 1L, 2L, 3L, 8L, 1L, previous[20006L:50000L]))) {
        reconstructed <- previous
        for (edit in semantic_token_delta(previous, current)) {
            start <- edit$start
            after <- start + edit$deleteCount
            reconstructed <- c(
                if (start) previous[seq_len(start)] else integer(),
                edit$data,
                if (after < length(previous)) previous[seq.int(after + 1L, length(previous))] else integer()
            )
        }
        expect_identical(reconstructed, current)
    }
    expect_error(semantic_token_delta(1L, integer()), "complete integer tokens")
})
