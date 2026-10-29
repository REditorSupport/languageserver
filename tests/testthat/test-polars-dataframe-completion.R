test_that("Polars conversions and later roots complete within a small inference budget", {
    skip_if_not_installed("polars")
    snapshots <- c(list(polars = member_prepare_package("polars")), member_prepare_startup_data("datasets"))
    metadata <- MemberMetadataCache$new(32 * 1024^2, max_entries = 16L)
    for (package in names(snapshots)) metadata$set(package, snapshots[[package]])
    workspace <- list(member_metadata = metadata, startup_packages = "datasets")
    budget_factory <- member_request_budget
    remaining <- 128L
    testthat::local_mocked_bindings(member_request_budget = function() {
        budget <- budget_factory()
        budget$remaining <- remaining
        budget
    }, .package = "languageserver")
    for (conversion in c("as_polars_df(iris)", "polars::as_polars_df(datasets::iris)",
            "as_polars_df(list(value = 1))", "pl$DataFrame(iris)")) {
        lines <- c("library(polars)", "", paste0("dt <- ", conversion),
            "# complete before the query",
            'dt$filter(pl$col("Sepal.Length") > pl$col("Sepal.Length")$mean())',
            "# complete after the query")
        for (row in c(3L, 5L)) {
            for (receiver in c("dt", "pl")) {
                content <- lines
                content[[row + 1L]] <- paste0(receiver, "$")
                document <- Document$new("file:///polars-conversion.R", language = "r", version = 1L, content = content)
                document$parse_data <- parse_document(document$uri, content)
                point <- list(row = row, col = 3L)
                resolved <- member_resolve_cursor(document$uri, workspace, document, point,
                    member_cursor(document, point))
                info <- paste(conversion, receiver, row)
                expect_identical(resolved$value$type, if (receiver == "dt") "polars_data_frame" else "pl", info = info)
                expect_false(resolved$budget$exhausted, info = info)
                items <- member_completion(document$uri, workspace, document, point, TRUE, 200L)
                labels <- vapply(items, `[[`, character(1L), "label")
                expected <- if (receiver == "dt") c("filter", "lazy", "select") else c("col", "DataFrame")
                expect_true(all(expected %in% labels), info = info)
                expect_true(all(vapply(items, function(item) identical(item$data$type, "member"), logical(1L))), info = info)
            }
        }
    }
    remaining <- 512L
    content <- c("library(polars)", "upstream <- opaque_source()", "converted <- as_polars_df(upstream)", "converted$")
    document <- Document$new("file:///polars-unknown-input.R", language = "r", version = 1L, content = content)
    document$parse_data <- parse_document(document$uri, content)
    point <- list(row = 3L, col = nchar(content[[4L]]))
    resolved <- member_resolve_cursor(document$uri, workspace, document, point, member_cursor(document, point))
    expect_null(resolved$value$type)
    items <- member_completion(document$uri, workspace, document, point, TRUE, 200L)
    expect_true(all(c("filter", "lazy", "select") %in% vapply(items, `[[`, character(1L), "label")))
    expect_true(isTRUE(attr(items, "truncated")))
    content[[4L]] <- "converted$filter()"
    document$set_content(2L, content)
    data <- parse_document(document$uri, content)
    data$version <- 2L
    document$update_parse_data(data)
    signature <- signature_reply(1L, document$uri, workspace, document, list(row = 3L, col = 17L))$result
    expect_identical(signature$signatures[[1L]]$label, "filter(...)")
    expect_match(signature$signatures[[1L]]$documentation$value, "Receiver type is uncertain", fixed = TRUE)
    hover <- hover_reply(1L, document$uri, workspace, document, list(row = 3L, col = 12L))$result
    expect_match(paste(hover$contents, collapse = "\n"), "filter(...)", fixed = TRUE)
    expect_match(paste(hover$contents, collapse = "\n"), "Receiver type is uncertain", fixed = TRUE)
    for (expression in c(
        'dt$filter(pl$col("a") > pl$col("b")$|)',
        'dt$filter(pl$col("a") > pl$col("b")$me|an())',
        "dt$filter()$|",
        "dt$filter()$fi|lter()",
        "dt$filter(unknown)$|")) {
        content <- c("library(polars)", "", "dt <- as_polars_df(iris)", "",
            'dt$filter(pl$col("Sepal.Length") > pl$col("Sepal.Length")$mean())', "", expression)
        marker <- regexpr("|", expression, fixed = TRUE)[[1L]]
        content[[7L]] <- sub("|", "", expression, fixed = TRUE)
        document <- Document$new("file:///polars-nested.R", language = "r", version = 1L, content = content)
        data <- parse_document(document$uri, content)
        data$version <- 1L
        document$update_parse_data(data)
        point <- list(row = 6L, col = marker - 1L)
        resolved <- member_resolve_cursor(document$uri, workspace, document, point, member_cursor(document, point))
        expected_type <- if (grepl('pl$col("b")$', expression, fixed = TRUE)) "polars_expr" else "polars_data_frame"
        expect_identical(resolved$value$type, expected_type, info = expression)
        expect_identical(resolved$value$member_package, "polars", info = expression)
        expect_false(resolved$budget$exhausted, info = expression)
        items <- member_completion(document$uri, workspace, document, point, TRUE, 200L)
        labels <- vapply(items, `[[`, character(1L), "label")
        expect_true(if (expected_type == "polars_expr") "mean" %in% labels else "filter" %in% labels, info = expression)
        expect_true(all(vapply(items, function(item) identical(item$data$package, "polars"), logical(1L))), info = expression)
    }
})
