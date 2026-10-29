test_that("Polars conversions and later roots complete within a small inference budget", {
    skip_if_not_installed("polars")
    snapshots <- c(list(polars = member_prepare_package("polars")), member_prepare_startup_data("datasets"))
    metadata <- MemberMetadataCache$new(32 * 1024^2, max_entries = 16L)
    for (package in names(snapshots)) metadata$set(package, snapshots[[package]])
    workspace <- list(member_metadata = metadata, startup_packages = "datasets")
    budget_factory <- member_request_budget
    testthat::local_mocked_bindings(member_request_budget = function() {
        budget <- budget_factory()
        budget$remaining <- 128L
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
})
