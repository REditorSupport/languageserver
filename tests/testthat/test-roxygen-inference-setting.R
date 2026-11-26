test_that("turning type hints off invalidates provider answers on an unchanged buffer", {
    f <- provider_fixture(c('Box <- R6::R6Class("Box", public = list(value = 1L))',
        'paint <- function(x, ...) UseMethod("paint")', 'paint.Box <- function(x, brush) x',
        "#' @param x (`Box`) Box to paint.", "use <- function(x) {", "  x$value", "  paint(x, )", "}",
        "plain <- list(answer = 1)", "plain$answer"))
    f$workspace$member_metadata <- MemberMetadataCache$new(16 * 1024^2)
    f$workspace$s3_metadata <- S3MetadataCache$new()
    for (snapshot in provider_startup_s3()) f$workspace$s3_metadata$set(snapshot$package, snapshot)
    prior <- lsp_settings$get("roxygen_type_hints")
    withr::defer(lsp_settings$set("roxygen_type_hints", prior))
    members <- function(row, prefix) {
        items <- member_completion(f$uri, f$workspace, f$document, list(row = row, col = nchar(prefix)), FALSE, 100L)
        vapply(items, `[[`, "", "label")
    }
    for (enabled in c(TRUE, FALSE, TRUE)) {
        lsp_settings$set("roxygen_type_hints", enabled)
        expect_identical("value" %in% members(5L, "  x$"), enabled)
        method <- resolve_s3_call(f$uri, f$workspace, f$document, list(row = 6L, col = nchar("  paint(x, ")))$method$name
        expect_identical(method, if (enabled) "paint.Box")
        expect_true("answer" %in% members(9L, "plain$"))
    }
})

test_that("disabled hints only drop namespace models used solely in comments", {
    prior <- lsp_settings$get("roxygen_type_hints")
    withr::defer(lsp_settings$set("roxygen_type_hints", prior))
    lsp_settings$set("roxygen_type_hints", FALSE)
    code <- c("#' @returns (`commentpkg::Thing | samepkg::Class`)", "read <- function() samepkg::read()")
    parsed <- parse_document("file:///example.R", code)
    expect_identical(member_package_request(parsed)$namespace_packages, "samepkg")
    lsp_settings$set("roxygen_type_hints", TRUE)
    expect_setequal(member_package_request(parsed)$namespace_packages, c("commentpkg", "samepkg"))
})

test_that("an advisory link alone never schedules a class model for S3 dispatch", {
    prior <- lsp_settings$get("member_completion")
    withr::defer(lsp_settings$set("member_completion", prior))
    code <- c("#' @param x [`onlydocs::Topic`]", "#' @return (`declared::Box`)", "read <- function(x) ordinary::read(x)")
    parsed <- parse_document("file:///example.R", code)
    lsp_settings$set("member_completion", FALSE)
    expect_setequal(member_package_request(parsed)$namespace_packages, c("ordinary", "declared"))
    lsp_settings$set("member_completion", TRUE)
    expect_setequal(member_package_request(parsed)$namespace_packages, c("ordinary", "declared", "onlydocs"))
})
