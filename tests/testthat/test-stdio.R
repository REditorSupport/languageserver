context("Test STDIO connection")

test_that("Language Server launches", {
    skip_on_cran()
    client <- language_client()
    expect_false(is.null(client$ServerCapabilities))
})
