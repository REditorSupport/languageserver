context("Test STDIO connection")

test_that("Language Server launches", {
    client <- language_client()
    expect_false(is.null(client$ServerCapabilities))
})
