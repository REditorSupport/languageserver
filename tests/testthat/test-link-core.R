test_that("Document link resolution reports missing and oversized files", {
    missing_path <- file.path(withr::local_tempdir(), "missing.R")
    missing <- document_link_resolve_reply(
        1L, NULL, list(data = list(path = missing_path))
    )
    expect_equal(missing$error$code, ErrorCodes$RequestCancelled)
    expect_match(missing$error$message, "missing")

    old_limit <- lsp_settings$get("link_file_size_limit")
    withr::defer(lsp_settings$set("link_file_size_limit", old_limit))
    lsp_settings$set("link_file_size_limit", 1L)
    path <- withr::local_tempfile()
    writeLines("larger than one byte", path)
    oversized <- document_link_resolve_reply(
        2L, NULL, list(data = list(path = path))
    )
    expect_equal(oversized$error$code, ErrorCodes$RequestCancelled)
    expect_match(oversized$error$message, "exceeds the limit")
})
