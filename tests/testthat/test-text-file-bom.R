test_that("the file probe and reader agree on all Unicode byte order marks", {
    # Hand-written wire signatures also cover the overlapping UTF-16LE/UTF-32LE prefix.
    marks <- list(
        "UTF-8" = as.raw(c(0xef, 0xbb, 0xbf)),
        "UTF-16LE" = as.raw(c(0xff, 0xfe)),
        "UTF-16BE" = as.raw(c(0xfe, 0xff)),
        "UTF-32LE" = as.raw(c(0xff, 0xfe, 0, 0)),
        "UTF-32BE" = as.raw(c(0, 0, 0xfe, 0xff))
    )
    path <- withr::local_tempfile()
    withr::local_locale(c(LC_CTYPE = "C"))
    expected <- c("\ufeff\u6e2c\U0001f680", "\ufeffsecond")
    text <- paste0(expected, collapse = "\r\n")
    decode_without_icu <- decode_text_bytes
    stub(decode_without_icu, "isNamespaceLoaded", FALSE)
    stub(decode_without_icu, "requireNamespace", FALSE)
    for (encoding in names(marks)) {
        mark <- marks[[encoding]]
        payload <- iconv(enc2utf8(text), from = "UTF-8", to = encoding, toRaw = TRUE)[[1L]]
        bytes <- c(mark, payload)
        writeBin(bytes, path)
        expect_true(is_text_file(path))
        expect_identical(read_text_lines(path), expected)
        expect_identical(read_text_lines(path, tolower(encoding)), expected)
        expect_identical(read_text_lines(path, sub("(LE|BE)$", "", encoding)), expected)
        expect_identical(decode_without_icu(bytes, NULL), text)

        writeBin(mark, path)
        expect_true(is_text_file(path))
        expect_identical(read_text_lines(path), "")
    }
    expect_null(text_bom_encoding(as.raw(c(0xef, 0xbb))))
    expect_null(text_bom_encoding(as.raw(c(0, 0, 0xfe))))
    writeBin(c(marks[["UTF-8"]], as.raw(c(0x61, 0, 0x62))), path)
    expect_false(is_text_file(path))
})

test_that("a file BOM overrides the default but respects an explicit conflicting charset", {
    path <- withr::local_tempfile()
    bytes <- as.raw(c(0xef, 0xbb, 0xbf, 0xc3, 0xa9))
    writeBin(bytes, path)
    expect_identical(read_text_lines(path, "ISO-8859-1"), "\u00ef\u00bb\u00bf\u00c3\u00a9")
    expect_identical(read_text_lines(path, "UTF8"), "\u00e9")

    skip_if_not_installed("stringi")
    testthat::local_mocked_bindings(stri_enc_get = function() "ISO-8859-1", .package = "stringi")
    expect_identical(read_text_lines(path), "\u00e9")
    writeBin(as.raw(c(0xff, 0xfe, 0x61, 0)), path)
    expect_identical(read_text_lines(path), "a")
})
