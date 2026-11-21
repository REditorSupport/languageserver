test_that("buffer and disk line splitting preserve Unicode separators and physical EOF", {
    path <- withr::local_tempfile()
    for (separator in c("\n", "\r", "\r\n", "\v", "\f", "\u0085", "\u2028", "\u2029")) {
        text <- paste0("\ufeffa", separator, separator, "\ufeffb", separator)
        writeBin(charToRaw(enc2utf8(text)), path)
        expect_identical(split_document_lines(text), c("a", "", "\ufeffb", ""))
        expect_identical(read_text_lines(path, "UTF-8"), c("a", "", "\ufeffb"))
    }
    writeBin(raw(), path)
    expect_identical(read_text_lines(path), "")
    expect_identical(split_text_lines(""), "")
    expect_identical(split_text_lines(NA_character_), NA_character_)
    expect_identical(split_document_lines("\ufeff\ufeffword"), "\ufeffword")
    expect_identical(split_text_lines("\ufeffword\n\ufeff"), c("\ufeffword", "\ufeff"))
})

test_that("source reads decode whole files and honor the declared charset", {
    path <- withr::local_tempfile()
    # Split a four-byte UTF-8 character across the reader's chunk boundary.
    line <- paste0(strrep("x", 65535L), "\U0001f600")
    writeBin(charToRaw(enc2utf8(paste0(line, "\r\nlast"))), path)
    expect_identical(read_text_lines(path, "UTF-8"), c(line, "last"))

    writeBin(as.raw(c(0xc3, 0xa9)), path)
    expect_identical(read_text_lines(path, "ISO-8859-1"), "\u00c3\u00a9")
    writeBin(as.raw(c(0xff, 0xfe, 0x61, 0, 0x0a, 0)), path)
    expect_identical(read_text_lines(path, "UTF-16LE"), "a")
    writeBin(as.raw(c(0x61, 0xff, 0x62)), path)
    expect_warning(lines <- read_text_lines(path, "UTF-8"))
    expect_identical(lines, "a\ufffdb")
    writeBin(as.raw(c(0x61, 0, 0x62)), path)
    expect_error(read_text_lines(path, "UTF-8"))
    expect_error(suppressWarnings(read_text_lines(paste0(path, ".missing"))))
})

test_that("a configured source charset is respected for valid-looking UTF-8 bytes", {
    skip_if_not_installed("stringi")
    # Some system ICU builds lock the default charset to UTF-8.
    testthat::local_mocked_bindings(stri_enc_get = function() "ISO-8859-1", .package = "stringi")
    path <- withr::local_tempfile()
    writeBin(as.raw(c(0xc3, 0xa9)), path)
    expect_identical(read_text_lines(path), "\u00c3\u00a9")
})

test_that("without ICU a non-UTF-8 locale reads unmarked UTF-8 from disk", {
    withr::local_locale(c(LC_CTYPE = "C"))
    decode <- decode_text_bytes
    stub(decode, "isNamespaceLoaded", FALSE)
    stub(decode, "requireNamespace", FALSE)
    bytes <- as.raw(c(0xc3, 0xa9))
    expect_identical(decode(bytes, NULL), "\u00e9")
    expect_identical(decode(bytes, "ISO-8859-1"), "\u00c3\u00a9")
    # When a single-byte locale is available, exercise both default paths:
    # the same valid UTF-8, and a source genuinely encoded in the native charset.
    locale <- suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.ISO8859-1"))
    if (nzchar(locale) && l10n_info()[["Latin-1"]]) {
        expect_identical(decode(bytes, NULL), "\u00e9")
        expect_identical(decode(as.raw(0xe9), NULL), "\u00e9")
    }
})

test_that("generated text files retain UTF-8 and the platform line separator", {
    path <- withr::local_tempfile()
    write_text_lines(c("\ufeff\u03b1", "", "\ufeff\U0001f600"), path)
    separator <- if (.Platform$OS.type == "windows") "\r\n" else "\n"
    expected <- paste0(c("\u03b1", "", "\ufeff\U0001f600"), separator, collapse = "")
    expect_identical(readBin(path, "raw", n = 100L), charToRaw(enc2utf8(expected)))
    expect_error(write_text_lines(NA_character_, path))
})

test_that("indentation follows Unicode whitespace and newline counts use LF", {
    spaces <- "\t\u0085\u00a0\u2003\u3000"
    expect_identical(leading_whitespace(paste0(spaces, "x")), spaces)
    expect_identical(leading_whitespace(paste0("\ufeff", spaces, "x")), "")
    expect_identical(leading_whitespace("\u180ex"), "")
    expect_identical(leading_whitespace(NA_character_), NA_character_)
    expect_identical(count_newlines(c("\u03b1\r\n\u2028\n", "\r", "", NA)), c(2L, 0L, 0L, NA_integer_))
})

test_that("indentation edits stop at a literal U+FEFF on a later line", {
    document <- Document$new("file:///format.R", version = 1L, content = c("first", "\ufeff  x <- 1", "  "))
    edits <- indentation_only_edits(document, list(row = 2L, col = 2L), list(tabSize = 2L, insertSpaces = TRUE))
    expect_identical(edits, list(text_edit(range(position(2L, 0L), position(2L, 2L)), "")))
})

test_that("BOM removal and document edits preserve a very long single line", {
    text <- paste0(strrep("x", 1000001L), "\u03b2")
    expect_identical(split_document_lines(paste0("\ufeff", text)), text)
    document <- Document$new("file:///long.R", version = 1L, content = text)
    document$apply_content_changes(2L, list(list(range = range(position(0L, 0L), position(0L, 0L)), text = "a")))
    expect_identical(document$content, paste0("a", text))
})

test_that("incremental edits preserve literal U+FEFF and count it in LSP positions", {
    document <- Document$new("file:///literal.R", version = 1L, content = "")
    document$apply_content_changes(2L, list(list(text = "\ufeffone\n\ufeffabc\n\ufeffxyz")))
    expect_identical(document$content, c("one", "\ufeffabc", "\ufeffxyz"))

    document$apply_content_changes(3L, list(
        list(range = range(position(1L, 2L), position(1L, 3L)), text = "X"),
        list(range = range(position(1L, 4L), position(2L, 2L)), text = "\ufeff\n\ufeff")
    ))
    expect_identical(document$content, c("one", "\ufeffaXc\ufeff", "\ufeffyz"))

    document$apply_content_changes(4L, list(
        list(range = range(position(0L, 0L), position(0L, 0L)), text = "\ufeff"),
        list(range = range(position(0L, 2L), position(0L, 2L)), text = "!")
    ))
    expect_identical(document$content[[1L]], "\ufeffo!ne")
})
