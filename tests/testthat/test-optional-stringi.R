test_that("ASCII matching and UTF-8 file paths do not probe optional ICU", {
    testthat::local_mocked_bindings(has_stringi = function() stop("Unnecessary ICU lookup"))
    expect_true(is_ascii_string("LanguageServer"))
    expect_true(is_ascii_vector(c("ABC", "a_Z", NA_character_)))
    expect_false(is_ascii_vector(c("abc", "\u00e9")))
    expect_identical(match_with(c("SERVER", NA_character_), "rve"), c(TRUE, FALSE))
    expect_identical(fuzzy_find(c("LanguageServer", NA_character_), "LS"), c(TRUE, FALSE))

    path <- withr::local_tempfile()
    writeBin(charToRaw(enc2utf8("x\u00e9\U0001f680")), path)
    expect_true(is_text_file(path))
})

test_that("Unicode completion has a literal, case-insensitive fallback", {
    testthat::local_mocked_bindings(has_stringi = function() FALSE)
    withr::local_locale(c(LC_CTYPE = "C"))
    latin1 <- rawToChar(as.raw(c(0xc9, 0x78)))
    Encoding(latin1) <- "latin1"
    values <- c(latin1, "\u00e9\n\\E", "\U0001f680\\E", NA_character_)
    expect_identical(match_with(values, "\u00e9"), c(TRUE, TRUE, FALSE, FALSE))
    expect_identical(fuzzy_find(values, "\u00c9\\E"), c(FALSE, TRUE, FALSE, FALSE))
    expect_identical(fuzzy_find(values, "\U0001f680\\E"), c(FALSE, FALSE, TRUE, FALSE))
    expect_identical(match_with(values, "\\E"), c(FALSE, TRUE, TRUE, FALSE))
    expect_identical(fuzzy_find(c("\u4e2d-----\u6587", "\u4e2d"), "\u4e2d\u6587"), c(TRUE, FALSE))

    single <- match_with("\u0131", "i")
    expect_identical(match_with(c("\u0131", "\uffff"), "i"), c(single, FALSE))

    invalid <- rawToChar(as.raw(0xff))
    Encoding(invalid) <- "UTF-8"
    expect_false(match_with("text", invalid))
    expect_false(fuzzy_find("text", invalid))

    noncharacter <- rawToChar(as.raw(c(0xef, 0xbf, 0xbf)))
    Encoding(noncharacter) <- "UTF-8"
    expect_identical(match_with(c("\u00e9", noncharacter), noncharacter), c(FALSE, TRUE))
})

test_that("Unicode completion uses the installed ICU case folding rules", {
    skip_if_not_installed("stringi")
    values <- c("\u00df", "ss", "s_s", NA_character_)
    expect_identical(fuzzy_find(values, "\u00df"), c(TRUE, TRUE, FALSE, FALSE))
    expected <- stringi::stri_detect_fixed(values, "\u00df", case_insensitive = TRUE)
    expected[is.na(expected)] <- FALSE
    expect_identical(match_with(values, "\u00df"), expected)
})

test_that("file path detection without ICU recognizes Unicode text and BOMs", {
    testthat::local_mocked_bindings(has_stringi = function() FALSE)
    path <- withr::local_tempfile()
    probe <- function(bytes, n = 1000L) {
        writeBin(as.raw(bytes), path)
        is_text_file(path, n)
    }
    expect_true(probe(raw()))
    expect_true(probe(c(0xef, 0xbb, 0xbf, 0x61)))
    expect_true(probe(c(0xff, 0xfe, 0x61, 0)))
    expect_true(probe(c(0xfe, 0xff, 0, 0x61)))
    expect_true(probe(c(0xff, 0xfe, 0, 0, 0x61, 0, 0, 0)))
    expect_true(probe(c(0, 0, 0xfe, 0xff, 0, 0, 0, 0x61)))
    expect_false(probe(c(0x61, 0, 0x62)))
    expect_false(probe(c(0x61, 0)))
    expect_false(probe(c(0xc0, 0xaf)))
    expect_false(probe(c(0xed, 0xa0, 0x80)))
    expect_false(probe(c(0x63, 0x61, 0x66, 0xe9)))
    expect_true(probe(c(0x61, 0xc3, 0xa9), n = 2L))
    expect_true(probe(c(0x61, 0xc3, 0xa9), n = 3L))
})

test_that("a file sample finishes the last UTF-8 character before classifying it", {
    path <- withr::local_tempfile()
    probe <- function(bytes, n = 1000L) {
        writeBin(bytes, path)
        is_text_file(path, n)
    }
    three <- charToRaw(enc2utf8("\u6e2c"))
    four <- charToRaw(enc2utf8("\U0001f680"))
    prefix <- rep(three, 333L)
    testthat::local_mocked_bindings(has_stringi = function() FALSE)
    expect_true(probe(rep(three, 400L)))
    for (used in 1:3) {
        expect_true(probe(c(charToRaw("a"), four), n = 1L + used))
    }
    expect_true(probe(c(prefix, three, as.raw(0xff))))
    expect_false(probe(c(prefix, three[1:2])))
    expect_false(probe(c(prefix, three[1], as.raw(c(0x80, 0xff)))))
    expect_false(probe(c(prefix, three[1], as.raw(0), three[2:3])))
    expect_false(probe(c(prefix, as.raw(0xff))))
})

test_that("ICU can recognize unmarked legacy text for file path navigation", {
    skip_if_not_installed("stringi")
    # An unmarked Japanese sentence encoded in Shift_JIS.
    sentence <- as.raw(c(
        0x82, 0xb1, 0x82, 0xea, 0x82, 0xcd, 0x93, 0xfa, 0x96, 0x7b, 0x8c, 0xea,
        0x82, 0xcc, 0x95, 0xb6, 0x8f, 0xcd, 0x82, 0xc5, 0x82, 0xb7, 0x81, 0x42
    ))
    path <- withr::local_tempfile()
    writeBin(rep(sentence, 40L), path)
    expect_true(is_text_file(path))
    testthat::local_mocked_bindings(has_stringi = function() FALSE)
    expect_false(is_text_file(path))
})
