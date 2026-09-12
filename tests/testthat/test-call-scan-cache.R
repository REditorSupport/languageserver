new_test_call_scan_cache <- function() {
    .Call("new_bracket_scan_cache_c", PACKAGE = "languageserver")
}

test_cached_bracket_scan <- function(content, row, col, cache, skip = FALSE) {
    .Call("find_unbalanced_bracket_cached_c", content, row, col, skip, cache,
        PACKAGE = "languageserver")
}

test_that("cached call scans preserve multiline lexical and stopping rules", {
    prefixes <- list(
        c("outer(", "inner("),
        c("outer(", "inner() # ) ' \" ("),
        c("outer(", ""),
        c("outer(", "'unterminated"),
        c("outer(", 'r"--[raw ) ( \"]--"'),
        c("outer(", 'r"--[unterminated ) ( \"'),
        c("outer(", "`name(with)brackets`"),
        c("outer(", paste0("`", intToUtf8(0x1f600), "`")),
        c("outer({", "inner([")
    )
    endings <- c("value", "value2", "), value", "]), value", ")}), value", "fn(value)")
    for (prefix in prefixes) {
        cache <- new_test_call_scan_cache()
        for (skip in c(FALSE, TRUE, FALSE)) {
            for (ending in endings) {
                content <- c(prefix, ending)
                row <- length(content) - 1L
                for (col in seq.int(0L, nchar(ending))) {
                    expected <- find_unbalanced_bracket(content, row, col, skip)
                    expect_identical(test_cached_bracket_scan(content, row, col, cache, skip), expected)
                    expect_identical(test_cached_bracket_scan(content, row, col, cache, skip), expected)
                }
            }
        }
    }
})

test_that("cached call scans match uncached scans after random edits", {
    withr::local_seed(463L)
    pieces <- c("f(", "g()", "([", "])", "{", "}", "#", " ", "'", '"',
        "`", "\\", 'r"--[', ']--"', "x", intToUtf8(0x1f600), intToUtf8(0x03b1))
    random_line <- function() paste0(sample(pieces, sample.int(12L, 1L), replace = TRUE), collapse = "")
    for (iteration in seq_len(50L)) {
        cache <- new_test_call_scan_cache()
        content <- vapply(seq_len(12L), function(i) random_line(), character(1L))
        actual <- expected <- vector("list", 20L)
        for (edit in seq_len(20L)) {
            changed <- if (edit %% 4L) length(content) else sample.int(length(content), 1L)
            content[[changed]] <- random_line()
            if (edit %% 5L == 0L) content <- append(content, random_line(), after = 2L)
            if (edit %% 7L == 0L) content <- content[-3L]
            row <- if (edit %% 3L) length(content) - 1L else sample.int(length(content), 1L) - 1L
            col <- sample.int(nchar(content[[row + 1L]]) + 1L, 1L) - 1L
            skip <- edit %% 2L == 0L
            expected[[edit]] <- find_unbalanced_bracket(content, row, col, skip)
            actual[[edit]] <- test_cached_bracket_scan(content, row, col, cache, skip)
        }
        expect_identical(actual, expected, info = paste("edit sequence", iteration))
    }
})

test_that("document call caches follow edits independently of stale parse data", {
    document <- Document$new("file:///call-scan-cache.R", version = 1L,
        content = c("outer(", "value"))
    document$parse_data <- list(version = 1L)
    expect_equal(document$detect_call(list(row = 1L, col = 5L))$token, "outer")
    cache <- document$call_scan_cache
    document$apply_content_changes(2L, list(list(
        range = range(position(1L, 5L), position(1L, 5L)), text = "s"
    )))
    expect_equal(document$detect_call(list(row = 1L, col = 6L))$token, "outer")
    expect_identical(document$call_scan_cache, cache)
    expect_equal(document$parse_data$version, 1L)

    document$set_content(3L, c("replacement(", "value"))
    expect_equal(document$detect_call(list(row = 1L, col = 5L))$token, "replacement")
    document$set_content(4L, c("replacement(", ") value"))
    expect_equal(document$detect_call(list(row = 1L, col = 7L))$token, "")
    document$set_content(5L, c("third(", "", "value"))
    expect_equal(document$detect_call(list(row = 2L, col = 5L))$token, "third")
    document$set_content(6L, c("third(", '"unterminated', "value"))
    expect_equal(document$detect_call(list(row = 2L, col = 5L))$token, "")
    document$set_content(7L, "value")
    expect_equal(document$detect_call(list(row = 0L, col = 5L))$token, "")
})

test_that("serialized call caches rebuild their native state", {
    content <- c("outer(", "value")
    cache <- new_test_call_scan_cache()
    expected <- find_unbalanced_bracket(content, 1L, 4L)
    expect_identical(test_cached_bracket_scan(content, 1L, 4L, cache), expected)
    restored <- unserialize(serialize(cache, NULL))
    expect_identical(test_cached_bracket_scan(content, 1L, 4L, restored), expected)
    expect_identical(test_cached_bracket_scan(content, 1L, 4L, restored), expected)
    expect_error(test_cached_bracket_scan(content, 1L, 4L, NULL), "invalid bracket scan cache")
})
