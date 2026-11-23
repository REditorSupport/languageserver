test_that("only template arguments of the standard glue and stringr functions contain R", {
    code <- c('glue("one {a}", "{b}", key = "{literal}")', 'glue::glue("{c}")',
        'glue_data("{data}", "{d}")', 'stringr::str_glue("{e}")',
        'str_glue_data("{f}", .x = data)', 'data %>% stringr::str_glue_data("{g}")',
        'other::glue("{skip}")', 'glue:::glue("{skip}")', 'obj$glue("{skip}")',
        'glue_sql("{skip}")', 'glue("{skip}", .open = "[")', 'glue("{skip}", .transformer = f)',
        'glue("{skip}", .literal = FALSE)', 'glue(r"({h})")', 'paste("{skip}")')
    parsed <- parse_document("file:///glue.R", code)
    expect_identical(vapply(parsed$glue_templates, "[[", "", "content"), letters[1:8])
    for (fragment in parsed$glue_templates) {
        expect_true(fragment$complete)
        expect_identical(parsed$syntax_data$token[[fragment$call_row]], "expr")
    }
    expect_null(glue_fragment_at(parsed, list(row = 0L, col = 6L)))
    expect_null(glue_fragment_at(list(), list(row = 0L, col = 0L)))
})

test_that("raw templates map their verbatim bodies to physical source ranges", {
    lines <- c('glue::glue(r"{value = {str1}}")', 'glue::glue(r"(value = {str1})")',
        "stringr::str_glue(R'--[\\u007b prose \\q {{literal}} \U0001f600 {str1}]--')",
        'glue(r"---(first {paste("a\\nb",', '\tstr1)} last)---")',
        'glue(r"{plain}", computed())', 'glue(r"{computed {x}}", computed())',
        'glue(r"{a {split}", "Name} and {joined}")')
    parsed <- parse_document("file:///raw.R", lines)
    fragments <- parsed$glue_templates
    expect_identical(lapply(fragments, "[[", "content"),
        list("str1", "str1", "str1", c('paste("a\\nb",', "\tstr1)"), "joined"))
    for (i in 1L:3L) {
        start <- regexpr("str1", lines[[i]], fixed = TRUE)[[1L]] - 1L
        fragment <- fragments[[i]]
        expect_identical(fragment$start, list(row = i - 1L, col = start))
        expect_identical(fragment$end, list(row = i - 1L, col = start + 4L))
        expect_identical(glue_fragment_at(parsed, fragment$end), fragment)
        expect_null(glue_fragment_at(parsed, list(row = i - 1L, col = start - 1L)))
    }
    inner <- list(row = 1L, col = 1L)
    host <- list(row = 4L, col = 1L)
    expect_identical(glue_source_point(fragments[[4L]], inner), host)
    expect_identical(glue_inner_point(fragments[[4L]], host), inner)
    unmapped <- attr(fragments, "unmapped")
    expect_length(unmapped, 2L)
    expect_null(unmapped[[1L]]$content)
    expect_identical(unmapped[[2L]]$content, "splitName")
})

test_that("glue balances nested braces, ignores literal braces and respects R quotes and comments", {
    code <- c('glue("{{literal}} {{{value}}} {if (ok) {paste(\\"}\\", \'}\', \u0060#}}\u0060)} # }}}',
        ' else other} {}")')
    parsed <- parse_document("file:///braces.R", code)
    fragments <- parsed$glue_templates
    expect_length(fragments, 3L)
    expect_identical(fragments[[1L]]$content, "value")
    expect_identical(fragments[[2L]]$content, c('if (ok) {paste("}", \'}\', \u0060#}}\u0060)} # }}}', " else other"))
    empty <- fragments[[3L]]
    expect_identical(empty$content, "")
    expect_identical(empty$start, empty$end)
    expect_identical(glue_fragment_at(parsed, empty$start), empty)
    expect_null(glue_fragment_at(parsed, list(row = 0L, col = 10L)))
})

test_that("glue maps physical and escaped newlines, quotes, tabs and Unicode in code points", {
    code <- c(paste0('"\U0001f600"; glue("prefix {f(\\"', "\U0001f600", '\\",\\n\\u0078,'),
        '\ty)} tail")')
    parsed <- parse_document("file:///unicode.R", code)
    fragment <- parsed$glue_templates[[1L]]
    expect_identical(fragment$content, c(paste0('f("', "\U0001f600", '",'), "x,", "\ty)"))
    point <- function(row, col) list(row = as.integer(row), col = as.integer(col))
    for (row in seq_along(fragment$content)) {
        for (col in 0L:nchar(fragment$content[[row]])) {
            inner <- point(row - 1L, col)
            host <- glue_source_point(fragment, inner)
            expect_identical(glue_inner_point(fragment, host), inner)
            expect_identical(glue_fragment_at(parsed, host), fragment)
        }
    }
    x <- regexpr("\\u0078", code[[1L]], fixed = TRUE)[[1L]] - 1L
    expect_identical(glue_source_point(fragment, point(1, 0)), point(0, x))
    expect_identical(glue_source_point(fragment, point(1, 1)), point(0, x + 6L))
    expect_null(glue_fragment_at(parsed, point(0, x + 1L)))
    expect_null(glue_source_point(fragment, point(1, 3)))
    expect_null(glue_inner_point(fragment, point(1, 15)))
    for (quote in c('"', "'")) {
        replacement <- paste0("f(", quote, "x", quote, ", a\\b)")
        encoded <- glue_encode_text(list(quote = quote), replacement)
        expect_identical(parse(text = paste0(quote, encoded, quote), keep.source = FALSE)[[1L]], replacement)
    }
})

test_that("glue scans concatenated literals without manufacturing a contiguous source edit", {
    code <- c('glue("{na", "me} and {after}")', 'glue("{", "{literal}} {ok}")',
        'glue("{first}", "", "{second}")', 'glue("{not_known}", get_text())',
        'glue("{custom}", "{sep}", .sep = get_sep())', 'glue("\\u007bbox\\u007d")')
    parsed <- parse_document("file:///joined.R", code)
    expect_identical(vapply(parsed$glue_templates, "[[", "", "content"), c("after", "ok", "first", "second", "box"))
    skipped <- attr(parsed$glue_templates, "unmapped")
    expect_identical(skipped[[1L]]$content, "name")
    expect_true(skipped[[1L]]$complete)
    expect_null(skipped[[2L]]$content)
    expect_null(skipped[[3L]]$content)
    expect_null(glue_fragment_at(parsed, list(row = 0L, col = 8L)))
})

test_that("the active interpolation prefix is available in partial R source and Quarto cells", {
    for (code in list('glue("{abc")', 'glue("{abc"', 'glue("{abc', c("f <- function() {", '  glue("{abc'))) {
        parsed <- parse_document("file:///partial.R", code)
        fragment <- parsed$glue_templates[[1L]]
        expect_identical(fragment$content, "abc")
        expect_false(fragment$complete)
        expect_identical(glue_inner_point(fragment, fragment$end), list(row = 0L, col = 3L))
    }
    fence <- strrep("\u0060", 3L)
    code <- c('glue("{prose}")', paste0(fence, "{r}"), 'glue("{abc', fence, paste0(fence, "{python}"),
        'glue("{foreign}")', fence, paste0(fence, "{r}"), 'glue("{ok}")', fence)
    parsed <- parse_document("file:///report.qmd", code, is_rmarkdown = TRUE)
    expect_identical(vapply(parsed$glue_templates, "[[", "", "content"), c("abc", "ok"))
    expect_identical(vapply(parsed$glue_templates, function(f) f$start$row, integer(1L)), c(2L, 8L))
    expect_identical(vapply(parsed$glue_templates, "[[", logical(1L), "complete"), c(FALSE, TRUE))
})

test_that("provisional calls retain enough callee information for lexical completion only", {
    cases <- list('glue::glue("{tot' = TRUE, 'glue("{tot' = TRUE, 'str_glue("{tot' = TRUE,
        'glue_data(data, "{tot' = FALSE, 'glue(tot = 1, "{tot' = FALSE,
        'glue(.envir = other, "{tot' = FALSE)
    for (line in names(cases)) {
        parsed <- parse_document("file:///typing.R", c("total <- 1", line))
        fragment <- parsed$glue_templates[[1L]]
        expect_identical(fragment$lexical_prefix, cases[[line]])
        expect_identical(fragment$call_row, 0L)
        expect_false(fragment$complete)
        expect_identical(fragment$call_start$row, 1L)
        expect_identical(substr(line, fragment$call_start$col + 1L,
                fragment$call_start$col + nchar(fragment$call_name)), fragment$call_name)
        expect_identical(fragment$package, if (startsWith(line, "glue::")) "glue")
    }
})

test_that("batch mapping and the scanner agree on single-line boundaries", {
    lines <- c('"\U0001f600"; glue("\u03b1 {\u03b2 + 1}")', 'glue("empty {}")', 'glue("{{literal}}")',
        'glue("inner {\'}\'}")', 'glue("{\\u0078}")', 'glue("before {x # }")')
    together <- parse_document("file:///batch.R", lines)$glue_templates
    for (i in seq_along(lines)) {
        # Keep the same physical rows and run the single-call path.
        separate <- parse_document("file:///single.R", c(rep("", i - 1L), lines[[i]]))$glue_templates
        found <- Filter(function(f) f$start$row == i - 1L, together)
        expect_length(found, length(separate))
        for (j in seq_along(separate)) {
            expected <- separate[[j]]
            actual <- found[[j]]
            expect_identical(actual$content, expected$content)
            for (col in seq.int(0L, nchar(expected$content))) {
                at <- list(row = 0L, col = col)
                host <- glue_source_point(expected, at)
                expect_identical(glue_source_point(actual, at), host)
                expect_identical(glue_inner_point(actual, host), at)
            }
        }
    }
})

test_that("unfinished raw glue templates recover the current expression within its file or cell", {
    cases <- list('glue::glue(r"{value = {str1', 'glue::glue(r"(value = {str1',
        c("f <- function() {", "  glue::glue(", "\f  R'--[value =", "  {str1"))
    for (code in cases) {
        parsed <- parse_document("file:///typing.R", code)
        fragment <- parsed$glue_templates[[1L]]
        expect_identical(fragment$content, "str1")
        expect_false(fragment$complete)
        expect_true(fragment$lexical_prefix)
        expect_identical(fragment$end, list(row = length(code) - 1L, col = nchar(code[[length(code)]])))
    }

    fence <- strrep("\u0060", 3L)
    code <- c(paste0(fence, "{r}"), 'glue::glue(r"--(', "value = {str1", fence, "{outside}",
        paste0(fence, "{r}"), 'glue("{next_value}")', fence)
    fragments <- parse_document("file:///raw.qmd", code, is_rmarkdown = TRUE)$glue_templates
    expect_identical(vapply(fragments, "[[", "", "content"), c("str1", "next_value"))
    expect_identical(vapply(fragments, function(f) f$end$row, integer(1L)), c(2L, 6L))
    expect_identical(vapply(fragments, "[[", logical(1L), "complete"), c(FALSE, TRUE))

    for (tail in list(rep("body", 64L), strrep("x", 16384L))) {
        expect_length(parse_document("file:///large.R", c('glue(r"({str1}', tail))$glue_templates, 0L)
    }
})
