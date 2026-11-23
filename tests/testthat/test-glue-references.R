glue_reference_starts <- function(result) {
    sort(vapply(result, function(x) paste(x$range$start$line, x$range$start$character, sep = ":"), ""))
}

test_that("references and highlights share binding identity across glue and ordinary R", {
    f <- provider_fixture(c("score <- 1", "show <- function(score) {",
            '  glue::glue("\U0001f539 {score} {twice(score)}")', "}", "twice <- function(x) x * 2",
            'glue::glue("text score {{score}} {score}")'))
    probe <- function(row, col, include = TRUE) {
        reply <- references_reply(1, f$uri, f$workspace, f$document, list(row = row, col = col), include)
        glue_reference_starts(reply$result)
    }
    expect_identical(probe(0L, 1L), c("0:0", "5:34"))
    expect_identical(probe(5L, 35L, FALSE), "5:34")
    expect_identical(probe(2L, 19L), c("1:17", "2:18", "2:32"))
    for (point in list(list(row = 1L, col = 19L), list(row = 2L, col = 19L))) {
        hits <- document_highlight_reply(1, f$uri, f$workspace, f$document, point)$result
        expect_identical(glue_reference_starts(hits), c("1:17", "2:18", "2:32"))
    }
})

test_that("glue named bindings cascade across delayed arguments and mask data columns", {
    f <- provider_fixture(c("x <- 100", 'glue::glue_data(list(x=1), "{x} {y}", x=x+1, y=x+2)', 'glue::glue("{x}")'))
    sites <- glue_reference_occurrences(f$workspace, f$document, "x")
    expect_setequal(vapply(sites, function(x) x$target$kind, ""), c("field", "named", "lexical"))
    at <- function(kind, embedded) {
        hits <- Filter(function(x) identical(x$target$kind, kind) && identical(!is.null(x$fragment), embedded), sites)
        hits[[1L]]
    }
    field <- at("field", FALSE)
    named <- at("named", TRUE)
    key_at <- function(site) glue_reference_starts(glue_site_locations(f$workspace, f$document, site))
    at_key <- function(key) glue_reference_starts(Filter(function(x) identical(key, x$key), sites))
    expect_identical(key_at(field), at_key(field$key))
    expect_identical(key_at(named), at_key(named$key))
    globals <- references_reply(1, f$uri, f$workspace, f$document, list(row = 0L, col = 1L))$result
    expect_identical(glue_reference_starts(globals), c("0:0", "2:13"))
})

test_that("static source fields work from either side of a pipe and a member access", {
    f <- provider_fixture(c("d <- list(value = 4)", "d$value", 'd %>% glue::glue_data("{value}")'))
    hits <- function(row, col) {
        reply <- references_reply(1, f$uri, f$workspace, f$document, list(row = row, col = col))
        glue_reference_starts(reply$result)
    }
    expect_setequal(hits(2L, 26L), c("0:10", "1:2", "2:24"))
    expect_setequal(hits(1L, 4L), hits(2L, 26L))
})

test_that("inner calls refer to source formals, including declarations in another file", {
    f <- provider_fixture(c("render <- function(value) value", "render(value = 1)"))
    uri <- "file:///glue-caller.R"
    doc <- Document$new(uri, "r", 1L, 'glue::glue("{render(value = 2)}")')
    parsed <- parse_document(uri, doc$content)
    parsed$version <- 1L
    doc$update_parse_data(parsed)
    f$workspace$documents$set(uri, doc)
    targets <- glue_reference_occurrences(f$workspace, doc, "value")
    expect_length(targets, 1L)
    expect_identical(targets[[1L]]$target$uri, f$uri)
    expect_true(targets[[1L]]$target$formal)
    refs <- references_reply(1, uri, f$workspace, doc, list(row = 0L, col = 21L))$result
    expect_setequal(vapply(refs, function(x) x$uri, ""), c(uri, f$uri))
    refs <- references_reply(1, f$uri, f$workspace, f$document, list(row = 0L, col = 20L))$result
    expect_true(uri %in% vapply(refs, function(x) x$uri, ""))
})

test_that("unknown masks, writes, and known shadowed calls stay outside lexical results", {
    f <- provider_fixture(c("x <- 1", 'glue::glue_data(make_data(), "{x}")',
            'glue::glue("{x <- 2}{x}")', "glue <- function(...) NULL", 'glue("{x}")',
            'glue::glue("{x}", .envir = arbitrary())'))
    expect_identical(glue_reference_starts(references_reply(1, f$uri, f$workspace, f$document,
                list(row = 0L, col = 1L))$result), "0:0")
    sites <- glue_reference_occurrences(f$workspace, f$document, "x")
    expect_length(sites, 4L)
    expect_true(all(vapply(sites, function(x) identical(x$target$kind, "unknown"), logical(1L))))
})

test_that("ordinary empty strings and embedded non-symbols have no glue reference site", {
    f <- provider_fixture(c('glue::glue("{1 + 2}")', 'x <- ""'))
    expect_null(glue_reference_site(f$workspace, f$document, list(row = 1L, col = 6L)))
    expect_null(glue_reference_site(f$workspace, f$document, list(row = 0L, col = 15L))$key)
})

test_that("inner callback and assignment callees do not acquire a host function's formal", {
    f <- provider_fixture(c("f <- function(long_name) long_name",
            'glue::glue("{(function(f) f(long_name = 1))(identity)}")',
            'glue::glue("{f <- another}{f(long_name = 1)}")',
            'glue::glue("{f(long_name = 1)}")'))
    targets <- glue_reference_occurrences(f$workspace, f$document, "long_name")
    expect_identical(vapply(targets, function(x) x$target$kind, ""), c("unknown", "unknown", "lexical"))
    refs <- references_reply(1, f$uri, f$workspace, f$document, list(row = 0L, col = 18L))$result
    expect_identical(glue_reference_starts(refs), c("0:14", "0:25", "3:15"))
})

test_that("reference lookup tokenizes candidate calls and retains names in damaged fragments", {
    f <- provider_fixture(c("amount <- 1", sprintf('glue::glue("{other_%03d + 2}")', seq_len(100L)),
            'glue::glue("{amount}")', 'glue::glue("{1 + * amount}")', 'glue::glue("{1Lamount}")'))
    sites <- glue_reference_occurrences(f$workspace, f$document, "amount")
    expect_true(any(vapply(sites, function(x) identical(x$key, "global:amount"), logical(1L))))
    expect_true(any(vapply(sites, function(x) identical(x$target$kind, "unknown"), logical(1L))))
    expect_equal(length(ls(f$document$parse_data$glue_reference_catalog$mapped)), 3L)
})

test_that("multiline delayed arguments contribute their physical names to reference lookup", {
    f <- provider_fixture(c("amount <- 1", 'glue::glue("{tag}",', "  tag = amount)"))
    keys <- glue_reference_occurrences(f$workspace, f$document, "amount")
    expect_length(keys, 1L)
    expect_identical(keys[[1L]]$key, "global:amount")
})

test_that("call heads and their formal tags reference the callable beyond a scalar glue binding", {
    f <- provider_fixture(c("render <- function(input) input", 'glue::glue("{render(input = 1)} {render}", render=4)',
            'glue::glue("{alias(input = 1)}", alias = render)', 'glue::glue("{label}", render=2, label=render(input=3))',
            'glue::glue("{render(input = 1)}", render=factory())', "d <- list(sum=4)", 'glue::glue_data(d, "{sum(1)} {sum}")'))
    hits <- glue_reference_occurrences(f$workspace, f$document, "render")
    heads <- Filter(function(x) x$function_position, hits)
    expect_identical(vapply(heads, function(x) x$target$kind, ""), c("lexical", "lexical", "unknown"))
    expect_identical(vapply(heads[1:2], "[[", "", "key"), rep("global:render", 2L))
    fields <- glue_reference_occurrences(f$workspace, f$document, "sum")
    expect_identical(vapply(fields, function(x) x$target$kind, ""), c("field", "package", "field"))
    expect_identical(fields[[2L]]$key, "package:base:sum")
    tags <- glue_reference_occurrences(f$workspace, f$document, "input")
    embedded <- Filter(function(x) !is.null(x$fragment), tags)
    expect_identical(vapply(embedded, function(x) x$target$kind, ""), c("lexical", "lexical", "unknown"))
    expect_identical(embedded[[1L]]$key, embedded[[2L]]$key)
    expect_true(embedded[[1L]]$target$formal)
})

test_that("call heads and tags in closures skip a scalar local and retain a dynamic formal's uncertainty", {
    f <- provider_fixture(c("jump <- function(distance) distance",
            'glue::glue("{(function() { jump <- 4; jump(distance = 2) })()}")',
            'glue::glue("{(function(jump) jump(distance = 2))(something())}")'))
    heads <- Filter(function(x) x$function_position, glue_reference_occurrences(f$workspace, f$document, "jump"))
    expect_identical(vapply(heads, function(x) x$target$kind, ""), c("lexical", "unknown"))
    expect_identical(heads[[1L]]$key, "global:jump")
    tags <- glue_reference_occurrences(f$workspace, f$document, "distance")
    expect_identical(vapply(tags, function(x) x$target$kind, ""), c("lexical", "unknown"))
    expect_true(tags[[1L]]$target$formal)
})
