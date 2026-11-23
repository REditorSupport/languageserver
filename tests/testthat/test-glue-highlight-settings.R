glue_highlight_test_config <- function(functions = list(), enabled = TRUE) {
    settings <- Settings$new()
    settings$set("glue_highlight_functions", functions)
    settings$set("glue_highlighting", enabled)
    glue_highlight_config(settings)
}

glue_highlight_test_fragments <- function(f, functions = list(), enabled = TRUE) {
    glue_highlight_fragments(f$workspace, f$document, f$document$parse_data,
        glue_highlight_test_config(functions, enabled))
}

test_that("glue highlighting settings select valid literal arguments by exact call spelling", {
    cfg <- glue_highlight_config(Settings$new())
    expect_true(cfg$enabled)
    expect_length(cfg$functions, 0L)
    settings <- Settings$new()
    settings$update_from_workspace(list(glue_highlight_functions = list(formatter = 2, "mypkg::render" = "fmt",
                "glue::glue" = FALSE, invalid = list(expr = 1), fractional = 1.4, empty = "", negative = -1,
                many = c(1, 2), "obj$method" = 1, "pkg:::internal" = 1, missing = NA_integer_)))
    cfg <- glue_highlight_config(settings)
    expect_identical(cfg$functions, list(formatter = 2L, "glue::glue" = FALSE, "mypkg::render" = "fmt"))
    expect_setequal(cfg$callees, c("formatter", "render"))
    expect_identical(cfg$overridden, "glue::glue")
    expect_length(glue_highlight_test_config(new.env(parent = emptyenv()))$functions, 0L)
    withr::local_options(languageserver.glue_highlighting = FALSE)
    settings$update_from_options()
    settings$update_from_workspace(list(glue_highlighting = TRUE))
    expect_false(glue_highlight_config(settings)$enabled)
})

test_that("custom glue registrations color only the selected direct literal", {
    f <- provider_fixture(c('notice("{first}", level="{other}", "hi {{literal}} {second}")',
            'pkg::notice("{wrong qualification}", "{skip}")', 'pkg::notify("{other}", fmt="{third}")',
            'other::notify(fmt="{skip}")', 'pkg:::notify(fmt="{skip}")', 'obj$notice("{one}", "{two}")',
            'notify(fmt="{skip}")', 'notice("first", paste("{nested}"))', 'notice("first", r"({raw})")',
            'pkg::notify(f="{partial}")', 'pkg::notify(fmt="{dup1}", fmt="{dup2}")',
            'pkg::notify(fmt="{overridden}", .open="[")', 'payload %>% notice("{piped}")',
            '"{external}" %>% notice(.)', 'pkg::`notify`(fmt="{quoted}")', 'paste("{ordinary}")'))
    selectors <- list(notice = 2L, "pkg::notify" = "fmt")
    expect_null(f$document$parse_data$glue_templates)
    fragments <- glue_highlight_test_fragments(f, selectors)
    expect_identical(vapply(fragments, "[[", "", "content"), c("second", "third", "raw", "piped", "quoted"))
    expect_null(glue_highlight_test_fragments(f, selectors, enabled = FALSE))
    expect_null(glue_fragment_at(f$document$parse_data, fragments[[1L]]$start))
})

test_that("bare and namespaced glue rules can be disabled or overridden independently", {
    f <- provider_fixture(c('glue("{bare}")', 'glue::glue("{qualified}")', 'str_glue("{str}")',
            'glue("{not selected}", tpl="{registered}")'))
    show <- function(config) vapply(glue_highlight_test_fragments(f, config), "[[", "", "content")
    expect_identical(show(list(glue = FALSE)), c("qualified", "str"))
    expect_identical(show(list("glue::glue" = FALSE)), c("bare", "str", "not selected"))
    expect_identical(show(list(glue = "tpl")), c("qualified", "str", "registered"))
    expect_identical(show(list(glue = FALSE, str_glue = FALSE, "glue::glue" = FALSE)), character())
    expect_length(f$document$parse_data$glue_templates, 4L)
})

test_that("native highlighting respects bare callee identity and explicit registered wrappers", {
    f <- provider_fixture(c("glue <- function(x) x", 'glue("{local}")', 'glue::glue("{real}")',
            "local_wrapper <- function(x) x", 'local_wrapper("{custom}")'))
    txt <- function(f, rules = list()) vapply(glue_highlight_test_fragments(f, rules), "[[", "", "content")
    expect_identical(txt(f), "real")
    expect_identical(txt(f, list(local_wrapper = 1L)), c("real", "custom"))
    expect_identical(txt(f, list(glue = 1L)), c("local", "real"))
    other <- provider_fixture(c('glue("{one}{two}")', 'glue::glue("{three}")'))
    reads <- 0L
    other$workspace$guess_namespace <- function(...) {
        reads <<- reads + 1L
        "other"
    }
    expect_identical(txt(other), "three")
    expect_identical(reads, 1L)
})

test_that("custom wrapper maps use the same decoded and physical positions as native glue", {
    line <- 'shell("\U0001f600 {paste(\\"}\\", \\u0076ar)} {second}")'
    f <- provider_fixture(c(line, sub("shell", "glue ", line, fixed = TRUE)))
    fragments <- glue_highlight_test_fragments(f, list(shell = 1L))
    expect_length(fragments, 4L)
    for (i in 1L:2L) {
        custom <- fragments[[i]]
        native <- fragments[[i + 2L]]
        expect_identical(custom$content, native$content)
        expect_identical(custom$start$col, native$start$col)
        expect_identical(custom$end$col, native$end$col)
    }
})

test_that("registered highlights stay in R cells and reuse recognized native maps", {
    lines <- c('Prose shell("{outside}").', "```{r}", 'shell("{inside}")', "```", "```{python}",
        'shell("{python}")', "```", "```{r}", 'glue::glue("{native}")', "```")
    doc <- Document$new("file:///report.qmd", "quarto", 1L, lines)
    parsed <- parse_document(doc$uri, lines, is_rmarkdown = TRUE, include_xml = FALSE)
    parsed$version <- doc$version
    doc$update_parse_data(parsed)
    config <- glue_highlight_test_config(list(shell = 1L))
    got <- glue_highlight_fragments(NULL, doc, parsed, config)
    expect_identical(vapply(got, "[[", "", "content"), c("inside", "native"))
    expect_identical(vapply(got, function(f) f$start$row, 0L), c(2L, 8L))
    expect_identical(glue_highlight_fragments(NULL, doc, parsed, glue_highlight_test_config()), parsed$glue_templates)
    ordinary <- provider_fixture(c('other("{skip}")', "x <- 4"))
    expect_null(glue_highlight_test_fragments(ordinary, list(shell = 1L)))
})

test_that("viewport collection matches all overlapping native and registered fragments", {
    f <- provider_fixture(c('wrap(tpl = "{early}")', 'glue::glue("{a}{b} {', "  nested + 1L", '} {c}")',
            "wrap(", '  unused = "{skip}", tpl = "{before} {', "    deep + 2", '} {after}{more}")',
            'wrap(tpl = "{last}"); glue("{bare}{sibling}")', 'glue::glue("plain")'))
    config <- glue_highlight_test_config(list(wrap = "tpl"))
    collect <- function(lines = NULL) glue_highlight_fragments(f$workspace, f$document, f$document$parse_data, config, lines)
    all <- collect()
    for (from in 0L:9L) for (to in from:9L) {
        selected <- Filter(function(x) x$end$row >= from && x$start$row <= to, all)
        got <- collect(c(from, to))
        expect_identical(lapply(got, identity), lapply(selected, identity))
        if (length(got)) expect_identical(attr(got, "rows"), vapply(selected, function(x) x$start$row, 0L))
    }
})
