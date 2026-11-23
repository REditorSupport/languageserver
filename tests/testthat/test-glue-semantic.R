glue_semantic_fixture <- function(lines) {
    source <- normalize_parse_content(lines)
    data <- utils::getParseData(parse(text = source, keep.source = TRUE))
    templates <- glue_template_index(syntax_parse_data(data), lines)
    list(raw = semantic_parse_data(data, source), fragments = templates$fragments)
}

glue_semantic_spans <- function(tokens, lines) {
    spelling <- vapply(seq_along(tokens$lines), function(i) {
        text <- lines[[tokens$lines[[i]] + 1L]]
        pts <- code_point_from_unit(text, c(tokens$cols[[i]], tokens$cols[[i]] + tokens$lengths[[i]]))
        substr(text, pts[[1L]] + 1L, pts[[2L]])
    }, "")
    data.frame(spelling = spelling, type = names(SemanticTokenTypes)[tokens$types + 1L], modifier = tokens$modifiers)
}

test_that("glue semantic coloring splits prose from ordinary R tokens and delimiters", {
    lines <- c('glue::glue("a {{literal}} {sum(x, 1L)} and {if (ok) obj$value else TRUE} end")', 'paste("{still a string}")')
    f <- glue_semantic_fixture(lines)
    original <- f$raw
    overlay <- semantic_glue_overlay(f$raw, f$fragments, lines)
    spans <- glue_semantic_spans(overlay, lines)
    expect_identical(spans$spelling, c("glue", "::", "glue", '\"a {{literal}} ', "{", "sum", "(", "x", ",", "1L", ")", "}", " and ",
            "{", "if", "(", "ok", ")", "obj", "$", "value", "else", "TRUE", "}", ' end\"', "paste", '\"{still a string}\"'))
    expect_identical(spans$type[5L:12L], c("operator", "function", "operator", "variable", "operator", "number", "operator", "operator"))
    expect_identical(spans$type[19L:23L], c("variable", "operator", "property", "keyword", "keyword"))
    expect_identical(spans$modifier[23L], semantic_readonly_default_modifier)
    expect_identical(spans$type[c(4L, 13L, 25L, 27L)], rep("string", 4L))
    expect_identical(f$raw, original)
    expect_identical(semantic_glue_overlay(f$raw, rev(f$fragments), lines), overlay)
    expect_identical(overlay$encoded, encode_semantic_tokens(lapply(seq_along(overlay$lines), function(i) {
        list(line = overlay$lines[[i]], col = overlay$cols[[i]], length = overlay$lengths[[i]],
            tokenType = overlay$types[[i]], tokenModifiers = overlay$modifiers[[i]])
    }))$data)
})

test_that("glue tokens follow physical escaped and UTF-16 boundaries", {
    emoji <- intToUtf8(0x1f419)
    lines <- c(paste0('glue("', emoji, ' {paste(\\"', emoji, '\\", \\u03b1, 2)} tail")'),
        'glue("\\u007bsum(n, 2)\\u{007d}")', 'glue("{n +\\nm}")')
    f <- glue_semantic_fixture(lines)
    overlay <- semantic_glue_overlay(f$raw, f$fragments, lines)
    spans <- glue_semantic_spans(overlay, lines)
    on <- function(i) spans[overlay$lines == i, ]
    expect_identical(on(0L)$spelling, c("glue", paste0('\"', emoji, " "), "{", "paste", "(", paste0('\\"', emoji, '\\"'),
            ",", "\\u03b1", ",", "2", ")", "}", ' tail\"'))
    expect_identical(on(0L)$type[c(6L, 8L)], c("string", "variable"))
    expect_identical(overlay$cols[overlay$lines == 0L][[3L]], 9L)
    expect_identical(on(1L)$spelling, c("glue", '\"', "\\u007b", "sum", "(", "n", ",", "2", ")", "\\u{007d}", '\"'))
    expect_identical(on(1L)$type[c(3L, 10L)], c("operator", "operator"))
    expect_identical(on(2L)$spelling, c("glue", '\"', "{", "n", "+", "m", "}", '\"'))
    expect_true(all(overlay$lengths > 0L))
})

test_that("glue classification preserves multiline scopes, comments, and range boundaries", {
    lines <- c('"ordinary prefix"', 'glue("begin {', "  function(arg) {", "", "    arg[[1]] + 2 # ignore } here",
        "  }", '} end")', '"ordinary suffix"')
    f <- glue_semantic_fixture(lines)
    full <- semantic_glue_overlay(f$raw, f$fragments, lines)
    spans <- glue_semantic_spans(full, lines)
    expect_identical(spans$type[spans$spelling == "arg"], c("parameter", "parameter"))
    expect_identical(spans$modifier[spans$spelling == "arg"], c(semantic_declaration_modifier, 0L))
    expect_identical(spans$type[spans$spelling == "# ignore } here"], "comment")
    expect_identical(spans$type[spans$spelling == "[["], "operator")
    viewport <- range(position(1L, 0L), position(7L, 0L))
    part <- semantic_glue_overlay(semantic_data_for_range(f$raw, viewport), f$fragments, lines)
    expect_identical(part$encoded, semantic_data_for_range(full, viewport)$encoded)
    incomplete <- semantic_data_for_range(f$raw, range(position(3L, 0L), position(7L, 0L)))
    expect_identical(semantic_glue_overlay(incomplete, f$fragments, lines), incomplete)
})

test_that("damaged or discontiguous glue expressions preserve their ordinary string color", {
    lines <- c('glue("{good} {broken +} {tail}")', 'glue("{unclosed")', 'glue("{wo", "rd}")', 'glue("{}")')
    f <- glue_semantic_fixture(lines)
    overlay <- semantic_glue_overlay(f$raw, f$fragments, lines)
    spans <- glue_semantic_spans(overlay, lines)
    expect_identical(spans$spelling[spans$type == "variable"], c("good", "tail"))
    expect_true(all(c(" {broken +} ", '\"{unclosed\"', '\"{wo\"', '\"rd}\"') %in% spans$spelling[spans$type == "string"]))
    expect_identical(spans$spelling[overlay$lines == 3L], c("glue", '\"', "{", "}", '\"'))
    expect_identical(semantic_glue_overlay(f$raw, list(), lines), f$raw)
})

test_that("a batched parse preserves independent expression meaning across chunks", {
    expressions <- c(sprintf("f(v%d, n = %d)", 1:135, 1:135), "", "1 +", "FALSE", "function(p) p", "# text")
    batch <- glue_semantic_expressions(as.list(expressions))
    separately <- lapply(expressions, function(text) {
        expr <- parse_provider_projection(text)
        if (!is.null(expr)) glue_semantic_classify(text, utils::getParseData(expr))
    })
    ordered <- function(m) if (!is.null(m)) m[order(m[, "line"], m[, "from"]), , drop = FALSE]
    expect_identical(unname(lapply(batch, ordered)), unname(lapply(separately, ordered)))
})

test_that("recovered glue calls color only delimiters present in the physical file", {
    for (lines in list('glue::glue("{sum(x)}"', c('glue::glue("{sum(x)}")', "other::"))) {
        doc <- parse_document("file:///incomplete.R", lines, include_xml = FALSE)
        withr::local_options(keep.parse.data = FALSE)
        result <- semantic_glue_overlay(doc$semantic_data, doc$glue_templates, lines)
        spans <- glue_semantic_spans(result, lines)
        expect_identical(spans$type[spans$spelling == "x"], "variable")
        expect_true("sum" %in% spans$spelling[spans$type == "function"])
        expect_false(getOption("keep.parse.data"))
    }
})

test_that("only proven reads of enclosing R formals receive the parameter token", {
    lines <- c("host <- function(arg, `two words`) {", '  glue::glue("{arg} {sum(arg)}")',
        '  glue::glue("{arg}", arg = 42)', '  glue::glue_data(list(arg = 2), "{arg}")',
        '  glue::glue_data(unknown_source(), "{arg}")', '  glue::glue("{stats::arg + obj$arg + arg}")',
        '  glue::glue("{(function(arg) arg)(arg)}")', '  glue::glue("{arg <- 2}{arg}")',
        '  glue::glue("{`two words`}")', '  custom("{arg}")', "}", 'glue::glue("{arg}")')
    f <- provider_fixture(lines)
    p <- f$document$parse_data
    custom <- list(enabled = TRUE, functions = list(custom = 1L), callees = "custom", overridden = character())
    fragments <- glue_highlight_fragments(f$workspace, f$document, p, custom)
    tokens <- semantic_glue_overlay(p$semantic_data, fragments, lines, f$workspace, f$document)
    spans <- glue_semantic_spans(tokens, lines)
    arg <- function(i, name = "arg") spans$type[tokens$lines == i & spans$spelling == name]
    expect_identical(arg(1L), c("parameter", "parameter"))
    expect_identical(arg(2L)[[1L]], "variable")
    expect_identical(utils::tail(arg(3L), 1L), "variable")
    expect_identical(arg(4L), "variable")
    expect_identical(arg(5L), c("variable", "property", "parameter"))
    expect_identical(arg(6L), rep("parameter", 3L))
    expect_identical(spans$modifier[tokens$lines == 6L & spans$spelling == "arg"], c(semantic_declaration_modifier, 0L, 0L))
    expect_identical(arg(7L), c("variable", "variable"))
    expect_identical(arg(8L, "`two words`"), "parameter")
    expect_identical(arg(9L), "variable")
    expect_identical(arg(11L), "variable")
    rows <- vapply(fragments, `[[`, 0L, "call_row")
    all <- glue_reference_occurrences(f$workspace, f$document, "arg")
    subset <- glue_reference_occurrences(f$workspace, f$document, "arg", rows[c(1L, 4L)])
    expect_identical(subset, Filter(function(x) x$call_row %in% rows[c(1L, 4L)], all))
})

test_that("formal highlighting respects narrower host bindings and fast call boundaries", {
    lines <- c("outer <- function(x) {", "  inner <- function() {", "    x <- 2", '    glue::glue("{x}")', "  }",
        '  glue::glue("{sum(x, n = 2)}")', '  glue::glue("{(function(y) x)(1)}")', "}",
        'glue::glue("{x}")')
    f <- provider_fixture(lines)
    p <- f$document$parse_data
    overlay <- semantic_glue_overlay(p$semantic_data, p$glue_templates, lines, f$workspace, f$document)
    spans <- glue_semantic_spans(overlay, lines)
    selected <- spans$spelling == "x" & overlay$lines %in% c(3L, 5L, 6L, 8L)
    expect_identical(spans$type[selected], c("variable", "parameter", "parameter", "variable"))
})

test_that("simple calls color a repeated formal without per-call scope resolution", {
    lines <- c("f <- function(x) {", rep('  glue::glue("{sum(x, x)}")', 20L), "}")
    f <- provider_fixture(lines)
    p <- f$document$parse_data
    testthat::local_mocked_bindings(glue_reference_occurrences = function(...) stop("unexpected general lookup"))
    overlay <- semantic_glue_overlay(p$semantic_data, p$glue_templates, lines, f$workspace, f$document)
    spans <- glue_semantic_spans(overlay, lines)
    expect_identical(spans$type[spans$spelling == "x"], rep("parameter", 41L))
})

test_that("simple mask lookups reuse decoded value spans with multiple enclosing formals", {
    lines <- c("f <- function(x, y) {", '  glue::glue_data(list(x = 2), "{sum(x, y)}")',
        '  glue::glue("{sum(x, y)}", y = x)', "}")
    f <- provider_fixture(lines)
    p <- f$document$parse_data
    testthat::local_mocked_bindings(glue_reference_occurrences = function(...) stop("unexpected general lookup"))
    overlay <- semantic_glue_overlay(p$semantic_data, p$glue_templates, lines, f$workspace, f$document)
    spans <- glue_semantic_spans(overlay, lines)
    for (i in 1L:2L) {
        at <- which(overlay$lines == i & spans$spelling == "sum")
        types <- spans$type[at + c(2L, 4L)]
        expect_identical(types, if (i == 1L) c("variable", "parameter") else c("parameter", "variable"))
    }
})
