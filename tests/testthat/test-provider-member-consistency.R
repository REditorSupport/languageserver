test_that("extract and inline preserve replacement assignment semantics", {
    actions <- function(lines, row, selection, kind) {
        fixture <- provider_fixture(lines)
        start <- regexpr(selection, lines[[row + 1L]], fixed = TRUE)[[1L]] - 1L
        stopifnot(start >= 0L)
        refactor_code_actions(fixture$uri, fixture$workspace, fixture$document,
            list(start = list(row = row, col = start), end = list(row = row, col = start + nchar(selection))),
            list(kind))
    }
    for (write in c("box$value <- 2", "box[['value']] <- 2", "2 -> box$value", "names(box) <- 'other'")) {
        lines <- c("f <- function(box) {", paste0("  ", write), "  box", "}")
        out <- actions(lines, 1L, write, "refactor.extract")
        expect_false("Extract selection to function" %in% vapply(out, `[[`, "", "title"))
    }
    for (write in c("box$value <- 2", "alias$value <- 2", "mutate(box)")) {
        lines <- c("f <- function(box) {", "  alias <- box", "  saved <- box$value", paste0("  ", write),
            "  saved", "}")
        expect_length(actions(lines, 2L, "saved", "refactor.inline"), 0L)
    }
    lines <- c("f <- function(box) {", "  saved <- box$value", "  saved", "}")
    expect_length(actions(lines, 1L, "saved", "refactor.inline"), 1L)
    for (use in c("c(bump(), saved)", "ignore(saved)", "box$setter(saved)")) {
        lines[[3L]] <- paste0("  ", use)
        expect_length(actions(lines, 1L, "saved", "refactor.inline"), 0L)
    }
})

test_that("inlay hints follow their member receiver and resolve parameter documentation", {
    fixture <- provider_fixture(c(
        "#' @param first First coordinate.", "draw <- function(first, second) NULL",
        "a <- list(run = draw)", "b <- list(run = function(height, width) NULL)",
        "a$run(1, 2)", "b[['run']](3, 4)", "unknown$run(5, 6)"
    ), formals_resolver = function(...) formals(function(wrong, other) NULL))
    request <- range(position(4L, 0L), position(7L, 0L))
    hints <- inlay_hint_reply(1L, fixture$uri, fixture$workspace, fixture$document, request)$result
    expect_identical(vapply(hints, `[[`, "", "label"), c("first =", "second =", "height =", "width ="))
    tip <- inlay_hint_resolve_reply(1L, fixture$workspace, hints[[1L]])$result$tooltip$value
    expect_match(tip, "First coordinate.", fixed = TRUE)
    fixture$document$version <- 2L
    expect_null(inlay_hint_resolve_reply(1L, fixture$workspace, hints[[1L]])$result$tooltip)
})

test_that("debugger variable lookups use only lexical names", {
    fixture <- provider_fixture(c("box$value + value", "box $ value", "box@value", 'box[["value"]]'))
    for (use_index in c(TRUE, FALSE)) {
        if (!use_index) fixture$document$parse_data$range_data <- NULL
        result <- inline_value_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            range(position(0L, 0L), position(4L, 0L)))$result
        expect_identical(vapply(result, `[[`, "", "variableName"), c("box", "value", "box", "box", "box"))
        expect_equal(result[[2L]]$range$start, position(0L, 12L))
    }
})

test_that("linked editing distinguishes documented methods with the same parameter", {
    fixture <- provider_fixture(c(
        'C <- R6::R6Class("C", public = list(',
        "  #' @param width First method.", "  first = function(width) width,",
        "  #' @param width Second method.", "  second = function(width) width))"
    ))
    linked <- function(line, col) {
        linked_editing_range_reply(1L, fixture$uri, fixture$workspace, fixture$document, position(line, col))$result$ranges
    }
    for (i in c(1L, 3L)) {
        ranges <- linked(i, 14L)
        expect_setequal(vapply(ranges, function(x) x$start$line, integer(1L)), c(i, i + 1L))
        expect_identical(linked(i + 1L, 21L), ranges)
    }
})

test_that("literal double brackets fold through both closers", {
    fixture <- provider_fixture(c("obj[[", '  "field"', "]", "]"))
    parsed <- fixture$document$parse_data
    for (outline in list(parsed$outline_data, NULL)) {
        ranges <- get_document_blocks(parsed$xml_doc, outline)
        expect_equal(ranges, list(list(type = "block", start_line = 1L, end_line = 3L)))
    }
})

test_that("selection range preserves a batch spanning R code and Quarto prose", {
    uri <- "file:///selection.qmd"
    lines <- c("Prose", "```{r}", "box$value", "```")
    doc <- Document$new(uri, language = "quarto", version = 1L, content = lines)
    parsed <- parse_document(uri, lines, is_rmarkdown = TRUE)
    parsed$version <- 1L
    doc$update_parse_data(parsed)
    ws <- list(get_parse_data = function(uri) parsed)
    point <- position(0L, 2L)
    ranges <- selection_range_reply(1L, uri, ws, doc,
        list(list(row = 2L, col = 5L), list(row = 0L, col = 2L)))$result
    expect_length(ranges, 2L)
    expect_false(is.null(ranges[[1L]]$parent))
    expect_equal(ranges[[2L]]$range, range(point, point))
})

test_that("document links follow static compound paths and source siblings", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    file.create(file.path(root, "R", "helper.R"))
    lines <- c('source(file.path("R", "helper.R"))', 'base::sys.source("helper.R", .GlobalEnv)',
        'other::source("helper.R")', 'source(file.path(dynamic, "helper.R"))')
    uri <- path_to_uri(file.path(root, "R", "main.R"))
    doc <- Document$new(uri, language = "r", version = 1L, content = lines)
    parsed <- parse_document(uri, lines)
    parsed$version <- 1L
    doc$update_parse_data(parsed)
    ws <- list(get_parse_data = function(uri) parsed)
    links <- document_link_reply(1L, uri, ws, doc, root)$result
    expect_length(links, 2L)
    expect_equal(links[[1L]]$range, range(position(0L, 7L), position(0L, nchar(lines[[1L]]) - 1L)))
    expected <- normalizePath(file.path(root, "R", "helper.R"))
    expect_equal(vapply(links, function(x) normalizePath(x$data$path), ""), rep(expected, 2L))
})

test_that("document colors include transparent and supported short hex strings", {
    fixture <- provider_fixture('c("transparent", "#abc", "#abcd", "#xyz")')
    colors <- document_color_reply(1L, fixture$uri, fixture$workspace, fixture$document)$result
    short <- tryCatch(!is.null(grDevices::col2rgb("#abc")), error = function(e) FALSE)
    expect_length(colors, if (short) 3L else 1L)
    expect_equal(colors[[1L]]$color$alpha, 0)
    if (short) expect_equal(colors[[3L]]$color$alpha, 221 / 255)
})

test_that("S4 reference requests connect registered names and unshadowed calls", {
    fixture <- provider_fixture(c(
        'methods::setGeneric("render", function(x) standardGeneric("render"))',
        'methods::setMethod("render", "numeric", function(x) x)',
        "render(1)", "local <- function(render) render(1)", 'label <- "render"', "box$render(1)"
    ))
    for (row in 0:2) {
        point <- list(row = row, col = regexpr("render", fixture$document$line0(row), fixed = TRUE)[[1L]])
        refs <- function(declarations) {
            references_reply(1L, fixture$uri, fixture$workspace, fixture$document, point,
                include_declaration = declarations)$result
        }
        expect_identical(vapply(refs(TRUE), function(x) x$range$start$line, integer(1L)), c(0L, 0:2))
        expect_identical(vapply(refs(FALSE), function(x) x$range$start$line, integer(1L)), c(0L, 2L))
    }
})

test_that("definition resolves named source arguments to the method formal", {
    fixture <- provider_fixture(c("a <- list(draw = function(width) width)", "a$draw(width = 4)"))
    reply <- definition_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 1L, col = 9L))
    expect_equal(reply$result$range, range(position(0L, 26L), position(0L, 31L)))
})

test_that("inlay hint dispatch selects the method for positional S3 arguments", {
    fixture <- provider_fixture(c(
        "render <- function(x, ...) UseMethod('render')", "#' @param digits Digits to show.",
        "render.numeric <- function(x, digits = 2, ...) NULL", "render(1, 4)",
        "alias <- render", "alias(2, 5)"
    ))
    fixture$workspace$s3_metadata <- S3MetadataCache$new()
    snapshots <- provider_startup_s3()
    for (name in names(snapshots)) fixture$workspace$s3_metadata$set(name, snapshots[[name]])
    hints <- inlay_hint_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        range(position(3L, 0L), position(6L, 0L)))$result
    expect_identical(vapply(hints, `[[`, "", "label"), rep("digits =", 2L))
    tip <- inlay_hint_resolve_reply(1L, fixture$workspace, hints[[1L]])$result$tooltip$value
    expect_match(tip, "Digits to show.", fixed = TRUE)
})

test_that("LSP member requests and refactorings agree on the current document", {
    skip_on_cran()
    root <- withr::local_tempdir()
    path <- file.path(root, "members.R")
    lines <- c("run <- function() 0", "a <- list(run = function(first, second) first)",
        "b <- list(run = function(height, width) width)", "a $ run(1, 2)", "b $ run(1, 2)",
        'a[["run"]](3, 4)', "f <- function(box) {", "  saved <- box$value",
        "  box$value <- 2", "  saved", "}")
    writeLines(lines, path)
    client <- language_client(root)
    did_open(client, path)
    uri <- path_to_uri(path)
    definition <- respond_definition(client, path, c(3L, 5L))
    expect_equal(definition$range$start$line, 1L)
    edits <- respond_rename(client, path, c(3L, 5L), "apply")$changes[[uri]]
    expect_setequal(vapply(edits, function(x) x$range$start$line, integer(1L)), c(1L, 3L, 5L))
    hints <- respond(client, "textDocument/inlayHint", list(
        textDocument = list(uri = uri), range = range(position(3L, 0L), position(6L, 0L))
    ))
    expect_identical(vapply(hints, `[[`, "", "label"), c("first =", "second =", "height =", "width =", "first =", "second ="))
    items <- respond_prepare_call_hierarchy(client, path, c(3L, 5L))
    expect_equal(items[[1L]]$kind, SymbolKind$Method)
    expect_equal(items[[1L]]$selectionRange, definition$range)
    action <- function(row, start, end, kind) {
        respond(client, "textDocument/codeAction", list(
            textDocument = list(uri = uri), range = range(position(row, start), position(row, end)),
            context = list(diagnostics = list(), only = list(kind))
        ), retry = FALSE)
    }
    actions <- action(8L, 2L, nchar(lines[[9L]]), "refactor.extract")
    expect_false("Extract selection to function" %in% vapply(actions, `[[`, "", "title"))
    expect_length(action(7L, 2L, 7L, "refactor.inline"), 0L)
})

test_that("member hint labels respect exact matches before partial argument tags", {
    f <- provider_fixture(c("obj <- list(draw = function(alpha, alphaLong, omega) NULL)",
            "obj$draw(1, al=2, alpha=3)", "obj$draw(1, al=2)", "obj[['draw']](alpha=3, 1, al=2)"))
    hints <- inlay_hint_reply(1L, f$uri, f$workspace, f$document, range(position(1L, 0L), position(4L, 0L)))$result
    expect_identical(vapply(hints, `[[`, "", "label"), c("omega =", "omega ="))
    expect_identical(vapply(hints, function(h) h$position$line, integer(1L)), c(1L, 3L))
    dots <- provider_fixture(c("obj <- list(draw=function(..., after) NULL)", "obj$draw(1, unknown=2)"))
    expect_length(inlay_hint_reply(1L, dots$uri, dots$workspace, dots$document,
            range(position(1L, 0L), position(2L, 0L)))$result, 0L)
})

test_that("inline debugger lookups and selection use the lexical token range", {
    f <- provider_fixture(c("base::pi + pi", "base:::pi", "`a b`", "box$value"))
    data <- f$document$parse_data
    for (index in c(TRUE, FALSE)) {
        if (!index) data$range_data <- data$syntax_data <- NULL
        hints <- inline_value_reply(1L, f$uri, f$workspace, f$document, range(position(0L, 0L), position(4L, 0L)))$result
        expect_identical(vapply(hints, `[[`, "", "variableName"), c("pi", "a b", "box"))
        expect_equal(hints[[1L]]$range$start, position(0L, 11L))
        select <- selection_range_reply(1L, f$uri, f$workspace, f$document, list(list(row = 3L, col = 4L)))$result
        expect_equal(select[[1L]]$range, range(position(3L, 4L), position(3L, 9L)))
    }
})

test_that("inline keeps eager reads out of lazy call arguments", {
    actions <- function(rhs, use, prefix = character()) {
        f <- provider_fixture(c(prefix, "f <- function(p, obj) {", paste("  saved <-", rhs), paste(" ", use), "}"))
        row <- length(prefix) + 1L
        refactor_code_actions(f$uri, f$workspace, f$document,
            list(start = list(row = row, col = 2L), end = list(row = row, col = 7L)), list("refactor.inline"))
    }
    for (rhs in c("p", "obj + 1", "obj$active", "might_fail()")) {
        expect_length(actions(rhs, "ignore(saved)"), 0L)
        expect_length(actions(rhs, "other <- saved"), 1L)
    }
    expect_length(actions("1 + 2", "ignore(saved)"), 1L)
    expect_length(actions("1 + 2", "ignore(saved)", "`+` <- function(a,b) side_effect()"), 0L)
    expect_length(actions("p", "saved * 2", "`*` <- function(a,b) 0"), 0L)
    expect_length(actions("p", "2 * saved"), 0L)
})
