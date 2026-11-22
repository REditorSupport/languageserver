test_that("partial structure preserves physical ranges across an isolated syntax error", {
    content <- c('#line 500 "elsewhere.R"', "first = function(x) x", "broken <- )", "after <- 2")
    parsed <- parse_document("file:///partial.R", content)
    expect_true(parsed$parse_error)
    expect_identical(names(parsed$definitions), c("first", "after"))
    expect_identical(parsed$partial_incomplete_lines, 2L)
    expect_equal(parsed$definitions$first$range$start, position(1L, 0L))
    expect_equal(parsed$definitions$after$range$start, position(3L, 0L))
    expect_identical(parsed$syntax_data$line1[parsed$partial_complete_roots], c(2L, 4L))
})

test_that("complete statements in verified standalone braces survive a sibling's syntax error", {
    uri <- "file:///partial-braces.R"
    content <- c("{", "  {", "    f <- function(arg) {", "      saved <- arg", "      saved * 2",
        "    }", "  }", "  pkg::", "  draw <- function(x) UseMethod('draw')",
        "  methods::setGeneric('display', function(x) standardGeneric('display'))", "}")
    fixture <- provider_fixture(content)
    parsed <- fixture$document$parse_data
    expect_true(parsed$parse_error)
    expect_identical(parsed$partial_incomplete_lines, 7L)
    expect_true(partial_root_contains(parsed, list(row = 3L, col = 6L)))
    inline <- refactor_code_actions(fixture$uri, fixture$workspace, fixture$document,
        list(start = list(row = 3L, col = 6L), end = list(row = 3L, col = 11L)), list("refactor.inline"))
    expect_length(inline, 1L)
    ids <- member_complete_item_ids(parsed)
    expect_identical(vapply(parsed$member_data$items[ids], function(item) item$start[[1L]], integer(1L)),
        c(2L, 8L, 9L))

    disk <- index_shallow_summary(path_from_uri(uri), content, tempdir())
    expect_identical(s3_index_summary(content, uri, parse_data = parsed)$index$functions$draw, disk$s3$index$functions$draw)
    expect_identical(disk$s3$index$functions$draw$name, "draw")
    expect_identical(parsed$s4_declarations, disk$s4_declarations)
    expect_length(disk$s4_declarations$display, 1L)
})

test_that("scope proofs stop at damaged delimiters, functions, calls and conditions", {
    for (head in c("{", "if (flag) {", "local({", "f <- function() {")) {
        end <- if (head == "local({") "})" else "}"
        content <- c(head, "  inside <- 1", "  pkg::", end)
        parsed <- parse_document("file:///partial.R", content)
        expect_identical(partial_root_contains(parsed, list(row = 1L, col = 3L)), head == "{", info = head)
    }
    expr <- parse(text = c("{", "{", "  hidden <- 1", "}", "}"), keep.source = TRUE)
    for (gap in c(0L, 1L, 2L, 3L, 4L)) {
        expect_length(top_level_statements(expr, gap), 0L)
    }
})

test_that("line directives keep definitions in physical coordinates on edit and repair", {
    source <- c('#line 1000 "elsewhere.R"', "first <- function(x) x", "second <- first(1)")
    for (edit in c("", "pkg::", "pkg::median")) {
        parsed <- parse_document("file:///physical.R", c(source, edit))
        expect_identical(names(parsed$definitions), c("first", "second"), info = edit)
        expect_equal(parsed$definitions$first$range$start, position(1L, 0L))
        expect_equal(parsed$definitions$second$range$start, position(2L, 0L))
    }
})

test_that("ordinary #line-prefixed comments preserve physical lexer-error recovery", {
    text <- c("#lineage from the query", "first <- 1", "x <- \"\\q\"")
    srcfile <- srcfilecopy("<text>", text)
    error <- tryCatch(parse(text = text, srcfile = srcfile, keep.source = TRUE), error = identity)
    skip_if(is.null(error$lineno), "This R version does not supply structured lexer positions")
    parsed <- parse_document("file:///lineage.R", text)
    expect_true(parsed$parse_error)
    expect_identical(names(parsed$definitions), "first")
})

test_that("literal tabs survive open-buffer source, signature and member recovery", {
    body <- c('source("a\tb.R")', 'f <- function(arg = "x\ty", missing_arg) {',
        '\tinner <- function() "z\tw"', "  inner()", "}")
    for (literate in c(FALSE, TRUE)) for (gap in c("", "pkg::", "pkg::median")) {
        content <- c(gap, body)
        if (literate) content <- c("```{r}", content, "```")
        uri <- if (literate) "file:///values.qmd" else "file:///values.R"
        parsed <- parse_document(uri, content, is_rmarkdown = literate)
        expect_identical(parsed$source_specs[[1L]]$path, "a\tb.R")
        expect_identical(parsed$member_data$sources[[1L]]$spec$path, "a\tb.R")
        expect_identical(formals(parsed$functions$f)$arg, "x\ty")
        expect_match(parsed$signatures$f, 'arg = "x\\ty"', fixed = TRUE)
        space <- parse_document(uri, gsub("\t", " ", content, fixed = TRUE), is_rmarkdown = literate)
        expect_false(identical(space$content_hash, parsed$content_hash))
        expect_false(identical(space$member_data$content_key, parsed$member_data$content_key))
        # Inspect the worker AST as well as the materialized top-level formals.
        item <- parsed$member_data$items[[length(parsed$member_data$items)]]
        inner <- item$expr[[3L]][[3L]][[2L]][[3L]]
        expect_identical(inner[[3L]], "z\tw")
        expect_true(inherits(inner[[4L]], "srcref"))
        expect_identical(as.integer(inner[[4L]][5L]), 11L)
    }

    long <- paste0(strrep("a", 5000L), "\t", strrep("b", 5000L))
    parsed <- parse_document("file:///long.R", c(sprintf('f <- function(x = "%s") 1', long), "pkg::"))
    expect_identical(formals(parsed$functions$f)$x, long)
})

test_that("physical tab columns do not merge or split equal quoted formal names", {
    for (literate in c(FALSE, TRUE)) for (gap in c("", "pkg::")) {
        for (first in c("a b", "a\\tb")) {
            args <- paste0("f <- function(`", first, "`, `a\tb`) 1")
            content <- c("before <- 1", args, "after <- 2", gap)
            if (literate) content <- c("```{r}", content, "```")
            parsed <- parse_document("file:///formals.R", content, is_rmarkdown = literate)
            expect_true(all(c("before", "after") %in% names(parsed$definitions)))
            if (first == "a b") {
                expect_identical(names(formals(parsed$functions$f)), c("a b", "a\tb"))
            } else {
                expect_false("f" %in% names(parsed$functions))
                expect_true((1L + as.integer(literate)) %in% parsed$partial_incomplete_lines)
            }
        }
    }
    duplicate <- "f <- function(`a\\tb`, `a\tb`) 1"
    content <- c("first <- 1", paste("g <- 2", duplicate, "last <- 3", sep = "; "), "after <- 4")
    parsed <- parse_document("file:///separate.R", content)
    expect_identical(names(parsed$definitions), c("first", "g", "last", "after"))
})

test_that("switching a literal space to a tab invalidates the worker and semantic result IDs", {
    uri <- "file:///value-cache.R"
    space <- c('source("a b.R")', "pkg")
    tab <- sub("a b", "a\tb", space, fixed = TRUE)
    workspace <- Workspace$new(NULL)
    doc <- Document$new(uri, version = 1L, content = space)
    workspace$documents$set(uri, doc)
    self <- list(get_workspace = function(...) workspace,
        resolve_task_manager = list(add_task = function(...) NULL),
        pending_replies = collections::dict(), request_handlers = list())
    first <- parse_document(uri, space)
    parse_callback(self, uri, 1L, first, space)
    testthat::local_mocked_bindings(create_task = function(args, callback, ...) {
        callback(do.call(parse_document, args))
    }, .package = "languageserver")
    doc$set_content(2L, tab)
    parse_task(self, uri, doc)
    expect_identical(doc$parse_data$source_specs[[1L]]$path, "a\tb.R")
    expect_false(identical(doc$parse_data$content_hash, first$content_hash))

    doc$apply_content_changes(3L, list(list(
        range = range(position(1L, 3L), position(1L, 3L)), text = "::")))
    early <- semantic_tokens_full_reply(1L, uri, workspace, doc)$result
    parse_task(self, uri, doc)
    expect_identical(early$resultId, doc$parse_data$content_hash)
    expect_identical(early$data, doc$parse_data$semantic_data$encoded)
    doc$set_content(4L, space)
    parse_task(self, uri, doc)
    expect_identical(doc$parse_data$source_specs[[1L]]$path, "a b.R")
})

test_that("discarding partial syntax preserves ownership of the following statement", {
    # Removing only pkg:: would parse the next assignment as stray's value.
    joined <- parse_document("file:///partial.R", c("known <- 1", "stray <-", "pkg::", "other <- 2"))
    expect_identical(names(joined$definitions), c("known", "other"))

    # The parser stopped inside an unclosed function. A statement that is
    # valid when parsed as a standalone suffix still belongs to that function.
    nested <- parse_document("file:///partial.R", c("known <- 1", "outer <- function() {",
            "  broken <- )", "  local_only <- 2"))
    expect_identical(names(nested$definitions), "known")
    expect_false("local_only" %in% nested$syntax_data$text)
})

test_that("a stray closer in a header preserves ownership of the following body", {
    headers <- c("outer <- function(x))", "if (x))", "while (x))", "for (x in xs))", "repeat )")
    if (getRversion() >= "4.1.0") headers <- c(headers, "outer <- \\(x))")
    for (header in headers) {
        content <- c("before <- 1", header, "    inside <- x", "tail <- 2")
        parsed <- parse_document("file:///headers.R", content)
        syntax <- parsed$syntax_data
        expect_true(parsed$parse_error, info = header)
        expect_identical(syntax$line1[parsed$partial_complete_roots], c(1L, 4L), info = header)
        roots <- syntax$roots[syntax$token[syntax$roots] %in% c("expr", "expr_or_assign_or_help")]
        expect_true(any(syntax$line1[roots] == 2L & syntax$line2[roots] == 3L), info = header)
    }
    # If removing a single closer cannot restore the header, do not publish
    # its unbraced body as a complete global statement.
    parsed <- parse_document("file:///headers.R", c("before <- 1", "outer <- function(x)) )",
            "    inside <- x", "tail <- 2"))
    expect_false("inside" %in% names(parsed$definitions))
})

test_that("discarding malformed syntax cannot reopen a completed function body", {
    for (first in list("f <- function(x) { 1 } bad", "f <- function(x) { 1; 2 } bad",
            c("f <- function(x) {", "  1 } bad"))) {
        content <- c(first, "shared <- x", "}", "tail <- 2")
        parsed <- parse_document("file:///closed-body.R", content)
        # Any returned function must end at the original closing brace.
        f <- parsed$definitions$f
        if (!is.null(f)) expect_lte(f$range$end$line, length(first) - 1L)
        expect_false(any(parsed$syntax_data$token == "expr" & parsed$syntax_data$line1 == 1L &
                    parsed$syntax_data$line2 > length(first)))
    }
})

test_that("partial highlighting overlays agree with the full token classifier", {
    prefix <- c("before <- function(y) y", "second <- 2")
    for (formal in c("arg", "stats")) {
        content <- c(prefix, paste0("f <- function(", formal, ") {"),
            "  stats:: # comment", "  1", "}", "after <- 3")
        parsed <- parse_document("file:///semantic-overlay.R", content)
        # The previous full-buffer token repair blanked just the operator.
        token_source <- content
        token_source[[4L]] <- "  stats   # comment"
        token_tree <- parse(text = token_source, keep.source = TRUE)
        full <- semantic_parse_data(utils::getParseData(token_tree), content)
        area <- range(position(3L, 0L), position(4L, 0L))
        expect_identical(semantic_data_for_range(parsed$semantic_data, area)$encoded,
            semantic_data_for_range(full, area)$encoded, info = formal)
    }
    content <- c(prefix, "f <- function(a) {", "  inner <- function(a) a", '  text <- "first',
        'last"; inner(a)', "}", "after <- function(a) a")
    data <- utils::getParseData(parse(text = content, keep.source = TRUE))
    full <- semantic_parse_data(data, content)
    for (lines in list(4L, 5L, c(3L, 5L), c(1L, 7L))) {
        overlay <- semantic_parse_data(partial_token_context(data, lines), content, only_lines = lines, encode = FALSE)
        for (field in c("lines", "cols", "lengths", "types", "modifiers")) {
            expect_identical(overlay[[field]], full[[field]][full$lines %in% lines])
        }
    }
})

test_that("reusing an unchanged prefix preserves bounded member recovery", {
    content <- c("f <- function(x) {", "  value <- list(first = x)", "  value", "}",
        "a <- list(yes = 1); alias <- a", "g <- function() {", "  input <- alias", "  stats::",
        "  input", "}", "last <- a", "last$")
    summarize <- function(index) {
        lapply(index$items, function(item) {
            list(expr = utils::removeSource(item$expr), start = item$start,
                end = item$end, prefix = item$cache_prefix)
        })
    }
    for (text in list(content, c(content[1:5], "recoveryfixture::object$"),
            c('descriptor <- "start', 'end"; f <- function() {', "  stats::", "}"))) {
        src <- srcfilecopy("<text>", text)
        error <- tryCatch(parse(text = text, srcfile = src, keep.source = TRUE), error = identity)
        recovery <- recover_partial_content(text, src, error)
        actual <- member_document_index(text, recovery$expr, recovery$syntax, recovery$incomplete_lines)
        expected <- member_document_index(text)
        expect_identical(summarize(actual), summarize(expected))
        expect_identical(actual$packages, expected$packages)
        expect_identical(actual$content_key, expected$content_key)
    }
})
