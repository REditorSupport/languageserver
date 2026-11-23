glue_cursor_fixture <- function(lines, filename = "example.R", files = list(), env = parent.frame()) {
    root <- normalizePath(withr::local_tempdir(.local_envir = env))
    writeLines(lines, file.path(root, filename))
    for (name in names(files)) writeLines(files[[name]], file.path(root, name))
    uri <- path_to_uri(file.path(root, filename))
    workspace <- testthat::with_mocked_bindings(Workspace$new(root),
        workspace_startup_s3 = provider_startup_s3, .package = "languageserver")
    doc <- Document$new(uri, "r", 1L, lines)
    doc$did_open()
    workspace$documents$set(uri, doc)
    parsed <- parse_document(uri, lines, is_rmarkdown = doc$is_rmarkdown, include_xml = FALSE)
    parsed$version <- doc$version
    workspace$update_parse_data(uri, parsed)
    server <- new.env(parent = emptyenv())
    server$get_workspace <- function(...) workspace
    server$deliver <- function(reply) server$reply <- reply
    server$ClientCapabilities <- list(textDocument = list(completion = list(completionItem = list(snippetSupport = TRUE))))
    methods <- list("textDocument/completion" = text_document_completion, "textDocument/hover" = text_document_hover,
        "textDocument/signatureHelp" = text_document_signature_help, "textDocument/definition" = text_document_definition)
    server$request_handlers <- methods
    server$pending_replies <- collections::dict()
    server$pending_replies$set(uri, lapply(methods, function(f) collections::queue()))
    pos <- function(line, text, after = 1L) {
        i <- regexpr(text, doc$content[[line]], fixed = TRUE)[[1L]]
        stopifnot(i > 0L)
        doc$to_lsp_position(line - 1L, i - 1L + after)
    }
    query <- function(method, line, text, after = 1L) {
        server$reply <- NULL
        methods[[paste0("textDocument/", method)]](server, 1L,
            list(textDocument = list(uri = uri), position = pos(line, text, after)))
        server$reply$result
    }
    list(workspace = workspace, document = doc, uri = uri, pos = pos, query = query, server = server)
}

test_that("cursor requests resolve the inner call and the host lexical scope", {
    f <- glue_cursor_fixture(c(
        "format_total <- function(value, digits = 2) round(value, digits)",
        "fmt <- function(local_total) {",
        "  format_total(local_total, digits = 2)",
        '  glue::glue("Total {format_total(local_total, digits = 2)}")', "}"))
    for (method in c("hover", "definition", "signatureHelp")) {
        name <- if (method == "signatureHelp") ", digits" else "format_total"
        ordinary <- f$query(method, 3L, name, 2L)
        embedded <- f$query(method, 4L, name, 2L)
        if (method == "hover") {
            expect_identical(embedded$contents, ordinary$contents)
            expect_equal(embedded$range$start, f$pos(4L, name, 0L))
        } else if (method == "signatureHelp") {
            expect_identical(embedded, ordinary)
            expect_equal(embedded$activeParameter, 1L)
        } else {
            expect_identical(embedded$uri, ordinary$uri)
            expect_equal(embedded$range$start, ordinary$range$start)
            expect_equal(embedded$range$end, f$pos(1L, "format_total", 12L))
        }
    }
    def <- f$query("definition", 4L, "local_total")
    expect_equal(def$range$start, f$pos(2L, "local_total", 0L))
    items <- f$query("completion", 4L, "local_to", 8L)$items
    item <- Filter(function(x) x$label == "local_total", items)[[1L]]
    expect_equal(item$textEdit$range, range(f$pos(4L, "local_total", 0L), f$pos(4L, "local_total", 11L)))
    completion_item_resolve(f$server, 2L, item)
    expect_identical(f$server$reply$result$textEdit, item$textEdit)
    expect_null(f$query("hover", 4L, "Total"))
})

test_that("glue masks, qualified names, and member origins retain exact source ranges", {
    f <- glue_cursor_fixture(c(
        "score <- 100", "obj <- list(field = 2)",
        'glue::glue("🦊 {alias} {obj$field}", alias = score)',
        'glue::glue_data(list(score = 2), "{score}")',
        'glue::glue_data(unknown, "{score}")', 'glue::glue("{stats::med}")',
        'glue <- function(...) "local"', 'glue("{score}")'))
    expect_equal(f$query("definition", 3L, "alias")$range$start, f$pos(3L, "alias =", 0L))
    items <- f$query("completion", 3L, "$field", 3L)$items
    field <- Filter(function(x) x$label == "field", items)[[1L]]
    expect_equal(field$textEdit$range, range(f$pos(3L, "$field", 1L), f$pos(3L, "$field", 6L)))
    expect_equal(f$query("definition", 3L, "$field", 3L)$range$start, f$pos(2L, "field", 0L))
    expect_equal(f$query("definition", 4L, "{score}", 2L)$range$start, f$pos(4L, "score =", 0L))
    expect_null(f$query("definition", 5L, "{score}", 2L))
    expect_true("median" %in% vapply(f$query("completion", 6L, "med", 3L)$items, `[[`, "", "label"))
    expect_null(f$query("definition", 8L, "score"))
})

test_that("completion edits re-encode quoted R and signatures stay inside the selected fragment", {
    f <- glue_cursor_fixture(c(
        'choose <- function(mode = c("auto", "manual")) mode',
        'glue::glue("{choose(aut)}")', 'glue::glue("{choose(1, }")',
        'glue::glue("{name}")', 'glue::glue("{{name}}")'))
    values <- f$query("completion", 2L, "aut", 3L)$items
    value <- Filter(function(x) identical(x$label, "auto"), values)[[1L]]
    expect_identical(value$textEdit$newText, '\\"auto\\"')
    doc <- f$document
    edit <- value$textEdit
    doc$apply_content_changes(2L, list(list(range = edit$range, text = edit$newText)))
    expect_identical(parse(text = doc$content[[2L]])[[1L]][[2L]], '{choose("auto")}')
    signature <- f$query("signatureHelp", 3L, "1,", 2L)
    expect_null(signature) # the changed document is waiting for its current parse
    data <- parse_document(f$uri, doc$content, include_xml = FALSE)
    data$version <- 2L
    f$workspace$update_parse_data(f$uri, data)
    drain_pending_replies(f$server, f$uri, 2L)
    expect_match(f$server$reply$result$signatures[[1L]]$label, "^choose\\(")
    expect_length(f$query("signatureHelp", 4L, "name")$signatures, 0L)
    expect_null(f$query("definition", 5L, "name"))
})

test_that("embedded navigation and signature help use an unsaved sourced function", {
    f <- glue_cursor_fixture(c('source("helpers.R")', 'glue::glue("{custom_total(1, digits = 2)}")'),
        files = list(helpers.R = "custom_total <- function(value) value"))
    peer <- file.path(f$workspace$root, "helpers.R")
    uri <- path_to_uri(peer)
    lines <- c("# unsaved signature", "custom_total <- function(value, digits) round(value, digits)")
    doc <- Document$new(uri, "r", 3L, lines)
    doc$did_open()
    data <- parse_document(uri, lines, include_xml = FALSE)
    data$version <- doc$version
    f$workspace$documents$set(uri, doc)
    f$workspace$update_parse_data(uri, data)
    expect_identical(f$query("definition", 2L, "custom_total")$uri, uri)
    signature <- f$query("signatureHelp", 2L, ", digits", 2L)
    expect_identical(signature$signatures[[1L]]$label, "custom_total(value, digits)")
    expect_equal(signature$activeParameter, 1L)
})

test_that("glue cursor recovery is confined to an executable R cell and decoded expression", {
    f <- glue_cursor_fixture(c(
        'glue::glue("{mean(1)}")', "```{r}", "name <- 2",
        'glue::glue("text {stats::med}")', 'glue::glue("{mean(1, }")',
        'glue::glue("{paste(\'name\', name)}")', "```"), "example.qmd")
    expect_null(f$query("definition", 1L, "mean"))
    expect_equal(f$query("definition", 6L, ", name", 3L)$range$start, f$pos(3L, "name", 0L))
    expect_null(f$query("hover", 6L, "'name'", 3L))
    expect_true("median" %in% vapply(f$query("completion", 4L, "med", 3L)$items, `[[`, "", "label"))
    signature <- f$query("signatureHelp", 5L, "1,", 2L)
    expect_match(signature$signatures[[1L]]$label, "^mean\\(")
})

test_that("unfinished outer calls give advisory completion and inner signatures", {
    f <- glue_cursor_fixture(c("my_local_name <- 1", 'glue::glue("{my_local_'))
    items <- f$query("completion", 2L, "my_local_", 9L)$items
    expect_true("my_local_name" %in% vapply(items, `[[`, "", "label"))
    expect_null(f$query("definition", 2L, "my_local_"))
    f <- glue_cursor_fixture('glue::glue("{base::sum(1,')
    expect_match(f$query("signatureHelp", 1L, "1,", 2L)$signatures[[1L]]$label, "^sum\\(")
    f <- glue_cursor_fixture(c('glue <- function(...) "ordinary"', 'glue("{my_local_'))
    expect_length(f$query("completion", 2L, "my_local_", 9L)$items, 0L)
})

test_that("named callable bindings mask the argument list of a caller function", {
    f <- glue_cursor_fixture(c(
        "format <- function(caller_argument) NULL",
        'glue::glue("{format(local)}", format = function(local_argument = "local") "")'))
    query <- f$query("completion", 2L, "format(local", 12L)
    labels <- vapply(query$items, `[[`, "", "label")
    expect_true(all(c("local_argument", "local") %in% labels))
    expect_false("caller_argument" %in% labels)
    signature <- f$query("signatureHelp", 2L, "format(", 7L)
    expect_match(signature$signatures[[1L]]$label, "local_argument")
})
