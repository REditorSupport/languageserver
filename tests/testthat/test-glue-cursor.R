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
    query <- function(method, line, text, after = 1L, context = NULL) {
        server$reply <- NULL
        methods[[paste0("textDocument/", method)]](server, 1L,
            list(textDocument = list(uri = uri), position = pos(line, text, after), context = context))
        server$reply$result
    }
    list(workspace = workspace, document = doc, uri = uri, pos = pos, query = query, server = server)
}

test_that("a glue opener starts an incomplete list at the mapped insertion point", {
    expect_true("{" %in% unlist(CompletionOptions$triggerCharacters))
    f <- glue_cursor_fixture(c("local_total <- 42",
            'glue::glue("🦊 {}")', 'glue::glue_data(list(field = 1), "{}")',
            'glue::glue_data(unknown, "{}", field = 2)', 'glue::glue("{{{}}}")',
            'glue::glue("{'))
    trigger <- list(triggerKind = 2L, triggerCharacter = "{")
    old <- lsp_settings$get("max_completions")
    withr::defer(lsp_settings$set("max_completions", old))
    lsp_settings$set("max_completions", 6L)
    for (line in c(2L, 5L, 6L)) {
        after <- if (line == 5L) 3L else 1L
        reply <- f$query("completion", line, "{", after, trigger)
        expect_true(reply$isIncomplete)
        expect_lte(length(reply$items), 6L)
        names <- vapply(reply$items, `[[`, "", "label")
        expect_true("local_total" %in% names)
        pos <- f$pos(line, "{", after)
        item <- reply$items[[match("local_total", names)]]
        expect_identical(item$textEdit$range, range(pos, pos))
    }
    both <- f$query("completion", 3L, "{", context = trigger)
    expect_true(all(c("field", "local_total") %in% vapply(both$items, `[[`, "", "label")))
    masked <- f$query("completion", 4L, "{", context = trigger)
    expect_identical(vapply(masked$items, `[[`, "", "label"), "field")
    expect_true(masked$isIncomplete)
})

test_that("brace triggers outside an interpolation opener yield no R completions", {
    f <- glue_cursor_fixture(c("local_total <- 42", "f <- function(x) { x }",
            '"text {literal}"', 'glue::glue("{{literal}}")', 'glue::glue("{if (TRUE) { 1 }}")',
            'glue::glue("{paste(\'{\')}")', 'glue::glue("{x}", x = "{literal}")',
            'other::glue("{}")', 'f <- function() { glue <- function(...) 1; glue("{}") }',
            'glue::glue("{}", .open = "[")'))
    trigger <- list(triggerKind = 2L, triggerCharacter = "{")
    for (i in list(list(2L, "{"), list(3L, "{"), list(4L, "{"), list(4L, "{{"),
            list(5L, "TRUE) {"), list(6L, "paste(\'{"), list(7L, 'x = "{'),
            list(8L, "{"), list(9L, 'glue("{'), list(10L, "{"))) {
        reply <- f$query("completion", i[[1]], i[[2]], nchar(i[[2L]]), trigger)
        expect_identical(reply, list(isIncomplete = FALSE, items = list()), info = i[[2L]])
    }
})

test_that("an inserted glue opener and its growing identifier complete before the next parse", {
    f <- glue_cursor_fixture(c("local_total <- 42", 'glue::glue("Heading', 'prefix ")'))
    doc <- f$document
    pos <- f$pos(3L, "prefix ", 7L)
    doc$apply_content_changes(2L, list(list(range = range(pos, pos), text = "{}")))
    trigger <- list(triggerKind = 2L, triggerCharacter = "{")
    reply <- f$query("completion", 3L, "{", context = trigger)
    expect_true(reply$isIncomplete)
    expect_true("local_total" %in% vapply(reply$items, `[[`, "", "label"))
    pos <- f$pos(3L, "{")
    doc$apply_content_changes(3L, list(list(range = range(pos, pos), text = "local_to")))
    reply <- f$query("completion", 3L, "local_to", 8L, list(triggerKind = 3L))
    expect_true("local_total" %in% vapply(reply$items, `[[`, "", "label"))
})

test_that("a brace trigger replays against its accepted version for concatenated template inputs", {
    f <- glue_cursor_fixture(c("local_total <- 42", 'glue::glue("label ",', '  "prefix ")'))
    pos <- f$pos(3L, "prefix ", 7L)
    f$document$apply_content_changes(2L, list(list(range = range(pos, pos), text = "{}")))
    expect_null(f$query("completion", 3L, "{", context = list(triggerKind = 2L, triggerCharacter = "{")))
    parsed <- parse_document(f$uri, f$document$content, include_xml = FALSE)
    parsed$version <- 2L
    f$workspace$update_parse_data(f$uri, parsed)
    drain_pending_replies(f$server, f$uri, 2L)
    reply <- f$server$reply$result
    expect_true(reply$isIncomplete)
    expect_true("local_total" %in% vapply(reply$items, `[[`, "", "label"))
})

test_that("an opener reuses only the verified template and its original bindings", {
    insert <- function(lines, near = "here", text = "{}", extra = NULL) {
        f <- glue_cursor_fixture(lines, env = parent.frame())
        n <- length(lines)
        pos <- f$pos(n, near, nchar(near))
        changes <- c(list(list(range = range(pos, pos), text = text)), extra)
        f$document$apply_content_changes(2L, changes)
        point <- f$document$from_lsp_position(pos)
        point$col <- point$col + 1L
        glue_identifier_context(f$workspace, f$document, point)
    }
    for (line in c('"here"', 'ordinary("here")', 'other::glue("here")', 'glue:::glue("here")',
            'glue::glue(text = "here")', 'glue::glue("here", .literal = TRUE)',
            'glue::glue_data("here", "{x}")', 'glue::glue(r"(here)")', 'glue::glue("{paste(\'here\')}")',
            'glue::glue("{if (TRUE) { here }}")')) {
        expect_null(insert(line), info = line)
    }
    expect_null(insert(c("glue <- function(...) NULL", 'glue("here")')))
    expect_null(insert('glue::glue("here")', text = "{{}}"))
    lines <- c("other <- 1", 'glue::glue("here", field = 2)')
    expect_identical(insert(lines, text = "{")$bindings$names, "field")
    expect_null(insert(lines, extra = list(list(range = range(position(0L, 0L), position(0L, 5L)), text = "edited"))))
    expect_null(insert(lines, extra = list(list(range = range(position(1L, 21L), position(1L, 26L)), text = "other"))))
})

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

test_that("verified single-name edits complete in the current buffer before its next parse", {
    f <- glue_cursor_fixture(c(
        "format <- function(local_total) {",
        '  glue::glue("🦊 {lo} done", local_note = 3)', "}"))
    doc <- f$document
    at <- f$pos(2L, "{lo", 3L)
    doc$apply_content_changes(2L, list(list(range = range(at, at), text = "cal_")))
    reply <- f$query("completion", 2L, "local_", 6L)
    expect_true(reply$isIncomplete)
    expect_null(f$server$reply$error)
    labels <- vapply(reply$items, `[[`, "", "label")
    expect_true(all(c("local_total", "local_note") %in% labels))
    item <- reply$items[[match("local_total", labels)]]
    expect_equal(item$textEdit$range, range(f$pos(2L, "{local_", 1L), f$pos(2L, "{local_", 7L)))
    expect_identical(item$data$completion_version, 2L)
    completion_item_resolve(f$server, 2L, item)
    expect_identical(f$server$reply$result$textEdit, item$textEdit)
    expect_identical(f$server$pending_replies$get(f$uri)[["textDocument/completion"]]$size(), 0L)
    expect_null(f$query("definition", 2L, "local_", 5L))
    expect_identical(f$server$pending_replies$get(f$uri)[["textDocument/definition"]]$size(), 1L)
    doc$apply_content_changes(3L, list(list(range = item$textEdit$range, text = "loc")))
    completion_item_resolve(f$server, 2L, item)
    expect_null(f$server$reply$result$data)
    expect_true(f$query("completion", 2L, "{loc", 4L)$isIncomplete)
})

test_that("unrelated changes, moved braces and escaped source cannot reuse glue identifier scope", {
    lines <- c("local_total <- 1", 'glue::glue("{lo}", local_note = 3)')
    for (change in c("moved", "named", "delimiter", "escape", "crossline", "full")) {
        f <- glue_cursor_fixture(lines)
        doc <- f$document
        r <- function(line, text) range(f$pos(line, text, 0L), f$pos(line, text, nchar(text)))
        changes <- switch(change,
            moved = list(list(range = r(2L, "{lo}"), text = "prefix {loc}")),
            named = list(list(range = r(2L, "local_note"), text = "other_note"), list(range = r(2L, "lo}"), text = "loc}")),
            delimiter = list(list(range = r(2L, "{lo}"), text = "{{loc}}")),
            escape = list(list(range = r(2L, "lo}"), text = "lo\\x63}")),
            crossline = list(list(range = r(1L, "local_total"), text = "other"), list(range = r(2L, "lo}"), text = "loc}")),
            full = list(list(text = paste(c(lines[[1L]], 'glue::glue("{loc}", local_note = 3)'), collapse = "\n"))))
        doc$apply_content_changes(2L, changes)
        point <- doc$from_lsp_position(f$pos(2L, if (change == "escape") "lo\\" else "loc", 2L))
        expect_null(glue_identifier_context(f$workspace, doc, point), info = change)
    }
    f <- glue_cursor_fixture(c("outside <- 1", 'glue::glue_data(list(inside = 2), "{i}")'))
    at <- f$pos(2L, "{i}", 2L)
    f$document$apply_content_changes(2L, list(list(range = range(at, at), text = "n")))
    expect_true("inside" %in% vapply(f$query("completion", 2L, "{in}", 3L)$items, `[[`, "", "label"))
})

test_that("mid-token glue edits replace whole backticked names from scope, mask and member providers", {
    f <- glue_cursor_fixture(c(
        "obj <- list(`first name` = 1, first_value = 2)",
        "f <- function(`first name`) {",
        '  glue::glue("🦊 {`firZZ name`} tail")',
        '  glue::glue("🦊 {`firZZ name`} tail", `first name` = 7)',
        '  glue::glue("🦊 {obj$`firZZ name`} tail")',
        '  glue::glue("🦊 {obj$firZZ_value} tail")',
        '  glue::glue("{(function() { `first name` <- function(inner) inner; `firZZ name` })()}", `first name` = 7)', "}"))
    for (line in 3:6) {
        reply <- f$query("completion", line, "firZZ", 3L)
        quoted <- line < 6L
        name <- if (quoted) "first name" else "first_value"
        items <- Filter(function(x) x$label %in% c(name, quote_completion_name(name)), reply$items)
        expect_length(items, 1L)
        item <- items[[1L]]
        if (line == 4L) expect_identical(item$detail, "[glue]")
        if (line %in% 3:4) expect_identical(item$filterText, "`first name`")
        previous <- if (quoted) "`firZZ name`" else "firZZ_value"
        replacement <- if (quoted) "`first name`" else "first_value"
        edit <- item$textEdit
        expect_equal(edit$range, range(f$pos(line, previous, 0L), f$pos(line, previous, nchar(previous))))
        doc <- f$document$clone()
        doc$apply_content_changes(2L, list(list(range = edit$range, text = edit$newText)))
        expect_identical(doc$content[[line]], sub(previous, replacement, f$document$content[[line]], fixed = TRUE))
    }
    inner <- f$query("completion", 7L, "firZZ", 3L)$items
    items <- Filter(function(x) x$label %in% c("first name", "`first name`"), inner)
    expect_length(items, 1L)
    expect_identical(items[[1L]]$kind, CompletionItemKind$Function)
    expect_identical(items[[1L]]$textEdit$newText, "`first name`($0)")
})

test_that("embedded call navigation and argument help look past non-callable glue values", {
    f <- glue_cursor_fixture(c("format_score <- function(value, digits = 1) value + 10",
            'glue::glue("{format_score(3, digits=1)} {format_score}", format_score=4)',
            "d <- list(sum=4)", 'glue::glue_data(d, "{sum(3)} {sum}")', "base::sum(3)",
            'glue::glue("{alias(3)}", alias=format_score)', 'glue::glue("{format_score(3)}", format_score=factory())'))
    expect_equal(f$query("definition", 2L, "{format_score(", 3L)$range$start, f$pos(1L, "format_score", 0L))
    expect_equal(f$query("definition", 2L, "{format_score}", 3L)$range$start, f$pos(2L, "format_score=", 0L))
    expect_match(paste(f$query("hover", 2L, "{format_score(", 3L)$contents, collapse = " "), "format_score\\(value, digits = 1\\)")
    expect_match(f$query("signatureHelp", 2L, "3,", 2L)$signatures[[1L]]$label, "format_score\\(value, digits = 1\\)")
    labels <- vapply(f$query("completion", 2L, "digits=", 3L)$items, "[[", "", "label")
    expect_true("digits" %in% labels)
    expect_equal(f$query("definition", 4L, "{sum(", 2L), f$query("definition", 5L, "sum", 1L))
    expect_equal(f$query("definition", 4L, "{sum}", 2L)$range$start, f$pos(3L, "sum", 0L))
    expect_match(f$query("signatureHelp", 6L, "alias(", 6L)$signatures[[1L]]$label, "^alias\\(value, digits = 1\\)")
    expect_null(f$query("definition", 7L, "{format_score(", 3L))
    expect_length(f$query("signatureHelp", 7L, "score(", 6L)$signatures, 0L)
})

test_that("embedded closure callees distinguish scalar locals from unknown formals", {
    f <- glue_cursor_fixture(c("jump <- function(distance) distance",
            'glue::glue("{(function() { jump <- 4; jump(distance = 2) })()}")',
            'glue::glue("{(function(jump) jump(distance = 2))(something())}")'))
    expect_equal(f$query("definition", 2L, "jump(distance", 2L)$range$start, f$pos(1L, "jump", 0L))
    expect_match(f$query("signatureHelp", 2L, "jump(distance", 5L)$signatures[[1L]]$label, "^jump\\(distance\\)")
    expect_null(f$query("definition", 3L, "jump(distance", 2L))
    expect_length(f$query("signatureHelp", 3L, "jump(distance", 5L)$signatures, 0L)
})
