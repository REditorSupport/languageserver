member_context_fixture <- function(lines) {
    fixture <- provider_fixture(lines, formals_resolver = function(...) {
        list(`a$pos` = NULL, `a@pos` = NULL, pos = 1, mode = quote(c("auto", "manual")))
    })
    fixture$workspace$guess_namespace <- function(...) "base"
    fixture$workspace$loaded_packages <- character()
    fixture$workspace$imported_objects <- collections::dict()
    fixture$workspace$get_namespace <- function(...) NULL
    fixture
}

member_context_items <- function(fixture, row, col) {
    completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = row, col = col), list())$result$items
}

test_that("Member access inside calls does not offer arguments or argument values", {
    old <- lsp_settings$get("member_completion")
    withr::defer(lsp_settings$set("member_completion", old))
    cases <- list(
        list(lines = "get($)", row = 0L, col = 5L),
        list(lines = "get(@)", row = 0L, col = 5L),
        list(lines = "get(unknown$)", row = 0L, col = 12L),
        list(lines = "get(unknown@)", row = 0L, col = 12L),
        list(lines = "get(unknown$pos)", row = 0L, col = 15L),
        list(lines = "get(unknown@pos)", row = 0L, col = 15L),
        list(lines = "get(unknown$`pos`)", row = 0L, col = 17L),
        list(lines = "get(unknown@`pos`)", row = 0L, col = 17L),
        list(lines = "get(unknown $  pos)", row = 0L, col = 18L),
        list(lines = "get(unknown$\tpos)", row = 0L, col = 16L),
        list(lines = c("get(unknown$", "# comment", "  pos)"), row = 2L, col = 5L),
        list(lines = c("get(unknown$", "  `pos`)"), row = 1L, col = 7L),
        list(lines = c("get(unknown@", "  `pos`)"), row = 1L, col = 7L),
        list(lines = c("get(unknown$", "  `a$po`)"), row = 1L, col = 7L),
        list(lines = c("get(unknown@", "  `a@po`)"), row = 1L, col = 7L)
    )
    for (enabled in c(TRUE, FALSE)) {
        lsp_settings$set("member_completion", enabled)
        for (case in cases) {
            fixture <- member_context_fixture(case$lines)
            items <- member_context_items(fixture, case$row, case$col)
            types <- vapply(items, function(item) {
                if (is.null(item$data$type)) "" else item$data$type
            }, "")
            expect_false(any(types %in% c("parameter", "argument_value", "s3_parameter")),
                info = paste(enabled, paste(case$lines, collapse = " ")))
            expect_false(any(vapply(items, function(item) !is.null(item$data$parameter), logical(1L))),
                info = paste(enabled, paste(case$lines, collapse = " ")))
        }
    }
})

test_that("Ordinary and quoted argument prefixes still complete", {
    for (case in list(
        list(text = "get()", col = 4L, label = "pos"),
        list(text = "get(pos)", col = 7L, label = "pos"),
        list(text = "get(`a$po`)", col = 9L, label = "a$pos"),
        list(text = "get(`a@po`)", col = 9L, label = "a@pos")
    )) {
        fixture <- member_context_fixture(case$text)
        items <- member_context_items(fixture, 0L, case$col)
        parameters <- Filter(function(item) identical(item$data$type, "parameter"), items)
        expect_true(case$label %in% vapply(parameters, `[[`, "", "label"), info = case$text)
    }
    fixture <- member_context_fixture("get()")
    items <- member_context_items(fixture, 0L, 4L)
    values <- Filter(function(item) identical(item$data$type, "argument_value"), items)
    expect_setequal(vapply(values, `[[`, "", "label"), c("auto", "manual"))
})

test_that("Known members and slots still complete inside calls", {
    cases <- list(
        list(lines = c("x <- list(alpha = 1)", "get(x$)"), col = 6L, label = "alpha"),
        list(lines = c("setClass('Widget', slots=c(value='numeric'))", "x <- new('Widget')", "get(x@)"),
            col = 6L, label = "value")
    )
    for (case in cases) {
        fixture <- member_context_fixture(case$lines)
        items <- member_context_items(fixture, length(case$lines) - 1L, case$col)
        expect_identical(vapply(items, `[[`, "", "label"), case$label)
        expect_identical(items[[1L]]$data$type, "member")
    }
})

test_that("Member triggers after edits do not offer enclosing call arguments", {
    skip_on_cran()
    root <- withr::local_tempdir()
    path <- file.path(root, "completion.R")
    uri <- path_to_uri(path)
    client <- language_client(working_dir = root)
    did_open(client, path, text = "get()")

    ordinary <- respond_completion(client, path, c(0L, 4L),
        retry_when = function(result) !any(vapply(result$items, function(item) {
            identical(item$data$type, "parameter") && identical(item$label, "pos")
        }, logical(1L))))
    expect_true(any(vapply(ordinary$items, function(item) identical(item$label, "pos"), logical(1L))))
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))

    for (i in seq_along(c("$", "@"))) {
        operator <- c("$", "@")[[i]]
        start <- list(line = 0L, character = 4L)
        after <- list(line = 0L, character = 5L)
        notify(client, "textDocument/didChange", list(
            textDocument = list(uri = uri, version = 2L * i),
            contentChanges = list(list(range = list(start = start, end = start), text = operator))
        ))
        result <- respond(client, "textDocument/completion", list(
            textDocument = list(uri = uri), position = after,
            context = list(triggerKind = 2L, triggerCharacter = operator)
        ), retry = FALSE)
        expect_false(any(vapply(result$items, function(item) {
            identical(item$data$type, "parameter") ||
                identical(item$data$type, "argument_value") || !is.null(item$data$parameter)
        }, logical(1L))), info = operator)
        notify(client, "textDocument/didChange", list(
            textDocument = list(uri = uri, version = 2L * i + 1L),
            contentChanges = list(list(range = list(start = start, end = after), text = ""))
        ))
    }

    member_path <- file.path(root, "member.R")
    member_uri <- path_to_uri(member_path)
    did_open(client, member_path, text = "x <- list(alpha = 1)\nget(x)")
    start <- list(line = 1L, character = 5L)
    after <- list(line = 1L, character = 6L)
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = member_uri, version = 2L),
        contentChanges = list(list(range = list(start = start, end = start), text = "$"))
    ))
    result <- respond(client, "textDocument/completion", list(
        textDocument = list(uri = member_uri), position = after,
        context = list(triggerKind = 2L, triggerCharacter = "$")
    ), retry = FALSE)
    expect_true(any(vapply(result$items, function(item) {
        identical(item$label, "alpha") && identical(item$data$type, "member")
    }, logical(1L))))
})
