owner_reply <- function(f, fun, row, text, ...) {
    col <- regexpr(text, f$document$line0(row), fixed = TRUE)[[1L]]
    stopifnot(col > 0L)
    fun(1L, f$uri, f$workspace, f$document, list(row = row, col = col), ...)
}

test_that("references, highlights and rename distinguish owners across member spellings", {
    f <- provider_fixture(c("run <- function() 0", "a <- list(run = function(arg) arg)",
            "b <- list(run = function() 2)", "a $ run()", "b $ run()", "a$run()",
            'a[["run"]](arg = 3)', "alias <- a", "alias[['run']](1)", '"run"', "run()"))
    for (i in c(1L, 3L, 5L, 6L, 8L)) {
        references <- owner_reply(f, references_reply, i, "run")$result
        expect_equal(vapply(references, function(x) x$range$start$line, integer(1L)), c(1L, 3L, 5L, 6L, 8L))
        uses <- owner_reply(f, references_reply, i, "run", include_declaration = FALSE)$result
        expect_length(uses, 4L)
        highlights <- owner_reply(f, document_highlight_reply, i, "run")$result
        expect_equal(lapply(highlights, `[[`, "range"), lapply(references, `[[`, "range"))
        expect_equal(vapply(highlights, `[[`, numeric(1L), "kind"), c(3, 2, 2, 2, 2))
        expect_equal(owner_reply(f, prepare_rename_reply, i, "run")$result$start$line, i)
        edits <- owner_reply(f, rename_reply, i, "run", "new name")$result$changes[[f$uri]]
        expect_equal(vapply(edits, `[[`, "", "newText"), c(rep("`new name`", 3L), '"new name"', "'new name'"))
    }
    expect_length(owner_reply(f, references_reply, 10L, "run")$result, 2L)
    unknown <- provider_fixture(c("run <- function() 1", "unknown $ run()"))
    expect_null(owner_reply(unknown, rename_reply, 1L, "run", "new")$result)
    expect_true(length(owner_reply(unknown, prepare_rename_reply, 1L, "run")$error) > 0)
})

test_that("renaming source parameters includes the owning call's named tags", {
    f <- provider_fixture(c("f <- function(arg) arg", "g <- function(arg) arg",
            "a <- list(run = function(arg) arg)", "b <- list(run = function(arg) arg)",
            "f(arg = 1)", "g(arg = 2)", "a$run(arg = 1)", 'a[["run"]](arg = 2)', "b$run(arg = 3)"))
    for (row in c(2L, 6L, 7L)) {
        edits <- owner_reply(f, rename_reply, row, "arg", "input")$result$changes[[f$uri]]
        expect_equal(vapply(edits, function(x) x$range$start$line, integer(1L)), c(2L, 2L, 6L, 7L))
    }
    for (row in c(0L, 4L)) {
        refs <- owner_reply(f, references_reply, row, "arg")$result
        expect_equal(vapply(refs, function(x) x$range$start$line, integer(1L)), c(0L, 0L, 4L))
    }
})

test_that("package functions cannot be renamed through qualified or bare calls", {
    f <- provider_fixture(c("stats::median(1)", "median(1)"))
    w <- Workspace$new(NULL)
    w$documents$set(f$uri, f$document)
    w$update_parse_data(f$uri, f$document$parse_data)
    f$workspace <- w
    for (row in 0:1) {
        expect_null(owner_reply(f, rename_reply, row, "median", "middle")$result)
        expect_equal(owner_reply(f, prepare_rename_reply, row, "median")$error$code, -32800L)
    }
})

test_that("method call hierarchy and lenses report the concrete source method", {
    f <- provider_fixture(c("a <- list(run = function(x) x)", "b <- list(run = function(x) x)",
            'api <- list(use = function(x) a[["run"]](x))', "wrapper <- function(x) a $ run(x)",
            "api$use(1)", "b$run(1)"))
    item <- owner_reply(f, prepare_call_hierarchy_reply, 3L, "run")$result[[1L]]
    expect_equal(item$kind, SymbolKind$Method)
    expect_equal(item$range$end$line, 0L)
    expect_equal(item$selectionRange, range(position(0L, 10L), position(0L, 13L)))
    incoming <- indexed_incoming_calls(f$workspace, item)
    expect_setequal(vapply(incoming, function(x) x$from$name, ""), c("use", "wrapper"))
    caller <- incoming[[which(vapply(incoming, function(x) x$from$name == "use", logical(1L)))]]$from
    expect_equal(caller$kind, SymbolKind$Method)
    outgoing <- indexed_outgoing_calls(f$workspace, caller)
    expect_length(outgoing, 1L)
    expect_equal(outgoing[[1L]]$to$selectionRange, item$selectionRange)
    lenses <- code_lens_reply(1L, f$uri, f$workspace, f$document)$result
    methods <- Filter(function(lens) !is.null(lens$data$memberDefinition), lenses)
    expect_equal(vapply(methods, function(lens) resolve_function_code_lens(f$workspace, lens)$command$title, ""),
        c("2 calls", "1 call", "1 call"))
})

test_that("member rename follows writes and stops at a rebound receiver", {
    f <- provider_fixture(c("a <- list(run=1)", "a$run", "a$run <- 2", 'a[["run"]]',
            "a <- list(run=7)", "a$run", "a$run <- 4", "a$run"))
    for (row in c(1L, 2L, 3L, 5L, 6L, 7L)) {
        edits <- owner_reply(f, rename_reply, row, "run", "renamed")$result$changes[[f$uri]]
        expect_equal(vapply(edits, function(x) x$range$start$line, integer(1L)), if (row < 4L) 0:3 else 4:7)
    }
})

test_that("rename rejects member and formal conflicts, dynamic alias keys, and stale peers", {
    for (op in c("alias[[key]]", "other[[key]]", "a[[1L]]")) {
        f <- provider_fixture(c("a <- list(run = 1, occupied = 2)", "other <- list(run = 4)",
                "alias <- a", "a$run", op))
        named <- owner_reply(f, rename_reply, 3L, "run", "occupied")
        expect_equal(named$error$code, ErrorCodes$RequestFailed)
        dynamic <- owner_reply(f, rename_reply, 3L, "run", "renamed")
        if (op == "alias[[key]]") {
            expect_equal(dynamic$error$code, ErrorCodes$RequestFailed)
        } else {
            expect_length(dynamic$result$changes[[f$uri]], 2L)
        }
    }
    f <- provider_fixture(c("box <- list(run = function(first, second) first + second)", "box$run(fir = 1)"))
    expect_equal(owner_reply(f, rename_reply, 1L, "fir", "second")$error$code, ErrorCodes$RequestFailed)
    edits <- owner_reply(f, rename_reply, 0L, "first", "renamed")$result$changes[[f$uri]]
    expect_length(edits, 3L)
    uri <- "file:///peer.R"
    peer <- Document$new(uri, version = 1L, content = "box$run()")
    parsed <- parse_document(uri, peer$content)
    parsed$version <- 1L
    peer$update_parse_data(parsed)
    f$workspace$documents$set(uri, peer)
    peer$set_content(2L, "box$run(")
    expect_equal(owner_reply(f, rename_reply, 1L, "run", "go")$error$code, ErrorCodes$RequestFailed)
})

test_that("inherited R6 calls use the declaring method while overrides stay independent", {
    f <- provider_fixture(c('A <- R6::R6Class("A", public = list(run = function(z) z, use = function() self$run(1)))',
            'B <- R6::R6Class("B", inherit = A)',
            'C <- R6::R6Class("C", inherit = A, public = list(run = function(z) z + 1))',
            "b <- B$new()", "c <- C$new()", "b$run(2)", "c$run(3)"))
    refs <- owner_reply(f, references_reply, 5L, "run")$result
    expect_equal(vapply(refs, function(x) x$range$start$line, integer(1L)), c(0L, 0L, 5L))
    expect_length(owner_reply(f, references_reply, 6L, "run")$result, 2L)
})

test_that("slot references distinguish constructors, helpers and unrelated key strings", {
    f <- provider_fixture(c('methods::setClass("Widget", slots = c(value = "numeric"))',
            'w <- methods::new("Widget", value = 3)', "w@value", 'methods::slot(w, "value")',
            '"value"', 'methods::setClass("Other", slots = c(value = "numeric"))'))
    for (row in 0:3) {
        refs <- owner_reply(f, references_reply, row, "value")$result
        expect_equal(vapply(refs, function(x) x$range$start$line, integer(1L)), 0:3)
        edits <- owner_reply(f, rename_reply, row, "value", "size")$result$changes[[f$uri]]
        expect_equal(vapply(edits, `[[`, "", "newText"), c("size", "size", "size", '"size"'))
    }
})

test_that("member references follow a source edge and exclude unrelated workspace files", {
    root <- index_normalize_path(withr::local_tempdir())
    workspace <- Workspace$new(root)
    open <- function(name, lines) {
        path <- file.path(root, name)
        writeLines(lines, path)
        uri <- path_to_uri(path)
        doc <- Document$new(uri, language = "r", version = 1L, content = lines)
        workspace$documents$set(uri, doc)
        parsed <- parse_document(uri, lines)
        parsed$version <- doc$version
        workspace$update_parse_data(uri, parsed)
        list(uri = uri, document = doc, workspace = workspace)
    }
    api <- open("api.R", "api <- list(run = function(input) input)")
    caller <- open("main.R", c('source("api.R")', "api$run(input = 1)"))
    isolated <- open("standalone.R", c("api <- list(run = function(input) input)", "api$run(input = 2)"))
    for (f in list(api, caller)) {
        row <- if (identical(f$uri, api$uri)) 0L else 1L
        refs <- owner_reply(f, references_reply, row, "run")$result
        expect_setequal(vapply(refs, `[[`, "", "uri"), c(api$uri, caller$uri))
        edits <- owner_reply(f, rename_reply, row, "input", "amount")$result$changes
        expect_setequal(names(edits), c(api$uri, caller$uri))
        expect_length(edits[[api$uri]], 2L)
        expect_length(edits[[caller$uri]], 1L)
        expect_null(edits[[isolated$uri]])
    }
})

test_that("R6 construction calls count for the actual source initializer", {
    f <- provider_fixture(c('A <- R6::R6Class("A", public = list(initialize = function(x) NULL))',
            'B <- R6::R6Class("B", inherit = A)', 'C <- R6::R6Class("C", public = list(initialize = function(y) NULL))',
            "B$new(1)", "C$new(2)"))
    lens <- code_lens_reply(1L, f$uri, f$workspace, f$document)$result
    methods <- Filter(function(x) identical(x$data$symbol, "initialize"), lens)
    expect_length(methods, 2L)
    for (x in methods) expect_equal(resolve_function_code_lens(f$workspace, x)$command$title, "1 call")
    expect_null(owner_reply(f, rename_reply, 3L, "new", "start")$result)
})

test_that("two slot helpers with the same key still resolve their individual receivers", {
    f <- provider_fixture(c('setClass("First", slots = c(value="numeric"))',
            'setClass("Second", slots = c(value="numeric"))', 'a <- new("First")', 'b <- new("Second")',
            'slot(a, "value")', 'slot(b, "value")'))
    for (row in 4:5) {
        refs <- owner_reply(f, references_reply, row, "value")$result
        expect_equal(vapply(refs, function(x) x$range$start$line, integer(1L)), c(row - 4L, row))
    }
})

test_that("member rename protects later writes on list copies and partial or subscript reads", {
    for (access in c("b$taken <- 2", "b$taken", "b$al", 'b[["alpha", exact=TRUE]]', 'b["alpha"]')) {
        f <- provider_fixture(c("a <- list(alpha = 1)", "b <- a", access, "a$alpha"))
        result <- owner_reply(f, rename_reply, 0L, "alpha", "taken")
        expect_equal(result$error$code, ErrorCodes$RequestFailed, label = access)
    }
    for (access in c('b["other"]', 'b[["other", exact=TRUE]]', "b[1L]", 'other["alpha"]',
            "other$al", "other$taken <- 2", "b$also")) {
        f <- provider_fixture(c("a <- list(alpha = 1, also = 3)", "b <- a", "other <- list(alpha = 4)", access, "a$alpha"))
        edits <- owner_reply(f, rename_reply, 0L, "alpha", "taken")$result$changes[[f$uri]]
        expect_equal(vapply(edits, function(edit) edit$range$start$line, integer(1L)), c(0L, 4L), label = access)
    }
})

test_that("member call hierarchy assigns nested helper calls to the helper", {
    f <- provider_fixture(c("a <- list(run = function(arg) arg)", "outer <- function() {",
            "    inner <- function() a$run(42)", "    inner()", "}", "outer()"))
    w <- Workspace$new(NULL)
    w$documents$set(f$uri, f$document)
    w$update_parse_data(f$uri, f$document$parse_data)
    f$workspace <- w
    target <- owner_reply(f, prepare_call_hierarchy_reply, 0L, "run")$result[[1L]]
    outer <- owner_reply(f, prepare_call_hierarchy_reply, 1L, "outer")$result[[1L]]
    incoming <- indexed_incoming_calls(w, target)
    expect_equal(vapply(incoming, function(x) x$from$name, ""), "inner")
    inner <- incoming[[1L]]$from
    expect_equal(inner$selectionRange, range(position(2L, 4L), position(2L, 9L)))
    expect_equal(vapply(indexed_outgoing_calls(w, inner), function(x) x$to$name, ""), "run")
    expect_equal(vapply(indexed_outgoing_calls(w, outer), function(x) x$to$name, ""), "inner")
    expect_equal(vapply(indexed_incoming_calls(w, inner), function(x) x$from$name, ""), "outer")
    top <- indexed_incoming_calls(w, outer)[[1L]]$from
    expect_true(top$data$topLevel)
    top_calls <- vapply(indexed_outgoing_calls(w, top), function(x) x$to$name, "")
    expect_true("outer" %in% top_calls)
    expect_false(any(c("inner", "run") %in% top_calls))
})

test_that("formal rename preserves R argument matching at calls on the same owner", {
    for (call in c("f(1, b=2)", "f(1, beta_long=2)")) {
        f <- provider_fixture(c("f <- function(alpha, beta, ...) alpha + beta", call))
        expect_equal(owner_reply(f, rename_reply, 0L, "alpha", "beta_long")$error$code, ErrorCodes$RequestFailed)
    }
    f <- provider_fixture(c("a <- list(f=function(alpha, beta) alpha + beta)",
            "b <- list(f=function(alpha, beta) alpha + beta)", "a$f(al=1, b=2)", "b$f(1, b=2)"))
    edits <- owner_reply(f, rename_reply, 0L, "alpha", "beta_long")$result$changes[[f$uri]]
    expect_equal(vapply(edits, function(edit) edit$range$start$line, integer(1L)), c(0L, 0L, 2L))
    f <- provider_fixture(c("a <- list(f=function(alpha, beta) alpha + beta)", "a$f(1, b=2)"))
    expect_equal(owner_reply(f, rename_reply, 0L, "alpha", "beta_long")$error$code, ErrorCodes$RequestFailed)
})
