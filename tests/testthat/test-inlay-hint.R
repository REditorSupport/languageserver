test_that("inlay hints name non-obvious positional R arguments", {
    expect_true(ServerCapabilities$inlayHintProvider$resolveProvider)
    fixture <- provider_fixture(
        "mean(values, TRUE, na.rm = FALSE)",
        formals_resolver = function(...) alist(x =, trim = 0, na.rm = FALSE)
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 0L, character = 35L)
        )
    )

    expect_equal(
        vapply(reply$result, `[[`, character(1L), "label"),
        "trim ="
    )
    expect_true(all(vapply(reply$result, `[[`, integer(1L), "kind") == 2L))

    fixture$workspace$get_documentation <- function(...) {
        list(arguments = list(trim = "the fraction of observations to trim."))
    }
    fixture$workspace$get_signature <- function(...) {
        "mean(x, trim = 0, na.rm = FALSE)"
    }
    resolved <- inlay_hint_resolve_reply(
        2L, fixture$workspace, reply$result[[1L]])$result
    expect_equal(
        resolved$tooltip$value,
        paste0(
            "```r\nmean(x, trim = 0, na.rm = FALSE)\n```\n\n",
            "`trim` - the fraction of observations to trim."
        )
    )
})

test_that("inlay hints skip syntax and simple calls", {
    fixture <- provider_fixture(
        c(
            "if (argument) value",
            "fun <- function(argument, second, third) other(argument)",
            "other(argument)",
            "other(argument, second)"
        ),
        formals_resolver = function(...) {
            alist(argument =, second =, third =)
        }
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 3L, character = 30L)
        )
    )

    expect_length(reply$result, 0L)
})

test_that("inlay hints are shown for two supplied arguments", {
    fixture <- provider_fixture(
        "target(one, two)",
        formals_resolver = function(...) alist(first =, second =)
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 0L, character = 16L)
        )
    )

    expect_equal(
        vapply(reply$result, `[[`, character(1L), "label"),
        c("first =", "second =")
    )
})

test_that("inlay hint argument length excludes an initial dot", {
    old_minimum <- lsp_settings$get("inlay_hints_minimum_argument_length")
    withr::defer(lsp_settings$set(
        "inlay_hints_minimum_argument_length",
        old_minimum
    ))
    lsp_settings$set("inlay_hints_minimum_argument_length", 3L)

    fixture <- provider_fixture(
        "target(one, two)",
        formals_resolver = function(...) alist(.ab =, .abc =)
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 0L, character = 16L)
        )
    )

    expect_equal(
        vapply(reply$result, `[[`, character(1L), "label"),
        ".abc ="
    )
})

test_that("inlay hints do not use global formals for member calls", {
    fixture <- provider_fixture(
        c(
            "ResponseErrorMessage$new(",
            "    id,",
            "    errortype = \"RequestCancelled\",",
            "    message = \"Cannot rename the symbol\"",
            ")"
        ),
        formals_resolver = function(...) alist(Class =, ...)
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 4L, character = 1L)
        )
    )

    expect_length(reply$result, 0L)
})

test_that("inlay hints work through the language server", {
    skip_on_cran()
    client <- language_client(capabilities = list(textDocument = list(
        inlayHint = list(resolveSupport = list(properties = list("tooltip")))
    )))
    path <- withr::local_tempfile(fileext = ".R")
    writeLines("stats::rnorm(10, 1, 2)", path)
    client %>% did_open(path)

    hints <- respond(
        client,
        "textDocument/inlayHint",
        list(
            textDocument = list(uri = path_to_uri(path)),
            range = list(
                start = list(line = 0L, character = 0L),
                end = list(line = 0L, character = 24L)
            )
        )
    )
    expect_equal(
        vapply(hints, `[[`, character(1L), "label"),
        c("mean =", "sd =")
    )

    resolved <- respond(client, "inlayHint/resolve", hints[[1L]])
    expect_match(resolved$tooltip$value, "```r\\nrnorm\\(")
    expect_match(resolved$tooltip$value, "`mean` - vector of means")

    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = path_to_uri(path), version = 2L),
        contentChanges = list(list(
            range = range(position(0L, 22L), position(0L, 22L)), text = "\nother("
        ))
    ))
    preserved <- respond(client, "textDocument/inlayHint", list(
        textDocument = list(uri = path_to_uri(path)),
        range = range(position(0L, 0L), position(2L, 0L))
    ), retry = FALSE)
    expect_equal(preserved, hints)
})

test_that("inlay hint helpers handle malformed and empty calls", {
    no_parentheses <- xml2::read_xml("<expr><SYMBOL>x</SYMBOL></expr>")
    reversed <- xml2::read_xml(paste0(
        "<expr><OP-RIGHT-PAREN>)</OP-RIGHT-PAREN>",
        "<OP-LEFT-PAREN>(</OP-LEFT-PAREN></expr>"
    ))
    empty <- provider_fixture("target()")$document$parse_data$xml_doc
    empty_call <- xml_find_first(empty, "//expr[expr/SYMBOL_FUNCTION_CALL]")

    expect_length(call_argument_groups(no_parentheses), 0L)
    expect_length(call_argument_groups(reversed), 0L)
    groups <- call_argument_groups(empty_call)
    expect_length(groups, 1L)
    expect_length(groups[[1L]]$nodes, 0L)

    expect_equal(match_named_formal("alpha", c("alpha", "beta")), 1L)
    expect_equal(match_named_formal("al", c("alpha", "beta")), 1L)
    expect_true(is.na(match_named_formal("a", c("alpha", "alpine"))))
})

test_that("inlay hints handle empty parse data and boundary ranges", {
    fixture <- provider_fixture("target(one, two)")
    fixture$document$parse_data$xml_doc <- NULL
    request <- list(
        start = list(line = 0L, character = 0L),
        end = list(line = 1L, character = 0L)
    )
    expect_length(inlay_hint_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, request
    )$result, 0L)

    fixture <- provider_fixture("value <- 1")
    expect_length(inlay_hint_reply(
        2L, fixture$uri, fixture$workspace, fixture$document, request
    )$result, 0L)
})

test_that("inlay hints validate settings and stop at formal boundaries", {
    old_minimum <- lsp_settings$get("inlay_hints_minimum_arguments")
    old_length <- lsp_settings$get("inlay_hints_minimum_argument_length")
    withr::defer({
        lsp_settings$set("inlay_hints_minimum_arguments", old_minimum)
        lsp_settings$set("inlay_hints_minimum_argument_length", old_length)
    })
    lsp_settings$set("inlay_hints_minimum_arguments", NA_real_)
    lsp_settings$set("inlay_hints_minimum_argument_length", -1L)
    request <- list(
        start = list(line = 0L, character = 0L),
        end = list(line = 0L, character = 100L)
    )

    missing_formals <- provider_fixture("target(one, two)")
    expect_length(inlay_hint_reply(
        1L,
        missing_formals$uri,
        missing_formals$workspace,
        missing_formals$document,
        request
    )$result, 0L)

    too_many <- provider_fixture(
        "target(one, two)",
        formals_resolver = function(...) alist(first =)
    )
    expect_equal(
        vapply(inlay_hint_reply(
            2L, too_many$uri, too_many$workspace, too_many$document, request
        )$result, `[[`, character(1L), "label"),
        "first ="
    )

    dots <- provider_fixture(
        "target(one, two)",
        formals_resolver = function(...) alist(... =)
    )
    expect_length(inlay_hint_reply(
        3L, dots$uri, dots$workspace, dots$document, request
    )$result, 0L)

    outside <- provider_fixture(
        "target(one, two)",
        formals_resolver = function(...) alist(first =, second =)
    )
    outside_request <- request
    outside_request$start$character <- 12L
    expect_equal(
        vapply(inlay_hint_reply(
            4L, outside$uri, outside$workspace, outside$document, outside_request
        )$result, `[[`, character(1L), "label"),
        "second ="
    )
})

test_that("inlay hint resolution tolerates missing metadata and documentation", {
    fixture <- provider_fixture("value <- 1")
    fixture$workspace$get_documentation <- function(...) NULL
    fixture$workspace$get_signature <- function(...) NULL
    unresolved <- list(label = "value =", data = list())
    expect_identical(
        inlay_hint_resolve_reply(1L, fixture$workspace, unresolved)$result,
        unresolved
    )

    hint <- list(
        label = "parameter =",
        data = list(
            functionName = "target",
            parameter = "parameter",
            package = "pkg"
        )
    )
    resolved <- inlay_hint_resolve_reply(2L, fixture$workspace, hint)$result
    expect_equal(
        resolved$tooltip$value,
        "Parameter `parameter` of `pkg::target()`."
    )
})

inlay_hint_edit <- function(fixture, version, changes) {
    document <- fixture$document
    document$apply_content_changes(version, changes)
    parsed <- parse_document(fixture$uri, document$content, document$is_rmarkdown)
    parsed$version <- version
    parsed$xml_doc <- xml2::read_xml(parsed$xml_data)
    document$update_parse_data(parsed)
    parsed
}

test_that("inlay hints survive unrelated incomplete edits and refresh after parsing", {
    fixture <- provider_fixture(
        c("target(one, two)", "", "target(three, four)"),
        function(...) formals(function(first, second) NULL)
    )
    request <- range(position(0L, 0L), position(20L, 0L))
    hints <- function() {
        inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request
        )$result
    }
    original <- hints()
    expect_length(original, 4L)

    parsed <- inlay_hint_edit(fixture, 2L, list(list(
        range = range(position(1L, 0L), position(1L, 0L)), text = "other("
    )))
    expect_true(parsed$parse_error)
    expect_identical(hints(), original)

    parsed <- inlay_hint_edit(fixture, 3L, list(list(
        range = range(position(1L, 6L), position(1L, 6L)), text = "one, two)"
    )))
    expect_false(parsed$parse_error)
    expect_length(hints(), 6L)

    parsed <- inlay_hint_edit(fixture, 4L, list(list(
        range = range(position(0L, 0L), position(3L, 0L)), text = ""
    )))
    expect_false(parsed$parse_error)
    expect_length(hints(), 0L)
})

test_that("inlay hints move untouched calls through sequential Unicode edits", {
    fixture <- provider_fixture(
        c('label <- "\U0001f600"; target(one, two)', "", "target(, two, three)"),
        function(...) formals(function(first, second, third) NULL)
    )
    request <- range(position(0L, 0L), position(20L, 0L))
    hints <- function(request_range = request) {
        inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request_range
        )$result
    }
    original <- hints()
    expect_length(original, 4L)

    parsed <- inlay_hint_edit(fixture, 2L, list(
        list(range = range(position(0L, 0L), position(0L, 0L)), text = "\n"),
        list(range = range(position(1L, 0L), position(1L, 0L)), text = "  "),
        list(range = range(position(2L, 0L), position(2L, 0L)), text = "other(")
    ))
    expect_true(parsed$parse_error)
    expected <- original
    for (i in seq_along(expected)) {
        expected[[i]]$position$line <- expected[[i]]$position$line + 1L
        if (expected[[i]]$position$line == 1L) {
            expected[[i]]$position$character <- expected[[i]]$position$character + 2L
        }
    }
    expect_equal(hints(), expected)
    expect_equal(hints(range(position(3L, 0L), position(4L, 0L))), expected[3:4])
    expect_equal(hints(range(position(0L, 0L), position(1L, 0L))), list())

    parsed <- inlay_hint_edit(fixture, 3L, list(list(
        range = range(position(0L, 0L), position(1L, 2L)), text = "\U00010400 <- 1; "
    )))
    expect_true(parsed$parse_error)
    for (i in seq_along(expected)) {
        expected[[i]]$position$line <- expected[[i]]$position$line - 1L
        if (expected[[i]]$position$line == 0L) {
            expected[[i]]$position$character <- expected[[i]]$position$character + 7L
        }
    }
    expect_equal(hints(), expected)
})

test_that("inlay hints discard changed outer calls while retaining untouched nested calls", {
    fixture <- provider_fixture(
        c("target(one, inner(two, three))", "", "target(four, five)"),
        function(...) formals(function(first, second) NULL)
    )
    request <- range(position(0L, 0L), position(20L, 0L))
    hints <- function() {
        inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request
        )$result
    }
    original <- hints()
    expect_length(original, 6L)
    parsed <- inlay_hint_edit(fixture, 2L, list(list(
        range = range(position(0L, 10L), position(0L, 10L)), text = " +"
    )))
    expect_true(parsed$parse_error)
    expected <- original[3:6]
    expected[[1L]]$position$character <- expected[[1L]]$position$character + 2L
    expected[[2L]]$position$character <- expected[[2L]]$position$character + 2L
    expect_equal(hints(), expected)

    parsed <- inlay_hint_edit(fixture, 3L, list(list(
        range = range(position(2L, 0L), position(2L, 0L)), text = "object$"
    )))
    expect_true(parsed$parse_error)
    expect_equal(hints(), expected[1:2])

    inlay_hint_edit(fixture, 4L, list(list(
        range = range(position(0L, 17L), position(0L, 22L)), text = "other"
    )))
    expect_length(hints(), 0L)
})

test_that("inlay fallback drops calls when prefix replacements change their callee", {
    for (prefix in c("obj$", "obj@", "pkg::", "renamed", "obj$\n")) {
        fixture <- provider_fixture(
            c("x <- target(one, two)", "", "target(three, four)"),
            function(...) formals(function(first, second) NULL)
        )
        request <- range(position(0L, 0L), position(20L, 0L))
        original <- inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request
        )$result
        expect_length(original, 4L)
        # A second edit can fail parsing before the prefix replacement has
        # produced a successful parse and replaced the cached call index.
        fixture$document$apply_content_changes(2L, list(list(
            range = range(position(0L, 0L), position(0L, 5L)), text = prefix
        )))
        added_lines <- length(stringi::stri_split_lines(prefix)[[1L]]) - 1L
        parsed <- inlay_hint_edit(fixture, 3L, list(list(
            range = range(position(1L + added_lines, 0L), position(1L + added_lines, 0L)),
            text = "other("
        )))
        expect_true(parsed$parse_error)
        expected <- original[3:4]
        for (i in seq_along(expected)) {
            expected[[i]]$position$line <- expected[[i]]$position$line + added_lines
        }
        expect_equal(inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request
        )$result, expected, info = prefix)
    }
})

test_that("inlay fallback treats deletions and whitespace at callee boundaries conservatively", {
    fixture <- provider_fixture(
        c("x <-  target(one, two)", "", "target(three, four)"),
        function(...) formals(function(first, second) NULL)
    )
    request <- range(position(0L, 0L), position(20L, 0L))
    original <- inlay_hint_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, request
    )$result
    expect_length(original, 4L)
    fixture$document$apply_content_changes(2L, list(list(
        range = range(position(0L, 0L), position(0L, 5L)), text = "obj$ "
    )))
    parsed <- inlay_hint_edit(fixture, 3L, list(
        list(range = range(position(0L, 5L), position(0L, 6L)), text = ""),
        list(range = range(position(1L, 0L), position(1L, 0L)), text = "other(")
    ))
    expect_true(parsed$parse_error)
    expect_equal(inlay_hint_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, request
    )$result, original[3:4])

    fixture <- provider_fixture(
        c("x <- target(one, two)", "", "target(three, four)"),
        function(...) formals(function(first, second) NULL)
    )
    original <- inlay_hint_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, request
    )$result
    parsed <- inlay_hint_edit(fixture, 2L, list(
        list(range = range(position(0L, 0L), position(0L, 5L)), text = "  "),
        list(range = range(position(1L, 0L), position(1L, 0L)), text = "other(")
    ))
    expect_true(parsed$parse_error)
    original[[1L]]$position$character <- original[[1L]]$position$character - 3L
    original[[2L]]$position$character <- original[[2L]]$position$character - 3L
    expect_equal(inlay_hint_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, request
    )$result, original)
})

test_that("inlay fallback retains local parameter names and honors settings", {
    available <- TRUE
    fixture <- provider_fixture(
        c("target <- function(first, second) NULL", "target(one, two)", ""),
        function(...) if (available) formals(function(first, second) NULL)
    )
    request <- range(position(0L, 0L), position(20L, 0L))
    hints <- function() {
        inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request
        )$result
    }
    original <- hints()
    available <- FALSE
    parsed <- inlay_hint_edit(fixture, 2L, list(list(
        range = range(position(2L, 0L), position(2L, 0L)), text = "other("
    )))
    expect_true(parsed$parse_error)
    expect_identical(hints(), original)

    old_minimum <- lsp_settings$get("inlay_hints_minimum_arguments")
    withr::defer(lsp_settings$set("inlay_hints_minimum_arguments", old_minimum))
    lsp_settings$set("inlay_hints_minimum_arguments", 3L)
    expect_length(hints(), 0L)
    lsp_settings$set("inlay_hints_minimum_arguments", old_minimum)

    inlay_hint_edit(fixture, 3L, list(list(
        range = range(position(2L, 6L), position(2L, 6L)), text = ")"
    )))
    expect_length(hints(), 0L)
})

test_that("inlay fallback clears on full replacement and waits for current parsing", {
    fixture <- provider_fixture("target(one, two)", function(...) formals(function(first, second) NULL))
    request <- range(position(0L, 0L), position(1L, 0L))
    fixture$document$apply_content_changes(2L, list(list(
        range = range(position(0L, 16L), position(0L, 16L)), text = " +"
    )))
    expect_null(inlay_hint_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, request
    ))
    parsed <- inlay_hint_edit(fixture, 3L, list(list(text = "target(")))
    expect_true(parsed$parse_error)
    expect_null(fixture$document$inlay_hint_data)
    expect_length(inlay_hint_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, request
    )$result, 0L)
})

test_that("inlay fallback drops calls affected by comments and strings", {
    for (text in c("#", "\"", "'", "`")) {
        fixture <- provider_fixture(
            c("target(one, two)", "", "target(three, four)"),
            function(...) formals(function(first, second) NULL)
        )
        request <- range(position(0L, 0L), position(20L, 0L))
        original <- inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request
        )$result
        inlay_hint_edit(fixture, 2L, list(list(
            range = range(position(1L, 0L), position(1L, 0L)), text = paste0("other(", text)
        )))
        expect_equal(inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request
        )$result, original[1:2])
    }
})

test_that("inlay fallback preserves hints in an incomplete literate R cell", {
    fixture <- provider_fixture("", function(...) formals(function(first, second) NULL))
    fixture$document <- Document$new(fixture$uri, language = "quarto", version = 1L,
        content = c("```{r}", "target(one, two)", "", "```",
            "```{r}", "target(three, four)", "```"))
    fixture$workspace$documents$set(fixture$uri, fixture$document)
    inlay_hint_edit(fixture, 1L, list())
    request <- range(position(0L, 0L), position(20L, 0L))
    hints <- function() {
        inlay_hint_reply(
            1L, fixture$uri, fixture$workspace, fixture$document, request
        )$result
    }
    original <- hints()
    expect_length(original, 4L)
    parsed <- inlay_hint_edit(fixture, 2L, list(list(
        range = range(position(2L, 0L), position(2L, 0L)), text = "other("
    )))
    expect_false(parsed$parse_error)
    expect_true(parsed$inlay_hint_incomplete)
    expect_equal(hints()[order(vapply(hints(), function(hint) hint$position$line, integer(1L)))],
        original)

    fixture$workspace$get_formals <- function(...) formals(function(first) NULL)
    expect_equal(hints(), c(list(original[[3L]]), original[1:2]))
    fixture$workspace$get_formals <- function(...) formals(function(first, second) NULL)

    # Changing the engine must not leave R hints in a Python cell.
    inlay_hint_edit(fixture, 3L, list(list(
        range = range(position(0L, 4L), position(0L, 5L)), text = "python"
    )))
    expect_equal(hints(), original[3:4])
})
