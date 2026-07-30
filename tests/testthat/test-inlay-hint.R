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
