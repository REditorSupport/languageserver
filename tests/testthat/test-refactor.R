refactor_test_range <- function(content, row, text) {
    start <- regexpr(text, content[[row + 1L]], fixed = TRUE)[[1L]] - 1L
    stopifnot(start >= 0L)
    list(
        start = list(row = row, col = start),
        end = list(row = row, col = start + nchar(text))
    )
}

refactor_test_offset <- function(document, point) {
    point <- document$from_lsp_position(point)
    preceding <- if (point$row > 0L) {
        sum(nchar(document$content[seq_len(point$row)])) + point$row
    } else {
        0L
    }
    preceding + point$col
}

refactor_apply_edits <- function(content, edits) {
    document <- Document$new("file:///apply-refactor.R", content = content)
    starts <- vapply(edits, function(edit) {
        refactor_test_offset(document, edit$range$start)
    }, numeric(1L))
    ends <- vapply(edits, function(edit) {
        refactor_test_offset(document, edit$range$end)
    }, numeric(1L))
    order <- order(starts, ends, decreasing = TRUE)
    text <- paste(content, collapse = "\n")
    for (i in order) {
        before <- if (starts[[i]] > 0L) {
            substr(text, 1L, starts[[i]])
        } else {
            ""
        }
        after <- if (ends[[i]] < nchar(text)) {
            substr(text, ends[[i]] + 1L, nchar(text))
        } else {
            ""
        }
        text <- paste0(before, edits[[i]]$newText, after)
    }
    text
}

refactor_action_with_title <- function(actions, title) {
    titles <- vapply(actions, `[[`, character(1L), "title")
    actions[[match(title, titles)]]
}

# Expand the emitted subset of the LSP snippet grammar, allowing the user to
# rename tab stop 1. Escaped dollar signs and braces stay literal source text.
refactor_render_snippet <- function(text, name = NULL) {
    result <- character()
    i <- 1L
    size <- nchar(text)
    while (i <= size) {
        token <- substr(text, i, i)
        if (token == "\\") {
            result <- c(result, substr(text, i + 1L, i + 1L))
            i <- i + 2L
        } else if (substr(text, i, i + 3L) == "${1:") {
            i <- i + 4L
            default <- character()
            while (i <= size && substr(text, i, i) != "}") {
                if (substr(text, i, i) == "\\") i <- i + 1L
                default <- c(default, substr(text, i, i))
                i <- i + 1L
            }
            if (is.null(name)) name <- paste(default, collapse = "")
            result <- c(result, name)
            i <- i + 1L
        } else if (substr(text, i, i + 1L) == "$1") {
            stopifnot(!is.null(name))
            result <- c(result, name)
            i <- i + 2L
        } else if (substr(text, i, i + 1L) == "$0") {
            i <- i + 2L
        } else {
            stopifnot(token != "$")
            result <- c(result, token)
            i <- i + 1L
        }
    }
    paste(result, collapse = "")
}

refactor_apply_snippet_edits <- function(content, edits, name = NULL) {
    edits <- lapply(edits, function(edit) {
        if (!is.null(edit$snippet)) {
            edit$newText <- refactor_render_snippet(edit$snippet$value, name)
        }
        edit
    })
    refactor_apply_edits(content, edits)
}

test_that("extract actions keep pipe RHS expressions in their call context", {
    skip_if(getRversion() < "4.1.0", "Native pipes require R 4.1")
    for (pipe in c("|>", "%>%")) {
        expression <- paste("x", pipe, "sum(1)")
        content <- c("outer <- function(x) {", paste0("  ", expression), "}")
        fixture <- provider_fixture(content)
        rhs <- refactor_test_range(content, 1L, "sum(1)")
        expect_length(refactor_code_actions(fixture$uri, fixture$workspace,
                fixture$document, rhs, list("refactor.extract")), 0L)

        whole <- refactor_test_range(content, 1L, expression)
        actions <- refactor_code_actions(fixture$uri, fixture$workspace,
            fixture$document, whole, list("refactor.extract"))
        expect_length(actions, 2L)
        for (action in actions) {
            actual <- refactor_apply_edits(content, action$edit$changes[[fixture$uri]])
            env <- new.env(parent = asNamespace("magrittr"))
            eval(parse(text = actual), env)
            expect_equal(env$outer(5), 6)
        }
    }
    content <- c("outer <- function(x) {", "  x %>% sum(. + 1)", "}")
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 1L, ". + 1")
    expect_length(refactor_code_actions(fixture$uri, fixture$workspace,
            fixture$document, selected, list("refactor.extract")), 0L)
})

test_that("extract actions allow independent expressions inside pipe arguments", {
    skip_if(getRversion() < "4.1.0", "Native pipes require R 4.1")
    cases <- list(
        list("x |> sum(1 + 2)", "1 + 2", 8),
        list("x %>% sum(1 + 2)", "1 + 2", 8),
        list("x %<>% sum(1 + 2)", "1 + 2", 8),
        list("x %T>% sum(1 + 2)", "1 + 2", 5),
        list("x %>% sum(., 1 + 2)", "1 + 2", 8),
        list('x %>% sum(nchar(".") + 1)', 'nchar(".") + 1', 7),
        list("x |> sum(. + 1)", ". + 1", 8)
    )
    if (getRversion() >= "4.2.0") {
        cases <- c(cases, list(list("x |> sum(1 + 2, data = _)", "1 + 2", 8)))
    }
    for (case in cases) {
        content <- c("outer <- function(x) {", "  . <- 2", paste0("  ", case[[1L]]), "}")
        fixture <- provider_fixture(content)
        selected <- refactor_test_range(content, 2L, case[[2L]])
        actions <- refactor_code_actions(fixture$uri, fixture$workspace,
            fixture$document, selected, list("refactor.extract"))
        expect_equal(length(actions), 2L, info = case[[1L]])
        for (action in actions) {
            actual <- refactor_apply_edits(content, action$edit$changes[[fixture$uri]])
            env <- new.env(parent = asNamespace("magrittr"))
            eval(parse(text = actual), env)
            expect_equal(env$outer(5), case[[3L]], info = case[[1L]])
        }
    }
})

test_that("extract actions retain pipe placeholders and data-mask bindings", {
    cases <- list(list("x %$% sum(value + 1)", "value + 1"))
    for (pipe in c("%>%", "%<>%", "%T>%")) {
        cases <- c(cases, list(
            list(paste("x", pipe, "sum(identity(.))"), "identity(.)"),
            list(paste("x", pipe, "sum(. + 1)"), ". + 1"),
            list(paste("x", pipe, "sum(`.` + 1)"), "`.` + 1"),
            list(paste("x", pipe, "list(., .(1))"), ".(1)"),
            list(paste("x", pipe, "sum(1 + 2)"), "sum(1 + 2)")
        ))
    }
    if (getRversion() >= "4.2.0") {
        cases <- c(cases, list(list("x |> sum(data = _)", "_")))
    }
    for (case in cases) {
        content <- c("outer <- function(x) {", paste0("  ", case[[1L]]), "}")
        fixture <- provider_fixture(content)
        selected <- refactor_test_range(content, 1L, case[[2L]])
        actions <- refactor_code_actions(fixture$uri, fixture$workspace,
            fixture$document, selected, list("refactor.extract"))
        expect_equal(length(actions), 0L, info = case[[1L]])
    }
})

test_that("extract variable creates a previewable collision-free edit", {
    content <- c(
        "outer <- function(a) {",
        "  extracted_value <- 0",
        "  result <- sqrt(a + 1)",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 2L, "a + 1")

    actions <- refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract")
    )
    action <- refactor_action_with_title(
        actions, "Extract expression to variable")

    expect_equal(action$kind, "refactor.extract")
    expect_named(action$edit, "changes")
    actual <- refactor_apply_edits(
        content, action$edit$changes[[fixture$uri]])
    expect_equal(actual, paste(c(
        "outer <- function(a) {",
        "  extracted_value <- 0",
        "  extracted_value_2 <- a + 1",
        "  result <- sqrt(extracted_value_2)",
        "}"
    ), collapse = "\n"))
    expect_silent(parse(text = actual))
})

test_that("refactor workspace edits are versioned when the client supports them", {
    content <- c(
        "outer <- function(argument) {",
        "  result <- argument + 1",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 1L, "argument + 1")
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE
    )))

    actions <- refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract"), capabilities
    )
    action <- refactor_action_with_title(
        actions, "Extract expression to variable")

    expect_named(action$edit, "documentChanges")
    expect_equal(
        action$edit$documentChanges[[1L]]$textDocument,
        list(uri = fixture$uri, version = fixture$document$version)
    )
    expect_length(action$edit$documentChanges[[1L]]$edits, 2L)
})

test_that("extract snippets link names and preserve source and document versions", {
    content <- c(
        "outer <- function(argument) {",
        '  label <- "extracted_value"',
        '  result <- paste0("😀 $1 } \\\\", argument$value)',
        "  result",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 2L, "argument$value")
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    snippets <- refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract"), capabilities
    )
    plain <- refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract")
    )

    expect_length(snippets, 2L)
    for (action in snippets) {
        document_edit <- action$edit$documentChanges[[1L]]
        expect_identical(document_edit$textDocument,
            list(uri = fixture$uri, version = fixture$document$version))
        expect_length(document_edit$edits, 1L)
        edit <- document_edit$edits[[1L]]
        expect_named(edit, c("range", "snippet"))
        expect_identical(edit$snippet$kind, "snippet")
        expect_identical(edit$range$start, fixture$document$to_lsp_position(2L, 0L))
        expect_identical(edit$range$end, refactor_lsp_range(fixture$document, selected)$end)
        expect_match(edit$snippet$value, "$1", fixed = TRUE)
        expect_match(edit$snippet$value, "$0", fixed = TRUE)

        original <- refactor_action_with_title(plain, action$title)
        expect_identical(
            refactor_apply_snippet_edits(content, document_edit$edits),
            refactor_apply_edits(content, original$edit$changes[[fixture$uri]])
        )
        renamed <- refactor_apply_snippet_edits(content, document_edit$edits, "chosen_name")
        expect_match(renamed, 'label <- "extracted_value"', fixed = TRUE)
        expect_equal(length(gregexpr("chosen_name", renamed, fixed = TRUE)[[1L]]), 2L)
        env <- new.env(parent = baseenv())
        eval(parse(text = renamed), env)
        expect_identical(env$outer(list(value = "value")), "😀 $1 } \\value")
    }
})

test_that("extract snippets preserve multiline strings in unchanged enclosing source", {
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    for (raw in c(FALSE, TRUE)) {
        content <- c(
            "outer <- function(argument) {",
            if (raw) '  result <- paste0(r"(a' else '  result <- paste0("a',
            if (raw) '    b)", argument$value + 1)' else '    b", argument$value + 1)',
            "}"
        )
        selected <- refactor_test_range(content, 2L, "argument$value")
        for (native in c(TRUE, FALSE)) {
            fixture <- provider_fixture(content)
            if (!native) fixture$document$parse_data$syntax_data <- NULL
            actions <- refactor_code_actions(fixture$uri, fixture$workspace,
                fixture$document, selected, list("refactor.extract"), capabilities)
            legacy <- refactor_code_actions(fixture$uri, fixture$workspace,
                fixture$document, selected, list("refactor.extract"))
            expect_length(actions, 2L)
            for (action in actions) {
                edits <- action$edit$documentChanges[[1L]]$edits
                expect_true(all(vapply(edits, function(edit) is.null(edit$snippet), logical(1L))))
                expect_equal(action$edit$documentChanges[[1L]]$textDocument$version, fixture$document$version)
                actual <- refactor_apply_edits(content, edits)
                expected <- refactor_action_with_title(legacy, action$title)
                expect_equal(actual, refactor_apply_edits(content, expected$edit$changes[[fixture$uri]]))
                environment <- new.env(parent = baseenv())
                eval(parse(text = actual), environment)
                expect_equal(environment$outer(list(value = 2)), "a\n    b3")
            }
        }
    }
})

test_that("extract snippets use ordinary edits when selected code contains multiline strings", {
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    for (raw in c(FALSE, TRUE)) {
        content <- c(
            "outer <- function(argument) {",
            if (raw) '  result <- paste0(r"(a' else '  result <- paste0("a',
            if (raw) '    b)", argument$value + 1)' else '    b", argument$value + 1)',
            "}"
        )
        fixture <- provider_fixture(content)
        selected <- list(start = list(row = 1L, col = 12L),
            end = list(row = 2L, col = nchar(content[[3L]])))
        actions <- refactor_code_actions(fixture$uri, fixture$workspace,
            fixture$document, selected, list("refactor.extract"), capabilities)
        legacy <- refactor_code_actions(fixture$uri, fixture$workspace,
            fixture$document, selected, list("refactor.extract"))
        expect_length(actions, 2L)
        for (action in actions) {
            edits <- action$edit$documentChanges[[1L]]$edits
            expect_true(all(vapply(edits, function(edit) is.null(edit$snippet), logical(1L))))
            actual <- refactor_apply_edits(content, edits)
            expected <- refactor_action_with_title(legacy, action$title)
            expect_equal(actual, refactor_apply_edits(content, expected$edit$changes[[fixture$uri]]))
            environment <- new.env(parent = baseenv())
            eval(parse(text = actual), environment)
            expect_equal(environment$outer(list(value = 2)), "a\n    b3")
        }
    }
})

test_that("extraction preserves multiline quoted identifiers and argument names", {
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    cases <- list(
        list(content = c("outer <- function(argument) {", "  result <- argument$`value",
                "    key` + 1", "}"), end = 8L, argument = stats::setNames(list(2), "value\n    key")),
        list(content = c("outer <- function(argument) {", "  result <- list(`value",
                "    key` = argument$value + 1)", "}"), argument = list(value = 2)),
        list(content = c("outer <- function(argument) {", '  result <- list("value',
                '    key" = argument$value + 1)', "}"), argument = list(value = 2))
    )
    for (case in cases) {
        end <- if (is.null(case$end)) nchar(case$content[[3L]]) else case$end
        selected <- list(start = list(row = 1L, col = 12L), end = list(row = 2L, col = end))
        original <- new.env(parent = baseenv())
        eval(parse(text = case$content), original)
        for (native in c(TRUE, FALSE)) {
            fixture <- provider_fixture(case$content)
            if (!native) fixture$document$parse_data$syntax_data <- NULL
            for (client in list(NULL, capabilities)) {
                actions <- refactor_code_actions(fixture$uri, fixture$workspace,
                    fixture$document, selected, list("refactor.extract"), client)
                expect_length(actions, 2L)
                for (action in actions) {
                    edits <- if (is.null(client)) {
                        action$edit$changes[[fixture$uri]]
                    } else {
                        action$edit$documentChanges[[1L]]$edits
                    }
                    expect_true(all(vapply(edits, function(edit) is.null(edit$snippet), logical(1L))))
                    actual <- refactor_apply_edits(case$content, edits)
                    environment <- new.env(parent = baseenv())
                    eval(parse(text = actual), environment)
                    expect_equal(environment$outer(case$argument), original$outer(case$argument))
                }
            }
        }
    }
})

test_that("extraction reindents surrounding code without changing a selected literal tail", {
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    for (raw in c(FALSE, TRUE)) {
        content <- c(
            "outer <- function(argument) {",
            if (raw) '  result <- paste0(r"(a' else '  result <- paste0("a',
            if (raw) '    b)",' else '    b",',
            "    argument$value + 1",
            "  )",
            "}"
        )
        selected <- list(start = list(row = 1L, col = 12L), end = list(row = 4L, col = 3L))
        original <- new.env(parent = baseenv())
        eval(parse(text = content), original)
        fixture <- provider_fixture(content)
        for (client in list(NULL, capabilities)) {
            actions <- refactor_code_actions(fixture$uri, fixture$workspace,
                fixture$document, selected, list("refactor.extract"), client)
            expect_length(actions, 2L)
            for (action in actions) {
                edits <- if (is.null(client)) {
                    action$edit$changes[[fixture$uri]]
                } else {
                    action$edit$documentChanges[[1L]]$edits
                }
                actual <- refactor_apply_edits(content, edits)
                environment <- new.env(parent = baseenv())
                eval(parse(text = actual), environment)
                expect_equal(environment$outer(list(value = 2)), original$outer(list(value = 2)))
            }
        }
    }
})

test_that("quoted argument tags before the selection retain their names", {
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    for (quoted in c("`", '"')) {
        content <- c("outer <- function(argument) {", paste0("  result <- list(", quoted, "value"),
            paste0("    key", quoted, " = argument$value + 1)"), "}")
        selected <- refactor_test_range(content, 2L, "argument$value")
        fixture <- provider_fixture(content)
        actions <- refactor_code_actions(fixture$uri, fixture$workspace,
            fixture$document, selected, list("refactor.extract"), capabilities)
        expect_length(actions, 2L)
        for (action in actions) {
            edits <- action$edit$documentChanges[[1L]]$edits
            expect_true(all(vapply(edits, function(edit) is.null(edit$snippet), logical(1L))))
            actual <- refactor_apply_edits(content, edits)
            environment <- new.env(parent = baseenv())
            eval(parse(text = actual), environment)
            expect_equal(environment$outer(list(value = 2)), stats::setNames(list(3), "value\n    key"))
        }
    }
})

test_that("multiline strings outside the enclosing edit do not disable linked snippets", {
    content <- c(
        "outer <- function(argument) {",
        '  label <- "a',
        '    b"',
        '  result <- argument$value + nchar("single line")',
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 3L, "argument$value")
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    actions <- refactor_code_actions(fixture$uri, fixture$workspace,
        fixture$document, selected, list("refactor.extract"), capabilities)
    expect_length(actions, 2L)
    for (action in actions) expect_false(is.null(action$edit$documentChanges[[1L]]$edits[[1L]]$snippet))
})

test_that("extract snippets require both workspace edit capabilities", {
    content <- c("outer <- function(x) {", "  result <- x + 1", "}")
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 1L, "x + 1")
    for (workspace_edit in list(
        list(snippetEditSupport = TRUE),
        list(documentChanges = FALSE, snippetEditSupport = TRUE),
        list(documentChanges = TRUE),
        list(documentChanges = TRUE, snippetEditSupport = FALSE)
    )) {
        capabilities <- list(workspace = list(workspaceEdit = workspace_edit))
        actions <- refactor_code_actions(fixture$uri, fixture$workspace,
            fixture$document, selected, list("refactor.extract"), capabilities)
        expect_length(actions, 2L)
        for (action in actions) {
            edits <- if (isTRUE(workspace_edit$documentChanges)) {
                action$edit$documentChanges[[1L]]$edits
            } else {
                action$edit$changes[[fixture$uri]]
            }
            expect_true(all(vapply(edits, function(edit) {
                is.character(edit$newText) && is.null(edit$snippet)
            }, logical(1L))))
        }
    }
})

test_that("function snippets preserve multiline bodies and live-out bindings", {
    content <- c(
        "outer <- function(argument) {",
        '  intermediate <- paste0(argument$value, "$1 } \\\\")',
        "  result <- nchar(intermediate)",
        "  result",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- list(start = list(row = 1L, col = 2L),
        end = list(row = 2L, col = nchar(content[[3L]])))
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    context <- refactor_selection_context(
        fixture$uri, fixture$workspace, fixture$document, selected)
    action <- refactor_extract_function_action(
        fixture$uri, fixture$workspace, fixture$document, context, capabilities)
    plain <- refactor_extract_function_action(
        fixture$uri, fixture$workspace, fixture$document, context)
    edits <- action$edit$documentChanges[[1L]]$edits
    expect_identical(refactor_apply_snippet_edits(content, edits),
        refactor_apply_edits(content, plain$edit$changes[[fixture$uri]]))
    renamed <- refactor_apply_snippet_edits(content, edits, "format_value")
    expect_match(renamed, "result <- format_value(argument)", fixed = TRUE)
    env <- new.env(parent = baseenv())
    eval(parse(text = renamed), env)
    expect_identical(env$outer(list(value = "value")), nchar("value$1 } \\"))
})

test_that("extract snippets stay within a literate R cell", {
    uri <- "file:///refactor-snippet.qmd"
    content <- c(
        "Text before the code: 😀 $1 }",
        "```{r}",
        "outer <- function(x) {",
        "  result <- x + 1",
        "}",
        "```",
        "Text after the code."
    )
    document <- Document$new(uri, language = "quarto", version = 7L, content = content)
    parse_data <- parse_document(uri, content, is_rmarkdown = TRUE)
    parse_data$version <- document$version
    parse_data$xml_doc <- xml2::read_xml(parse_data$xml_data)
    document$update_parse_data(parse_data)
    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- new.env(parent = baseenv())
    workspace$documents <- documents
    workspace$get_parse_data <- function(request_uri) documents$get(request_uri, NULL)$parse_data
    capabilities <- list(workspace = list(workspaceEdit = list(
        documentChanges = TRUE, snippetEditSupport = TRUE
    )))
    selected <- refactor_test_range(content, 3L, "x + 1")
    actions <- refactor_code_actions(uri, workspace, document, selected,
        list("refactor.extract"), capabilities)
    expect_length(actions, 2L)
    for (action in actions) {
        document_edit <- action$edit$documentChanges[[1L]]
        expect_identical(document_edit$textDocument$version, 7L)
        expect_length(document_edit$edits, 1L)
        rendered <- refactor_apply_snippet_edits(content, document_edit$edits, "chosen_name")
        lines <- strsplit(rendered, "\n", fixed = TRUE)[[1L]]
        expect_identical(lines[1:3], content[1:3])
        expect_identical(tail(lines, 3L), tail(content, 3L))
        expect_silent(parse(text = lines[3:(length(lines) - 2L)]))
    }
})

test_that("extract variable preserves valid multiline source", {
    content <- c(
        "outer <- function(argument) {",
        "  result <- sum(",
        "    argument,",
        "    2",
        "  )",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- list(
        start = list(row = 1L, col = 12L),
        end = list(row = 4L, col = 3L)
    )
    context <- refactor_selection_context(
        fixture$uri, fixture$workspace, fixture$document, selected)
    action <- refactor_extract_variable_action(
        fixture$uri, fixture$workspace, fixture$document, context)
    actual <- refactor_apply_edits(
        content, action$edit$changes[[fixture$uri]])

    expect_match(actual, "extracted_value <- sum\\(")
    expect_match(actual, "result <- extracted_value", fixed = TRUE)
    expect_silent(parse(text = actual))
})

test_that("extract function passes local free variables only", {
    content <- c(
        "global_value <- 10",
        "outer <- function(argument) {",
        "  result <- sqrt(argument + global_value)",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(
        content, 2L, "argument + global_value")

    actions <- refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract")
    )
    action <- refactor_action_with_title(
        actions, "Extract selection to function")
    actual <- refactor_apply_edits(
        content, action$edit$changes[[fixture$uri]])

    expect_equal(actual, paste(c(
        "global_value <- 10",
        "outer <- function(argument) {",
        "  extracted_function <- function(argument) {",
        "    argument + global_value",
        "  }",
        "  result <- sqrt(extracted_function(argument))",
        "}"
    ), collapse = "\n"))
    expect_silent(parse(text = actual))
})

test_that("extract function supports one live-out binding", {
    content <- c(
        "outer <- function(argument) {",
        "  intermediate <- argument + 1",
        "  result <- intermediate * 2",
        "  print(result)",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- list(
        start = list(row = 1L, col = 2L),
        end = list(row = 2L, col = nchar(content[[3L]]))
    )
    context <- refactor_selection_context(
        fixture$uri, fixture$workspace, fixture$document, selected)

    expect_equal(
        refactor_function_bindings(context),
        list(free = "argument", outputs = "result")
    )
    action <- refactor_extract_function_action(
        fixture$uri, fixture$workspace, fixture$document, context)
    actual <- refactor_apply_edits(
        content, action$edit$changes[[fixture$uri]])
    expect_equal(actual, paste(c(
        "outer <- function(argument) {",
        "  extracted_function <- function(argument) {",
        "    intermediate <- argument + 1",
        "    result <- intermediate * 2",
        "  }",
        "  result <- extracted_function(argument)",
        "  print(result)",
        "}"
    ), collapse = "\n"))
    expect_silent(parse(text = actual))
})

test_that("extract function passes a binding read by its own assignment", {
    content <- c(
        "outer <- function(value) {",
        "  value <- value + 1",
        "  print(value)",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 1L, "value <- value + 1")
    context <- refactor_selection_context(
        fixture$uri, fixture$workspace, fixture$document, selected)

    expect_equal(
        refactor_function_bindings(context),
        list(free = "value", outputs = "value")
    )
    action <- refactor_extract_function_action(
        fixture$uri, fixture$workspace, fixture$document, context)
    actual <- refactor_apply_edits(
        content, action$edit$changes[[fixture$uri]])
    expect_equal(actual, paste(c(
        "outer <- function(value) {",
        "  extracted_function <- function(value) {",
        "    value <- value + 1",
        "  }",
        "  value <- extracted_function(value)",
        "  print(value)",
        "}"
    ), collapse = "\n"))
})

test_that("inline local variable removes its declaration and preserves precedence", {
    content <- c(
        "outer <- function(argument) {",
        "  value <- argument + 1",
        "  print(value * 2)",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 2L, "value")

    actions <- refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.inline")
    )
    expect_length(actions, 1L)
    expect_equal(actions[[1L]]$kind, "refactor.inline")
    actual <- refactor_apply_edits(
        content, actions[[1L]]$edit$changes[[fixture$uri]])
    expect_equal(actual, paste(c(
        "outer <- function(argument) {",
        "  print((argument + 1) * 2)",
        "}"
    ), collapse = "\n"))
    expect_silent(parse(text = actual))
})

test_that("inline local variable rejects ambiguous bindings", {
    multiple_reads <- c(
        "outer <- function(argument) {",
        "  value <- argument + 1",
        "  value + value",
        "}"
    )
    fixture <- provider_fixture(multiple_reads)
    selected <- refactor_test_range(multiple_reads, 2L, "value")
    expect_null(refactor_inline_variable_action(
        fixture$uri, fixture$workspace, fixture$document, selected))

    reassigned <- c(
        "outer <- function(argument) {",
        "  value <- argument + 1",
        "  value <- value * 2",
        "  print(value)",
        "}"
    )
    fixture <- provider_fixture(reassigned)
    selected <- refactor_test_range(reassigned, 3L, "value")
    expect_null(refactor_inline_variable_action(
        fixture$uri, fixture$workspace, fixture$document, selected))

    commented <- c(
        "outer <- function(argument) {",
        "  value <- argument + 1 # keep this explanation",
        "  print(value)",
        "}"
    )
    fixture <- provider_fixture(commented)
    selected <- refactor_test_range(commented, 2L, "value")
    expect_null(refactor_inline_variable_action(
        fixture$uri, fixture$workspace, fixture$document, selected))
})

test_that("extract refactorings reject unsafe evaluation contexts", {
    assignment_target <- c(
        "outer <- function(values) {",
        "  values[1] <- compute()",
        "}"
    )
    fixture <- provider_fixture(assignment_target)
    selected <- refactor_test_range(assignment_target, 1L, "values[1]")
    expect_identical(refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract")), list())

    quoted <- c(
        "outer <- function(argument) {",
        "  quoted <- quote(argument + 1)",
        "}"
    )
    fixture <- provider_fixture(quoted)
    selected <- refactor_test_range(quoted, 1L, "argument + 1")
    expect_identical(refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract")), list())

    multiple_outputs <- c(
        "outer <- function(argument) {",
        "  first <- argument + 1",
        "  second <- argument + 2",
        "  first + second",
        "}"
    )
    fixture <- provider_fixture(multiple_outputs)
    selected <- list(
        start = list(row = 1L, col = 2L),
        end = list(row = 2L, col = nchar(multiple_outputs[[3L]]))
    )
    expect_identical(refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract")), list())

    top_level_definition <- c(
        "first <- argument + 1",
        "print(first)"
    )
    fixture <- provider_fixture(top_level_definition)
    selected <- refactor_test_range(
        top_level_definition, 0L, "first <- argument + 1")
    expect_identical(refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.extract")), list())
})

test_that("refactor actions honor requested kinds and current parse versions", {
    content <- c(
        "outer <- function(argument) {",
        "  result <- argument + 1",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 1L, "argument + 1")

    expect_identical(refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("quickfix")), list())
    fixture$document$version <- fixture$document$version + 1L
    expect_identical(refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor")), list())
})

test_that("refactor selections cannot cross literate R cells", {
    uri <- "file:///refactor.qmd"
    content <- c(
        "```{r}",
        "first <- 1 + 2",
        "```",
        "",
        "```{r}",
        "second <- first * 2",
        "```"
    )
    document <- Document$new(
        uri, language = "quarto", version = 1L, content = content)
    parse_data <- parse_document(uri, content, is_rmarkdown = TRUE)
    parse_data$version <- document$version
    parse_data$xml_doc <- xml2::read_xml(parse_data$xml_data)
    document$update_parse_data(parse_data)
    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- new.env(parent = baseenv())
    workspace$documents <- documents
    workspace$get_parse_data <- function(request_uri) {
        documents$get(request_uri, NULL)$parse_data
    }
    selected <- list(
        start = list(row = 1L, col = 0L),
        end = list(row = 5L, col = nchar(content[[6L]]))
    )

    expect_null(refactor_selection_context(
        uri, workspace, document, selected))
})

test_that("inline variable ignores comments around assignment operators", {
    content <- c(
        "outer <- function(a) {",
        "  step <-",
        "    # comment after operator",
        "    a + 1",
        "  step * 2",
        "}"
    )
    fixture <- provider_fixture(content)
    selected <- refactor_test_range(content, 1L, "step")

    actions <- refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.inline")
    )
    action <- refactor_action_with_title(
        actions, "Inline local variable `step`")
    actual <- refactor_apply_edits(
        content, action$edit$changes[[fixture$uri]])
    expect_equal(actual, paste(c(
        "outer <- function(a) {",
        "  (a + 1) * 2",
        "}"
    ), collapse = "\n"))

    right_assign_comment <- c(
        "outer <- function(a) {",
        "  a + 1 ->",
        "    # comment before target",
        "    step",
        "  step * 2",
        "}"
    )
    fixture <- provider_fixture(right_assign_comment)
    selected <- refactor_test_range(right_assign_comment, 3L, "step")
    actions <- refactor_code_actions(
        fixture$uri, fixture$workspace, fixture$document,
        selected, list("refactor.inline")
    )
    action <- refactor_action_with_title(
        actions, "Inline local variable `step`")
    actual <- refactor_apply_edits(
        right_assign_comment, action$edit$changes[[fixture$uri]])
    expect_equal(actual, paste(c(
        "outer <- function(a) {",
        "  (a + 1) * 2",
        "}"
    ), collapse = "\n"))
})
