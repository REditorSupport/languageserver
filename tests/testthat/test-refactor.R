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
