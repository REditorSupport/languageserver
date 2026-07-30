test_that("LSP positions, ranges, and locations validate and print", {
    start <- position(1L, 2L)
    end <- position(3L, 4L)
    selection <- range(start, end)
    uri <- document_uri("file:///interfaces.R")
    where <- location(uri, selection)

    expect_error(position("1", 2L), "numeric arguments")
    expect_error(range(start, list()), "position")
    expect_error(location(uri, list()), "range")
    expect_error(document_uri(1L), "character parameter")

    expect_output(print.position(start), "<Position> Line: 1")
    expect_output(print.range(selection), "<Range>")
    expect_output(
        print.document_uri(uri),
        "<DocumentURI> file:///interfaces.R",
        fixed = TRUE
    )
    expect_output(print.location(where), "<Location>")
})

test_that("LSP value constructors preserve optional fields", {
    uri <- document_uri("file:///interfaces.R")
    selection <- range(position(0L, 0L), position(0L, 4L))
    child <- document_symbol(
        "child", SymbolKind$Variable, selection, selection
    )
    parent <- document_symbol(
        "parent", SymbolKind$Function, selection, selection,
        detail = "function", children = list(child)
    )

    expect_s3_class(symbol_information(
        "symbol", SymbolKind$Variable, location(uri, selection)
    ), "symbol_information")
    expect_equal(parent$detail, "function")
    expect_identical(parent$children, list(child))
    expect_null(child$detail)
    expect_null(child$children)

    edit <- text_edit(selection, "replacement")
    expect_s3_class(edit, "text_edit")
    expect_equal(edit$newText, "replacement")
    expect_s3_class(
        text_document_position_params(uri, position(0L, 1L)),
        "text_document_position_params"
    )
})

test_that("Request parameter constructors use the protocol field names", {
    uri <- document_uri("file:///params.R")
    point <- position(1L, 2L)
    selection <- range(point, position(1L, 3L))
    options <- list(tabSize = 2L, insertSpaces = TRUE)
    context <- list(triggerKind = 1L)

    cases <- list(
        completion_params(uri, point, context),
        reference_params(uri, point, context),
        document_symbol_params(uri),
        code_action_params(uri, selection, context),
        code_lens_params(uri),
        document_link_params(uri),
        document_formatting_params(uri, options),
        document_range_formatting_params(uri, selection, options),
        document_on_type_formatting_params(uri, point, "\n", options),
        rename_params(uri, point, "renamed"),
        did_open_text_document_params(uri),
        did_change_text_document_params(uri, list(list(text = "changed"))),
        will_save_text_document_params(uri, 1L),
        did_save_text_document_params(uri, "saved"),
        did_close_text_document_params(uri),
        did_change_configuration_params(list(languageserver = list(debug = TRUE)))
    )

    expect_true(all(vapply(cases, function(params) {
        is.list(params) && length(class(params)) == 1L
    }, logical(1L))))
    expect_identical(cases[[1L]]$context, context)
    expect_identical(cases[[4L]]$range, selection)
    expect_identical(cases[[8L]]$options, options)
    expect_equal(cases[[9L]]$character, "\n")
    expect_equal(cases[[10L]]$newName, "renamed")
    expect_equal(cases[[14L]]$text, "saved")
    expect_true(cases[[16L]]$languageserver$debug)
})
