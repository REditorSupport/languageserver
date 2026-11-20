test_that("providers serve unchanged declarations and uses in a newly opened incomplete script", {
    skip_on_cran()
    root <- withr::local_tempdir()
    client <- language_client(working_dir = root)
    path <- file.path(root, "partial.R")
    content <- c(
        "#' @param ticket A ticket.",
        "ticket_value <- function(ticket) {",
        "  doubled <- ticket + ticket",
        "  doubled",
        "}",
        "stats::",
        "consume <- function(key) ticket_value(key)",
        'Box <- R6::R6Class("Box")'
    )
    did_open(client, path, text = content)
    doc <- list(uri = path_to_uri(path))
    # Wait for the worker to publish this version before issuing immediate requests.
    expect_gt(length(respond_semantic_tokens_full(client, path)$data), 0L)
    request <- function(method, params = list()) {
        respond(client, paste0("textDocument/", method), c(list(textDocument = doc), params), retry = FALSE)
    }
    names_of <- function(items) vapply(items, "[[", character(1L), "name")
    expect_true(all(c("ticket_value", "consume", "Box") %in% names_of(request("documentSymbol"))))
    expect_true("consume" %in% names_of(respond_workspace_symbol(client, "consume", retry = FALSE)))
    folds <- request("foldingRange")
    expect_true(any(vapply(folds, function(fold) fold$startLine == 1L && fold$endLine >= 3L, logical(1L))))
    selected <- request("selectionRange", list(positions = list(position(2L, 15L))))
    expect_false(is.null(selected[[1L]]$parent$parent))
    highlights <- request("documentHighlight", list(position = position(2L, 15L)))
    expect_length(highlights, 3L)

    visible <- range(position(1L, 0L), position(5L, 0L))
    values <- request("inlineValue", list(range = visible, context = list(frameId = 1L, stoppedLocation = visible)))
    expect_true(all(c("doubled", "ticket") %in% vapply(values, "[[", character(1L), "variableName")))
    linked <- request("linkedEditingRange", list(position = position(0L, 12L)))
    expect_length(linked$ranges, 2L)
    lenses <- request("codeLens")
    expect_true("consume" %in% vapply(lenses, function(lens) lens$data$symbol, character(1L)))

    use <- c(6L, 27L)
    definition <- respond_definition(client, path, use, retry = FALSE)
    expect_equal(definition$range$start$line, 1L)
    hover <- respond_hover(client, path, use, retry = FALSE)
    expect_true(any(grepl("ticket_value", unlist(hover$contents), fixed = TRUE)))
    signature <- respond_signature(client, path, c(6L, 39L), retry = FALSE)
    expect_identical(signature$signatures[[1L]]$label, "ticket_value(ticket)")
    calls <- respond_prepare_call_hierarchy(client, path, c(6L, 1L), retry = FALSE)
    expect_identical(names_of(calls), "consume")
    outgoing <- respond_call_hierarchy_outgoing_calls(client, calls[[1L]], retry = FALSE)
    expect_true("ticket_value" %in% vapply(outgoing, function(call) call$to$name, character(1L)))
    types <- respond_prepare_type_hierarchy(client, path, c(7L, 1L), retry = FALSE)
    expect_identical(names_of(types), "Box")
})

test_that("a fence edit invalidates cached provider results for identical extracted R text", {
    one <- c("```{r}", 's <- "open', "", "", 'close"', "f <- function() 1", "```")
    two <- one
    two[3:4] <- c("```", "```{r}")
    expect_identical(purl(one), purl(two))
    uri <- "file:///cells.qmd"
    workspace <- Workspace$new(NULL)
    document <- Document$new(uri, language = "rmd", version = 1L, content = one)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, one, is_rmarkdown = TRUE)
    self <- list(get_workspace = function(...) workspace,
        resolve_task_manager = list(add_task = function(...) NULL),
        pending_replies = collections::dict(), request_handlers = list())
    parse_callback(self, uri, 1L, parsed, one)
    expect_true("f" %in% names(parsed$definitions))
    document$set_content(2L, two)
    testthat::local_mocked_bindings(create_task = function(args, callback, ...) {
        callback(do.call(parse_document, args))
    }, .package = "languageserver")
    parse_task(self, uri, document)
    split <- document$parse_data
    expect_false(identical(parsed$content_hash, split$content_hash))
    expect_identical(split$content_hash, parse_document(uri, two, is_rmarkdown = TRUE)$content_hash)
    expect_false("f" %in% names(split$definitions))
})
