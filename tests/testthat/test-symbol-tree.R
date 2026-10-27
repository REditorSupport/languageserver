symbol_tree_fixture <- function(content, uri = "file:///symbol-tree.R") {
    document <- Document$new(uri, version = 1L, content = content)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, content, is_rmarkdown = document$is_rmarkdown)
    parsed$version <- document$version
    workspace$update_parse_data(uri, parsed)
    list(document = document, workspace = workspace, uri = uri)
}

symbol_tree_reply <- function(fixture, hierarchical = TRUE) {
    document_symbol_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(hierarchicalDocumentSymbolSupport = hierarchical))$result
}

test_that("R sections and definitions form the outline tree from issue 814", {
    fixture <- symbol_tree_fixture(c(
        "# First section ----",
        "",
        "## Subsection of the first section ----",
        "",
        "var <- 'dummy'",
        "",
        "## Another subsection ----",
        "",
        "variable <- 2",
        "",
        "# Second section ----",
        "",
        "another_var <- 'Hey!'"
    ))
    result <- symbol_tree_reply(fixture)
    expect_equal(map_chr(result, "name"), c("First section", "Second section"))
    expect_equal(map_chr(result[[1L]]$children, "name"),
        c("Subsection of the first section", "Another subsection"))
    expect_equal(result[[1L]]$children[[1L]]$children[[1L]]$name, "var")
    expect_equal(result[[1L]]$children[[2L]]$children[[1L]]$name, "variable")
    expect_equal(result[[2L]]$children[[1L]]$name, "another_var")

    flat <- symbol_tree_reply(fixture, hierarchical = FALSE)
    expect_length(flat, 7L)
    expect_true(all(map_lgl(flat, ~ !is.null(.x$location))))
    expect_true(all(map_lgl(flat, ~ is.null(.x$children))))
    expect_setequal(map_chr(flat, "name"), map_chr(flatten_document_symbols(result), "name"))
})

test_that("comment-only outlines retain all four levels from issue 685", {
    fixture <- symbol_tree_fixture(c(
        "# 1 Top section ####",
        "## 1.1 Subsection ####",
        "### 1.1.1 Sub-subsection ####",
        "#### 1.1.1.1 Details ----------------------------------"
    ))
    result <- symbol_tree_reply(fixture)
    expect_length(result, 1L)
    expect_equal(result[[1L]]$name, "1 Top section")
    expect_equal(result[[1L]]$children[[1L]]$name, "1.1 Subsection")
    expect_equal(result[[1L]]$children[[1L]]$children[[1L]]$name, "1.1.1 Sub-subsection")
    expect_equal(result[[1L]]$children[[1L]]$children[[1L]]$children[[1L]]$name, "1.1.1.1 Details")
})

test_that("section succession breaks leave subsequent sections at the root", {
    withr::local_options(languageserver.nline_to_break_succession = 2L)
    fixture <- symbol_tree_fixture(c(
        "# First ----", "value <- 1", "", "", "## Detached ----", "other <- 2"
    ))
    result <- symbol_tree_reply(fixture)
    expect_equal(map_chr(result, "name"), c("First", "Detached"))
    expect_equal(result[[1L]]$children[[1L]]$name, "value")
    expect_equal(result[[2L]]$children[[1L]]$name, "other")
})

test_that("sections inside R6 classes contain their members", {
    fixture <- symbol_tree_fixture(c(
        "# Main ----",
        "Widget <- R6::R6Class('Widget',",
        "  public = list(",
        "    # Methods ----",
        "    run = function() { 1 },",
        "    # Fields ----",
        "    value = 1",
        "  )",
        ")"
    ))
    result <- symbol_tree_reply(fixture)
    widget <- result[[1L]]$children[[1L]]
    expect_equal(widget$name, "Widget")
    expect_equal(map_chr(widget$children, "name"), c("Methods", "Fields"))
    expect_equal(widget$children[[1L]]$children[[1L]]$name, "run")
    expect_equal(widget$children[[1L]]$children[[1L]]$kind, SymbolKind$Method)
    expect_equal(widget$children[[2L]]$children[[1L]]$name, "value")
    expect_equal(widget$children[[2L]]$children[[1L]]$kind, SymbolKind$Field)
    expect_setequal(map_chr(flatten_document_symbols(result), "name"),
        c("Main", "Widget", "Methods", "Fields", "run", "value"))
    expect_length(flatten_document_symbols(result), 6L)
})

test_that("normalization preserves supplied class owners and nested subtrees", {
    make_symbol <- function(name, first, last, children = NULL) {
        span <- range(position(first, 0L), position(last, 10L))
        document_symbol(name, SymbolKind$Class, span, span, children = children)
    }
    leaf <- make_symbol("leaf", 3L, 3L)
    member <- make_symbol("member", 3L, 4L, list(leaf))
    equal <- make_symbol("equal", 1L, 7L)
    external <- make_symbol("external", 10L, 10L)
    result <- document_symbol_tree(list(
        make_symbol("Widget", 1L, 7L, list(member, external, equal)),
        make_symbol("Members", 2L, 5L),
        make_symbol("Elsewhere", 9L, 12L)
    ))
    expect_equal(map_chr(result, "name"), c("Widget", "Elsewhere"))
    widget <- result[[1L]]
    expect_equal(map_chr(widget$children, "name"), c("equal", "external"))
    expect_equal(widget$children[[1L]]$children[[1L]]$name, "Members")
    expect_identical(widget$children[[1L]]$children[[1L]]$children[[1L]], member)
    expect_identical(widget$children[[2L]], external)
    expect_null(result[[2L]]$children)
    expect_length(flatten_document_symbols(result), 7L)

    # A sole supplied owner still needs its direct child group normalized.
    result <- document_symbol_tree(list(make_symbol("Owner", 0L, 6L,
                list(member, make_symbol("Group", 2L, 5L)))))
    expect_equal(result[[1L]]$children[[1L]]$name, "Group")
    expect_identical(result[[1L]]$children[[1L]]$children[[1L]], member)
})

test_that("symbol nesting keeps class children and handles equal or crossing ranges", {
    make_symbol <- function(name, first, last, children = NULL) {
        span <- range(position(first, 0L), position(last, 10L))
        document_symbol(name, SymbolKind$Class, span, span, children = children)
    }
    member <- make_symbol("member", 2L, 3L)
    result <- document_symbol_tree(list(
        make_symbol("Widget", 1L, 5L, list(member)),
        make_symbol("Main", 0L, 10L),
        make_symbol("After", 11L, 12L)
    ))
    expect_equal(map_chr(result, "name"), c("Main", "After"))
    expect_equal(result[[1L]]$children[[1L]]$name, "Widget")
    expect_identical(result[[1L]]$children[[1L]]$children, list(member))

    result <- document_symbol_tree(list(
        make_symbol("outer", 0L, 9L),
        make_symbol("first", 1L, 4L),
        make_symbol("equal", 1L, 4L),
        make_symbol("crossing", 3L, 6L)
    ))
    expect_equal(map_chr(result[[1L]]$children, "name"), c("first", "equal", "crossing"))
    expect_identical(document_symbol_tree(list()), list())
})
