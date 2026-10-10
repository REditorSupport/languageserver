class_scope_fixture <- function(before, after = character(), token = "") {
    content <- c(before, paste0("  ", token), after)
    fixture <- provider_fixture(content)
    fixture$point <- list(row = length(before), col = 2L + nchar(token))
    fixture
}

class_scope_items <- function(fixture, token = "", snippets = TRUE, limit = Inf) {
    scope_completion(fixture$uri, fixture$workspace, token, fixture$point,
        snippet_support = snippets, limit = limit)
}

class_scope_labels <- function(fixture, ...) {
    vapply(class_scope_items(fixture, ...), `[[`, character(1L), "label")
}

test_that("R6 methods complete their available references before any usage", {
    for (section in c("public", "private", "active")) {
        fixture <- class_scope_fixture(c(
            'Parent <- R6::R6Class("Parent", private = list(secret = 1))',
            sprintf('Child <- R6::R6Class("Child", inherit = Parent, %s = list(', section),
            "  probe = function(argument) {",
            "    local_value <- 1"
        ), c("  }))"))
        labels <- class_scope_labels(fixture)
        expect_true(all(c("self", "private", "super", "argument", "local_value") %in% labels))
        for (token in c("se", "pr", "su")) {
            expected <- c(se = "self", pr = "private", su = "super")[[token]]
            expect_true(expected %in% class_scope_labels(fixture, token = token))
        }
        refs <- Filter(function(item) item$label %in% c("self", "private", "super"),
            class_scope_items(fixture))
        expect_length(refs, 3L)
        expect_true(all(vapply(refs, function(item) identical(item$detail, "[scope]"), logical(1L))))
        expect_true(all(vapply(refs, function(item) is.null(item$insertText), logical(1L))))
        resolved <- completion_item_resolve_reply(1L, fixture$workspace, refs[[1L]], list())$result
        expect_identical(resolved$label, refs[[1L]]$label)
        expect_null(resolved$data)
    }
})

test_that("R6 scope follows private, inheritance and portability declarations", {
    for (portable in c(TRUE, FALSE)) {
        for (private in c(TRUE, FALSE)) {
            for (inherit in c(TRUE, FALSE)) {
                fixture <- class_scope_fixture(c(
                    sprintf('Parent <- R6::R6Class("Parent", portable = %s, public = list(parent_value = 1))', portable),
                    sprintf('Child <- R6::R6Class("Child", portable = %s, %s %s public = list(',
                        portable, if (private) "private = list(secret = 1)," else "",
                        if (inherit) "inherit = Parent," else ""),
                    "  value = 1, probe = function() {"
                ), "  }))")
                labels <- class_scope_labels(fixture)
                expect_true("self" %in% labels)
                expect_identical("private" %in% labels, private)
                expect_identical("super" %in% labels, inherit)
                expect_identical("value" %in% labels, !portable)
                expect_identical("secret" %in% labels, !portable && private)
                expect_identical("parent_value" %in% labels, !portable && inherit)
                expect_identical("probe" %in% labels, !portable)
                expect_identical("clone" %in% labels, !portable)
                expect_identical(".__enclos_env__" %in% labels, !portable)
            }
        }
    }
    fixture <- class_scope_fixture(c(
        'C <- R6::R6Class("C", portable = FALSE, cloneable = FALSE, inherit = NULL, public = list(',
        "  probe = function() {"
    ), "  }))")
    expect_false(any(c("clone", "super", "private") %in% class_scope_labels(fixture)))
})

test_that("Context scope includes nested closures, default arguments and shadowing", {
    fixture <- class_scope_fixture(c(
        'C <- R6::R6Class("C", public = list(probe = function(argument) {',
        "  self <- 1",
        "  nested <- function(inner) {"
    ), c("  }", "}))"))
    labels <- class_scope_labels(fixture)
    expect_true(all(c("self", "argument", "inner") %in% labels))
    expect_equal(sum(labels == "self"), 1L)
    item <- Filter(function(item) identical(item$label, "self"), class_scope_items(fixture))[[1L]]
    expect_identical(item$data$type, "nonfunction")

    fixture <- provider_fixture(c(
        'C <- R6::R6Class("C", public = list(probe = function(argument = se) NULL))'
    ))
    fixture$point <- list(row = 0L, col = regexpr("se)", fixture$document$content, fixed = TRUE)[[1L]] + 1L)
    expect_true("self" %in% class_scope_labels(fixture, token = "se"))
})

test_that("Non-portable methods complete inherited methods and quoted names", {
    fixture <- class_scope_fixture(c(
        'Parent <- R6::R6Class("Parent", portable = FALSE, public = list(run = function(x) NULL),',
        "  private = list(hidden = function() NULL), active = list(property = function(value) stop('getter')))",
        'Child <- R6::R6Class("Child", portable = FALSE, inherit = Parent, public = list(',
        "  `a b` = function() NULL, probe = function() {"
    ), "  }))")
    labels <- class_scope_labels(fixture)
    expect_true(all(c("run", "hidden", "property", ".__active__", "a b") %in% labels))
    for (snippets in c(TRUE, FALSE)) {
        items <- class_scope_items(fixture, snippets = snippets)
        method <- Filter(function(item) identical(item$label, "run"), items)[[1L]]
        expect_identical(method$kind, CompletionItemKind$Function)
        expect_identical(method$insertText, if (snippets) "run($0)" else NULL)
        quoted <- Filter(function(item) identical(item$label, "a b"), items)[[1L]]
        expect_identical(quoted$insertText, if (snippets) "`a b`($0)" else "`a b`")
        property <- Filter(function(item) identical(item$label, "property"), items)[[1L]]
        expect_identical(property$kind, CompletionItemKind$Field)
    }
    items <- class_scope_items(fixture, limit = 2L)
    expect_length(items, 2L)
    expect_true(isTRUE(attr(items, "truncated")))
})

test_that("Class references stay within verified method scopes", {
    fixture <- class_scope_fixture("ordinary <- function() {", "}")
    expect_false(any(c("self", "private", "super") %in% class_scope_labels(fixture)))
    for (head in c("other::R6Class", "R6Class")) {
        fixture <- class_scope_fixture(c(
            sprintf('C <- %s("C", public = list(probe = function() {', head)
        ), "}))")
        expect_false("self" %in% class_scope_labels(fixture))
    }
    fixture <- class_scope_fixture(c(
        "library(R6)",
        'C <- R6Class("C", public = list(probe = function() {'
    ), "}))")
    expect_true("self" %in% class_scope_labels(fixture))

    fixture <- class_scope_fixture(c(
        "library(R6)", "R6Class <- function(...) NULL",
        'C <- R6Class("C", public = list(probe = function() {'
    ), "}))")
    expect_false("self" %in% class_scope_labels(fixture))

    fixture <- class_scope_fixture(c(
        'C <- R6::R6Class("C", public = list(value = {',
        "  helper <- function() {"
    ), c("  }", "}))"))
    expect_false("self" %in% class_scope_labels(fixture))
})

test_that("Scope completion excludes sibling method and nested function locals", {
    fixture <- class_scope_fixture(c(
        'C <- R6::R6Class("C", public = list(',
        "  first = function(other_argument) {",
        "    other_local <- 1",
        "    for (other_loop in 1:3) other_loop",
        "    other_function <- function() NULL",
        "  },",
        "  probe = function(argument) {",
        "    local_value <- 1",
        "    local_function <- function() { nested_local <- 1 }"
    ), c("  }))"))
    for (indexed in c(TRUE, FALSE)) {
        if (!indexed) fixture$document$parse_data$completion_data <- NULL
        labels <- class_scope_labels(fixture)
        expect_true(all(c("self", "argument", "local_value", "local_function") %in% labels))
        excluded <- c("other_argument", "other_local", "other_loop", "other_function", "nested_local")
        expect_false(any(excluded %in% labels))
    }
})

test_that("Scope recovery handles incomplete class edits without evaluating them", {
    marker <- withr::local_tempfile()
    fixture <- class_scope_fixture(c(
        'C <- R6::R6Class("C", public = list(probe = function(argument) {',
        sprintf('  writeLines("ran", %s)', encodeString(marker, quote = '"')),
        "  local_value <- 1"
    ), token = "se")
    expect_true(fixture$document$parse_data$parse_error)
    expect_true("self" %in% class_scope_labels(fixture, token = "se"))
    expect_true(all(c("argument", "local_value") %in% class_scope_labels(fixture)))
    expect_false(file.exists(marker))
})

test_that("Scope lookup uses indexed syntax for large class declarations", {
    fixture <- class_scope_fixture(c(
        'C <- R6::R6Class("C", public = list(',
        rep("  earlier = function() NULL,", 150L),
        "  probe = function(argument) {",
        rep("    1", 150L)
    ), c("  }), private = list(secret = 1))"), token = "se")
    expect_true(all(c("self", "private", "argument") %in% class_scope_labels(fixture)))
})

test_that("Context completion respects comments, strings and literate code regions", {
    for (text in c("  # se", '  "se"', '  r"(se)"')) {
        fixture <- provider_fixture(c(
            'C <- R6::R6Class("C", public = list(probe = function() {', text, "}))"
        ))
        fixture$point <- list(row = 1L, col = regexpr("se", text, fixed = TRUE)[[1L]] + 1L)
        expect_false("self" %in% class_scope_labels(fixture, token = "se"))
    }
    uri <- "file:///class-scope.qmd"
    content <- c("```{r}",
        'C <- R6::R6Class("C", public = list(probe = function() {',
        "  se", "}))", "```", "se")
    document <- Document$new(uri, language = "quarto", version = 1L, content = content)
    document$update_parse_data(parse_document(uri, content, is_rmarkdown = TRUE))
    documents <- collections::dict()
    documents$set(uri, document)
    workspace <- list(documents = documents, get_parse_data = function(...) document$parse_data)
    items <- scope_completion(uri, workspace, "se", list(row = 2L, col = 4L))
    expect_true("self" %in% vapply(items, `[[`, character(1L), "label"))
    items <- scope_completion(uri, workspace, "se", list(row = 5L, col = 2L))
    expect_false("self" %in% vapply(items, `[[`, character(1L), "label"))
})

test_that("Completion replies include class references on empty and partial tokens", {
    fixture <- class_scope_fixture(c(
        'C <- R6::R6Class("C", public = list(probe = function() {'
    ), "}), private = list(secret = 1))", token = "se")
    fixture$workspace$loaded_packages <- character()
    fixture$workspace$imported_objects <- collections::dict()
    fixture$workspace$get_namespace <- function(...) NULL
    capabilities <- list(completionItem = list(snippetSupport = TRUE))
    for (col in c(2L, 4L)) {
        items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = fixture$point$row, col = col), capabilities)$result$items
        expect_true("self" %in% vapply(items, `[[`, character(1L), "label"))
    }
})
