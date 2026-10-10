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
        expect_true(all(vapply(refs, function(item) startsWith(item$detail, "[scope] "), logical(1L))))
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

test_that("R6 context bindings hide enclosing functions without hiding method locals", {
    for (indexed in c(TRUE, FALSE)) {
        for (case in list(
            list(outer = "self <- function() NULL", declaration = 'R6::R6Class("C", public = list(',
                name = "self", description = "R6 instance of `C`"),
            list(outer = "value <- function() NULL", declaration = 'R6::R6Class("C", portable = FALSE, public = list(value = 1,',
                name = "value", description = "Public field of R6 class `C`"),
            list(outer = "run <- 1", declaration = 'R6::R6Class("C", portable = FALSE, public = list(run = function() NULL,',
                name = "run", description = "Public method of R6 class `C`"))) {
            for (local in c("", paste0(case$name, " <- 1"), paste0(case$name, " <- function() NULL"),
                    paste0(case$name, " <- unknown"), paste0(case$name, " <- unknown()"))) {
                fixture <- class_scope_fixture(c("make_class <- function() {", case$outer,
                        case$declaration, "probe = function() {", local), c("}))", "}"), token = case$name)
                if (!indexed) fixture$document$parse_data$completion_data <- NULL
                items <- class_scope_items(fixture, token = case$name)
                items <- Filter(function(item) identical(item$label, case$name), items)
                expect_length(items, 1L)
                item <- items[[1L]]
                function_expected <- endsWith(local, "function() NULL") || (!nzchar(local) && case$name == "run")
                expect_identical(item$kind, if (function_expected) CompletionItemKind$Function else CompletionItemKind$Field)
                expect_identical(item$insertText, if (function_expected) paste0(case$name, "($0)") else NULL)
                expect_identical(item$documentation$value, if (!nzchar(local)) case$description else NULL)
            }
        }
    }
})

test_that("Ordinary completion reuses its lexical inference context", {
    fixture <- class_scope_fixture(c(
        'C <- R6::R6Class("C", public = list(probe = function() {'
    ), "}))", token = "se")
    fixture$workspace$loaded_packages <- character()
    fixture$workspace$imported_objects <- collections::dict()
    fixture$workspace$get_namespace <- function(...) NULL
    calls <- 0L
    resolve <- member_resolve_cursor
    testthat::local_mocked_bindings(member_resolve_cursor = function(uri, workspace, document, point, cursor, ...) {
        if (!is.null(cursor)) calls <<- calls + 1L
        resolve(uri, workspace, document, point, cursor, ...)
    }, .package = "languageserver")
    items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point, list())$result$items
    expect_identical(calls, 1L)
    expect_true("self" %in% vapply(items, `[[`, character(1L), "label"))
})

test_that("Opaque method locals hide enclosing function snippets in completion replies", {
    for (nested in c(FALSE, TRUE)) {
        fixture <- class_scope_fixture(c(
            "make_class <- function() {", "self <- function() NULL",
            'R6::R6Class("C", public = list(run = function() {',
            if (nested) "nested <- function() {",
            "self <- unknown()"
        ), c(if (nested) "}", "}))", "}"), token = "se")
        fixture$workspace$loaded_packages <- character()
        fixture$workspace$imported_objects <- collections::dict()
        fixture$workspace$get_namespace <- function(...) NULL
        items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            fixture$point, list(completionItem = list(snippetSupport = TRUE)))$result$items
        items <- Filter(function(item) identical(item$label, "self"), items)
        expect_length(items, 1L)
        expect_identical(items[[1L]]$kind, CompletionItemKind$Field)
        expect_null(items[[1L]]$insertText)
        expect_null(items[[1L]]$documentation)
    }
})

test_that("Ordinary completion avoids class inference without source or package classes", {
    fixture <- class_scope_fixture(c("probe <- function(argument) {",
            sprintf("  value%d <- list(first = 1, second = list(a = 2))", seq_len(1000L))), "}")
    fixture$workspace$loaded_packages <- character()
    fixture$workspace$imported_objects <- collections::dict()
    fixture$workspace$get_namespace <- function(...) NULL
    fixture$workspace$member_metadata <- collections::dict()
    ordinary <- member_index_freeze(member_generic_index("identity <- function(x) x"))
    expect_false(ordinary$class_scope)
    fixture$workspace$member_metadata$set("ordinary", member_index_thaw(ordinary))
    resolve <- member_resolve_cursor
    calls <- new.env(parent = emptyenv())
    calls$count <- 0L
    testthat::local_mocked_bindings(member_resolve_cursor = function(uri, workspace, document, point, cursor, ...) {
        if (!is.null(cursor)) calls$count <- calls$count + 1L
        resolve(uri, workspace, document, point, cursor, ...)
    }, .package = "languageserver")
    for (token in c("", "value999")) {
        items <- scope_completion(fixture$uri, fixture$workspace, token, fixture$point)
        labels <- vapply(items, `[[`, character(1L), "label")
        expect_true("value999" %in% labels)
        expect_null(attr(items, "member_context"))
    }
    items <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point, list())$result$items
    expect_true("argument" %in% vapply(items, `[[`, character(1L), "label"))
    expect_identical(calls$count, 0L)
})

test_that("Class inference hints preserve source, package and incomplete contexts", {
    fixture <- class_scope_fixture("probe <- function() {", "}")
    expect_false(member_scope_required(fixture$workspace, fixture$document))
    fixture$document$set_content(2L, fixture$document$content)
    expect_true(member_scope_required(fixture$workspace, fixture$document))
    fixture <- class_scope_fixture("probe <- function() {", token = "value")
    expect_true(member_scope_required(fixture$workspace, fixture$document))

    for (head in c("R6::R6Class", "R6Class")) {
        fixture <- class_scope_fixture(c(sprintf('C <- %s("C", public = list(run = function() {', head)), "}))")
        expect_true(member_scope_required(fixture$workspace, fixture$document))
    }
    fixture <- class_scope_fixture('probe <- function(C = R6::R6Class("C")) {', "}")
    expect_true(member_scope_required(fixture$workspace, fixture$document))
    fixture <- class_scope_fixture('probe <- function(C = R6::R6Class("C", public = list(run = function() {',
        "}))) NULL", token = "se")
    expect_true("self" %in% class_scope_labels(fixture, token = "se"))
    fixture <- class_scope_fixture("probe <- function() {", "}")
    fixture$workspace$member_metadata <- collections::dict()
    index <- member_generic_index('factory <- function() R6::R6Class("C")')
    snapshot <- member_index_freeze(index)
    expect_true(snapshot$class_scope)
    fixture$workspace$member_metadata$set("fixture", member_index_thaw(snapshot))
    expect_true(member_scope_required(fixture$workspace, fixture$document))

    defaults <- member_generic_index('factory <- function(C = R6::R6Class("C")) C')
    expect_true(member_index_freeze(defaults)$class_scope)

    index <- member_generic_index("")
    index$package_roots$C <- member_r6_runtime_shape(R6::R6Class("C"), index)
    expect_true(member_index_freeze(index)$class_scope)
    snapshot$class_scope <- NULL
    fixture$workspace$member_metadata$set("fixture", member_index_thaw(snapshot))
    expect_true(member_scope_required(fixture$workspace, fixture$document))
})

test_that("Package class hints retain class information for ordinary scope variables", {
    fixture <- class_scope_fixture(c("probe <- function() {", "  object <- fixture::C$new()"),
        "}", token = "obj")
    index <- member_package_index(list(package = "fixture", exports = "C"))
    value <- member_r6_runtime_shape(R6::R6Class("Widget"), index)
    index$package_roots$C <- value
    index$roots$C <- value
    index$namespace_roots[["fixture::C"]] <- value
    fixture$workspace$member_metadata <- collections::dict()
    fixture$workspace$member_metadata$set("fixture", member_index_thaw(member_index_freeze(index)))
    items <- class_scope_items(fixture, token = "obj")
    object <- items[[match("object", vapply(items, `[[`, character(1L), "label"))]]
    expect_match(object$documentation$value, "R6 instance of `Widget`", fixed = TRUE)
})
