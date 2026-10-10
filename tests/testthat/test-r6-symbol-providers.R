r6_symbol_fixture <- function(lines, language = "r") {
    fixture <- provider_fixture(lines)
    if (language != "r") {
        fixture$document <- Document$new(fixture$uri, language = language, version = 1L, content = lines)
        data <- parse_document(fixture$uri, lines, is_rmarkdown = TRUE)
        data$version <- 1L
        data$xml_doc <- xml2::read_xml(data$xml_data)
        fixture$document$update_parse_data(data)
        fixture$workspace$documents$set(fixture$uri, fixture$document)
    }
    fixture$workspace$get_documentation <- function(...) NULL
    fixture$workspace$get_help <- function(...) NULL
    fixture$workspace$get_signature <- function(...) NULL
    fixture
}

r6_symbol_hover <- function(fixture, row, symbol) {
    col <- regexpr(symbol, fixture$document$line0(row), fixed = TRUE)[[1L]]
    hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = row, col = col))$result
}

r6_symbol_signature <- function(fixture, row = fixture$document$nline - 1L) {
    signature_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = row, col = nchar(fixture$document$line0(row))))$result
}

test_that("R6 scope completion and hover identify the referenced class", {
    for (section in c("public", "private", "active")) {
        fixture <- r6_symbol_fixture(c(
            'Base <- R6::R6Class("Parent", public = list(run = function() NULL))',
            sprintf('C <- R6::R6Class("Widget", inherit = Base, %s = list(probe = function() {', section),
            "  self", "  private", "  super",
            if (section == "private") "}, secret = 1))" else "}), private = list(secret = 1))"
        ))
        for (row in 2:4) {
            symbol <- trimws(fixture$document$line0(row))
            expected <- switch(symbol, self = "R6 instance of `Widget`",
                private = "Private environment of `Widget`", super = "Superclass methods of `Parent`")
            items <- context_scope_completion(fixture$uri, fixture$workspace, substr(symbol, 1L, 2L),
                list(row = row, col = 4L), FALSE, Inf, character())
            item <- items[[match(symbol, vapply(items, `[[`, character(1L), "label"))]]
            expect_identical(item$documentation$value, expected)
            expect_identical(item$detail, paste("[scope]", gsub("`", "", expected, fixed = TRUE)))
            resolved <- completion_item_resolve_reply(1L, fixture$workspace, item, list())$result
            expect_identical(resolved$documentation$value, expected)
            hover <- r6_symbol_hover(fixture, row, symbol)
            expect_identical(hover$contents, c("```r\nenvironment\n```", expected))
            expect_identical(hover$range$start$character, 2L)
            expect_identical(hover$range$end$character, 2L + nchar(symbol))
        }
    }
})

test_that("R6 reference hover follows aliases and respects lexical shadowing", {
    fixture <- r6_symbol_fixture(c(
        'C <- R6::R6Class("Widget", public = list(probe = function() {',
        "  alias <- self", "  nested <- function() alias", "  self <- 1", "  self",
        "}), private = list(secret = 1))"
    ))
    expect_identical(r6_symbol_hover(fixture, 1L, "self")$contents[[2L]], "R6 instance of `Widget`")
    expect_identical(r6_symbol_hover(fixture, 2L, "alias")$contents[[2L]], "R6 instance of `Widget`")
    expect_false(any(grepl("Widget", r6_symbol_hover(fixture, 4L, "self")$contents, fixed = TRUE)))
    fixture <- r6_symbol_fixture(c(
        'C <- R6::R6Class("Widget", public = list(probe = function(self, private, super) {',
        "  self", "  private", "  super", "}))"
    ))
    for (row in 1:3) {
        expect_false(any(grepl("of `Widget`", r6_symbol_hover(fixture, row, trimws(fixture$document$line0(row)))$contents,
                    fixed = TRUE)))
    }
    fixture <- r6_symbol_fixture(c("self <- 1", "f <- function() self"))
    expect_false(any(grepl("R6", r6_symbol_hover(fixture, 1L, "self")$contents, fixed = TRUE)))
})

test_that("R6 hover recovers large methods while preserving earlier assignments", {
    fixture <- r6_symbol_fixture(c(
        'C <- R6::R6Class("Widget", public = list(',
        rep("  earlier = function() NULL,", 150L),
        "  probe = function() {", "    alias <- self", rep("    1", 150L),
        "    self", "    alias", "    self <- 42", "    self", "  }))"
    ))
    expect_identical(r6_symbol_hover(fixture, 303L, "self")$contents[[2L]], "R6 instance of `Widget`")
    expect_identical(r6_symbol_hover(fixture, 304L, "alias")$contents[[2L]], "R6 instance of `Widget`")
    expect_false(any(grepl("Widget", r6_symbol_hover(fixture, 306L, "self")$contents, fixed = TRUE)))
    fixture <- r6_symbol_fixture(c(
        'C <- R6::R6Class("Widget", public = list(probe = function() {', "  self"
    ))
    expect_true(fixture$document$parse_data$parse_error)
    expect_identical(r6_symbol_hover(fixture, 1L, "self")$contents[[2L]], "R6 instance of `Widget`")
})

test_that("R6 scope recovery selects the block at the cursor", {
    fixture <- r6_symbol_fixture(c(
        'C <- R6::R6Class("Widget", public = list(probe = function() {',
        "  { self <- 1 }", "  self", "  { private <- 1; private }", "  super", "}))"
    ))
    expect_false(any(grepl("of `Widget`", r6_symbol_hover(fixture, 2L, "self")$contents, fixed = TRUE)))
    expect_false(any(grepl("of `Widget`", r6_symbol_hover(fixture, 3L, "private")$contents, fixed = TRUE)))
    expect_null(member_symbol(fixture$uri, fixture$workspace, fixture$document,
            member_symbol_location(fixture$document, list(row = 4L, col = 3L)))$value$r6_role)
})

test_that("Non-portable R6 context bindings carry class descriptions", {
    fixture <- r6_symbol_fixture(c(
        'classname <- "Widget"',
        "C <- R6::R6Class(classname = classname, portable = FALSE, public = list(probe = function() {",
        "  clone", "  .__enclos_env__", "  .__active__", "}), active = list(value = function() stop()))"
    ))
    for (row in 2:4) {
        symbol <- trimws(fixture$document$line0(row))
        hover <- r6_symbol_hover(fixture, row, symbol)
        expect_match(hover$contents[[2L]], "of `Widget`", fixed = TRUE)
        items <- context_scope_completion(fixture$uri, fixture$workspace, symbol,
            list(row = row, col = nchar(fixture$document$line0(row))), FALSE, Inf, character())
        expect_identical(items[[1L]]$documentation$value, hover$contents[[2L]])
    }
    expect_identical(r6_symbol_hover(fixture, 2L, "clone")$contents[[1L]], "```r\nclone(deep = FALSE)\n```")
})

test_that("R6 reference hover excludes strings, comments and prose and uses UTF-16 ranges", {
    for (text in c("  # self", '  "self"', '  r"(self)"')) {
        fixture <- r6_symbol_fixture(c(
            'C <- R6::R6Class("Widget", public = list(probe = function() {', text, "}))"
        ))
        expect_null(r6_symbol_hover(fixture, 1L, "self"))
    }
    fixture <- r6_symbol_fixture(c(
        "```{r}", 'C <- R6::R6Class("Widget", public = list(probe = function() {',
        '  "\U0001f680"; self', "}))", "```", "self"
    ), language = "quarto")
    hover <- r6_symbol_hover(fixture, 2L, "self")
    expect_identical(hover$contents[[2L]], "R6 instance of `Widget`")
    expect_identical(hover$range$start$character, 8L)
    expect_identical(hover$range$end$character, 12L)
    expect_null(r6_symbol_hover(fixture, 5L, "self"))
})

test_that("R6 new hover, completion and signature use public initialize formals", {
    marker <- withr::local_tempfile()
    fixture <- r6_symbol_fixture(c(
        sprintf('C <- R6::R6Class("Widget", public = list(initialize = function(value, flag = {writeLines("ran", %s); TRUE}, ...) stop("initialize")))',
            encodeString(marker, quote = '"')),
        "C$new(flag = "
    ))
    signature <- r6_symbol_signature(fixture)
    expect_true(startsWith(signature$signatures[[1L]]$label, "new(value, flag = "))
    expect_identical(extract_parameter_names(signature$signatures[[1L]]$label), c("value", "flag", "..."))
    expect_identical(signature$signatures[[1L]]$documentation$value, "Create an R6 instance of `Widget`")
    expect_identical(signature$activeParameter, 1L)
    hover <- r6_symbol_hover(fixture, 1L, "new")
    expect_identical(hover$contents[[1L]], sprintf("```r\n%s\n```", signature$signatures[[1L]]$label))
    expect_identical(hover$contents[[2L]], "Create an R6 instance of `Widget`")
    items <- member_completion(fixture$uri, fixture$workspace, fixture$document,
        list(row = 1L, col = 4L), FALSE, 200L)
    expect_identical(items[[1L]]$detail, signature$signatures[[1L]]$label)
    expect_identical(items[[1L]]$documentation$value, "Create an R6 instance of `Widget`")
    arguments <- member_constructor_arguments(fixture$uri, fixture$workspace, fixture$document,
        list(row = 1L, col = 13L), "")
    expect_identical(vapply(arguments, `[[`, character(1L), "label"), c("value", "flag"))
    expect_false(file.exists(marker))
})

test_that("R6 constructors distinguish inherited, overridden and missing initializers", {
    parent <- 'Base <- R6::R6Class("Parent", public = list(initialize = function(value, flag = TRUE) NULL))'
    for (case in list(
        list(definition = 'C <- R6::R6Class("Widget", inherit = Base)', expected = "new(value, flag = TRUE)"),
        list(definition = 'C <- R6::R6Class("Widget", inherit = Base, public = list(initialize = function(key = 1) NULL))', expected = "new(key = 1)"),
        list(definition = 'C <- R6::R6Class("Widget")', expected = "new()"),
        list(definition = 'C <- R6::R6Class("Widget", private = list(initialize = function(secret) NULL))', expected = "new()"),
        list(definition = 'C <- R6::R6Class("Widget", inherit = unknown)', expected = "new(...)"),
        list(definition = 'C <- R6::R6Class("Widget", public = list(initialize = NULL))', expected = "new()"),
        list(definition = 'C <- R6::R6Class("Widget", public = list(initialize = unknown))', expected = "new(...)"))) {
        fixture <- r6_symbol_fixture(c(parent, case$definition, "Alias <- C", "Alias$new("))
        expect_identical(r6_symbol_signature(fixture)$signatures[[1L]]$label, case$expected)
        expect_identical(r6_symbol_hover(fixture, 3L, "new")$contents,
            c(sprintf("```r\n%s\n```", case$expected), "Create an R6 instance of `Widget`"))
        expect_identical(r6_symbol_hover(fixture, 3L, "Alias")$contents[[2L]], "R6 class generator of `Widget`")
    }
    fixture <- r6_symbol_fixture(c(parent, "C <- list(new = function(ordinary = 1) NULL)", "C$new("))
    expect_identical(r6_symbol_signature(fixture)$signatures[[1L]]$label, "new(ordinary = 1)")
    expect_identical(r6_symbol_hover(fixture, 2L, "new")$contents, "```r\nnew(ordinary = 1)\n```")
    fixture <- r6_symbol_fixture(c(parent, "construct <- Base$new", "construct(flag = "))
    expect_identical(r6_symbol_signature(fixture)$signatures[[1L]]$label, "construct(value, flag = TRUE)")
    expect_identical(r6_symbol_hover(fixture, 2L, "construct")$contents[[2L]], "Create an R6 instance of `Parent`")
})

test_that("Runtime R6 metadata retains declared names and constructor signatures", {
    marker <- withr::local_tempfile()
    generator <- R6::R6Class("RuntimeWidget", public = list(
        initialize = function(value, flag = TRUE) writeLines("ran", marker)
    ))
    index <- member_package_index(list(package = "fixture"))
    value <- member_r6_runtime_shape(generator, index)
    symbol <- member_symbol_info("new", value$fields$new, NULL, index)
    expect_identical(symbol$signature, "new(value, flag = TRUE)")
    expect_identical(symbol$description, "Create an R6 instance of `RuntimeWidget` (package `fixture`)")
    expect_identical(value$fields$new$result_shape$r6_class$name, "RuntimeWidget")
    value <- member_r6_runtime_shape(R6::R6Class("EmptyWidget"), index)
    expect_identical(member_symbol_info("new", value$fields$new, NULL, index)$signature, "new()")
    expect_false(file.exists(marker))
})

test_that("R6 references and new providers refresh on the first request after edits", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    uri <- path_to_uri(path)
    did_open(client, path, text = 'C <- R6::R6Class("Widget")')
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    code <- c('C <- R6::R6Class("Widget", public = list(',
        "  initialize = function(value, flag = TRUE) NULL,", "  probe = function() self))")
    notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = 2L),
            contentChanges = list(list(text = paste(code, collapse = "\n")))))
    expect_identical(respond_hover(client, path, c(2L, 23L), retry = FALSE)$contents[[2L]], "R6 instance of `Widget`")
    code <- c(code, "C$new(flag = ")
    notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = 3L),
            contentChanges = list(list(text = paste(code, collapse = "\n")))))
    result <- respond_signature(client, path, c(3L, 13L), retry = FALSE)
    expect_identical(result$signatures[[1L]]$label, "new(value, flag = TRUE)")
    expect_identical(result$activeParameter, 1L)
    expect_identical(respond_hover(client, path, c(3L, 3L), retry = FALSE)$contents[[2L]], "Create an R6 instance of `Widget`")
})

test_that("Stale comment ranges do not hide edited R6 references", {
    fixture <- r6_symbol_fixture(c('C <- R6::R6Class("Widget", public = list(probe = function() {',
            "  # self", "}))"))
    fixture$document$set_content(2L, c(fixture$document$content[[1L]], "  self", "}))"))
    expect_false(is.null(member_symbol_location(fixture$document, list(row = 1L, col = 3L))))
})
