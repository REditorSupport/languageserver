s7_fixture <- function(lines, snapshots = list(), point = NULL, language = "r") {
    uri <- if (language == "r") "file:///s7-members.R" else "file:///s7-members.qmd"
    document <- Document$new(uri, language = language, version = 1L, content = lines)
    data <- parse_document(uri, lines, is_rmarkdown = document$is_rmarkdown)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    for (package in names(snapshots)) metadata$set(package, member_index_thaw(snapshots[[package]]))
    workspace <- list(member_metadata = metadata, get_documentation = function(...) NULL)
    if (is.null(point)) point <- list(row = length(lines) - 1L, col = nchar(tail(lines, 1L)))
    list(uri = uri, document = document, workspace = workspace, point = point)
}

s7_items <- function(lines, snapshots = list(), point = NULL, language = "r") {
    fixture <- s7_fixture(lines, snapshots, point, language)
    member_completion(fixture$uri, fixture$workspace, fixture$document, fixture$point, TRUE, 200L)
}

s7_labels <- function(...) vapply(s7_items(...), `[[`, character(1L), "label")

s7_signature <- function(fixture) {
    signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point)$result
}

s7_hover <- function(fixture, point = fixture$point) {
    hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)$result
}

test_that("S7 properties and constructor signatures follow the user's declarations", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    code <- c("library(S7)",
        "Dog <- new_class(\"Dog\", properties=list(name=class_character, age=class_numeric))",
        "lola <- Dog(name=\"Lola\", age=11)")
    expect_identical(s7_labels(c(code, "lola@"), list(S7 = snapshot)), c("age", "name"))
    expect_identical(s7_labels(c(code, "lola@ag"), list(S7 = snapshot)), "age")
    expect_length(s7_labels(c(code, "lola$"), list(S7 = snapshot)), 0L)
    fixture <- s7_fixture(c(code, "lola@age"), list(S7 = snapshot))
    expect_identical(s7_hover(fixture)$contents, "```r\n11\n```")
    fixture <- s7_fixture(c(code, "Dog()@age"), list(S7 = snapshot))
    expect_identical(s7_hover(fixture)$contents, "```r\ndouble | integer\n```")
    expected <- "Dog(name = character(0), age = integer(0))"
    fixture <- s7_fixture(c(code, "Dog(age = "), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, expected)
    expect_identical(s7_signature(fixture)$activeParameter, 1L)
    args <- member_constructor_arguments(fixture$uri, fixture$workspace, fixture$document, fixture$point, "")
    expect_identical(vapply(args, `[[`, character(1L), "label"), c("name", "age"))
    expect_identical(args[[2L]]$insertText, "age = ")
    fixture <- s7_fixture(c(code, "Dog"), list(S7 = snapshot))
    expect_identical(s7_hover(fixture)$contents, sprintf("```r\n%s\n```", expected))
    expect_identical(s7_hover(fixture)$range$start$character, 0L)
    fixture <- s7_fixture(c(code, "Alias <- Dog", "Alias("), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, sub("Dog", "Alias", expected))
    fixture <- s7_fixture(c(code, "Dog@constructor("), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, sub("Dog", "constructor", expected))
    expect_length(s7_labels(c(code, "Dog@constructor()@"), list(S7 = snapshot)), 0L)
})

test_that("S7 inheritance, nested properties, defaults and unions remain static", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    code <- c("library(S7)",
        "Leaf <- new_class(\"Leaf\", properties=list(value=class_numeric))",
        "Box <- new_class(\"Box\", properties=list(leaf=Leaf, items=class_list))",
        "Child <- new_class(\"Child\", parent=Box, properties=list(label=class_character))")
    expect_identical(s7_labels(c(code, "Child()@"), list(S7 = snapshot)), c("items", "label", "leaf"))
    expect_identical(s7_labels(c(code, "Child()@leaf@"), list(S7 = snapshot)), "value")
    expect_identical(s7_labels(c(code, "Box(items=list(alpha=1))@items$"), list(S7 = snapshot)), "alpha")
    expect_identical(s7_labels(c(code, "alias <- Child", "factory <- function() alias()", "factory()@leaf@"), list(S7 = snapshot)), "value")
    fixture <- s7_fixture(c(code, "Child("), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, "Child(leaf = Leaf(), items = list(), label = character(0))")
    code <- c(code,
        "Other <- new_class(\"Other\", properties=list(value=class_numeric, extra=class_character))",
        "UnionBox <- new_class(\"UnionBox\", properties=list(item=new_union(Leaf, Other)))",
        "Defaults <- new_class(\"Defaults\", properties=list(items=new_property(class_list, default=quote(list(alpha=1)))))")
    expect_identical(s7_labels(c(code, "UnionBox()@item@"), list(S7 = snapshot)), "value")
    expect_identical(s7_labels(c(code, "Defaults()@items$"), list(S7 = snapshot)), "alpha")
    expect_identical(s7_labels(c(code, "S7::prop(Child(), \"leaf\")@"), list(S7 = snapshot)), "value")
    expect_identical(s7_labels(c(code, "S7::set_props(Box(), items=list(beta=2))@items$"), list(S7 = snapshot)), "beta")
    expect_identical(s7_labels(c("library(S7)", "Named <- new_class(\"Named\", constructor=NULL, properties=list(new_property(class_numeric, name=\"value\")))", "Named()@"), list(S7 = snapshot)), "value")
    fixture <- s7_fixture(c("library(S7)", "Empty <- new_class(\"Empty\", properties=list())", "Empty("), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, "Empty()")
})

test_that("S7 custom constructors and function properties use syntax without executing callbacks", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    marker <- withr::local_tempfile()
    code <- c("library(S7)",
        "Runner <- new_class(\"Runner\", properties=list(run=class_function, items=class_list),",
        "constructor=function(first, option=TRUE) new_object(S7_object(),",
        "run=function(value, flag=FALSE) value, items=list(done=first)))",
        "x <- Runner(unresolved)")
    fixture <- s7_fixture(c(code, "Runner("), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, "Runner(first, option = TRUE)")
    fixture <- s7_fixture(c(code, "x@run(flag = "), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, "run(value, flag = FALSE)")
    expect_identical(s7_hover(fixture, list(row = 5L, col = 4L))$contents, "```r\nrun(value, flag = FALSE)\n```")
    expect_identical(s7_labels(c(code, "x@items$"), list(S7 = snapshot)), "done")
    code <- c("library(S7)", sprintf(
        "Safe <- new_class(\"Safe\", properties=list(value=new_property(class_numeric, getter=function(self) {writeLines(\"getter\", %s); stop(\"getter\")}), writable=new_property(class_numeric, setter=function(self,value) stop(\"setter\"))), constructor=function(x=1) {stop(\"constructor\"); new_object(S7_object())}, validator=function(self) stop(\"validator\"))",
        encodeString(marker, quote = "\"")))
    expect_identical(s7_labels(c(code, "Safe(stop(\"argument\"))@"), list(S7 = snapshot)), c("value", "writable"))
    fixture <- s7_fixture(c(code, "Safe()@value"), list(S7 = snapshot))
    expect_identical(s7_hover(fixture)$contents, "```r\ndouble | integer\n```")
    expect_false(file.exists(marker))
    fixture <- s7_fixture(c("library(S7)", "ReadOnly <- new_class(\"ReadOnly\", properties=list(value=new_property(class_numeric, getter=function(self) 1)))", "ReadOnly("), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, "ReadOnly()")
})

test_that("S7 recovery supports multiline and quoted properties and stays conservative", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    code <- c("Widget <- S7::new_class(\"Widget\", properties=list(`a b`=S7::class_numeric, beta=S7::class_character))")
    items <- s7_items(c(code, "x <- Widget()", "x@", "  `a"), list(S7 = snapshot))
    expect_identical(items[[1L]]$textEdit$newText, "`a b`")
    expect_identical(items[[1L]]$textEdit$range$start$character, 2L)
    expect_identical(s7_labels(c("bad <- )", code, "Widget()@be"), list(S7 = snapshot)), "beta")
    expect_length(s7_labels(c(code, "# Widget()@"), list(S7 = snapshot)), 0L)
    expect_length(s7_labels(c(code, "Widget <- function(...) unknown", "Widget()@"), list(S7 = snapshot)), 0L)
    expect_length(s7_labels(c("new_class <- function(...) unknown", "Widget <- new_class(\"Widget\", properties=list(beta=S7::class_character))", "Widget()@"), list(S7 = snapshot)), 0L)
    expect_length(s7_labels(c("Widget <- S7::new_class(\"Widget\", properties=unknown)", "Widget()@"), list(S7 = snapshot)), 0L)
    expect_length(s7_labels(c("Widget <- S7::new_class(\"Widget\", parent=unknown)", "Widget()@"), list(S7 = snapshot)), 0L)
    expect_length(s7_labels(c("Widget <- S7::new_class(\"Widget\", abstract=TRUE)", "Widget()@"), list(S7 = snapshot)), 0L)
    fixture <- s7_fixture(c("library(S7)", "Widget <- new_class(\"Widget\", properties=list(beta=class_character))", "f <- function(Widget) Widget("), list(S7 = snapshot))
    expect_null(member_constructor_symbol(fixture$uri, fixture$workspace, fixture$document,
            member_call_location(fixture$document, fixture$point, symbols = TRUE)))
    expect_identical(s7_labels(c("```{r}", code, "Widget()@", "```"), list(S7 = snapshot),
            point = list(row = 2L, col = 9L), language = "quarto"), c("a b", "beta"))
    expect_length(s7_labels(c("```{r}", code, "```", "```{python}", "Widget()@", "```"), list(S7 = snapshot),
            point = list(row = 4L, col = 9L), language = "quarto"), 0L)
})

test_that("Installed S7 metadata supplies current properties and constructor formals", {
    skip_if_not_installed("S7")
    marker <- withr::local_tempfile()
    scope <- new.env(parent = asNamespace("S7"))
    scope$Widget <- S7::new_class("Widget", properties = list(size = S7::class_numeric),
        constructor = function(value = 1, flag = TRUE) {
            writeLines("ran", marker)
            stop("constructor")
            S7::new_object(S7::S7_object())
        })
    scope$api <- new.env(parent = emptyenv())
    scope$api$Widget <- scope$Widget
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(scope, package = "s7fixture", exports = c("Widget", "api"))))
    expect_identical(s7_labels("s7fixture::Widget()@", list(s7fixture = snapshot)), "size")
    expect_identical(s7_labels("s7fixture::api$Widget()@", list(s7fixture = snapshot)), "size")
    fixture <- s7_fixture("s7fixture::Widget(", list(s7fixture = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, "Widget(value = 1, flag = TRUE)")
    expect_identical(s7_hover(fixture, list(row = 0L, col = 14L))$contents, "```r\nWidget(value = 1, flag = TRUE)\n```")
    scope$Widget <- S7::new_class("Widget", properties = list(changed = S7::class_character))
    changed <- member_index_freeze(member_package_index(member_namespace_input(scope, package = "s7fixture", exports = "Widget")))
    expect_identical(s7_labels("s7fixture::Widget()@", list(s7fixture = changed)), "changed")
    expect_false(file.exists(marker))
})

test_that("S7 constructor and property providers work on first requests after edits", {
    skip_on_cran()
    skip_if_not_installed("S7")
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    uri <- path_to_uri(path)
    code <- c("library(S7)", "Dog <- new_class(\"Dog\", properties=list(name=class_character, age=class_numeric))", "lola <- Dog(name=\"Lola\", age=11)")
    did_open(client, path, text = c(code, "lola"))
    # Namespace preparation is asynchronous; wait for it before edit regressions.
    expected <- "Dog(name = character(0), age = integer(0))"
    ready <- respond_signature(client, path, c(2L, 12L),
        retry_when = function(result) !identical(result$signatures[[1L]]$label, expected))
    expect_identical(ready$signatures[[1L]]$label, expected)
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    edit <- function(line, version) {
        notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = version),
                contentChanges = list(list(text = paste(c(code, line), collapse = "\n")))))
    }
    edit("lola@", 2L)
    result <- respond_completion(client, path, c(3L, 5L), retry = FALSE)
    expect_setequal(vapply(result$items, `[[`, character(1L), "label"), c("age", "name"))
    edit("Dog(ag", 3L)
    result <- respond_completion(client, path, c(3L, 6L), retry = FALSE)
    args <- Filter(function(item) identical(item$insertText, "age = "), result$items)
    expect_length(args, 1L)
    edit("Dog(age = ", 4L)
    expect_identical(respond_signature(client, path, c(3L, 10L), retry = FALSE)$signatures[[1L]]$label, "Dog(name = character(0), age = integer(0))")
    edit("Dog", 5L)
    expect_identical(respond_hover(client, path, c(3L, 1L), retry = FALSE)$contents[[1L]], "```r\nDog(name = character(0), age = integer(0))\n```")
    edit("lola@age", 6L)
    expect_identical(respond_hover(client, path, c(3L, 6L), retry = FALSE)$contents[[1L]], "```r\n11\n```")
})
