s4_fixture <- function(lines, snapshots = list(), point = NULL, language = "r") {
    uri <- if (language == "r") "file:///s4-members.R" else "file:///s4-members.qmd"
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

s4_items <- function(lines, snapshots = list(), point = NULL, language = "r") {
    fixture <- s4_fixture(lines, snapshots, point, language)
    member_completion(fixture$uri, fixture$workspace, fixture$document, fixture$point, TRUE, 200L)
}

s4_labels <- function(...) vapply(s4_items(...), `[[`, character(1L), "label")

test_that("S4 slots follow declarations, inheritance, factories and aliases", {
    declarations <- c(
        "setClass(\"Parent\", slots=c(value=\"numeric\"))",
        "Widget <- methods::setClass(\"Widget\", contains=\"Parent\", slots=list(label=\"character\"))"
    )
    expect_identical(s4_labels(c(declarations, "x <- new(\"Widget\")", "y <- x", "y@")), c("label", "value"))
    expect_identical(s4_labels(c(declarations, "Widget()@")), c("label", "value"))
    expect_identical(s4_labels(c(
        declarations,
        "factory <- function(x) new(\"Widget\", value=x)",
        "factory(stop(\"must not run\"))@"
    )), c("label", "value"))
    expect_identical(s4_labels(c(
        "setClass(\"Legacy\", representation(value=\"numeric\", \"Parent\"))",
        declarations[[1L]], "new(\"Legacy\")@"
    )), "value")
    expect_identical(s4_labels(c(
        "factory <- function() {setClass(\"Local\", slots=c(value=\"numeric\")); new(\"Local\")}",
        "factory()@"
    )), "value")
    expect_identical(s4_labels(c(
        "setClass(\"Vector\", contains=\"numeric\", slots=c(label=\"character\"))",
        "new(\"Vector\")@"
    )), c(".Data", "label"))
    expect_true("@" %in% unlist(CompletionOptions$triggerCharacters))
})

test_that("S4 nested slots retain declared and supplied structures", {
    declarations <- c(
        "setClass(\"Leaf\", slots=c(value=\"numeric\"))",
        "setClass(\"Tree\", slots=c(leaf=\"Leaf\", items=\"list\"))"
    )
    expect_identical(s4_labels(c(declarations, "new(\"Tree\")@leaf@")), "value")
    expect_identical(s4_labels(c(declarations, "new(\"Tree\", items=list(alpha=1))@items$")), "alpha")
    expect_identical(s4_labels(c(declarations, "methods::slot(new(\"Tree\"), \"leaf\")@")), "value")
    expect_length(s4_labels(c(declarations, "new(\"Tree\")$")), 0L)
    expect_length(s4_labels("list(value=1)@"), 0L)
    expect_length(s4_labels(c(declarations, "new(\"Tree\")@missing@")), 0L)
    expect_identical(s4_labels(c(
        "setClass(\"A\", slots=c(common=\"numeric\", a=\"character\"))",
        "setClass(\"B\", slots=c(common=\"numeric\", b=\"character\"))",
        "setClassUnion(\"Either\", c(\"A\", \"B\"))",
        "setClass(\"Box\", slots=c(item=\"Either\"))", "new(\"Box\")@item@"
    )), "common")
})

test_that("S4 slot completion recovers edits and quotes names", {
    declaration <- "setClass(\"Widget\", slots=c(`a b`=\"numeric\", beta=\"character\"))"
    expect_identical(s4_labels(c("bad <- )", declaration, "new(\"Widget\") @  be")), "beta")
    items <- s4_items(c(declaration, "x <- new(\"Widget\")", "x@`a"))
    expect_identical(items[[1L]]$textEdit$newText, "`a b`")
    expect_identical(items[[1L]]$textEdit$range$start$character, 2L)
    expect_identical(items[[1L]]$detail, "a b: numeric")
    expect_length(s4_labels(c(declaration, "# x@")), 0L)
    expect_length(s4_labels(c(declaration, "\"x@\"")), 0L)
    item <- s4_items(c(declaration, "x <- new(\"Widget\")", "\"\U0001f680\"; x@beXYZ"), point = list(row = 2L, col = 9L))[[1L]]
    expect_identical(item$textEdit$range$start$character, 8L)
    expect_identical(item$textEdit$range$end$character, 13L)
    expect_identical(s4_labels(c("```{r}", declaration, "new(\"Widget\")@", "```"),
            point = list(row = 2L, col = 14L), language = "quarto"), c("a b", "beta"))
    expect_length(s4_labels(c("```{r}", declaration, "```", "```{python}", "new(\"Widget\")@", "```"),
            point = list(row = 4L, col = 14L), language = "quarto"), 0L)
})

test_that("S4 declarations remain conservative around shadowing and unknown types", {
    declaration <- "setClass(\"Widget\", slots=c(value=\"numeric\"))"
    expect_length(s4_labels(c("new(\"Widget\")@", declaration), point = list(row = 0L, col = 14L)), 0L)
    expect_length(s4_labels(c(declaration, "new <- function(...) unknown()", "new(\"Widget\")@")), 0L)
    expect_identical(s4_labels(c(declaration, "new <- function(...) unknown()", "methods::new(\"Widget\")@")), "value")
    expect_length(s4_labels(c("setClass <- function(...) NULL", declaration, "new(\"Widget\")@")), 0L)
    expect_length(s4_labels(c(declaration, "new(class_name)@")), 0L)
    expect_length(s4_labels(c("setClass(\"Widget\", slots=dynamic)", "new(\"Widget\")@")), 0L)
    expect_length(s4_labels(c("c <- function(...) unknown", declaration, "new(\"Widget\")@")), 0L)
    expect_length(s4_labels(c("list <- function(...) unknown", "setClass(\"Widget\", slots=list(value=\"numeric\"))", "new(\"Widget\")@")), 0L)
    expect_length(s4_labels(c("setClass(\"Widget\", contains=\"Unknown\")", "new(\"Widget\")@")), 0L)
    expect_length(s4_labels(c("setClass(\"Widget\", contains=\"Widget\")", "new(\"Widget\")@")), 0L)
    expect_length(s4_labels(c("setClass(\"Widget\", contains=\"VIRTUAL\")", "new(\"Widget\")@")), 0L)
    expect_identical(s4_labels(c(declaration, "setClass(\"Widget\", slots=c(changed=\"logical\"))", "new(\"Widget\")@")), "changed")
})

test_that("S4 hover and signatures use known function slot syntax without execution", {
    marker <- withr::local_tempfile()
    declarations <- c(
        "setClass(\"Widget\", slots=c(value=\"numeric\", run=\"function\"),",
        "prototype=list(run=function(value, flag=TRUE) value), validity=function(object) stop(\"validity\"))",
        "setMethod(\"initialize\", \"Widget\", function(.Object, ...) stop(\"initialize\"))",
        "x <- new(\"Widget\")"
    )
    fixture <- s4_fixture(c(declarations, "x@run(flag = "))
    signature <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point)$result
    expect_identical(signature$signatures[[1L]]$label, "run(value, flag = TRUE)")
    expect_identical(signature$activeParameter, 1L)
    hover <- hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, list(row = 4L, col = 4L))$result
    expect_identical(hover$contents, "```r\nrun(value, flag = TRUE)\n```")
    fixture <- s4_fixture(c(declarations, "x@value"))
    hover <- hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point)$result
    expect_identical(hover$contents, "```r\ndouble\n```")

    fixture <- s4_fixture(c(declarations, sprintf(
        "new(\"Widget\", run=function(first, second=2) NULL)@run({writeLines(\"ran\", %s); stop(\"argument\")}, ",
        encodeString(marker, quote = "\"")
    )))
    signature <- signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point)$result
    expect_identical(signature$signatures[[1L]]$label, "run(first, second = 2)")
    expect_false(file.exists(marker))
    fixture <- s4_fixture(c("setClass(\"Opaque\", slots=c(run=\"function\"))", "new(\"Opaque\")@run("))
    expect_length(signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point)$result$signatures, 0L)
})

test_that("Installed S4 metadata supplies current slots without constructing objects", {
    skip_if_not_installed("stats4")
    snapshot <- member_prepare_package("stats4")
    expected <- sort(names(methods::getSlots(methods::getClassDef("mle", where = asNamespace("stats4")))))
    expect_identical(s4_labels(c("library(stats4)", "new(\"mle\")@"), list(stats4 = snapshot)), expected)
    expect_identical(s4_labels("stats4::mle(function(x) x^2)@", list(stats4 = snapshot)), expected)
    expect_length(s4_labels("new(\"mle\")@", list(stats4 = snapshot)), 0L)
    expect_identical(member_index_freeze(member_index_thaw(snapshot))$s4_classes, snapshot$s4_classes)
})

test_that("S4 namespace constructors and objects use inert descriptor metadata", {
    scope <- new.env(parent = asNamespace("methods"))
    name <- paste0("S4Fixture", Sys.getpid())
    scope$Widget <- methods::setClass(name, slots = c(value = "numeric"), where = scope, package = "s4fixture")
    withr::defer(suppressWarnings(methods::removeClass(name, where = scope)))
    scope$object <- scope$Widget()
    marker <- withr::local_tempfile()
    makeActiveBinding("unsafe", function() {
        writeLines("ran", marker)
        stop("getter")
    }, scope)
    input <- member_namespace_input(scope, package = "s4fixture", exports = c("Widget", "object"))
    snapshot <- member_index_freeze(member_package_index(input))
    expect_identical(s4_labels(c("library(s4fixture)", "Widget()@"), list(s4fixture = snapshot)), "value")
    expect_identical(s4_labels("s4fixture::Widget()@", list(s4fixture = snapshot)), "value")
    expect_identical(s4_labels(c("library(s4fixture)", "object@"), list(s4fixture = snapshot)), "value")
    expect_false(file.exists(marker))
    changed <- snapshot
    changed$s4_classes[[name]]$slots <- list(changed = "character")
    expect_identical(s4_labels("s4fixture::Widget()@", list(s4fixture = changed)), "changed")
})

test_that("S4 dependency slots and union branches preserve class package identities", {
    skip_if_not_installed("stats4")
    loadNamespace("stats4")
    classes <- list(Box = list(name = "Box", slots = list(fit = structure("mle", package = "stats4"))))
    dependencies <- member_s4_dependencies(classes)
    expect_true("mle" %in% names(dependencies$stats4))
    input <- list(package = "s4fixture", definitions = list(), exports = "object", s4_classes = classes,
        s4_dependencies = dependencies, s4_objects = list(object = list(name = "Box", package = "s4fixture")))
    snapshot <- member_index_freeze(member_package_index(input))
    expect_true("coef" %in% s4_labels("s4fixture::object@fit@", list(s4fixture = snapshot)))
    fixture <- s4_fixture("s4fixture::object@fit@coef", list(s4fixture = snapshot))
    expect_identical(hover_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point)$result$contents, "```r\ndouble\n```")

    index <- member_generic_index("")
    index$s4_dependencies <- list(
        a = list(Leaf = list(slots = list(common = "numeric", a = "character"))),
        b = list(Leaf = list(slots = list(common = "numeric", b = "character")))
    )
    a <- member_value(type = "Box", slot_types = list(item = "Leaf"), s4_class = list(name = "Box", package = "a"))
    b <- member_value(type = "Box", slot_types = list(item = "Leaf"), s4_class = list(name = "Box", package = "b"))
    nested <- member_s4_slot(member_join(a, b), "item", index, list())
    expect_identical(names(nested$slot_types), "common")
})

test_that("S4 source packages, local slot writes and prototype cycles stay static", {
    code <- c("Widget <- methods::setClass(\"Widget\", slots=c(value=\"numeric\"))")
    input <- list(package = "s4fixture", exports = "Widget", definitions = list(Widget = parse(text = code)[[1L]][[3L]]),
        expressions = list(parse(text = code)))
    snapshot <- member_index_freeze(member_package_index(input))
    expect_identical(s4_labels("s4fixture::Widget()@", list(s4fixture = snapshot)), "value")
    fixture <- s4_fixture(c(
        "setClass(\"Widget\", slots=c(run=\"function\"))",
        "factory <- function() {x <- new(\"Widget\"); x@run <- function(first, second=2) NULL; x}",
        "factory()@run("
    ))
    expect_identical(signature_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point)$result$signatures[[1L]]$label,
        "run(first, second = 2)")
    expect_identical(s4_labels(c(
        "setClass(\"Loop\", slots=c(item=\"Loop\"), prototype=list(item=new(\"Loop\")@item))",
        "new(\"Loop\")@item@"
    )), "item")
    index <- member_generic_index("")
    index$s4_classes <- list(Widget = list(slots = list(value = "numeric")))
    budget <- new.env(parent = emptyenv())
    budget$remaining <- 0L
    expect_identical(member_s4_shape("Widget", index, list(), budget = budget)$reason, "budget")
    expect_true(budget$exhausted)
})

test_that("S4 providers wait for the first request after an edit over LSP", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    uri <- path_to_uri(path)
    lines <- c(
        "setClass(\"Widget\", slots=c(value=\"numeric\", run=\"function\"), prototype=list(run=function(value, flag=TRUE) value))",
        "x <- new(\"Widget\")", "x"
    )
    did_open(client, path, text = paste(lines, collapse = "\n"))
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    edit <- function(text, version) {
        lines[[3L]] <<- text
        notify(client, "textDocument/didChange", list(
            textDocument = list(uri = uri, version = version),
            contentChanges = list(list(text = paste(lines, collapse = "\n")))
        ))
    }
    edit("x@", 2L)
    result <- respond_completion(client, path, c(2L, 2L), retry = FALSE)
    expect_setequal(vapply(result$items, `[[`, character(1L), "label"), c("run", "value"))
    edit("x@run(flag = ", 3L)
    result <- respond_signature(client, path, c(2L, nchar(lines[[3L]])), retry = FALSE)
    expect_identical(result$signatures[[1L]]$label, "run(value, flag = TRUE)")
    edit("x@value", 4L)
    result <- respond_hover(client, path, c(2L, 4L), retry = FALSE)
    expect_identical(result$contents[[1L]], "```r\ndouble\n```")
})
