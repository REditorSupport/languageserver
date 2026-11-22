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
    expect_length(member_call_arguments(fixture$uri, fixture$workspace, fixture$document, fixture$point, ""), 0L)
    fixture <- s7_fixture(c(code, "Dog("), list(S7 = snapshot))
    args <- member_call_arguments(fixture$uri, fixture$workspace, fixture$document, fixture$point, "")
    expect_identical(vapply(args, `[[`, character(1L), "label"), c("name", "age"))
    expect_identical(args[[2L]]$insertText, "age = ")
    fixture <- s7_fixture(c(code, "Dog"), list(S7 = snapshot))
    expect_identical(s7_hover(fixture)$contents, sprintf("```r\n%s\n```", expected))
    expect_identical(s7_hover(fixture)$range$start$character, 0L)
    fixture <- s7_fixture(c(code, "Alias <- Dog", "Alias("), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, sub("Dog", "Alias", expected))
    fixture <- s7_fixture(c(code, "Dog@constructor("), list(S7 = snapshot))
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, sub("Dog", "constructor", expected))
    expected_slots <- if (isTRUE(snapshot$s7_capabilities$class_ref)) c("age", "name") else character()
    expect_identical(s7_labels(c(code, "Dog@constructor()@"), list(S7 = snapshot)), expected_slots)
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
    # This regression needs only its open document. Scanning the test checkout
    # queues unrelated package preparation and can delay S7 under parallel covr.
    root <- withr::local_tempdir()
    client <- language_client(working_dir = root)
    path <- file.path(root, "s7-members.R")
    uri <- path_to_uri(path)
    code <- c("library(S7)", "Dog <- new_class(\"Dog\", properties=list(name=class_character, age=class_numeric))", "lola <- Dog(name=\"Lola\", age=11)")
    did_open(client, path, text = c(code, "lola"))
    # Cold metadata workers share the CI runner with other test servers. This
    # barrier prepares the fixture; subsequent edits retain the ordinary
    # request timeout and still check their first response without retries.
    timeout <- if (identical(Sys.getenv("R_COVR"), "true")) 120 else 60
    expected <- "Dog(name = character(0), age = integer(0))"
    ready <- respond_signature(client, path, c(2L, 12L), timeout = timeout,
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

test_that("S7 installed instances and nested records preserve stored members safely", {
    skip_if_not_installed("S7")
    scope <- new.env(parent = asNamespace("S7"))
    scope$Dog <- S7::new_class("Dog", properties = list(age = S7::class_numeric,
            run = S7::class_function, items = S7::class_list))
    scope$lola <- scope$Dog(age = 11, run = function(value, flag = FALSE) value, items = list(alpha = 1))
    scope$api <- new.env(parent = emptyenv())
    scope$api$dogs <- list(lola = scope$lola)
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(scope,
                package = "s7fixture", exports = c("Dog", "lola", "api"))))
    metadata <- list(s7fixture = snapshot)
    for (path in c("s7fixture::lola", "s7fixture::api$dogs$lola")) {
        expect_identical(s7_labels(paste0(path, "@"), metadata), c("age", "items", "run"))
        expect_identical(s7_labels(paste0(path, "@items$"), metadata), "alpha")
        fixture <- s7_fixture(paste0(path, "@run("), metadata)
        expect_identical(s7_signature(fixture)$signatures[[1L]]$label, "run(value, flag = FALSE)")
        expect_identical(s7_hover(s7_fixture(paste0(path, "@age"), metadata))$contents, "```r\n11\n```")
    }
    marker <- withr::local_tempfile()
    ref <- new.env(parent = emptyenv())
    class(ref) <- "S7_class_ref"
    makeActiveBinding("class", function() {
        writeLines("ran", marker)
        scope$Dog
    }, ref)
    fake <- structure(list(), class = "S7_object", `_S7_class` = ref)
    expect_null(member_s7_instance_descriptor(fake))
    expect_false(file.exists(marker))
    # Legacy objects remain recognizable even with a newer installed S7.
    legacy <- structure(list(), class = "S7_object", S7_class = scope$Dog)
    expect_identical(member_s7_instance_descriptor(legacy)$name, "Dog")
})

test_that("S7 generated constructor signatures and matching follow installed rules", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    metadata <- list(S7 = snapshot)
    code <- c("library(S7)",
        "Parent <- new_class(\"Parent\", properties=list(payload=class_list), constructor=function(seed=1L, mode=\"raw\") new_object(S7_object(), payload=list(seed=seed)))",
        "Child <- new_class(\"Child\", parent=Parent, properties=list(label=class_character))")
    scope <- new.env(parent = asNamespace("S7"))
    eval(parse(text = code[-1L]), scope)
    expected <- get_signature("Child", member_s7_function(formals(scope$Child)))
    fixture <- s7_fixture(c(code, "Child("), metadata)
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, expected)
    expect_identical(s7_hover(s7_fixture(c(code, "Child"), metadata))$contents,
        sprintf("```r\n%s\n```", expected))
    args <- member_call_arguments(fixture$uri, fixture$workspace, fixture$document, fixture$point, "")
    expect_false("..." %in% vapply(args, `[[`, character(1L), "label"))
    expect_identical(s7_labels(c(code, "Child(3L, label=\"child\")@payload$"), metadata), "seed")
    if (isTRUE(snapshot$s7_capabilities$forward)) {
        expect_null(s7_hover(s7_fixture(c(code, "Child(3L, \"mode\", \"invalid\")@label"), metadata)))
        # No partial or positional matching of a child formal after dots.
        hover <- s7_hover(s7_fixture(c(code, "Child(3L, label=\"child\")@label"), metadata))
        expect_identical(hover$contents, "```r\n\"child\"\n```")
        expect_null(s7_hover(s7_fixture(c(code, "Child(lab=\"child\")@label"), metadata)))
    }
    overrides <- c("library(S7)",
        "Base <- new_class(\"Base\", properties=list(x=new_property(class_numeric, default=1)))",
        "Narrow <- new_class(\"Narrow\", parent=Base, properties=list(x=new_property(class_numeric, default=2)))")
    eval(parse(text = overrides[-1L]), scope)
    expected <- get_signature("Narrow", member_s7_function(formals(scope$Narrow)))
    expect_identical(s7_signature(s7_fixture(c(overrides, "Narrow("), metadata))$signatures[[1L]]$label, expected)
    expect_identical(s7_hover(s7_fixture(c(overrides, "Narrow()@x"), metadata))$contents,
        sprintf("```r\n%s\n```", scope$Narrow()@x))
    environment_parent <- c("library(S7)", "Env <- new_class(\"Env\", parent=class_environment, properties=list(age=class_numeric))")
    if (isTRUE(snapshot$s7_capabilities$class_ref)) {
        eval(parse(text = environment_parent[-1L]), scope)
        expect_identical(s7_signature(s7_fixture(c(environment_parent, "Env("), metadata))$signatures[[1L]]$label,
            get_signature("Env", member_s7_function(formals(scope$Env))))
    }
})

test_that("S7 property updates match named object parameters and invalidate unknown writes", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    metadata <- list(S7 = snapshot)
    code <- c("library(S7)",
        "Box <- new_class(\"Box\", properties=list(items=new_property(class_list, default=quote(list(old=1)))))")
    object <- snapshot$s7_capabilities$object
    update <- sprintf("S7::set_props(items=list(new=2), `%s`=Box(), .check=FALSE)@items$", object)
    expect_identical(s7_labels(c(code, update), metadata), "new")
    if (isTRUE(snapshot$s7_capabilities$lists)) {
        expect_identical(s7_labels(c(code, "set_props(Box(), list(items=list(new=2)))@items$"), metadata), "new")
        expect_length(s7_labels(c(code, "set_props(Box(), unknown)@items$"), metadata), 0L)
        custom <- c("library(S7)",
            "Box <- new_class(\"Box\", properties=list(items=class_list), constructor=function() new_object(S7_object(), list(items=list(alpha=1))))")
        expect_identical(s7_labels(c(custom, "Box()@items$"), metadata), "alpha")
    } else {
        expect_length(s7_labels(c(code, "set_props(Box(), list(items=list(new=2)))@items$"), metadata), 0L)
    }
})

test_that("S7 environment-backed classes retain ordinary fields through aliases", {
    skip_if_not_installed("S7", "1.0.0")
    metadata <- list(S7 = member_prepare_package("S7"))
    # class_environment reference example, also inside a function body.
    code <- c("library(S7)", "Counter := new_class(class_environment)",
        "counter <- Counter()", "counter$n <- 0L", "copy <- counter", "copy$n <- 10L")
    expect_identical(s7_labels(c(code, "counter$"), metadata), "n")
    expect_identical(s7_hover(s7_fixture(c(code, "counter$n"), metadata))$contents, "```r\n10L\n```")
    expect_identical(s7_labels(c(code, 'copy[["n'), metadata), "n")
    expect_length(s7_labels(c(code, "counter@"), metadata), 0L)
    expect_identical(s7_labels(c(code, "method(print, Counter) <- function(x, ...) x$n", "counter$"), metadata), "n")
    expect_identical(s7_labels(c(code[1:2], "f <- function() {", code[3:6], "counter", "}", "f()$"), metadata), "n")
    expect_length(s7_labels(c(code, "`$.Counter` <- function(x, name) 99L", "counter$"), metadata), 0L)
    expect_length(s7_labels(c(code, "method(`$`, Counter) <- function(x, name) 99L", "counter$"), metadata), 0L)

    # A registration from a different package may intercept an inherited
    # accessor. Discover its presence without invoking the method.
    owner <- new.env(parent = baseenv())
    owner$override <- function(x, name) stop("accessor must stay inert")
    table <- asNamespace("base")[[".__S3MethodsTable__."]]
    key <- "$.S7MemberEnvironmentBase"
    registerS3method("$", "S7MemberEnvironmentBase", owner$override, envir = asNamespace("base"))
    withr::defer(rm(list = key, envir = table))
    metadata$other <- member_index_freeze(member_package_index(member_namespace_input(owner, "other", exports = "override")))
    expect_identical(metadata$other$subset_registry[[key]], "opaque")
    subclass <- c(code[[1L]], "S7MemberEnvironmentBase := new_class(class_environment)",
        "Counter := new_class(S7MemberEnvironmentBase)", code[3:6])
    expect_length(s7_labels(c(subclass, "counter$"), metadata), 0L)
})

test_that("S7 accessor registrations are inspected without forcing user bindings", {
    skip_if_not_installed("S7")
    key <- "$.S7_object"
    table <- new.env(parent = emptyenv())
    scope <- new.env(parent = .BaseNamespaceEnv)
    scope$home <- asNamespace("S7")
    scope$method <- key
    delayedAssign(key, get(method, envir = home), scope, table)
    expect_identical(member_s7_registered_accessor(table, key), list("value", get(key, scope$home)))
    scope$get <- function(...) stop("custom get was called")
    expect_identical(member_s7_registered_accessor(table, key)[[1L]], "deferred")
    rm("get", envir = scope)
    delayedAssign("home", stop("home was forced"), assign.env = scope)
    expect_identical(member_s7_registered_accessor(table, key)[[1L]], "deferred")
    delayedAssign(key, stop("registry promise was forced"), assign.env = table)
    expect_identical(member_s7_registered_accessor(table, key)[[1L]], "deferred")
})

test_that("S7 named bindings are verified and recovered in document and function scopes", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    skip_if_not(isTRUE(snapshot$s7_capabilities$bind))
    metadata <- list(S7 = snapshot)
    code <- c("library(S7)", "Dog := new_class(properties=list(age=class_numeric))")
    expect_identical(s7_labels(c(code, "Dog()@"), metadata), "age")
    expect_identical(s7_signature(s7_fixture(c(code, "Dog("), metadata))$signatures[[1L]]$label,
        "Dog(age = integer(0))")
    expect_identical(s7_hover(s7_fixture(c(code, "Dog"), metadata))$contents,
        "```r\nDog(age = integer(0))\n```")
    expect_identical(s7_labels(c("library(S7)", "factory <- function() {", code[[2L]], "Dog()", "}", "factory()@"), metadata), "age")
    expect_identical(s7_labels(c("library(S7)", "f <- function() {", code[[2L]], "Dog()@", "}"), metadata,
            point = list(row = 3L, col = 6L)), "age")
    shadowed <- c("library(S7)", "`:=` <- function(lhs, rhs) NULL", code[[2L]], "Dog()@")
    expect_length(s7_labels(shadowed, metadata), 0L)
    expect_identical(s7_labels(c(code, "Dog := new_class(name=\"Other\", properties=list(wrong=class_numeric))", "Dog()@"), metadata), "age")
    expect_length(s7_labels(c("library(S7)", "Dog := unknown()", "Dog()@"), metadata), 0L)
})

test_that("S7 external descriptors respect exports, versions and recursion bounds", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    skip_if_not("new_external_class" %in% snapshot$s7_capabilities$exports)
    scope <- new.env(parent = asNamespace("S7"))
    scope$Dog <- S7::new_class("Dog", properties = list(age = S7::class_numeric))
    scope$Hidden <- scope$Dog
    dep <- member_index_freeze(member_package_index(member_namespace_input(scope,
                package = "s7fixture", exports = "Dog")))
    dep$identity <- list(version = "1.0.0")
    metadata <- list(S7 = snapshot, s7fixture = dep)
    code <- c("library(S7)", "Wrapped <- new_class(\"Wrapped\", properties=list(child=new_external_class(\"s7fixture\", \"Dog\")))")
    expect_identical(s7_labels(c(code, "Wrapped()@child@"), metadata), "age")
    expected <- "Wrapped(child = (S7::as_class(s7fixture::Dog))())"
    expect_identical(s7_signature(s7_fixture(c(code, "Wrapped("), metadata))$signatures[[1L]]$label, expected)
    for (spec in c("new_external_class(\"s7fixture\", \"Hidden\")",
            "new_external_class(\"missingpkg\", \"Dog\")", "new_external_class(\"s7fixture\", \"Dog\", version=\"2.0.0\")")) {
        source <- c("library(S7)", paste0("Wrapped <- new_class(\"Wrapped\", properties=list(child=", spec, "))"), "Wrapped()@child@")
        expect_length(s7_labels(source, metadata), 0L)
    }
    code <- c("library(S7)", "Child <- new_class(\"Child\", parent=s7fixture::Dog, package=\"consumer\", properties=list(label=class_character))")
    expect_identical(s7_signature(s7_fixture(c(code, "Child("), metadata))$signatures[[1L]]$label,
        "Child(..., label = character(0))")
    expect_identical(s7_labels(c(code, "Child()@"), metadata), c("age", "label"))
    cyclic <- member_index_thaw(dep)
    cyclic$package_roots$Loop <- member_s7_value(s7_descriptor = list(kind = "external", package = "s7fixture", name = "Loop"))
    budget <- new.env(parent = emptyenv())
    expect_null(member_s7_external(cyclic$package_roots$Loop$s7_descriptor, cyclic, list(), budget))
})

test_that("S7 S4 parents, S3 defaults and deprecation declarations retain descriptors", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    metadata <- list(S7 = snapshot)
    if (isTRUE(snapshot$s7_capabilities$s4)) {
        code <- c("library(S7)", "Base <- methods::setClass(\"S7CompatBase\", slots=c(value=\"numeric\"))",
            "Hybrid <- new_class(\"Hybrid\", parent=Base, properties=list(label=class_character))")
        expect_identical(s7_labels(c(code, "Hybrid()@"), metadata), c("label", "value"))
        expect_identical(s7_signature(s7_fixture(c(code, "Hybrid("), metadata))$signatures[[1L]]$label,
            "Hybrid(value = integer(0), label = character(0))")
        lookup <- c("library(S7)", 'methods::setClass("S4Point", slots = c(x = "numeric", y = "numeric"))',
            'ColoredPoint <- new_class("ColoredPoint", parent = methods::getClass("S4Point"), properties = list(color = class_character))')
        expect_identical(s7_signature(s7_fixture(c(lookup, "ColoredPoint("), metadata))$signatures[[1L]]$label,
            "ColoredPoint(x = integer(0), y = integer(0), color = character(0))")
        f <- s7_fixture(c(lookup, "pt <- ColoredPoint(x = 1, y = 2, color = 'red')", "pt@x"), metadata)
        expect_equal(member_definition_location(f$uri, f$workspace, f$document, f$point)$result$range,
            range(position(1L, 39L), position(1L, 40L)))
        scope <- new.env(parent = globalenv())
        scope$new_class <- S7::new_class
        scope$class_character <- S7::class_character
        scope$Base <- methods::setClass("S7CompatBase", slots = c(value = "numeric"), where = scope)
        eval(parse(text = code[[3L]]), scope)
        withr::defer(methods::removeClass("S7CompatBase", where = globalenv()))
        withr::defer(methods::removeClass("Hybrid", where = globalenv()))
        scope$hybrid <- scope$Hybrid(value = 1, label = "hybrid")
        installed <- member_index_freeze(member_package_index(member_namespace_input(scope,
                    package = "s7fixture", exports = c("Hybrid", "hybrid"))))
        expect_identical(s7_labels("s7fixture::hybrid@", list(s7fixture = installed)), c("label", "value"))
    }
    code <- c("library(S7)", "Dates <- new_class(\"Dates\", properties=list(date=class_Date, data=class_data.frame, choice=class_factor))")
    scope <- new.env(parent = asNamespace("S7"))
    eval(parse(text = code[-1L]), scope)
    if (isTRUE(snapshot$s7_capabilities$class_ref)) {
        expect_identical(s7_signature(s7_fixture(c(code, "Dates("), metadata))$signatures[[1L]]$label,
            get_signature("Dates", member_s7_function(formals(scope$Dates))))
    } else {
        expect_match(s7_signature(s7_fixture(c(code, "Dates("), metadata))$signatures[[1L]]$label,
            ".__unknown_s7_default__", fixed = TRUE)
    }
    if ("deprecated_class" %in% snapshot$s7_capabilities$exports) {
        code <- c("library(S7)", "Basket := deprecated_class(properties=list(size=class_double, deprecated_property(\"count\", new=\"size\", when=\"1.0.0\")), when=\"2.0.0\")")
        expect_identical(s7_labels(c(code, "Basket()@"), metadata), c("count", "size"))
        expect_identical(s7_hover(s7_fixture(c(code, "Basket(size=3)@count"), metadata))$contents, "```r\n3\n```")
        expect_identical(s7_signature(s7_fixture(c(code, "Basket("), metadata))$signatures[[1L]]$label,
            "Basket(size = numeric(0), count = size)")
    }
})

test_that("S7 union syntax and same-package external properties remain bounded", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    metadata <- list(S7 = snapshot)
    code <- c("library(S7)", "Leaf <- new_class(\"Leaf\", properties=list(value=class_numeric))",
        "Box <- new_class(\"Box\", properties=list(item=Leaf | NULL))")
    expect_identical(s7_labels(c(code, "Box()@item@"), metadata), character())
    fixture <- s7_fixture(c(code, "Box("), metadata)
    expect_identical(s7_signature(fixture)$signatures[[1L]]$label, "Box(item = Leaf())")
    shadowed <- c(code[1:2], "`|` <- function(x, y) unknown", code[[3L]], "Box()@item@")
    expect_length(s7_labels(shadowed, metadata), 0L)
    if ("new_external_class" %in% snapshot$s7_capabilities$exports) {
        scope <- new.env(parent = asNamespace("S7"))
        scope$Hidden <- S7::new_class("Hidden", properties = list(age = S7::class_numeric))
        scope$Box <- S7::new_class("Box", package = "s7fixture", properties = list(
            child = S7::new_external_class("s7fixture", "Hidden")))
        dep <- member_index_freeze(member_package_index(member_namespace_input(scope,
                    package = "s7fixture", exports = "Box")))
        expect_identical(s7_labels("s7fixture::Box()@child@", list(s7fixture = dep)), "age")
        recursive <- c("library(S7)", "Tree <- new_class(\"Tree\", package=\"localpkg\", properties=list(child=new_external_class(\"localpkg\", \"Tree\")))")
        expect_identical(s7_labels(c(recursive, "Tree()@child@"), metadata), "child")
    }
})

test_that("S7 workers snapshot loaded external dependencies without constructor calls", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    skip_if_not("new_external_class" %in% snapshot$s7_capabilities$exports)
    ns <- new.env(parent = asNamespace("S7"))
    info <- new.env(parent = emptyenv())
    info$spec <- c(name = "s7compatdep", version = "1.0.0")
    info$exports <- new.env(parent = emptyenv())
    ns[[".__NAMESPACE__."]] <- info
    internal <- get(".Internal", baseenv())
    internal(registerNamespace("s7compatdep", ns))
    withr::defer(internal(unregisterNamespace("s7compatdep")))
    marker <- withr::local_tempfile()
    ns$Dog <- S7::new_class("Dog", properties = list(age = S7::class_numeric),
        constructor = function(age = 11) {
            writeLines("ran", marker)
            stop("constructor")
            S7::new_object(S7::S7_object())
        })
    info$exports$Dog <- "Dog"
    consumer <- new.env(parent = asNamespace("S7"))
    consumer$Wrapped <- S7::new_class("Wrapped", properties = list(
        child = S7::new_external_class("s7compatdep", "Dog")))
    metadata <- member_index_freeze(member_package_index(member_namespace_input(consumer,
                package = "s7consumer", exports = "Wrapped")))
    expect_identical(metadata$s7_dependencies$s7compatdep$identity$version, "1.0.0")
    expect_identical(s7_labels("s7consumer::Wrapped()@child@", list(s7consumer = metadata)), "age")
    expect_false(file.exists(marker))
})

test_that("S7 new_object keeps statically known parent properties", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    code <- c("library(S7)",
        "Parent <- new_class(\"Parent\", properties=list(items=class_list))",
        "Child <- new_class(\"Child\", parent=Parent, properties=list(label=class_character), constructor=function() new_object(Parent(items=list(alpha=1)), label=\"child\"))")
    expect_identical(s7_labels(c(code, "Child()@items$"), list(S7 = snapshot)), "alpha")
})


test_that("S7 setters never retain a stale default member shape", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    code <- c("library(S7)",
        "Box <- new_class(\"Box\", properties=list(items=new_property(class_list, default=quote(list(old=1)), setter=function(self, value) stop(\"setter\"))))")
    expect_length(s7_labels(c(code, "Box()@items$"), list(S7 = snapshot)), 0L)
    expect_length(s7_labels(c(code, "set_props(Box(), items=list(new=2))@items$"), list(S7 = snapshot)), 0L)
})

test_that("S7 class deprecation distinguishes a recommendation from an alias", {
    skip_if_not_installed("S7")
    skip_if_not("deprecated_class" %in% getNamespaceExports("S7"))
    metadata <- list(S7 = member_prepare_package("S7"))
    # deprecated_class() examples in the S7 evolution guide and reference.
    code <- c("library(S7)", "Pet := new_class(properties = list(name = class_character))",
        'Dog := deprecated_class(new = Pet, when = "2.0.0", properties = list(age = class_integer))',
        'Hound := deprecated_class(new = Pet, when = "3.0.0", alias = TRUE)',
        "Puppy := new_class(parent = Hound, properties = list(trained = class_logical))")
    scope <- new.env(parent = asNamespace("S7"))
    eval(parse(text = code[-1L]), scope)
    for (name in c("Pet", "Dog", "Hound", "Puppy")) {
        ctor <- scope[[name]]
        object <- suppressWarnings(ctor())
        expect_identical(s7_labels(c(code, paste0(name, "()@")), metadata), sort(S7::prop_names(object)))
        result <- s7_signature(s7_fixture(c(code, paste0(name, "(")), metadata))
        expect_identical(result$signatures[[1L]]$label, get_signature(name, member_s7_function(formals(ctor))))
    }
    installed <- member_index_freeze(member_package_index(member_namespace_input(scope,
                package = "s7pets", exports = c("Hound", "Dog"))))
    expect_identical(s7_labels("s7pets::Hound()@", list(s7pets = installed)), "name")
    expect_identical(s7_labels("s7pets::Dog()@", list(s7pets = installed)), "age")
    for (args in c("alias = unknown", "alias = TRUE, properties = list(wrong = class_list)")) {
        invalid <- sprintf('Bad := deprecated_class(new = Pet, when = "3.0.0", %s)', args)
        expect_length(s7_labels(c(code, invalid, "Bad()@"), metadata), 0L)
    }
})

test_that("S7 renamed properties follow constructor and set_props values", {
    skip_if_not_installed("S7")
    skip_if_not("deprecated_property" %in% getNamespaceExports("S7"))
    metadata <- list(S7 = member_prepare_package("S7"))
    code <- c("library(S7)", "Person := new_class(properties = list(",
        "  first_name = class_character,",
        '  deprecated_property("firstName", new = "first_name", when = "1.1.0")',
        "))", 'hadley <- Person(firstName = "Hadley")')
    scope <- new.env(parent = asNamespace("S7"))
    suppressWarnings(eval(parse(text = code[-1L]), scope))
    expect_value <- function(expr) {
        actual <- suppressWarnings(eval(parse(text = expr), scope))
        hover <- s7_hover(s7_fixture(c(code, expr), metadata))
        expect_identical(hover$contents, sprintf("```r\n%s\n```", deparse(actual)))
    }
    expect_value("hadley@first_name")
    expect_value('Person(firstName = "John", first_name = "Hadley")@firstName')
    expect_value('set_props(hadley, firstName = "John", first_name = "Sam")@firstName')
    expect_value('set_props(hadley, first_name = "Sam", firstName = "John")@first_name')
    keys <- c("library(S7)", "Card := new_class(properties = list(contact = class_list))",
        'card <- Card(contact = list(name = "Hadley"))', 'property <- "contact"')
    expect_identical(s7_labels(c(keys, "prop(card, property)$"), metadata), "name")
    expect_length(s7_labels(c(keys, "prop(card, contact)$"), metadata), 0L)
})

test_that("S7 wrapped S3 property defaults follow the installed public formals", {
    skip_if_not_installed("S7")
    metadata <- list(S7 = member_prepare_package("S7"))
    # new_S3_class() documents a Date constructor/default and optional attributes.
    code <- c("library(S7)",
        'S3_Date <- new_S3_class("Date", function(.data = integer()) .Date(.data),',
        '  default = quote(.Date(integer())), attributes = "names")',
        'Calendar <- new_class("Calendar", properties = list(start = S3_Date))')
    scope <- new.env(parent = asNamespace("S7"))
    if ("attributes" %in% names(formals(S7::new_S3_class))) {
        eval(parse(text = code[-1L]), scope)
        signature <- s7_signature(s7_fixture(c(code, "Calendar("), metadata))
        expect_identical(signature$signatures[[1L]]$label, get_signature("Calendar", member_s7_function(formals(scope$Calendar))))
    } else {
        expect_error(eval(parse(text = code[-1L]), scope), "unused argument")
        index <- member_generic_index("")
        index$s7_capabilities <- metadata$S7$s7_capabilities
        expect_null(member_infer(quote(S7::new_S3_class("Date", attributes = "names")), index)$s7_descriptor)
    }
})

test_that("S7 property introspection follows the actual class and table schema", {
    skip_if_not_installed("S7")
    metadata <- list(S7 = member_prepare_package("S7"))
    code <- c("library(S7)",
        'Point <- new_class("Point", properties = list(x = class_numeric, y = class_numeric))',
        "p <- Point(x = 3, y = 4)")
    scope <- new.env(parent = asNamespace("S7"))
    eval(parse(text = code[-1L]), scope)
    expect_identical(s7_labels(c(code, "S7_class(p)()@"), metadata), sort(S7::prop_names(scope$p)))
    expect_identical(s7_labels(c(code, "props(p)$"), metadata), sort(names(S7::props(scope$p))))
    expect_identical(s7_labels(c(code, 'props(p, "y")$'), metadata), "y")
    expect_length(s7_labels(c(code, "props(p, input)$"), metadata), 0L)
    if ("prop_info" %in% getNamespaceExports("S7")) {
        # The release announcement inspects Point and its properties with prop_info().
        expected <- sort(names(S7::prop_info(scope$Point)))
        expect_identical(s7_labels(c(code, "prop_info(Point)$"), metadata), expected)
        expect_identical(s7_labels(c(code, "prop_info(p)$"), metadata), expected)
    }
})

test_that("quoted property keys resolve against the supplied instance or class object", {
    skip_if_not_installed("S7")
    snapshot <- member_prepare_package("S7")
    metadata <- list(S7 = snapshot)
    # Horse from the prop_names() and props() reference pages.
    code <- c("library(S7)", 'Horse <- new_class("Horse", properties = list(',
        "  name = class_character, colour = class_character))",
        'lex <- Horse(name = "Lex", colour = "bay")')
    probe <- function(line, source = code) {
        at <- regexpr("|", line, fixed = TRUE)[[1L]] - 1L
        lines <- c(source, sub("|", "", line, fixed = TRUE))
        s7_fixture(lines, metadata, list(row = length(source), col = at))
    }
    for (line in c('prop_exists(lex, "co|lour")', 'S7::props(names = "co|lour", object = lex)')) {
        f <- probe(line)
        item <- member_completion(f$uri, f$workspace, f$document, f$point, FALSE, 200L)[[1L]]
        expect_identical(item$label, "colour")
        expect_identical(item$textEdit$newText, '"colour"')
        expect_match(s7_hover(f)$contents, "bay", fixed = TRUE)
        expect_equal(member_definition_location(f$uri, f$workspace, f$document, f$point)$result$range,
            range(position(2L, 26L), position(2L, 32L)))
    }
    f <- probe('prop_exists(Horse, "na|me")')
    expect_match(s7_hover(f)$contents, "Horse", fixed = TRUE)
    expect_null(member_definition_location(f$uri, f$workspace, f$document, f$point)$result)
    for (f in list(probe('prop_exists(Horse, "colo|")'), probe('props("co|", "colour")'),
            probe('prop_exists(lex, "co|")', c(code, "prop_exists <- function(object, name) FALSE")))) {
        expect_length(member_completion(f$uri, f$workspace, f$document, f$point, FALSE, 200L), 0L)
    }
})

test_that("S7 property helpers on an S4 subclass use the S7 parent's properties", {
    skip_if_not_installed("S7", "1.0.0")
    metadata <- list(S7 = member_prepare_package("S7"))
    code <- c("library(S7)", 'Sample <- new_class("Sample", properties = list(name = class_character, score = class_numeric))',
        "S4_register(Sample)", "Sample_S4 <- S4_contains(Sample)",
        'methods::setClass("ReplicatedSample", contains = Sample_S4, slots = c(replicate = "integer"))',
        'methods::setClass("Child", contains = "ReplicatedSample")',
        'rs <- methods::new("Child", name = "A", score = 3, replicate = 1L)')
    expect_identical(s7_labels(c(code, "rs@"), metadata), c("name", "replicate", "score"))
    expect_identical(s7_labels(c(code, "props(rs)$"), metadata), c("name", "score"))
    expect_identical(s7_labels(c(code, 'prop_exists(rs, "'), metadata), c("name", "score"))
    f <- s7_fixture(c(code, 'prop(rs, "score")'), metadata, list(row = length(code), col = 12L))
    expect_match(s7_hover(f)$contents, "3", fixed = TRUE)
    expect_equal(member_definition_location(f$uri, f$workspace, f$document, f$point)$result$range,
        range(position(1L, 72L), position(1L, 77L)))
    expect_length(s7_labels(c(code, 'prop(rs, "rep'), metadata), 0L)
    expect_identical(s7_labels(c(code, 'methods::slot(rs, "rep'), metadata), "replicate")
})

test_that("S7 lazy conversion preserves the original subclass before dispatch", {
    skip_if_not_installed("S7")
    skip_if_not("convert_lazy" %in% getNamespaceExports("S7"))
    metadata <- list(S7 = member_prepare_package("S7"))
    # convert_lazy() reference examples use an ancestor with x and a subclass with y.
    code <- c("library(S7)", "Foo1 := new_class(properties = list(x = class_integer))",
        "Foo2 := new_class(Foo1, properties = list(y = class_double))",
        "object <- Foo2(x = 1L, y = 2)")
    scope <- new.env(parent = asNamespace("S7"))
    eval(parse(text = code[-1L]), scope)
    for (target in c("Foo1", "Foo2")) {
        expr <- paste0("convert_lazy(object, ", target, ")")
        actual <- eval(parse(text = expr), scope)
        expect_identical(s7_labels(c(code, paste0(expr, "@")), metadata), sort(S7::prop_names(actual)))
        expect_identical(s7_hover(s7_fixture(c(code, paste0(expr, "@y")), metadata))$contents, "```r\n2\n```")
    }
    expect_length(s7_labels(c(code, "convert_lazy(Foo1(), Foo2)@"), metadata), 0L)
    expect_length(s7_labels(c(code, "convert_lazy <- function(...) NULL", "convert_lazy(object, Foo1)@"), metadata), 0L)
})

test_that("the namespace budget fallback retains public S7 shapes and lazy data", {
    skip_if_not_installed("S7")
    ns <- new.env(parent = asNamespace("S7"))
    ns$Point <- S7::new_class("Point", properties = list(x = S7::class_double, y = S7::class_double))
    ns$sample <- ns$Point(x = 3, y = 4)
    ns$private <- ns$Point
    makeActiveBinding("active", function() stop("active binding ran"), ns)
    delayedAssign("deferred", stop("promise was forced"), assign.env = ns)
    data <- list2env(list(table = data.frame(id = 1L)), parent = emptyenv())
    exports <- c("Point", "sample", "active", "deferred")
    input <- member_namespace_input(ns, "geometry", exports, data, lazydata_only = TRUE)
    compact <- member_public_s7_input(input, ns)
    expect_setequal(compact$namespace_exports, c("Point", "sample"))
    expect_length(member_public_s7_input(input, ns, max_bytes = 1L)$s7_roots, 0L)

    # Exercise the full preparation path when a namespace was read but its
    # frozen index was too large. The fallback reuses the collected roots.
    rm("deferred", envir = ns)
    capture <- member_namespace_input
    freeze <- member_index_freeze
    testthat::local_mocked_bindings(
        member_namespace_input = function(ns, lazydata = NULL, lazydata_only = FALSE) {
            capture(scope, "geometry", exports, data, lazydata_only)
        },
        member_index_freeze = function(index) {
            if (!isTRUE(index$namespace_attempted)) stop("Namespace exceeds snapshot budget")
            freeze(index)
        },
        .package = "languageserver"
    )
    scope <- ns
    withr::local_options(languageserver.package_cache_dir = FALSE)
    withr::local_libpaths(.libPaths())
    snapshot <- member_prepare_package("utils", identity = list(package = "geometry"))
    expect_true(snapshot$namespace_attempted)
    expect_identical(snapshot$preparation_error, "Namespace exceeds snapshot budget")
    expect_setequal(snapshot$bounded_s7_exports, c("Point", "sample"))
    cache <- MemberMetadataCache$new(16 * 1024^2)
    cache$set("geometry", snapshot)
    expect_true(member_bounded_s7_snapshot(snapshot))
    expect_true(member_bounded_s7_snapshot(cache$catalog("geometry")))
    incomplete <- snapshot
    incomplete$roots$sample <- NULL
    expect_false(member_bounded_s7_snapshot(incomplete))
    failed <- snapshot
    failed$preparation_error <- "namespace load failed"
    expect_false(member_bounded_s7_snapshot(failed))
    dependent <- snapshot
    dependent$s7_dependencies <- list(optionalpkg = list())
    expect_false(member_bounded_s7_snapshot(dependent, packages = "geometry"))
    expect_true(member_bounded_s7_snapshot(dependent, packages = c("geometry", "optionalpkg")))
    expect_setequal(names(snapshot$roots), c("Point", "sample", "table"))
    metadata <- list(geometry = snapshot)
    expect_identical(s7_labels("geometry::Point()@", metadata), sort(S7::prop_names(ns$sample)))
    expect_identical(s7_labels("geometry::sample@", metadata), c("x", "y"))
    expect_identical(names(snapshot$roots$table$fields), "id")
})
