test_that("Source package registries expose prefixed methods and literal active properties", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: registryfixture", file.path(root, "DESCRIPTION"))
    writeLines(c("export(registry, make_box, register, Point)", "importFrom(S7, new_class)"),
        file.path(root, "NAMESPACE"))
    writeLines(c(
        "registry <- new.env()",
        "registry$direct <- function(x) list(result = x)",
        "plugin_child <- function(parent) {",
        "    value <- new.env(); value$parent <- parent; class(value) <- 'child'; value",
        "}",
        "maps <- list(plugin_ = registry, unrelated_ = missing_registry)",
        "unused <- list(unused_ = registry)",
        "unused_hidden <- function(x) x",
        "collect <- function(target, prefix) {",
        "    for (key in ls(pattern = prefix)) assign(sub(prefix, '', key), get(key), envir = target)",
        "}",
        "for (prefix in names(maps)) collect(maps[[prefix]], sprintf('^%s', prefix))",
        "for (prefix in names(unused)) collect(unused[[prefix]], prefix)",
        "keys <- setdiff(c('alpha', 'beta', 'reserved'), 'reserved')",
        "lapply(keys, function(key) makeActiveBinding(key, function() key, registry))",
        "makeActiveBinding('record', function() list(done = TRUE), registry)",
        "lapply(unknown(), function(key) makeActiveBinding(key, function() 42L, registry))",
        "lapply('ignored', function(key) makeActiveBinding(key, unresolved, registry))",
        "makeActiveBinding('ignored', function() 42L, missing_registry)",
        "make_box <- function() {",
        "    obj <- new.env()",
        "    makeActiveBinding('child', function() registry[['child']](obj), obj)",
        "    makeActiveBinding('info', function() list(ready = TRUE), obj)",
        "    class(obj) <- 'box'; return(obj)",
        "}",
        "register <- function(name, factory) assign(name, factory, envir = registry)",
        "Point <- new_class('Point', properties = list(x = NULL, y = NULL))"
    ), file.path(root, "R", "registry.R"))
    index <- member_package_index(member_source_input(root))
    infer <- function(expr, scope = index$roots) member_infer(expr, index, scope)

    expect_identical(index$registries$registry, c(direct = "registry$direct", child = "plugin_child"))
    expect_identical(infer(quote(registry$direct(2L)$result))$literal, 2L)
    expect_identical(infer(quote(registry$alpha))$literal, "alpha")
    expect_identical(infer(quote(registry$beta))$literal, "beta")
    expect_identical(infer(quote(registry$record$done))$literal, TRUE)
    expect_false(any(c("ignored", "reserved", "hidden") %in% names(index$members$registry)))
    expect_identical(infer(quote(make_box()$info$ready))$literal, TRUE)
    expect_identical(index$properties$box$child$type, "child")
    expect_identical(index$constructor_inputs$plugin_child$parent$type, "box")
    expect_setequal(names(infer(quote(Point()))$slot_types), c("x", "y"))

    # A registration in the editor changes only the scope after that call.
    scope <- member_registration_effect(quote(register("custom", function(obj) list(owner = obj))),
        index, index$roots)
    expect_identical(infer(quote(make_box()$custom$owner), scope)$type, "box")
    expect_null(infer(quote(make_box()$custom))$type)
    named <- member_registration_effect(quote(register(factory = function(obj) 42L, name = "answer")),
        index, scope)
    expect_identical(infer(quote(make_box()$answer), named)$literal, 42L)
    for (expr in expression(register(unknown, function(obj) obj), register("incomplete"),
            register("extra", identity, TRUE), register("opaque", unknown), unknown(), value)) {
        expect_identical(member_registration_effect(expr, index, scope), scope)
    }
})

test_that("Static S3 return summaries retry long method bodies and honor external registration", {
    scope <- new.env(parent = baseenv())
    evalq({
        raw_box <- function(pointer) {
            obj <- new.env()
            obj$read <- reader(pointer)
            class(obj) <- "raw_box"
            obj
        }
        reader <- function(pointer) function() raw_box(pointer)
        public_box <- function(raw) UseMethod("public_box")
        public_box.raw_box <- function(raw) {
            obj <- new.env()
            obj$raw <- raw
            class(obj) <- "public_box"
            obj
        }
        convert <- function(x) UseMethod("convert")
        convert.public_box <- function(x) {
            x
        }
        convert.other <- function(x) list(other = TRUE)
        convert.default <- function(x) x
    }, scope)
    statements <- rep(list(quote(tmp <- identity(x))), 80L)
    body_fn <- as.call(c(list(as.name("{")), statements, list(quote(public_box(raw_box(tmp))))))
    scope$convert.long <- as.function(list(x = quote(expr = ), body_fn), scope)
    input <- member_namespace_input(scope, package = "returnfixture", exports = "convert")
    index <- member_package_index(input)
    expect_identical(index$method_results$convert.long$type, "public_box")
    expect_equal(sort(unname(vapply(index$generic_returns$convert, `[[`, "", "type"))), c("list", "public_box"))
    expect_identical(index$generic_returns$convert$convert.other$fields$other$literal, TRUE)
    expect_identical(member_infer(quote(convert(public_box(raw_box(NULL)))), index)$type, "public_box")
    member_prepare_returns(index, max_nodes = 10L)
    expect_null(index$method_results$convert.long)
    expect_identical(member_infer(quote(convert.long(1L)), index)$type, "public_box")
    input$generic_external <- "convert"
    external <- member_package_index(input)
    expect_null(external$generic_returns$convert)
    expect_identical(external$method_results$convert.long$type, "public_box")
})

test_that("constructors and closure factories follow nested standalone statements", {
    constructor <- member_constructor(quote(function() {
        { obj <- new.env(); { obj$field <- 1L } }
        if (flag) { obj$conditional <- 2L }
        local({ obj$private <- 3L })
        { class(obj) <- "box"; { return(obj) } }
    }))
    expect_identical(constructor$type, "box")
    expect_named(constructor$fields, "field")

    factory <- member_returned_function(quote(function(original) {
        { captured <- 1L; { wrapper <- function() captured } }
        { formals(wrapper) <- formals(original); { return(wrapper) } }
    }))
    expect_identical(factory$fn, quote(function() captured))
    expect_identical(factory$prelude, list(quote(captured <- 1L)))
    expect_identical(factory$copied, "original")
    expect_null(member_returned_function(quote(function() { function() 1L; {} })))
})
