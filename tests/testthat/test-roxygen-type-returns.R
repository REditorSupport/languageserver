roxygen_call_receiver <- function(definitions, expression) {
    fixture <- provider_fixture(c(definitions, paste0(expression, "$probe")))
    point <- list(row = length(definitions), col = nchar(expression) + 1L)
    cursor <- member_cursor(fixture$document, point)
    member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor)$value
}

test_that("documented argument types propagate through wrappers and dependent defaults", {
    definitions <- c(
        'Pipeline <- R6::R6Class("Pipeline", public = list(collect = function() list(done = TRUE)))',
        "#' @param pipeline (`Pipeline`) A pipeline.",
        "wrap <- function(unused, pipeline, current = pipeline) current",
        "collect <- function(x) wrap(NULL, pipe = x)$collect()"
    )
    expect_named(roxygen_call_receiver(definitions, "collect(opaque())")$fields, "done")
    expect_named(roxygen_call_receiver(definitions, "wrap(NULL, list(actual = 1))")$fields, "actual")
    expect_identical(roxygen_call_receiver(definitions, "wrap(NULL)")$type, ".missing")
    expect_identical(roxygen_call_receiver(definitions, "wrap(NULL, 1, pipeline = 2, excess = 3)")$reason,
        "argument_matching")
})

test_that("proven results and unused formals do not resolve annotations", {
    aliases <- c('G0 <- R6::R6Class("Deep", public = list(field = 1))', sprintf("G%d <- G%d", 1:145, 0:144))
    definitions <- c(aliases, "#' @param x (`G145`) Input.", "#' @return (`G145`) Output.",
        "wrap <- function(x, y = x, z = y) list(real = TRUE)")
    for (call in c("wrap(123)", "wrap(opaque())", "wrap()")) {
        expect_named(roxygen_call_receiver(definitions, call)$fields, "real")
    }
    doc <- c(aliases[[1L]], "#' @param x (`G0`) Input.")
    expect_named(roxygen_call_receiver(c(doc, "wrap <- function(x = y, y = list(later = 1)) x"), "wrap()")$fields, "later")
    expect_identical(roxygen_call_receiver(c(doc, "wrap <- function(x = y, y = x) x"), "wrap()")$reason, "recursive_default")
})

test_that("a formal's class name is captured independently of that formal", {
    definitions <- c('Thing <- R6::R6Class("Global", public = list(wrong = 1))',
        "factory <- function() {",
        '  Thing <- R6::R6Class("Local", public = list(right = 1))',
        "  #' @param Thing (`Thing`) A local instance.",
        "  function(Thing) Thing", "}")
    expect_setequal(names(roxygen_call_receiver(definitions, "factory()(opaque())")$fields), c("right", "clone"))
})

test_that("documented results follow calls through wrappers, stored functions and fluent methods", {
    definitions <- c(
        'Pipeline <- R6::R6Class("Pipeline", public = list(limit = function(n) self,',
        "    collect = function() list(data = NULL, metadata = list(cached = FALSE))))",
        "#' @return (`Pipeline`) The restored pipeline.",
        "restore <- function(path) opaque(path)",
        "methods <- list(restore = restore)",
        "run <- function(path) methods$restore(path)$limit(5L)$collect()"
    )
    expect_named(roxygen_call_receiver(definitions, 'run("input")')$fields, c("data", "metadata"))
    expect_named(roxygen_call_receiver(definitions, 'run("input")$metadata')$fields, "cached")
})

test_that("R6 methods use their own documented results when called", {
    definitions <- c(
        'Result <- R6::R6Class("Result", public = list(details = function() list(tag = 1)))',
        'Repository <- R6::R6Class("Repository", public = list(',
        "  #' @return (`Result`) The matching record.",
        "  find = function(id) lookup(id)",
        "))",
        "repo <- Repository$new()"
    )
    expect_named(roxygen_call_receiver(definitions, "repo$find(1)$details()")$fields, "tag")
})

test_that("return hints use lexical class bindings across nested calls", {
    definitions <- c(
        'Result <- R6::R6Class("Result", public = list(global = TRUE))',
        "#' @return (`Result`) A result.",
        "restore <- function() opaque()",
        "wrap <- function(Result) restore()",
        "make <- function() {",
        '    Result <- R6::R6Class("Result", public = list(local = TRUE))',
        "    #' @return (`Result`) A local result.",
        "    from_local <- function() opaque()",
        "    from_local",
        "}"
    )
    expect_setequal(names(roxygen_call_receiver(definitions, "wrap(list())")$fields), c("global", "clone"))
    expect_setequal(names(roxygen_call_receiver(definitions, "make()()")$fields), c("local", "clone"))
})

test_that("documented results preserve known outcomes and their members at multiple exits", {
    definitions <- c(
        'Pipeline <- R6::R6Class("Pipeline", public = list(collect = function() list(done = TRUE)))',
        "#' @return (`Pipeline`) Usually a pipeline.",
        "choose <- function(flag) { if (flag) return(list(actual = 1)); opaque() }",
        "#' @return (`Pipeline`) Usually a pipeline.",
        "inline <- function(flag) if (flag) list(actual = 1) else opaque()",
        "#' @return (`list`) Result details.",
        "details <- function() list(nested = list(actual = 1))"
    )
    for (name in c("choose", "inline")) {
        expect_named(roxygen_call_receiver(definitions, paste0(name, "(TRUE)"))$fields, "actual")
        expect_true("collect" %in% names(roxygen_call_receiver(definitions, paste0(name, "(FALSE)"))$fields))
        value <- roxygen_call_receiver(definitions, paste0(name, "(opaque())"))
        expect_false("collect" %in% names(value$fields))
        expect_false("actual" %in% names(value$fields))
    }
    expect_named(roxygen_call_receiver(definitions, "details()$nested")$fields, "actual")
})

test_that("grouping and local assignments preserve mixed return evidence", {
    code <- c('Pipe <- R6::R6Class("Pipe", public = list(only_pipe = 1))', "#' @return (`Pipe`) Output.")
    bodies <- c("(if (flag) list(actual = 1) else opaque())",
        "{ out <- if (flag) list(actual = 1) else opaque(); out }",
        "{ if (flag) out <- list(actual = 1) else out <- opaque(); out }",
        "{ unrelated <- if (flag) 1 else opaque(); opaque() }")
    for (body in bodies[1:3]) {
        definitions <- c(code, paste("choose <- function(flag)", body))
        expect_named(roxygen_call_receiver(definitions, "choose(TRUE)")$fields, "actual")
        expect_true("only_pipe" %in% names(roxygen_call_receiver(definitions, "choose(FALSE)")$fields))
        expect_false("only_pipe" %in% names(roxygen_call_receiver(definitions, "choose(opaque())")$fields))
    }
    definitions <- c(code, paste("choose <- function(flag)", bodies[[4L]]))
    expect_true("only_pipe" %in% names(roxygen_call_receiver(definitions, "choose(opaque())")$fields))
    definitions <- c(code[[1L]], "#' @param x (`Pipe`) Input.", "id <- function(x) x")
    expect_false("only_pipe" %in% names(roxygen_call_receiver(definitions, "id(if (flag) list(actual=1) else opaque())")$fields))
    definitions <- c(code[[1L]], "#' @param x (`Pipe`) Input.", "id <- function(x, flag) { if (flag) x <- Pipe$new(); x }")
    expect_true("only_pipe" %in% names(roxygen_call_receiver(definitions, "id(opaque(), flag)")$fields))
})

test_that("registered methods preserve exact factory captures after the implementation is rebound", {
    definitions <- c('g <- function(x) UseMethod("g")', "factory <- function(value) function(x) value",
        "impl <- factory(list(first = 1))", 'registerS3method("g", "widget", impl)',
        "impl <- factory(list(second = 2))", 'x <- structure(1, class = "widget")')
    expect_named(roxygen_call_receiver(definitions, "g(x)")$fields, "first")
    definitions[[4L]] <- 'registerS3method("g", "widget", factory(list(inline = 1)))'
    expect_named(roxygen_call_receiver(definitions, "g(x)")$fields, "inline")
})

test_that("package generics analyze caller-visible source methods and retain namespace fallbacks", {
    index <- member_generic_index(paste('draw <- function(object, ...) UseMethod("draw")',
            "draw.default <- function(object, ...) list(package_result = 1L)", sep = "\n"))
    index$package <- "drawing"
    index$exports <- "draw"
    index$roots$draw <- member_value(function_key = "draw", metadata = "drawing")
    metadata <- MemberMetadataCache$new(1024^2)
    metadata$set("drawing", member_index_freeze(index))
    definitions <- c('Answer <- R6::R6Class("Answer", public = list(answer = 42L))',
        "#' @return (`Answer`) The drawing.", "draw.foo <- function(object, ...) opaque()",
        'input <- structure(1, class = "foo")')
    resolve <- function(lines, call) {
        f <- provider_fixture(c(lines, paste0(call, "$probe")))
        f$workspace$member_metadata <- metadata
        p <- list(row = length(lines), col = nchar(call) + 1L)
        member_resolve_cursor(f$uri, f$workspace, f$document, p, member_cursor(f$document, p))$value
    }
    for (call in c("drawing::draw(object = input)", "draw(input)")) {
        value <- resolve(c("library(drawing)", definitions), call)
        expect_setequal(names(value$fields), c("answer", "clone"))
    }
    expect_named(resolve(definitions, "drawing::draw(1L)")$fields, "package_result")
    local_generic <- c('draw <- function(object, ...) UseMethod("draw")', "impl <- function(...) list(local = 1)",
        'registerS3method("draw", "foo", impl)', 'input <- structure(1, class = "foo")')
    expect_named(resolve(local_generic, "draw(input)")$fields, "local")
    expect_named(resolve(local_generic, "drawing::draw(input)")$fields, "package_result")
    other <- member_generic_index('draw <- function(object, ...) UseMethod("draw")\ndraw.default <- function(...) list(other = 1)')
    other$package <- "drawing2"
    other$exports <- "draw"
    metadata$set("drawing2", member_index_freeze(other))
    packages <- c("drawing", "drawing2")
    s3_metadata <- S3MetadataCache$new()
    for (pkg in packages) {
        fn <- list(name = "draw", package = pkg, owner = pkg, formals = alist(object = , ... = ),
            generic = list(name = "draw", formal = "object"))
        s3_metadata$set(pkg, list(schema = 1L, package = pkg, generation = pkg, functions = list(draw = fn),
                methods = list(), objects = list()))
    }
    registered <- c("library(drawing)", "impl <- function(...) list(registered = TRUE)",
        'registerS3method("draw", "foo", impl)', "library(drawing2)", 'obj <- structure(1, class="foo")')
    for (pkg in packages) {
        call <- paste0(pkg, "::draw(obj)")
        f <- provider_fixture(c(registered, paste0(call, "$probe")))
        f$workspace$member_metadata <- metadata
        f$workspace$s3_metadata <- s3_metadata
        p <- list(row = length(registered), col = nchar(call) + 1L)
        result <- member_resolve_cursor(f$uri, f$workspace, f$document, p, member_cursor(f$document, p))$value
        expect_named(result$fields, if (pkg == "drawing") "registered" else "other")
    }
    f <- provider_fixture(c('input <- structure(1, class = "foo")', "drawing::draw(input)$probe"))
    f$workspace$member_metadata <- metadata
    point <- list(row = 1L, col = nchar("drawing::draw(input)$"))
    query <- function() member_resolve_cursor(f$uri, f$workspace, f$document, point, member_cursor(f$document, point))$value
    for (i in 1:3) expect_named(query()$fields, "package_result")
    uri <- "file:///drawing-method.R"
    for (name in c("first", "edited")) {
        content <- sprintf("draw.foo <- function(object, ...) list(%s = TRUE)", name)
        doc <- Document$new(uri, version = 1L, content = content)
        data <- parse_document(uri, content)
        data$version <- 1L
        doc$update_parse_data(data)
        f$workspace$documents$set(uri, doc)
        expect_named(query()$fields, name)
    }
    # A namespace binding can mask base::UseMethod. Its ordinary function
    # body keeps control even when a conventionally named source method exists.
    index$definitions$UseMethod <- quote(function(name) list(masked = 1))
    metadata$set("drawing", member_index_freeze(index))
    expect_named(resolve(definitions, "drawing::draw(input)")$fields, "masked")
})

test_that("declared unions propagate every receiver through calls and double brackets", {
    definitions <- c(
        'Parent <- R6::R6Class("Parent", public = list(step = function(n) self))',
        'A <- R6::R6Class("A", inherit = Parent, public = list(a = 1,',
        "    read = function() list(shared = list(leaf = 1), only_a = TRUE),",
        "    partial = function() list(assumed = 1)))",
        'B <- R6::R6Class("B", inherit = Parent, public = list(b = 2,',
        "    read = function(id) list(shared = list(leaf = 2), only_b = TRUE),",
        "    partial = function() opaque()))",
        "#' @return (`A | B`) A receiver.",
        "read_any <- function() opaque()"
    )
    index <- member_generic_index("")
    shape <- function(expr) roxygen_call_receiver(definitions, expr)
    candidates <- member_possible_receiver(shape("read_any()"), index)
    expect_true("step" %in% candidates$members)
    expect_false("step" %in% candidates$ambiguous)
    expect_true("read" %in% candidates$ambiguous)
    expect_false(any(c("a", "b") %in% candidates$members))
    for (step in c("read_any()$step(2)$read()", 'read_any()[["step"]](2)[["read"]]()')) {
        fields <- member_possible_receiver(shape(step), index)$members
        expect_identical(fields, "shared")
        fields <- member_possible_receiver(shape(paste0(step, '[["shared"]]')), index)$members
        expect_identical(fields, "leaf")
    }
    partial <- shape("read_any()$partial()")
    expect_null(member_possible_receiver(partial, index))
    expect_null(partial$type)
})

test_that("declared unions propagate class slots and actual matches in generic dispatch", {
    index <- member_generic_index(paste(c(
        'g <- function(payload, object) UseMethod("g", object)',
        "g.A <- function(payload, object) list(shared = object@item, only_a = payload)",
        "g.B <- function(payload, object) list(shared = object@item, only_b = payload)"
    ), collapse = "\n"))
    variant <- function(class, field) {
        item <- member_value(type = "list", fields = stats::setNames(list(member_literal(1L)), field))
        member_value(type = class, classes = class, slot_types = c(item = "list"), slots = list(item = item))
    }
    shape <- member_value(reason = "type_hint_union", possible_returns = list(variant("A", "x"), variant("B", "y")))
    env <- list(object = shape)
    result <- member_infer(quote(object@item), index, env)
    expect_identical(lapply(result$possible_returns, function(x) names(x$fields)), list("x", "y"))
    for (call in expression(g(payload = 1, object = object), g(ob = object, payload = 1))) {
        result <- member_infer(call, index, env)
        expect_identical(member_possible_receiver(result, index)$members, "shared")
        access <- member_access(result, "shared", index, env)
        expect_identical(lapply(access$possible_returns, function(x) names(x$fields)), list("x", "y"))
    }
    index$definitions$g.B <- quote(function(payload, object) opaque())
    index$cache <- new.env(parent = emptyenv())
    result <- member_infer(quote(g(1, object)), index, env)
    expect_null(result$type)
    expect_null(result$possible_returns)
})

test_that("source generic calls return the documented or inferred shape of the selected method", {
    definitions <- c(
        'Result <- R6::R6Class("Result", public = list(done = function() list(tag = TRUE)))',
        'draw <- function(x) UseMethod("draw")',
        "#' @return (`Result`) The drawing.",
        "draw.widget <- function(x) opaque(x)",
        "draw.other <- function(x) list(done = function() list(tag = FALSE, extra = 1))",
        "#' @param x (`widget`) A widget.",
        "from_widget <- function(x) draw(x)$done()",
        "#' @param x (`other`) Another object.",
        "from_other <- function(x) draw(x)$done()",
        "#' @param x (`widget | other`) Either object.",
        "from_either <- function(x) draw(x)$done()",
        "#' @param x (`widget`) A widget.",
        "from_local <- function(x) { draw.widget <- function(x) list(local = TRUE); draw(x) }"
    )
    expect_named(roxygen_call_receiver(definitions, "from_widget(opaque())")$fields, "tag")
    expect_named(roxygen_call_receiver(definitions, "from_other(opaque())")$fields, c("tag", "extra"))
    value <- roxygen_call_receiver(definitions, "from_either(opaque())")
    expect_identical(member_possible_receiver(value, member_generic_index(""))$members, "tag")
    expect_named(roxygen_call_receiver(definitions, "from_local(opaque())")$fields, "local")
    unresolved <- c(definitions, "draw.other <- function(x) NextMethod()")
    expect_null(roxygen_call_receiver(unresolved, "from_either(opaque())")$possible_returns)
})

test_that("method edits in another open document invalidate a wrapper's result", {
    lines <- c('draw <- function(x) UseMethod("draw")', "#' @param x (`widget`) A widget.",
        "wrap <- function(x) draw(x)", "result <- wrap(opaque())", "result$probe")
    fixture <- provider_fixture(lines)
    uri <- "file:///widget-method.R"
    set_method <- function(field, version) {
        content <- sprintf("draw.widget <- function(x) list(%s = 1)", field)
        doc <- Document$new(uri, language = "r", version = version, content = content)
        parsed <- parse_document(uri, content)
        parsed$version <- version
        doc$update_parse_data(parsed)
        fixture$workspace$documents$set(uri, doc)
    }
    resolve <- function() {
        point <- list(row = 4L, col = 7L)
        member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point,
            member_cursor(fixture$document, point))$value
    }
    set_method("original", 1L)
    expect_named(resolve()$fields, "original")
    set_method("edited", 2L)
    expect_named(resolve()$fields, "edited")
})

test_that("quoted property and slot reads use every documented union alternative", {
    probe <- function(definitions, types, expression, metadata = NULL) {
        marker <- regexpr("|", expression, fixed = TRUE)[[1L]]
        line <- sub("|", "", expression, fixed = TRUE)
        fixture <- provider_fixture(c(definitions, paste0("#' @param x (`", types, "`) A receiver."),
                "f <- function(x) {", line, "}"))
        fixture$workspace$member_metadata <- metadata
        point <- list(row = length(definitions) + 2L, col = marker - 1L)
        items <- member_completion(fixture$uri, fixture$workspace, fixture$document, point, FALSE, 200L)
        vapply(items, `[[`, "", "label")
    }
    s4 <- c('methods::setClass("Leaf", slots = c(value = "integer"))',
        'methods::setClass("Left", slots = c(shared = "Leaf", left = "integer"))',
        'methods::setClass("Right", slots = c(shared = "Leaf", right = "integer"))')
    expect_identical(probe(s4, "Left | Right", 'methods::slot(x, "|")'), "shared")
    expect_identical(probe(s4, "Left | Right", 'methods::slot(x, "shared")@|value'), "value")
    skip_if_not_installed("S7")
    metadata <- MemberMetadataCache$new(32 * 1024^2)
    metadata$set("S7", member_prepare_package("S7"))
    s7 <- c('Leaf <- S7::new_class("Leaf", properties = list(value = S7::class_integer))',
        'A <- S7::new_class("A", properties = list(shared = Leaf, left = S7::class_double))',
        'B <- S7::new_class("B", properties = list(right = S7::class_double, shared = Leaf))')
    for (call in c('S7::prop(x, "|")', 'S7::props(x, "|")', "S7::props(x)$|shared")) {
        expect_identical(probe(s7, "A | B", call, metadata), "shared")
    }
    for (call in c('S7::prop(x, "shared")@|value', 'S7::props(x, "shared")$shared@|value')) {
        expect_identical(probe(s7, "A | B", call, metadata), "value")
    }
    expect_length(probe(s7, "A | B", 'S7::props(x, "left")$|left', metadata), 0L)
    for (call in c('S7::prop(x, "|")', 'methods::slot(x, "|")')) {
        expect_length(probe(c(s4, s7), "Left | B", call, metadata), 0L)
    }
})

test_that("cached shapes reject documented arguments nested in optional receiver state", {
    concrete <- member_value(type = "integer")
    documented <- member_value(type = "integer")
    documented$hint_types <- "integer"
    for (field in c("r6_private", "r6_super", "r6_self", "s7_storage", "element_shape")) {
        value <- member_value(type = "list", fields = list(ordinary = concrete))
        value[[field]] <- member_value(type = "list", fields = list(child = concrete))
        expect_true(member_cacheable(value))
        value[[field]]$fields$child <- documented
        expect_false(member_cacheable(value))
    }
})
