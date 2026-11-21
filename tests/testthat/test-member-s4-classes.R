test_that("S4 class unions expose common slots across namespace-owned members", {
    index <- member_generic_index("")
    index$package <- "consumer"
    foreign <- member_generic_index("")
    declare <- function(expr) member_s4_recipe(expr, index, list())
    foreign$s4_classes <- list(Record = declare(quote(methods::setClass("Record",
                    slots = base::c(id = "integer", foreign = "character")))))
    index$namespace_indices <- list(provider = foreign)
    index$s4_classes$Record <- declare(quote(methods::setClass("Record", slots = c(id = "integer", local = "numeric"))))
    index$s4_classes$Choice <- list(name = "Choice", virtual = TRUE,
        union = list("Record", structure("Record", package = "provider")))
    shape <- member_s4_shape("Choice", index, list(), "consumer")
    expect_named(shape$slot_types, "id")
    expect_identical(member_s4_slot(shape, "id", index, list())$type, "integer")
    index$s4_classes$Empty <- declare(quote(methods::setClassUnion("Empty", character())))
    expect_identical(member_s4_shape("Empty", index, list())$reason, "unknown_s4_union")
    expect_null(member_s4_constructor(list(name = "Choice"), index, list(), member_request_budget()))

    recipe <- declare(quote(methods::setClass("Box", slots = c(payload = "ANY", ext = "External"),
                prototype = list(payload = list(key = "v")))))
    index$s4_classes$Box <- recipe
    box <- member_s4_shape("Box", index, list())
    expect_identical(member_s4_slot(box, "payload", index, list())$fields$key$literal, "v")
    expect_identical(member_s4_slot(box, "ext", index, list())$type, "External")
    infer <- function(expr) member_infer(expr, index, list(box = box))
    expect_identical(infer(quote(methods::slot(box, name = "payload")))$fields$key$literal, "v")
    expect_null(infer(quote(methods::slot(box, name = field)))$type)
})

test_that("Incomplete S4 descriptions do not produce a usable constructor", {
    index <- member_generic_index("")
    recipe <- function(expr) member_s4_recipe(expr, index, list())
    expect_null(recipe(quote(methods::setClass(symbol))))
    for (expr in expression(methods::setClass("A", slots = c(name = computed)),
            methods::setClass("A", slots = c("integer")),
            methods::setClass("A", slots = c(missing = , ok = "integer")))) {
        a <- recipe(expr)
        expect_false(a$complete)
        expect_null(member_s4_constructor(list(name = "A", recipe = a), index, list(), member_request_budget()))
    }
    parents <- paste0("Parent", seq_len(33L))
    expect_false(recipe(substitute(methods::setClass("A", contains = p), list(p = parents)))$complete)
    slots <- stats::setNames(rep("integer", 257L), paste0("s", seq_len(257L)))
    decl <- function(name, fields, parents = character()) {
        recipe(substitute(methods::setClass(name, slots = fields, contains = parents),
                list(name = name, fields = fields, parents = parents)))
    }
    expect_false(decl("TooMany", slots)$complete)
    index$s4_classes$Base <- decl("Base", slots[-1L])
    index$s4_classes$Child <- decl("Child", slots[1L], "Base")
    expect_identical(member_s4_shape("Child", index, list())$reason, "s4_slot_limit")
    expect_null(member_s4_constructor(list(name = "Missing"), index, list(), member_request_budget()))
})

test_that("S4 initializer signatures obey declaration order and ignore unrelated registrations", {
    index <- member_generic_index("")
    index$s4_classes$Thing <- member_s4_recipe(quote(setClass("Thing", slots = c(id = "integer"))), index, list())
    registration <- function(expr, row) list(start = c(row, 0L), end = c(row, 80L), expr = expr)
    index$document_initializers <- list(
        registration(quote(setMethod("initialize", "Thing", function(.Object, old) .Object)), 1L),
        registration(quote(setMethod("initialize", signature(.Object = "Thing"), function(.Object, current) .Object)), 2L),
        registration(quote(setMethod("initialize", c(.Object = "Other"), function(.Object, other) .Object)), 3L),
        registration(quote(setMethod("initialize", "Thing", function(.Object, hidden) .Object, where = elsewhere)), 4L),
        registration(quote(setMethod("show", "Thing", function(object) NULL)), 5L),
        registration(quote(setMethod("initialize", "Thing", opaque)), 6L))
    signature <- function(row) {
        value <- member_s4_constructor(list(name = "Thing"), index, list(.__member_position__ = c(row, 0L)),
            member_request_budget(), new = TRUE)
        names(value$function_expr[[2L]])
    }
    expect_identical(signature(2L), c("Class", "old"))
    expect_identical(signature(6L), c("Class", "current"))
    expect_identical(signature(7L), c("Class", "..."))

    body <- quote(function(x, y, z) x)
    method <- substitute(setMethod("combine", c(x = "Thing", y = , z = dynamic), fn), list(fn = body))
    inferred <- member_s4_method_formals(method, body, index, list(), member_request_budget())
    expect_named(inferred, "x")
    expect_named(inferred$x$slot_types, "id")
    method$where <- quote(elsewhere)
    expect_null(member_s4_method_formals(method, body, index, list(), member_request_budget()))
})

test_that("Reference class fields and methods retain the declaring parent's source range", {
    code <- c("Base <- methods::setRefClass('Base',",
        "    fields = list(id = 'integer'), methods = list(report = function() id))",
        "Child <- methods::setRefClass('Child', contains = c('Base', 'Unknown'), fields = 'extra')",
        "object <- Child$new()", "object$id", "object$report()")
    fixture <- provider_fixture(code)
    for (member in c("id", "report")) {
        row <- if (member == "id") 4L else 5L
        target <- member_definition_location(fixture$uri, fixture$workspace, fixture$document,
            list(row = row, col = 8L))$result
        col <- regexpr(member, code[[2L]], fixed = TRUE)[[1L]] - 1L
        expect_equal(target, list(uri = fixture$uri, range = range(position(1L, col), position(1L, col + nchar(member)))))
    }

    index <- member_generic_index("")
    shape <- function(expr) member_infer(expr, index)$refclass_instance
    fields <- shape(quote(methods::setRefClass("Store", fields = c("item", id = "integer", empty = ))))
    expect_setequal(fields$refclass_fields, c("item", "id"))
    expect_null(fields$fields$item$type)
    expect_identical(fields$fields$id$type, "integer")
    expect_length(shape(quote(methods::setRefClass("Store", fields = dynamic)))$refclass_fields, 0L)
    expect_identical(member_infer(quote(methods::setRefClass("Scoped", where = some_env)), index)$reason,
        "dynamic_reference_class")
    parents <- paste0("Parent", seq_len(17L))
    expr <- substitute(methods::setRefClass("Child", contains = parents), list(parents = parents))
    expect_identical(member_infer(expr, index)$reason, "reference_class_limit")
    wide <- stats::setNames(rep("integer", 257L), paste0("field", seq_len(257L)))
    expr <- substitute(methods::setRefClass("Wide", fields = fields), list(fields = wide))
    expect_identical(member_infer(expr, index)$reason, "reference_class_limit")
})
