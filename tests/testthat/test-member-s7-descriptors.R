test_that("S7 descriptors retain quoted S3 defaults and external class identity", {
    index <- member_generic_index("")
    index$package <- "consumer"
    index$s7_capabilities <- list(exports = c("new_external_class", "deprecated_property"))
    infer <- function(expr, bindings = list()) member_infer(expr, index, bindings)
    date <- infer(quote(S7::new_S3_class("Date", constructor = function(days) days,
                default = quote(as.Date("1970-01-01")))))
    expect_identical(date$s7_descriptor$name, "Date")
    expect_identical(date$s7_descriptor$default, quote(as.Date("1970-01-01")))
    expect_identical(names(date$s7_descriptor$constructor[[2L]]), "days")
    scope <- list(date = date)
    record <- infer(quote(S7::new_class("Record", properties = list(day = date))), scope)
    expect_identical(record$function_expr[[2L]]$day, quote(as.Date("1970-01-01")))
    expect_identical(infer(quote(S7::as_class(date)), scope)$s7_descriptor, date$s7_descriptor)
    expect_identical(infer(quote(S7::as_class(unknown)), scope)$type, NULL)

    owner <- member_generic_index("")
    owner$package_roots <- list(Record = record)
    owner$exports <- "Record"
    owner$identity <- list(version = "1.2.0")
    index$namespace_indices <- list(records = owner)
    ref <- infer(quote(S7::new_external_class("records", "Record", version = "1.0")))
    resolved <- infer(quote(S7::as_class(ref)), list(ref = ref))
    expect_identical(resolved$s7_generator$name, "Record")
    expect_identical(resolved$s7_generator$package, "records")
    expect_named(resolved$s7_generator$properties, "day")
    owner$exports <- character()
    hidden <- infer(quote(S7::as_class(ref)), list(ref = ref))
    expect_identical(hidden$s7_descriptor, ref$s7_descriptor)
    expect_null(hidden$s7_generator)
    index$s7_capabilities <- list(exports = character())
    expect_identical(infer(quote(S7::new_external_class("records", "Record")))$reason, "unavailable_s7_api")
})

test_that("Incomplete S7 declarations and ambiguous calls never invent property shapes", {
    index <- member_generic_index("")
    index$s7_capabilities <- list(exports = c("deprecated_property", "new_external_class"))
    infer <- function(expr, scope = list()) member_infer(expr, index, scope)
    scope <- list(record = infer(quote(S7::new_class("Record", properties = list(
        right = S7::new_property(default = quote(list(id = 1L))),
        rights = S7::new_property(), left = S7::new_property()
    )))))
    value <- infer(quote(record(le = "ok", righ = 1L)), scope)
    expect_identical(value$reason, "s7_argument_matching")
    expect_identical(infer(quote(record(left = 1, left = 2)), scope)$reason, "s7_argument_matching")
    expect_identical(infer(quote(record(le = "ok")@left), scope)$literal, "ok")
    expect_identical(infer(quote(S7::prop(record(), "right")$id), scope)$literal, 1L)

    for (expr in expression(S7::new_union(), S7::new_union(opaque), S7::new_S3_class(opaque),
            S7::new_external_class("pkg", opaque), S7::as_class(x = , arg = "label"),
            S7::deprecated_property(opaque), S7::new_property(class = NULL, class = NULL),
            S7::new_class(name = opaque), S7::prop(name = "extra", name = "dup"),
            S7::set_props(list(), right = 1L))) {
        expect_null(infer(expr, scope)$type)
    }
    expect_identical(infer(quote(S7::new_class("X", abstract = input)))$reason, "dynamic_s7_abstract")
    expect_identical(infer(quote(S7::new_class("X", constructor = input)))$reason, "dynamic_s7_constructor")
    expect_identical(infer(quote(S7::new_class("X", properties = list(S7::new_property()))))$reason,
        "dynamic_s7_properties")
    expect_identical(infer(quote(S7::new_class("X", properties = list(a = NULL, a = NULL))))$reason,
        "dynamic_s7_properties")
    expect_identical(infer(quote(S7::set_props(record(), 1, 2)), scope)$reason, "unnamed_s7_properties")
    expect_identical(infer(quote(S7::set_props(record(), left = 1, left = 2)), scope)$reason, "unknown_s7_property")
    expect_identical(infer(quote(S7::set_props(record(), absent = 1)), scope)$reason, "unknown_s7_property")
    expect_identical(infer(quote(S7::set_props(object = record(), object = record())), scope)$reason, "s7_argument_matching")
})

test_that("Deprecated S7 properties follow aliases without evaluating their callbacks", {
    index <- member_generic_index("")
    index$s7_capabilities <- list(exports = "deprecated_property")
    cls <- member_infer(quote(S7::new_class("Contact", properties = list(
        label = S7::new_property(default = quote("new")),
        S7::deprecated_property("name", new = "label", default = quote(stop("unused"))),
        S7::deprecated_property("other", new = "absent")
    ))), index)
    obj <- member_infer(quote(Contact()), index, list(Contact = cls))
    expect_identical(member_s7_slot(obj, "name", index, list())$literal, "new")
    expect_null(member_s7_slot(obj, "other", index, list())$type)
    expect_identical(member_s7_slot(obj, "absent", index, list())$reason, "unknown_s7_property")
})

test_that("Recursive and excessively wide S7 descriptor input stays bounded", {
    index <- member_generic_index("")
    parent <- member_infer(quote(S7::new_class("Base")), index)
    prop <- list(class = list(kind = "any"), getter = FALSE, setter = FALSE)
    parent$s7_descriptor$properties <- stats::setNames(rep(list(prop), 256L), paste0("p", seq_len(256L)))
    child <- member_infer(quote(S7::new_class("Child", parent = Parent, properties = list(extra = NULL))),
        index, list(Parent = parent))
    expect_identical(child$reason, "s7_property_limit")
    deep <- list(kind = "base", name = "integer")
    for (i in seq_len(33L)) deep <- list(kind = "union", classes = list(deep))
    expect_identical(member_s7_shape(deep)$reason, "s7_recursion")
    union <- structure(list(classes = 1:2), class = "S7_union")
    expect_null(member_s7_runtime_descriptor(union))
    bad <- structure(list(class = "Date", constructor = quote(ctor)), class = "S7_S3_class")
    expect_null(member_s7_runtime_descriptor(bad))
    bad$constructor <- function(x) x
    bad$default <- function() stop("never called")
    expect_identical(member_s7_runtime_descriptor(bad)$default, as.name(".__unknown_s7_default__"))
})
