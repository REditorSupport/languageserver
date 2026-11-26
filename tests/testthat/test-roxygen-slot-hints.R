slot_hint_labels <- function(f, prefix) {
    line <- paste0(prefix, "probe")
    row <- match(line, f$document$content) - 1L
    point <- list(row = row, col = nchar(prefix))
    items <- member_completion(f$uri, f$workspace, f$document, point, TRUE, 200L)
    vapply(items, `[[`, "", "label")
}

test_that("documented S4 slots refine inherited ANY reads and preserve actual values", {
    box <- c("Holder()@payload$", 'methods::slot(Holder(), "payload")$',
        'methods::new("Child")@payload$step()$', "Holder(payload = unknown())@payload$", "Inline()@thing$")
    empty <- c("Holder(payload = NULL)@payload$", "Holder()@fixed$", "EmptyDefault()@payload$")
    probes <- c(box, empty, "Holder(payload = list(real = 1))@payload$", "WithDefault()@payload$", "Holder()@")
    f <- provider_fixture(c(
        'Box <- R6::R6Class("Box", public = list(step = function() self, item = 1L))',
        "#' @slot payload (`Box`) Contents.",
        "#' @slot phantom (`Box`) Undeclared slot.",
        "#' @slot fixed (`Box`) Incompatible native class.",
        'Holder <- methods::setClass("Holder", slots = c(payload = "ANY", fixed = "numeric"))',
        'methods::setClass("Child", contains = "Holder")',
        'WithDefault <- methods::setClass("WithDefault", contains = "Holder", prototype = list(payload = list(actual = 1)))',
        'EmptyDefault <- methods::setClass("EmptyDefault", contains = "Holder", prototype = list(payload = NULL))',
        'Inline <- methods::setClass("Inline", slots = c(',
        "  #' @slot thing (`Box`) Inline slot.",
        '  thing = "ANY"))', paste0(probes, "probe")))
    for (query in box) expect_setequal(slot_hint_labels(f, query), c("step", "item", "clone"))
    for (query in empty) expect_length(slot_hint_labels(f, query), 0L)
    expect_identical(slot_hint_labels(f, "Holder(payload = list(real = 1))@payload$"), "real")
    expect_identical(slot_hint_labels(f, "WithDefault()@payload$"), "actual")
    expect_setequal(slot_hint_labels(f, "Holder()@"), c("payload", "fixed"))
    method <- list(row = match('methods::new("Child")@payload$step()$probe', f$document$content) - 1L,
        col = nchar('methods::new("Child")@payload$st'))
    target <- member_definition_location(f$uri, f$workspace, f$document, method)$result
    expect_identical(target$uri, f$uri)
    expect_equal(target$range$start$line, 0L)
})

test_that("S7 property and generated constructor documentation refine actual property reads", {
    skip_if_not_installed("S7")
    box <- c("Holder()@data$", "Holder()@computed$", 'S7::prop(Holder(), "computed")$',
        "S7::props(Holder())$computed$", "Child()@data$", "Holder()@inline$", "Explicit()@data$")
    empty <- c("Overridden()@data$", "Holder()@fixed$", "Holder()@phantom$",
        "Holder()@only_arg$", "Custom()@data$", "Holder()@other$")
    probes <- c(box, empty, "Holder(data = list(real = 1))@data$", "Holder()@defaulted$")
    f <- provider_fixture(c(
        'Box <- R6::R6Class("Box", public = list(item = 1L))',
        'Other <- R6::R6Class("Other", public = list(wrong = TRUE))',
        "#' @param data (`Box`) Value supplied to the constructor.",
        "#' @prop Holder@computed (`Box`) Read-only property.",
        "#' @param computed (`Other`) Not an input.",
        "#' @param only_arg (`Box`) Getter-only property.",
        "#' @prop fixed (`Box`) Incompatible native class.",
        "#' @prop phantom (`Box`) Undeclared property.",
        "#' @prop Different@other (`Box`) Belongs to another class on the shared page.",
        "#' @prop defaulted (`Box`) Concrete default takes precedence.",
        'Holder <- S7::new_class("Holder", properties = list(data = S7::class_any,',
        "  computed = S7::new_property(getter = function(self) opaque()),",
        "  only_arg = S7::new_property(getter = function(self) opaque()),",
        "  fixed = S7::class_integer, other = S7::class_any,",
        "  defaulted = S7::new_property(default = quote(list(actual = 1))),",
        "  #' @prop inline (`Box`) An inline property.",
        "  inline = S7::class_any))",
        'Child <- S7::new_class("Child", parent = Holder)',
        'Overridden <- S7::new_class("Overridden", parent = Holder, properties = list(data = S7::class_integer))',
        "#' @param data (`Box`) A custom constructor can transform this input.",
        'Custom <- S7::new_class("Custom", properties = list(data = S7::class_any), constructor = function(data) opaque())',
        "#' @param data (`Other`) Input.",
        "#' @prop data (`Box`) Explicit property contract.",
        'Explicit <- S7::new_class("Explicit", properties = list(data = S7::class_any))',
        paste0(probes, "probe"), "Holder()"))
    metadata <- MemberMetadataCache$new(16 * 1024^2)
    metadata$set("S7", member_prepare_package("S7"))
    f$workspace$member_metadata <- metadata
    for (query in box) expect_setequal(slot_hint_labels(f, query), c("item", "clone"))
    for (query in empty) expect_length(slot_hint_labels(f, query), 0L)
    expect_identical(slot_hint_labels(f, "Holder(data = list(real = 1))@data$"), "real")
    expect_identical(slot_hint_labels(f, "Holder()@defaulted$"), "actual")
    args <- member_call_arguments(f$uri, f$workspace, f$document, list(row = f$document$nline - 1L, col = 7L), "")
    expect_setequal(vapply(args, `[[`, "", "label"), c("data", "fixed", "other", "defaulted", "inline"))
})

test_that("documented slots respect union members and conflicting inheritance or writes", {
    queries <- c("Branch()@payload$", "Conflict()@payload$", "Override()@payload$", "Choice()@payload$", "replace()@payload$")
    f <- provider_fixture(c(
        'Left <- R6::R6Class("Left", public = list(common = TRUE, left = TRUE))',
        'Right <- R6::R6Class("Right", public = list(common = TRUE, right = TRUE))',
        "#' @slot payload (`Left`) First contract.",
        'A <- methods::setClass("A", slots = c(payload = "ANY"))',
        "#' @slot payload (`Right`) A different contract.",
        'B <- methods::setClass("B", slots = c(payload = "ANY"))',
        'Achild <- methods::setClass("Achild", contains = "A")',
        "Branch <- function() if (condition) A() else Achild()",
        'Conflict <- methods::setClass("Conflict", contains = c("A", "B"))',
        "#' @slot payload (`Right`) Child explicitly documents the inherited slot.",
        'Override <- methods::setClass("Override", contains = c("A", "B"))',
        "#' @slot payload (`Left | Right`) Either receiver.",
        'Choice <- methods::setClass("Choice", slots = c(payload = "ANY"))',
        "replace <- function() if (condition) A() else A(payload = list(observed = TRUE))",
        "read <- function() {", '  Left <- R6::R6Class("Shadow", public = list(wrong = TRUE))',
        '  methods::new("A")@payload$probe', "}",
        paste0(queries, "probe")))
    expect_setequal(slot_hint_labels(f, "Branch()@payload$"), c("clone", "common", "left"))
    expect_length(slot_hint_labels(f, "Conflict()@payload$"), 0L)
    expect_setequal(slot_hint_labels(f, "Override()@payload$"), c("clone", "common", "right"))
    expect_setequal(slot_hint_labels(f, "Choice()@payload$"), c("clone", "common"))
    expect_length(slot_hint_labels(f, "replace()@payload$"), 0L)
    expect_setequal(slot_hint_labels(f, '  methods::new("A")@payload$'), c("clone", "common", "left"))
})

test_that("slot hints resolve a generator in the defining package file", {
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines("Package: slotfixture", file.path(root, "DESCRIPTION"))
    writeLines("", file.path(root, "NAMESPACE"))
    workspace <- Workspace$new(root)
    open <- function(file, lines) {
        path <- file.path(root, "R", file)
        writeLines(lines, path)
        uri <- path_to_uri(path)
        doc <- Document$new(uri, version = 1L, content = lines)
        workspace$documents$set(uri, doc)
        parsed <- parse_document(uri, lines)
        parsed$version <- doc$version
        workspace$update_parse_data(uri, parsed)
        workspace$index$update_content(uri, lines, parse_data = parsed)
        list(uri = uri, workspace = workspace, document = doc)
    }
    open("holder.R", c('Box <- R6::R6Class("Box", public = list(item = TRUE))',
            "#' @slot value (`Box`) Stored receiver.",
            'Holder <- methods::setClass("Holder", slots = c(value = "ANY"))'))
    caller <- open("caller.R", c("inspect <- function() {",
            '  Box <- R6::R6Class("Shadow", public = list(wrong = TRUE))',
            "  Holder()@value$probe", "}"))
    expect_setequal(slot_hint_labels(caller, "  Holder()@value$"), c("clone", "item"))
})

test_that("class hints follow stored properties across supported S4 and S7 inheritance", {
    skip_if_not_installed("S7", "1.0.0")
    metadata <- MemberMetadataCache$new(16 * 1024^2)
    snapshot <- member_prepare_package("S7")
    metadata$set("S7", snapshot)
    from_s4 <- if (isTRUE(snapshot$s7_capabilities$s4)) "NewHolder()@value$"
    get <- c("S4Holder()@data$", 'S7::prop(S4Holder(), "data")$')
    f <- provider_fixture(c('Box <- R6::R6Class("Box", public = list(item = TRUE))',
        "#' @slot value (`Box`) Stored receiver.",
        'OldHolder <- methods::setClass("OldHolder", slots = c(value = "ANY"))',
        'NewHolder <- S7::new_class("NewHolder", parent = OldHolder)',
        "#' @prop data (`Box`) Stored property.",
        'Container <- S7::new_class("Container", properties = list(data = S7::class_any))',
        "S7::S4_register(Container)",
        'S4Holder <- methods::setClass("S4Holder", contains = S7::S4_contains(Container))',
        "#' @slot data (`list`) Narrow inherited property to list in this subclass.",
        'Narrow <- methods::setClass("Narrow", contains = "S4Holder")',
        paste0(c(get, from_s4), "probe"), 'S7::prop(Narrow(), "data")$probe'))
    f$workspace$member_metadata <- metadata
    for (query in c(get, from_s4)) expect_setequal(slot_hint_labels(f, query), c("clone", "item"))
    expect_length(slot_hint_labels(f, 'S7::prop(Narrow(), "data")$'), 0L)
})
