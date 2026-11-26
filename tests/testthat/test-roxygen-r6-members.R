r6_doc_members <- function(lines) {
    row <- grep("@@", lines, fixed = TRUE)
    col <- regexpr("@@", lines[[row]], fixed = TRUE)[[1L]] - 1L
    lines[[row]] <- sub("@@", "", lines[[row]], fixed = TRUE)
    f <- provider_fixture(lines)
    items <- member_completion(f$uri, f$workspace, f$document, list(row = row - 1L, col = col), TRUE, 200L)
    sort(vapply(items, `[[`, "", "label"))
}

test_that("R6 typed fields and active bindings resolve lazily through nested member reads", {
    code <- c('Item <- R6::R6Class("Item", public = list(next_item = function() self, amount = 0L))',
        'Other <- R6::R6Class("Other", public = list(other = TRUE))',
        'Box <- R6::R6Class("Box", public = list(',
        "#' @field item (`Item`) A delayed payload.", "item = NULL,",
        "#' @field fixed (`Item`) A fallback.", "fixed = list(actual = TRUE),",
        "read = function() self$item", "), active = list(",
        "#' @field cached (`Item`) A computed payload.", "cached = function() stop('never execute the getter'),",
        "#' @return (`Other`) A computed item.", "external = function() opaque()", "))",
        'Sub <- R6::R6Class("Sub", inherit = Box)', "box <- Sub$new()")
    for (site in c("box$item$@@amount", "box[['item']]$next_item()$@@amount", "box$cached$@@amount", "box$read()$@@amount")) {
        expect_identical(r6_doc_members(c(code, site)), c("amount", "clone", "next_item"), info = site)
    }
    expect_identical(r6_doc_members(c(code, "box$external$@@other")), c("clone", "other"))
    expect_identical(r6_doc_members(c(code, "box$fixed$@@actual")), "actual")
    suppress <- c('Sub <- R6::R6Class("Sub", inherit = Box, public = list(',
        "#' @field item NULL", "item = NULL))", "Sub$new()$item$@@amount")
    expect_length(r6_doc_members(c(code, suppress)), 0L)
    init <- function(rhs) {
        c(code, 'Built <- R6::R6Class("Built", inherit = Box, public = list(',
            paste0("initialize = function() self$item <- ", rhs, "))"), "Built$new()$item$@@probe")
    }
    expect_identical(r6_doc_members(init("opaque()")), c("amount", "clone", "next_item"))
    expect_identical(r6_doc_members(init("list(actual = TRUE)")), "actual")
    expect_length(r6_doc_members(init("NULL")), 0L)
})

test_that("R6 methods use local, class, initializer-field and superclass parameter documentation", {
    classes <- c('Item <- R6::R6Class("Item", public = list(from_item = TRUE))',
        'Other <- R6::R6Class("Other", public = list(from_other = TRUE))')
    base <- c("#' @param input (`Item`) Class default.", 'Parent <- R6::R6Class("Parent", public = list(',
        "#' @field input (`Other`) Field default.", "input = NULL,",
        "#' @field payload (`Other`) Payload.", "payload = NULL,",
        "initialize = function(input, payload) { input$from_item; payload$from_other },",
        "take = function(input) input,",
        "#' @param input (`Other`) Method override.", "override = function(input) input,",
        "#' @param input Local prose.", "untagged = function(input) input,",
        "#' @returns (`Item`) The fetched item.", "fetch = function() opaque()", "))")
    parent <- c(classes, base)
    expect_identical(r6_doc_members(c(parent, "Parent$new()$take(opaque())$@@from_item")), c("clone", "from_item"))
    expect_identical(r6_doc_members(c(parent, "Parent$new()$override(opaque())$@@from_other")), c("clone", "from_other"))
    expect_length(r6_doc_members(c(parent, "Parent$new()$untagged(opaque())$@@from_item")), 0L)
    expect_identical(r6_doc_members(c(parent, "Parent$new()$take(list(actual = 1))$@@actual")), "actual")
    expect_identical(r6_doc_members(c(parent, "Parent$new()$fetch()$@@from_item")), c("clone", "from_item"))
    for (field in c("from_item", "from_other")) {
        marked <- sub(field, paste0("@@", field), base, fixed = TRUE)
        expect_identical(r6_doc_members(c(classes, marked)), sort(c("clone", field)))
    }
    child <- c('Sub <- R6::R6Class("Sub", inherit = Parent, public = list(',
        "override = function(input) input, take = function(input) input))")
    expect_identical(r6_doc_members(c(parent, child, "Sub$new()$override(opaque())$@@from_other")), c("clone", "from_other"))
    expect_identical(r6_doc_members(c(parent, sub("input, take", "input$@@from_other, take", child, fixed = TRUE))),
        c("clone", "from_other"))
    expect_identical(r6_doc_members(c(parent, "#' @param input (`Item`) New class default.", child,
                "Sub$new()$override(opaque())$@@from_item")), c("clone", "from_item"))
    expect_length(r6_doc_members(c(parent, "#' @param input Just documentation.", child,
                "Sub$new()$override(opaque())$@@from_other")), 0L)
})

test_that("R6 $set documents only the statically registered function", {
    code <- c('Item <- R6::R6Class("Item", public = list(value = 1L))',
        "#' @param item (`Item`) A class-level default.", 'G <- R6::R6Class("G")')
    setter <- c("#' @description Fetch an object.", "#' @returns (`Item`) Result.",
        'G$set("public", "fetch", function() opaque())')
    expect_identical(r6_doc_members(c(code, setter, "G$new()$fetch()$@@value")), c("clone", "value"))
    setter <- c("#' @param arg (`Item`) An item.",
        'G[["set"]](name = "echo", value = function(arg) arg, which = "public")')
    expect_identical(r6_doc_members(c(code, setter, "G$new()$echo(opaque())$@@value")), c("clone", "value"))
    expect_identical(r6_doc_members(c(code, 'G$set("public", "echo", function(item) item)',
                "G$new()$echo(opaque())$@@value")), c("clone", "value"))
    expect_identical(r6_doc_members(c(code, 'G$set("public", "echo", function(item) item$@@value)')), c("clone", "value"))
    expect_true("echo" %in% r6_doc_members(c(code, 'G$set("public", "echo", function(item) self$@@echo(item))')))
    masked <- c(code, "G <- list(set = function(which, name, value) value)",
        "#' @param arg (`Item`) Not an R6 registration.", 'G$set("public", "echo", function(arg) arg$@@value)')
    expect_length(r6_doc_members(masked), 0L)
})
