test_that("class hierarchy preserves raw strings and escaped class and slot names", {
    parent <- 'Pa`rent"name'
    child <- "Ch\u03b9ld"
    slot <- 'a"b'
    quoted_slot <- "odd`slot"
    code <- c(
        sprintf('setClass(r"(%s)")', parent),
        sprintf('setClass("Ch\\u03b9ld", contains = r"(%s)", slots = c(%s = "character", %s = "numeric"))',
            parent, encodeString(slot, quote = '"'), encodeString(quoted_slot, quote = "`"))
    )
    fixture <- provider_fixture(code)
    parsed <- fixture$document$parse_data
    for (indexed in c(TRUE, FALSE)) {
        xdoc <- parse_data_xml(parsed)
        if (!indexed) attr(xdoc, "class_index") <- NULL
        detected <- detect_type_definition(fixture$uri, fixture$workspace, fixture$document,
            list(row = 0L, col = 15L), "rent")
        expect_identical(detected$name, parent)
        expect_identical(vapply(find_s4_supertypes(fixture$document, xdoc, child, fixture$uri),
                function(item) item$name, character(1L)), parent)
        expect_identical(vapply(find_s4_subtypes(fixture$document, xdoc, parent, fixture$uri),
                function(item) item$name, character(1L)), child)
        members <- extract_s4_members(fixture$document, xdoc, list(name = child, type = "S4"))
        expect_identical(vapply(members, function(member) member$name, character(1L)), c(slot, quoted_slot))
    }
    restored <- unserialize(serialize(parsed$class_data, NULL))
    expect_identical(restored$supertypes[[class_index_key("S4", child)]][[1L]]$name, parent)
    expect_identical(restored$subtypes[[class_index_key("S4", parent)]][[1L]]$name, child)
})

test_that("R6 hierarchy and members preserve quoted generator and member names", {
    generator <- "Parent generator"
    parent <- "Pa`rent"
    child <- 'Child"name'
    field <- "odd`field"
    method <- 'run"method'
    code <- c(
        sprintf('%s <- R6::R6Class(r"(%s)")', encodeString(generator, quote = "`"), parent),
        sprintf('Child <- R6::R6Class(r"(%s)", inherit = %s, public = list(%s = 1, %s = function() NULL))',
            child, encodeString(generator, quote = "`"), encodeString(field, quote = "`"),
            encodeString(method, quote = '"'))
    )
    fixture <- provider_fixture(code)
    parsed <- fixture$document$parse_data
    xdoc <- parse_data_xml(parsed)
    supers <- find_r6_supertypes(fixture$document, xdoc, child, fixture$uri)
    expect_identical(supers[[1L]]$name, generator)
    subs <- find_r6_subtypes(fixture$document, xdoc, generator, fixture$uri)
    expect_identical(subs[[1L]]$name, child)
    members <- extract_r6_members(fixture$document, xdoc, list(name = "Child", type = "R6"))
    expect_identical(vapply(members, function(member) member$name, character(1L)), c(field, method))
    expect_identical(vapply(members, function(member) member$kind, numeric(1L)),
        c(SymbolKind$Field, SymbolKind$Method))
})

test_that("static class literal decoding never evaluates expressions", {
    text <- c('r"(a`b\"c)"', '"a\\u0062"', "`a\\`b`", "plain", 'stop("must not run")',
        "r'(foo)'", "R'[bar]'")
    expect_identical(class_token_text(text), c('a`b"c', "ab", "a`b", "plain", text[[5L]], "foo", "bar"))
})

test_that("single-quoted raw class literals retain hierarchy identities", {
    fixture <- provider_fixture(c("setClass(r'(Parent)')",
            "setClass(R'[Child]', contains = r'(Parent)')"))
    parsed <- fixture$document$parse_data
    xdoc <- parse_data_xml(parsed)
    detected <- detect_type_definition(fixture$uri, fixture$workspace, fixture$document,
        list(row = 0L, col = 13L), "Parent")
    expect_identical(detected$name, "Parent")
    expect_identical(find_s4_supertypes(fixture$document, xdoc, "Child", fixture$uri)[[1L]]$name, "Parent")
    expect_identical(find_s4_subtypes(fixture$document, xdoc, "Parent", fixture$uri)[[1L]]$name, "Child")
    expect_identical(parsed$class_data$supertypes[[class_index_key("S4", "Child")]][[1L]]$name, "Parent")
})
