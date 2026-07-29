parse_type_hierarchy_xdoc <- function(code) {
    parsed <- parse(text = code, keep.source = TRUE)
    xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
}

test_that("S4 hierarchy parsing handles named arguments", {
    code <- c(
        'setClass("BaseEntity")',
        'setClass("User", contains = "BaseEntity", slots = c(name = "character"))',
        'setClass("AdminUser", contains = "User")',
        'setMethod("show", "User", function(object) object)'
    )
    document <- Document$new("file:///s4.R", content = code)
    xdoc <- parse_type_hierarchy_xdoc(code)

    definition <- detect_s4class(xdoc, "User", document, document$uri)
    expect_equal(definition$range, range(position(1, 10), position(1, 14)))

    supertypes <- find_s4_supertypes(document, xdoc, "User", document$uri)
    expect_equal(map_chr(supertypes, "name"), "BaseEntity")
    expect_equal(supertypes[[1]]$range, range(position(1, 29), position(1, 39)))

    subtypes <- find_s4_subtypes(document, xdoc, "User", document$uri)
    expect_equal(map_chr(subtypes, "name"), "AdminUser")
    expect_equal(subtypes[[1]]$range, range(position(2, 10), position(2, 19)))

    members <- extract_s4_members(
        document, xdoc, list(name = "User", type = "S4")
    )
    expect_setequal(map_chr(members, "name"), c("name", "show"))
})

test_that("RefClass hierarchy parsing handles named arguments", {
    code <- c(
        'setRefClass("BaseReference")',
        paste0(
            'setRefClass("UserReference", contains = "BaseReference", ',
            'fields = list(name = "character", metadata = list(source = "character")), ',
            'methods = list(greet = function() paste("hi", sep = "-")))'
        ),
        'setRefClass("AdminReference", contains = "UserReference")'
    )
    document <- Document$new("file:///refclass.R", content = code)
    xdoc <- parse_type_hierarchy_xdoc(code)

    definition <- detect_refclass(xdoc, "UserReference", document, document$uri)
    expect_equal(definition$range, range(position(1, 13), position(1, 26)))

    supertypes <- find_refclass_supertypes(
        document, xdoc, "UserReference", document$uri
    )
    expect_equal(map_chr(supertypes, "name"), "BaseReference")
    expect_equal(supertypes[[1]]$range, range(position(1, 41), position(1, 54)))

    subtypes <- find_refclass_subtypes(
        document, xdoc, "UserReference", document$uri
    )
    expect_equal(map_chr(subtypes, "name"), "AdminReference")
    expect_equal(subtypes[[1]]$range, range(position(2, 13), position(2, 27)))

    members <- extract_refclass_members(
        document, xdoc, list(name = "UserReference", type = "RefClass")
    )
    expect_setequal(map_chr(members, "name"), c("name", "metadata", "greet"))
})

test_that("R6 hierarchy parsing finds inheritance and real members", {
    code <- c(
        'Base <- R6Class("Base", public = list(base_field = 1))',
        paste0(
            'Child <- R6::R6Class("Child", inherit = Base, ',
            'public = list(value = 1, run = function(x) { list(nested = x) }), ',
            'private = list(secret = 2, hide = function() secret), ',
            'active = list(ignored = function() value), cloneable = TRUE)'
        ),
        'Sibling <- R6Class("Sibling", inherit = Base, public = list())'
    )
    document <- Document$new("file:///r6.R", content = code)
    xdoc <- parse_type_hierarchy_xdoc(code)

    assignment <- detect_r6class(xdoc, "Child", document, document$uri)
    expect_equal(assignment$name, "Child")
    expect_equal(assignment$classType, "R6")

    string_definition <- detect_r6class(xdoc, "Sibling", document, document$uri)
    expect_equal(string_definition$name, "Sibling")

    supertypes <- find_r6_supertypes(document, xdoc, "Child", document$uri)
    expect_equal(map_chr(supertypes, "name"), "Base")
    expect_equal(supertypes[[1L]]$classType, "R6")

    subtypes <- find_r6_subtypes(document, xdoc, "Base", document$uri)
    expect_setequal(map_chr(subtypes, "name"), c("Child", "Sibling"))

    members <- extract_r6_members(
        document, xdoc, list(name = "Child", type = "R6")
    )
    expect_setequal(
        map_chr(members, "name"),
        c("value", "run", "secret", "hide")
    )
    expect_false("nested" %in% map_chr(members, "name"))
    expect_setequal(
        map_chr(members, "detail"),
        c("public", "private")
    )
    kinds <- setNames(map_int(members, "kind"), map_chr(members, "name"))
    expect_equal(kinds[["value"]], SymbolKind$Field)
    expect_equal(kinds[["run"]], SymbolKind$Method)

    expect_equal(
        extract_class_members(
            document, xdoc, list(name = "Child", type = "R6")
        ),
        members
    )
})

test_that("S3 and setMethod definitions are detected without false positives", {
    code <- c(
        "print.widget <- function(x, ...) x",
        'setMethod("show", "Special", function(object) object)',
        "plain_name <- 1"
    )
    document <- Document$new("file:///methods.R", content = code)
    xdoc <- parse_type_hierarchy_xdoc(code)

    s3_scopes <- xdoc_find_enclosing_scopes(xdoc, 1L, 2L, top = TRUE)
    s3 <- detect_s3class(s3_scopes, "print.widget", document, document$uri)
    expect_equal(s3$name, "widget")
    expect_equal(s3$classType, "S3")

    method_scopes <- xdoc_find_enclosing_scopes(xdoc, 2L, 20L, top = TRUE)
    s4 <- detect_s3class(method_scopes, "Special", document, document$uri)
    expect_equal(s4$name, "Special")
    expect_equal(s4$classType, "S4")

    expect_null(detect_s3class(xdoc, "plain_name", document, document$uri))
    expect_identical(
        find_s3_supertypes(document, xdoc, "widget", document$uri),
        list()
    )
    expect_identical(
        find_s3_subtypes_child(document, xdoc, "widget", document$uri),
        list()
    )
})

test_that("Type detection uses the token under the cursor", {
    code <- c(
        'Parent <- R6Class("Parent")',
        'Child <- R6Class("Child", inherit = Parent)',
        "Child"
    )
    uri <- "file:///detected-r6.R"
    document <- Document$new(uri, content = code)
    xdoc <- parse_type_hierarchy_xdoc(code)
    workspace <- list(get_parse_data = function(request_uri) {
        expect_identical(request_uri, uri)
        list(xml_doc = xdoc)
    })

    detected <- detect_type_definition(
        uri, workspace, document, list(row = 1L, col = 1L), "Child"
    )
    expect_equal(detected$name, "Child")
    expect_equal(detected$classType, "R6")

    no_parse <- list(get_parse_data = function(...) list(xml_doc = NULL))
    expect_null(detect_type_definition(
        uri, no_parse, document, list(row = 1L, col = 1L), "Child"
    ))
    expect_null(detect_type_definition(
        uri, workspace, document, list(row = 1L, col = 6L), ""
    ))

    fallback <- detect_type_definition(
        uri, workspace, document, list(row = 2L, col = 2L), "Child"
    )
    expect_equal(fallback$name, "Child")
    expect_equal(fallback$classType, "R6")
    expect_null(detect_type_definition(
        uri, workspace, document, list(row = 99L, col = 0L), "Child"
    ))
})

test_that("Type detection falls through to S3 definitions and null results", {
    fixture <- provider_fixture(c(
        "print.widget <- function(x) x",
        "plain_name <- 1"
    ))
    s3 <- detect_type_definition(
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(row = 0L, col = 2L),
        "print.widget"
    )
    expect_equal(s3$name, "widget")
    expect_equal(s3$classType, "S3")

    expect_null(detect_type_definition(
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(row = 1L, col = 2L),
        "plain_name"
    ))

    no_xml <- list(
        documents = fixture$workspace$documents,
        type_hierarchy_cache = collections::dict(),
        get_parse_data = function(...) list(xml_doc = NULL)
    )
    expect_length(find_type_supertypes(no_xml, list(
        name = "widget",
        uri = fixture$uri,
        classType = "S3"
    )), 0L)
})

test_that("R6 hierarchy accepts quoted inheritance", {
    code <- c(
        'Base <- R6Class("Base")',
        'Child <- R6Class("Child", inherit = "Base")'
    )
    document <- Document$new("file:///quoted-r6.R", content = code)
    xdoc <- parse_type_hierarchy_xdoc(code)

    supertypes <- find_r6_supertypes(
        document, xdoc, "Child", document$uri
    )
    expect_equal(map_chr(supertypes, "name"), "Base")
})

test_that("type hierarchy caches empty and S3 hierarchy results", {
    fixture <- provider_fixture("print.widget <- function(x) x")
    fixture$workspace$type_hierarchy_cache <- collections::dict()
    definition <- list(
        name = "widget",
        uri = fixture$uri,
        classType = "S3"
    )

    expect_length(find_type_supertypes(fixture$workspace, definition), 0L)
    cache_size <- fixture$workspace$type_hierarchy_cache$size()
    expect_length(find_type_supertypes(fixture$workspace, definition), 0L)
    expect_equal(fixture$workspace$type_hierarchy_cache$size(), cache_size)

    missing <- list(
        documents = collections::dict(),
        type_hierarchy_cache = collections::dict()
    )
    missing$documents$set(definition$uri, NULL)
    expect_length(find_type_supertypes(missing, definition), 0L)

    fixture$workspace$type_hierarchy_cache$set(
        paste("sub", fixture$uri, "S3", "widget", sep = "\r"),
        list(list(name = "cached"))
    )
    expect_equal(
        find_type_subtypes(fixture$workspace, definition)[[1L]]$name,
        "cached"
    )
})

test_that("Element ranges reject absent or incomplete parse nodes", {
    document <- Document$new("file:///ranges.R", content = '"quoted"')
    xdoc <- parse_type_hierarchy_xdoc('"quoted"')
    string <- xml2::xml_find_first(xdoc, "//STR_CONST")

    expect_equal(
        get_element_range(document, string),
        range(position(0L, 1L), position(0L, 7L))
    )
    expect_null(get_element_range(document, xml2::xml_missing()))

    incomplete <- xml2::read_xml("<SYMBOL line1='1' col1='1'>x</SYMBOL>")
    expect_null(get_element_range(document, incomplete))
    expect_null(extract_class_members(
        document, xdoc, list(name = "Anything", type = "unknown")
    ))
    expect_null(extract_class_members(
        document, xdoc, list(name = "Anything")
    ))
})
