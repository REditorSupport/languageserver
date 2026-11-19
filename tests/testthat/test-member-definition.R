member_definition_test <- function(fixture, row, member) {
    line <- fixture$document$line0(row)
    point <- list(row = row, col = as.integer(regexpr(member, line, fixed = TRUE)[[1L]]))
    definition_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, getwd())$result
}

test_that("member definitions retain their field identity and code-point source positions", {
    fixture <- provider_fixture(c(
        '"😀"; x <- list(`odd name` = 1, nested = list(field = 2))',
        "y <- list(`odd name` = 0)",
        "x$`odd name`",
        "x$nested$field",
        "x$`odd name` <- function(a) a",
        "x$`odd name`(1)",
        "x$`odd name` <- NULL",
        "x$`odd name`"))
    expect_equal(member_definition_test(fixture, 2L, "odd")$range,
        range(position(0, 16), position(0, 26)))
    expect_equal(member_definition_test(fixture, 3L, "field")$range,
        range(position(0, 46), position(0, 51)))
    expect_equal(member_definition_test(fixture, 5L, "odd")$range,
        range(position(4, 2), position(4, 12)))
    expect_null(member_definition_test(fixture, 7L, "odd"))
})

test_that("member definitions recover physical positions in incomplete scopes", {
    for (closing in list(c("  obj$fun()", "}"), "  obj$fun(")) {
        text <- c("f <- function() {", '\t"😀"; obj <- list(fun = function(x) x)', closing)
        fixture <- provider_fixture(text)
        expect_equal(member_definition_test(fixture, 2L, "fun")$range,
            range(position(1, 19), position(1, 22)))
    }
    # Files read as native strings can have unmarked UTF-8 text.
    text <- "\t\"\U0001F600\"; obj <- list(fun = 1)"
    if (l10n_info()[["UTF-8"]]) Encoding(text) <- "unknown"
    fixture <- provider_fixture(c(text, "obj$fun"))
    expect_equal(member_definition_test(fixture, 1L, "fun")$range, range(position(0, 19), position(0, 22)))
    fixture <- provider_fixture(c("bad ! !", "x <- 0; f <- function() { obj <- list(thing = 1); obj$thing("))
    expect_equal(member_definition_test(fixture, 1L, "$thing")$range, range(position(1, 38), position(1, 43)))
})

test_that("F12 keeps ambiguous and unknown members out of bare-name navigation", {
    fixture <- provider_fixture(c("first <- list(value = 1)", "second <- list(value = 1)",
            "x <- if (opaque) first else second", "x$value", "unknown $ mean", "value <- 1", "value"))
    fixture$workspace$get_definition <- function(...) stop("unrelated symbol lookup")
    expect_null(member_definition_test(fixture, 3L, "value"))
    expect_null(member_definition_test(fixture, 4L, "mean"))
    expect_equal(member_definition_test(fixture, 6L, "value")$range, range(position(5, 0), position(5, 10)))
})

test_that("member definitions follow R6 declaration owners and native pipe argument tags", {
    fixture <- provider_fixture(c('C <- R6::R6Class("C", public = list(fun = function(x) x))',
            'D <- R6::R6Class("D", inherit = C)', "D$new()$fun(2)"))
    expect_equal(member_definition_test(fixture, 2L, "fun")$range, range(position(0, 36), position(0, 39)))
    skip_if(getRversion() < "4.1.0")
    fixture <- provider_fixture(c("out <- 1 |> list(alpha = 2)", "out$alpha"))
    expect_equal(member_definition_test(fixture, 1L, "alpha")$range, range(position(0, 17), position(0, 22)))
})

test_that("namespace member navigation verifies preserved physical function sources", {
    path <- withr::local_tempfile(fileext = ".R")
    code <- "\t\"\U0001F600\"; api <- list(run = function(input) input)"
    writeLines(code, path)
    scope <- new.env(parent = baseenv())
    sys.source(path, scope, keep.source = TRUE)
    input <- member_namespace_input(scope, package = "originfixture", exports = "api")
    index <- member_package_index(input)
    index$roots <- member_public_roots(index, input)
    index$namespace_roots[["originfixture::api"]] <- index$roots$api
    store <- MemberMetadataCache$new(32 * 1024^2)
    store$set("originfixture", member_index_freeze(index))
    fixture <- provider_fixture("originfixture::api$run(1)")
    fixture$workspace$member_metadata <- store
    expect_equal(member_definition_test(fixture, 0L, "run"),
        list(uri = path_to_uri(path), range = range(position(0, 25), position(0, 33))))
    source <- Document$new(path_to_uri(path), version = 1L, content = c("# unsaved change", code))
    source$is_open <- TRUE
    fixture$workspace$documents$set(source$uri, source)
    expect_null(member_definition_test(fixture, 0L, "run"))
    fixture$workspace$documents$remove(source$uri)
    writeLines(c("# source tree edited after installation", code), path)
    expect_null(member_definition_test(fixture, 0L, "run"))
})

test_that("member definitions are available in unsaved untitled documents", {
    uri <- "untitled:Untitled-1"
    text <- c("obj <- list(fun = function(x) x)", "obj$fun(")
    document <- Document$new(uri, language = "r", version = 1L, content = text)
    parsed <- parse_document(uri, text)
    parsed$version <- document$version
    document$update_parse_data(parsed)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    expect_equal(definition_reply(1L, uri, workspace, document, list(row = 1L, col = 6L))$result,
        list(uri = uri, range = range(position(0, 12), position(0, 15))))
})
