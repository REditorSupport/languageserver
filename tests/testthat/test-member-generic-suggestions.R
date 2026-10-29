test_that("Unknown generic inputs offer common known members without claiming a type", {
    code <- paste(c(
        "native_box <- function(pointer) {box <- new.env(); box$step <- make_step(pointer); class(box) <- \"raw_box\"; box}",
        "make_step <- function(pointer) function() native_box(.Call(\"never\", pointer))",
        "adapt <- function(x) UseMethod(\"adapt\")",
        "adapt.raw_box <- function(x) {box <- new.env(); box$raw <- x; class(box) <- \"public_box\"; box}",
        "convert <- function(x) UseMethod(\"convert\")",
        "convert.default <- function(x) adapt(native_box(.Call(\"never\", x)))",
        "convert.data.frame <- function(x) adapt(native_box(.Call(\"never\", x)))"
    ), collapse = "\n")
    ns <- new.env(parent = baseenv())
    eval(parse(text = code), ns)
    make_metadata <- function() {
        metadata <- MemberMetadataCache$new(1024^2)
        index <- member_package_index(member_namespace_input(ns, package = "suggestionfixture", exports = "convert"))
        metadata$set("suggestionfixture", member_index_freeze(index))
        metadata
    }
    resolve <- function(lines, metadata) {
        document <- Document$new("file:///generic-suggestion.R", language = "r", version = 1L, content = lines)
        document$parse_data <- parse_document(document$uri, lines)
        point <- list(row = length(lines) - 1L, col = nchar(tail(lines, 1L)))
        workspace <- list(member_metadata = metadata)
        resolved <- member_resolve_cursor(document$uri, workspace, document, point, member_cursor(document, point))
        items <- member_completion(document$uri, workspace, document, point, TRUE, 200L)
        list(value = resolved$value, items = items)
    }
    lines <- c("library(suggestionfixture)", "input <- opaque_source()", "converted <- convert(input)", "converted$")
    metadata <- make_metadata()
    result <- resolve(lines, metadata)
    expect_null(result$value$type)
    expect_identical(result$value$reason, "open_dispatch")
    expect_identical(vapply(result$items, `[[`, character(1L), "label"), "raw")
    expect_true(isTRUE(attr(result$items, "truncated")))
    overridden <- append(lines, "convert.custom <- function(x) list(other = TRUE)", after = 0L)
    expect_length(resolve(overridden, metadata)$items, 0L)
    ns$convert.custom <- function(x) list(other = TRUE)
    expect_length(resolve(lines, make_metadata())$items, 0L)
})
