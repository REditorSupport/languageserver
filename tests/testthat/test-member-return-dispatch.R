test_that("Prepared S3 method returns retain namespace ownership and argument matching", {
    code <- paste(c(
        "native_box <- function(pointer) {box <- new.env(); box$step <- make_step(pointer); class(box) <- \"raw_box\"; box}",
        "make_step <- function(pointer) function() native_box(.Call(\"never\", pointer))",
        "adapt <- function(x) UseMethod(\"adapt\")",
        "adapt.raw_box <- function(x) {box <- new.env(); box$raw <- x; class(box) <- \"public_box\"; box}",
        "convert <- function(x, flag = TRUE) UseMethod(\"convert\")",
        "convert.data.frame <- function(x, flag = TRUE) {copied <- lapply(x, function(column) identity(column)); adapt(native_box(.Call(\"never\", copied)))}",
        "convert.list <- function(x, flag = TRUE) if(flag) adapt(native_box(NULL)) else list(other = TRUE)"
    ), collapse = "\n")
    scope <- new.env(parent = baseenv())
    eval(parse(text = code), scope)
    index <- member_package_index(member_namespace_input(scope, package = "conversionfixture", exports = "convert"))
    expect_identical(index$method_results$convert.data.frame$type, "public_box")
    expect_identical(index$method_results$convert.data.frame$member_package, "conversionfixture")
    expect_null(index$method_results$convert.list)
    for (call in c(
        'convert(structure(list(value = 1), class = "data.frame"))',
        'convert(structure(list(value = 1), class = "data.frame"), fl = FALSE)')) {
        budget <- member_request_budget()
        budget$remaining <- 32L
        result <- member_infer(str2lang(call), index, index$package_roots, budget = budget)
        expect_identical(result$type, "public_box", info = call)
        expect_false(budget$exhausted, info = call)
    }
    expect_identical(member_infer(quote(convert(list(value = 1), FALSE)), index, index$package_roots)$type, "list")
    expect_identical(member_infer(quote(convert(structure(list(value = 1), class = "data.frame"),
                    flag = TRUE, flag = FALSE)), index, index$package_roots)$reason, "argument_matching")
    expect_identical(member_infer(quote(convert(structure(list(value = 1), class = "data.frame"),
                    unknown = TRUE)), index, index$package_roots)$reason, "argument_matching")
    data <- new.env(parent = baseenv())
    data$records <- data.frame(value = 1)
    snapshots <- list(conversionfixture = member_index_freeze(index),
        datafixture = member_index_freeze(member_package_index(member_namespace_input(
            data, package = "datafixture", exports = "records"))))
    content <- c("library(conversionfixture)", "converted <- convert(datafixture::records)", "converted$")
    document <- Document$new("file:///conversion.R", language = "r", version = 1L, content = content)
    document$parse_data <- parse_document(document$uri, content)
    metadata <- collections::dict()
    for (package in names(snapshots)) metadata$set(package, member_index_thaw(snapshots[[package]]))
    items <- member_completion(document$uri, list(member_metadata = metadata), document,
        list(row = 2L, col = nchar(content[[3L]])), FALSE, 200L)
    expect_identical(vapply(items, `[[`, character(1L), "label"), "raw")
    content[[3L]] <- "converted$raw$step()$"
    document <- Document$new("file:///conversion-chain.R", language = "r", version = 1L, content = content)
    document$parse_data <- parse_document(document$uri, content)
    items <- member_completion(document$uri, list(member_metadata = metadata), document,
        list(row = 2L, col = nchar(content[[3L]])), FALSE, 200L)
    expect_identical(vapply(items, `[[`, character(1L), "label"), "step")
})
