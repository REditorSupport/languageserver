test_that("Unknown generic inputs offer common known members without claiming a type", {
    code <- paste(c(
        "commands <- new.env(parent = emptyenv())",
        "native_box <- function(pointer) {box <- new.env(); box$step <- make_step(pointer); class(box) <- \"raw_box\"; box}",
        "make_step <- function(pointer) function() native_box(.Call(\"never\", pointer))",
        "adapt <- function(x) UseMethod(\"adapt\")",
        "adapt.raw_box <- function(x) {box <- new.env(); box$raw <- x; class(box) <- \"public_box\"; box}",
        "`$.public_box` <- function(x, name) {method_names <- names(commands); if(name %in% method_names) {f <- commands[[name]]; current <- x; environment(f) <- environment(); f} else NextMethod()}",
        "step <- function(delta = 1, flag = TRUE) adapt(current$raw$step(delta))",
        "commands$step <- step",
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
    expect_identical(vapply(result$items, `[[`, character(1L), "label"), c("raw", "step"))
    expect_true(isTRUE(attr(result$items, "truncated")))
    providers <- function(lines, metadata) {
        document <- Document$new("file:///generic-provider.R", language = "r", version = 1L, content = lines)
        data <- parse_document(document$uri, lines)
        data$version <- 1L
        document$update_parse_data(data)
        workspace <- list(member_metadata = metadata, get_documentation = function(...) {
            list(description = "Advance the object.", arguments = list(delta = "Step size.", flag = "A flag."))
        })
        row <- length(lines) - 1L
        opening <- regexpr("(", tail(lines, 1L), fixed = TRUE)[[1L]]
        list(signature = signature_reply(1L, document$uri, workspace, document,
                list(row = row, col = opening))$result,
            hover = hover_reply(1L, document$uri, workspace, document, list(row = row, col = 12L))$result)
    }
    calls <- lines
    calls[[4L]] <- "converted$step()"
    replies <- providers(calls, metadata)
    expect_identical(replies$signature$signatures[[1L]]$label, "step(delta = 1, flag = TRUE)")
    expect_match(replies$signature$signatures[[1L]]$documentation$value, "Receiver type is uncertain", fixed = TRUE)
    expect_match(paste(replies$hover$contents, collapse = "\n"), "step(delta = 1, flag = TRUE)", fixed = TRUE)
    expect_match(paste(replies$hover$contents, collapse = "\n"), "Receiver type is uncertain", fixed = TRUE)
    overridden <- append(lines, "convert.custom <- function(x) list(other = TRUE)", after = 0L)
    expect_length(resolve(overridden, metadata)$items, 0L)
    suppressed <- providers(append(calls, "convert.custom <- function(x) list(other = TRUE)", after = 0L), metadata)
    expect_length(suppressed$signature$signatures, 0L)
    expect_null(suppressed$hover)
    ns$convert.custom <- function(x) list(other = TRUE)
    expect_length(resolve(lines, make_metadata())$items, 0L)
    suppressed <- providers(calls, make_metadata())
    expect_length(suppressed$signature$signatures, 0L)
    expect_null(suppressed$hover)
})

test_that("Conflicting possible member signatures stay unresolved", {
    index <- member_generic_index("first <- function(delta = 1) NULL\nsecond <- function(other = TRUE) NULL")
    index$package <- "ambiguousfixture"
    index$exports <- c("candidate", "first", "second")
    index$members <- list(first_type = c(step = "first"), second_type = c(step = "second"))
    index$roots$candidate <- member_value(reason = "open_dispatch", possible_returns = list(
        member_value(type = "first_type", member_package = "ambiguousfixture"),
        member_value(type = "second_type", member_package = "ambiguousfixture")))
    metadata <- MemberMetadataCache$new(1024^2)
    metadata$set("ambiguousfixture", member_index_freeze(index))
    content <- c("library(ambiguousfixture)", "object <- candidate", "object$step()")
    document <- Document$new("file:///ambiguous-member.R", language = "r", version = 1L, content = content)
    document$parse_data <- parse_document(document$uri, content)
    workspace <- list(member_metadata = metadata)
    items <- member_completion(document$uri, workspace, document, list(row = 2L, col = 11L), TRUE, 200L)
    expect_identical(vapply(items, `[[`, character(1L), "label"), "step")
    expect_identical(items[[1L]]$detail, "[possible member]")
    signature <- signature_reply(1L, document$uri, workspace, document, list(row = 2L, col = 12L))$result
    expect_length(signature$signatures, 0L)
    expect_null(hover_reply(1L, document$uri, workspace, document, list(row = 2L, col = 9L))$result)
})
