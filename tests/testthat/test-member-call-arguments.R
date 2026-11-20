member_call_test_items <- function(lines, metadata = NULL) {
    fixture <- provider_fixture(lines)
    workspace <- fixture$workspace
    workspace$loaded_packages <- character()
    workspace$imported_objects <- collections::dict()
    workspace$get_namespace <- function(...) NULL
    workspace$guess_namespace <- function(...) NULL
    workspace$member_metadata <- metadata
    point <- list(row = length(lines) - 1L, col = nchar(tail(lines, 1L)))
    items <- completion_reply(1L, fixture$uri, workspace, fixture$document, point, list())$result$items
    Filter(function(item) !is.null(item$data$parameter) || identical(item$data$type, "parameter"), items)
}

test_that("Member calls complete named arguments from their signature", {
    code <- "obj <- list(fun = function(x, ..., y = 1, `two words` = NULL) NULL)"
    for (call in c("obj$fun(", "obj$fun(x")) {
        items <- member_call_test_items(c(code, call))
        labels <- vapply(items, `[[`, character(1L), "label")
        expected <- if (endsWith(call, "(")) c("x", "y", "two words") else "x"
        expect_identical(labels, expected)
        expect_identical(items[[1L]]$insertText, "x = ")
        expect_identical(items[[1L]]$detail, "fun(x, ..., y = 1, `two words` = NULL)")
        if (length(items) == 3L) expect_identical(items[[3L]]$insertText, "`two words` = ")
    }
})

test_that("Package member argument completion uses the exposed function formals", {
    index <- member_generic_index("make_fun <- function(internal) function(exposed, flag = FALSE) NULL")
    index$package <- "callfixture"
    index$exports <- "obj"
    index$roots$obj <- member_value(type = "fixture_object", member_package = "callfixture")
    index$members <- list(fixture_object = c(fun = "make_fun"))
    index$native_factories <- "make_fun"
    metadata <- MemberMetadataCache$new(1024^2)
    metadata$set("callfixture", member_index_freeze(index))
    items <- member_call_test_items(c("library(callfixture)", "obj$fun("), metadata)
    expect_identical(vapply(items, `[[`, character(1L), "label"), c("exposed", "flag"))
    expect_identical(items[[1L]]$detail, "fun(exposed, flag = FALSE)")
})

test_that("Unknown and non-callable members never borrow bare function arguments", {
    for (declaration in c("obj <- opaque()", "obj <- list(fun = 1)", "obj <- list(fun = function() NULL)")) {
        items <- member_call_test_items(c("fun <- function(unrelated) NULL", declaration, "obj$fun("))
        expect_length(items, 0L)
    }
})

test_that("A complete literal subscript shares formals, hover and signature help", {
    declaration <- "box <- list(); box[['two words']] <- function(x, y = TRUE) NULL"
    for (access in list('box[["two words"]]', c("box[[", "  'two words' # selected key", "]", "]"))) {
        lines <- c(declaration, access)
        end <- length(lines)
        lines[[end]] <- paste0(lines[[end]], "(")
        args <- member_call_test_items(lines)
        expect_identical(vapply(args, `[[`, character(1L), "insertText"), c("x = ", "y = "))
        lines[[end]] <- paste0(lines[[end]], "x = 1, y")
        args <- member_call_test_items(lines)
        expect_identical(vapply(args, `[[`, character(1L), "insertText"), "y = ")
        fixture <- provider_fixture(lines)
        document <- fixture$document
        point <- list(row = end - 1L, col = nchar(lines[[end]]))
        sig <- signature_reply(1L, fixture$uri, fixture$workspace, document, point)$result
        expect_identical(sig$signatures[[1L]]$label, "`two words`(x, y = TRUE)")
        expect_identical(sig$activeParameter, 1L)
        key <- grep("two words", access, fixed = TRUE)
        point <- list(row = key, col = regexpr("two words", access[[key]], fixed = TRUE)[[1L]])
        hover <- hover_reply(1L, fixture$uri, fixture$workspace, document, point)$result
        expect_identical(hover$contents, "```r\n`two words`(x, y = TRUE)\n```")
        expect_equal(hover$range$end$character - hover$range$start$character, nchar("'two words'"))
    }
})

test_that("Bracket-call providers use subset dispatch and reject other subscripts", {
    code <- c("fun <- function(unrelated) NULL", "box <- structure(list(fun = function(storage) NULL), class = 'box')",
        "`$.box` <- function(x, name) function(dollar) NULL", "`[[.box` <- function(x, i) function(bracket) NULL")
    args <- member_call_test_items(c(code, 'box[["fun"]]('))
    expect_identical(vapply(args, `[[`, character(1L), "label"), "bracket")
    plain <- c(code[[1L]], "box <- list(fun = function(stored) NULL)")
    for (call in c("box[[fun]](", 'box[["fun", 1]](', 'box[["fun" + extra]](', 'box[["absent"]](')) {
        expect_length(member_call_test_items(c(plain, call)), 0L)
    }
    for (call in c("box[[fun]]()", 'box[["fun", 1]]()', 'box[["fun" + extra]]()')) {
        fixture <- provider_fixture(c(plain, call))
        expect_null(member_hover_location(fixture$document, list(row = 2L, col = 8L)))
    }
    masked <- c(plain, "`[[` <- function(x, i) stop('masked operator')", 'box[["fun"]](')
    expect_length(member_call_test_items(masked), 0L)
})

test_that("Default double brackets select exact stored fields independently of dollar methods", {
    index <- member_generic_index("")
    index$members <- list(box = c(virtual = "dollar_method"))
    bindings <- list(box = member_value(type = "box", fields = list(run = member_value(function_key = "stored"))),
        .__member_properties__ = list(box = list(run = member_value(function_key = "dollar_property"))))
    infer <- function(expr) member_infer(expr, index, bindings)
    expect_identical(infer(quote(box$virtual))$function_key, "dollar_method")
    expect_null(infer(quote(box[["virtual"]]))$function_key)
    expect_identical(infer(quote(box$run))$function_key, "dollar_property")
    expect_identical(infer(quote(box[["run"]]))$function_key, "stored")
})
