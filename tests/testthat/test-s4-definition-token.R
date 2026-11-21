test_that("S4 definition literals follow the generic argument of the actual helper", {
    lines <- c(
        'methods::setMethod(signature = "Widget", definition = function(x) x, f = r"(draw)")',
        'getMethod("draw", "Widget")',
        'selectMethod(sig = "Widget", "dr\\u0061w")',
        'setReplaceMethod("size", "Widget", function(x, value) x)',
        'setGeneric(def = function(x) base::standardGeneric("draw"), name = "draw")',
        'removeGeneric(f = "draw")',
        'getGeneric(package = "other", f = "draw")',
        'setMethod("draw", "Widget", f = "other", definition = function(x) x)',
        'other::getMethod("draw")',
        'object$getMethod("draw")',
        'setMethod(paste0("draw", suffix), "Widget", function(x) "body")',
        'source("draw")',
        'f <- function(getMethod) getMethod("draw")',
        'f <- function() { getMethod <- identity; getMethod("draw") }',
        'getMethod("f" = "draw")',
        'methods::setGroupGeneric("draw")',
        'base::standardGeneric(f = "draw")',
        'methods::standardGeneric("draw")',
        'x %>% getMethod("Widget")',
        '"bare"'
    )
    expected <- list(
        c(NA, "draw"), c("draw", NA), c(NA, "draw"), c("size<-", NA), c("draw", "draw"),
        "draw", c(NA, NA), c(NA, NA, NA), NA, NA, c(NA, NA, NA), NA, NA, NA,
        c(NA, "draw"), "draw", "draw", NA, NA, NA
    )
    parsed <- provider_fixture(lines)$document$parse_data
    syntax <- parsed$syntax_data
    literals <- which(syntax$token == "STR_CONST")
    actual <- vapply(literals, function(i) {
        name <- s4_definition_literal(parsed, i)
        if (is.null(name)) NA_character_ else name
    }, character(1L))
    expect_identical(actual, unlist(expected, use.names = FALSE))
})

test_that("S4 helper literals respect a redefined helper", {
    parsed <- provider_fixture(c('getGeneric("draw")', "getGeneric <- identity", 'getGeneric("draw")',
            'methods::getGeneric("draw")'))$document$parse_data
    tokens <- which(parsed$syntax_data$token == "STR_CONST")
    expect_identical(s4_definition_literal(parsed, tokens[[1L]]), "draw")
    expect_null(s4_definition_literal(parsed, tokens[[2L]]))
    expect_identical(s4_definition_literal(parsed, tokens[[3L]]), "draw")
})

test_that("S4 replacement names apply to the target call or subset operator", {
    lines <- c("size(widget) <- size(other)", "value -> size(widget)",
        "size(widget) = size(other)", "size(widget) <<- value", "outer(size(widget)) <- value",
        "size(widget)[1] <- value", "value -> widget[[1]]", "widget[[1]]", "widget[1]")
    parsed <- provider_fixture(lines)$document$parse_data
    syntax <- parsed$syntax_data
    tokens <- which(syntax$token %in% c("SYMBOL_FUNCTION_CALL", "'['", "LBB"))
    actual <- vapply(tokens, function(i) s4_definition_replacement(syntax, i), logical(1L))
    expect_identical(actual, c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("S4 calls use the nearest visible ordinary binding", {
    fixture <- provider_fixture(c(
        "other <- function() { draw <- identity; `size<-` <- identity }",
        "draw(model)",
        "work <- function(draw) {",
        "  draw(model)",
        "  draw <- function(x) draw(x)",
        "  inner <- function() draw(model)",
        "  draw(model)",
        "}",
        "size(model) <- 1",
        "assign_size <- function(`size<-`) { size(model) <- 1 }",
        "`[<-` <- function(x, i, value) x",
        "model[1] <- 1",
        "check <- function() { draw(model); draw <- identity }",
        "for (draw in functions) draw(model)"
    ))
    parsed <- fixture$document$parse_data
    syntax <- parsed$syntax_data
    binding <- function(line, kind, name) {
        tokens <- which(syntax$line1 == line & syntax$token == kind)
        token <- utils::tail(tokens, 1L)
        s4_definition_binding(parsed, fixture$document, token, name)
    }
    expect_null(binding(2L, "SYMBOL_FUNCTION_CALL", "draw"))
    expect_equal(binding(4L, "SYMBOL_FUNCTION_CALL", "draw"), range(position(2, 17), position(2, 21)))
    for (line in 5:7) expect_equal(binding(line, "SYMBOL_FUNCTION_CALL", "draw"), range(position(4, 2), position(4, 29)))
    expect_null(binding(9L, "SYMBOL_FUNCTION_CALL", "size<-"))
    expect_equal(binding(10L, "SYMBOL_FUNCTION_CALL", "size<-"), range(position(9, 24), position(9, 32)))
    expect_equal(binding(12L, "'['", "[<-"), range(position(10, 0), position(10, 32)))
    expect_null(binding(13L, "SYMBOL_FUNCTION_CALL", "draw"))
    expect_equal(binding(14L, "SYMBOL_FUNCTION_CALL", "draw"), range(position(13, 5), position(13, 9)))
})
