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

test_that("S4 replacement names follow the target's receiver through compound calls", {
    cases <- list(
        "size(widget) <- size(other)" = c(TRUE, FALSE),
        "value -> size(widget)" = TRUE,
        "size(widget) = size(other)" = c(TRUE, FALSE),
        "size(widget) <<- value" = TRUE,
        "outer(size(widget)) <- value" = c(TRUE, TRUE),
        "size(widget)[1] <- value" = c(TRUE, TRUE),
        "value -> widget[[1]]" = TRUE,
        "widget[[1]]" = FALSE,
        "widget[1]" = FALSE,
        "outer(, size(widget)) <- value" = c(TRUE, FALSE),
        "receiver$fun(size(widget)) <- value" = c(FALSE, FALSE),
        "size(widget) %>% outer(other) -> value" = c(FALSE, FALSE),
        "base::attr(size(widget), which) <- value" = c(TRUE, TRUE)
    )
    syntax <- syntax_parse_data(utils::getParseData(parse(text = names(cases), keep.source = TRUE)))
    for (line in seq_along(cases)) {
        tokens <- which(syntax$line1 == line & syntax$token %in% c("SYMBOL_FUNCTION_CALL", "'['", "LBB"))
        actual <- vapply(tokens, function(i) s4_definition_replacement(syntax, i), logical(1L))
        expect_identical(actual, cases[[line]], info = names(cases)[[line]])
    }
})

test_that("definition selects setters along the object path and getters in indices and values", {
    declarations <- c(
        'setMethod("value", "Widget", function(x) x)',
        'setReplaceMethod("value", "Widget", function(x, value) x)',
        'setMethod("[", "Widget", function(x, i, j, ..., drop) x)',
        'setReplaceMethod("[", "Widget", function(x, i, j, ..., value) x)',
        'setMethod("[[", "Widget", function(x, i, j, ...) x)',
        'setReplaceMethod("[[", "Widget", function(x, i, j, ..., value) x)',
        'setMethod("outer", "Widget", function(object, index) object)',
        'setReplaceMethod("outer", "Widget", function(object, index, value) object)'
    )
    # Expected zero-based definition lines for successive calls and brackets.
    calls <- list(
        "value(x)[value(i)] <- value(rhs)" = c(1, 3, 0, 0),
        "value(rhs) -> outer(object = value(x)[[value(i)]], index = value(j))$slot" = c(0, 7, 1, 5, 0, 0),
        "outer(value(x)@slot, value(i))[[value(j)]] = value(rhs)" = c(7, 1, 0, 5, 0, 0),
        "value(x)[[i]][value(j)] <<- value(rhs)" = c(1, 5, 3, 0, 0),
        "sink <- outer(value(x)[value(i)], value(j))" = c(6, 0, 2, 0, 0),
        "outer(index = value(x), object = value(i)) <- value(rhs)" = c(7, 1, 0, 0)
    )
    fixture <- provider_fixture(c(declarations, names(calls)))
    fixture$workspace$get_definition <- function(...) NULL
    syntax <- fixture$document$parse_data$syntax_data
    for (i in seq_along(calls)) {
        line <- length(declarations) + i
        tokens <- which(syntax$line1 == line & syntax$token %in% c("SYMBOL_FUNCTION_CALL", "'['", "LBB"))
        targets <- vapply(tokens, function(token) {
            column <- syntax$col1[[token]] - 1L
            point <- list(row = line - 1L, col = column)
            reply <- definition_reply(1L, fixture$uri, fixture$workspace, fixture$document, point)
            as.integer(reply$result$range$start$line)
        }, integer(1L))
        expect_equal(targets, calls[[i]], info = names(calls)[[i]])
    }
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
        row <- s4_definition_binding_row(parsed, token, name)
        if (!is.null(row)) navigation_range(fixture$document, syntax, row)
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
