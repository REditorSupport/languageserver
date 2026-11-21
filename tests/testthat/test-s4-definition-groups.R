test_that("standard S4 group membership follows methods from closest to broadest", {
    members <- c("+", "%/%", "==", "&", "Arith", "Ops", "sinpi", "round", "sum", "Mod", "%*%")
    namespace <- asNamespace("methods")
    expected <- lapply(members, function(member) {
        generic <- methods::getGeneric(member, where = namespace)
        parents <- methods::getGroup(generic, recursive = TRUE, where = namespace)
        as.character(unlist(parents, use.names = FALSE))
    })
    expect_identical(lapply(members, s4_definition_groups), expected)
    expect_identical(s4_definition_groups("+"), c("Arith", "Ops"))
    for (name in c("!", "&&", "||", "mean", "crossprod", "not_a_generic")) {
        expect_identical(s4_definition_groups(name), character())
    }
})

test_that("Go to Definition includes individual and group methods for S4 operations", {
    declarations <- c(
        'setMethod("Ops", "Widget", function(e1, e2) e1)',
        'setMethod("Arith", "Widget", function(e1, e2) e1)',
        'setMethod("+", "Widget", function(e1, e2) e1)',
        'setMethod("Compare", "Widget", function(e1, e2) TRUE)',
        'setMethod("Logic", "Widget", function(e1, e2) TRUE)',
        'setMethod("Math", "Widget", function(x) x)',
        'setMethod("Math2", "Widget", function(x, digits) x)',
        'setMethod("Summary", "Widget", function(x, ..., na.rm = FALSE) x)',
        'setMethod("Complex", "Widget", function(z) z)',
        'setMethod("matrixOps", "Widget", function(x, y) x)'
    )
    queries <- c("x+y", "x / y", "x == y", "x & y", "sin(x)", "signif(x, 2)",
        "max(x)", "Mod(x)", "x %*% y", "!x", "x || y")
    # Definition for an operator or a function identifier, in zero-based columns.
    columns <- c(1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 3L, 0L, 2L)
    fixture <- provider_fixture(c(declarations, queries))
    fixture$workspace$get_definition <- function(...) NULL
    rows <- lapply(seq_along(queries), function(i) {
        point <- list(row = length(declarations) + i - 1L, col = columns[[i]])
        targets <- definition_reply(1L, fixture$uri, fixture$workspace, fixture$document, point, getwd())$result
        if (!is.null(targets$uri)) targets <- list(targets)
        vapply(targets, function(target) target$range$start$line, numeric(1L))
    })
    matrix <- if (exists("matrixOps", asNamespace("methods"), inherits = FALSE)) 9 else numeric()
    expect_equal(rows, list(2:0, 1:0, c(3, 0), c(4, 0), 5, 6, 7, 8, matrix, numeric(), numeric()))
})

test_that("a standard group registration does not promote an unrelated local function", {
    lines <- c("sum <- function(...) 0",
        'setMethod("Summary", "Widget", function(x, ..., na.rm = FALSE) x)', "sum(obj)")
    fixture <- provider_fixture(lines)
    target <- definition_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        list(row = 2L, col = 1L), getwd())$result
    expect_equal(target$range$start, position(0L, 0L))
})
