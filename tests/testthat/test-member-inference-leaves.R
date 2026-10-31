test_that("Leaf inference honors shared node and time limits", {
    index <- member_generic_index("")
    bindings <- list(value = member_literal(1L))
    for (expr in list(1L, quote(value))) {
        budget <- new.env(parent = emptyenv())
        budget$remaining <- 1L
        budget$exhausted <- budget$transient <- FALSE
        expect_identical(member_infer(expr, index, bindings, budget = budget), member_literal(1L))
        expect_identical(budget$remaining, 0L)
        expect_false(budget$exhausted)
        expect_identical(member_infer(expr, index, bindings, budget = budget)$reason, "budget")
        expect_identical(budget$remaining, -1L)
        expect_true(budget$exhausted)

        budget <- new.env(parent = emptyenv())
        budget$remaining <- 100L
        budget$exhausted <- budget$transient <- FALSE
        budget$deadline <- -1
        expect_identical(member_infer(expr, index, bindings, budget = budget)$reason, "budget")
        expect_true(budget$timed_out)
    }
})

test_that("Leaf inference preserves lexical shadowing and deferred contexts", {
    index <- member_generic_index("value <- function() list(other = TRUE)")
    local <- member_literal(2L)
    expect_identical(member_infer(quote(value), index, list(value = local)), local)
    expect_identical(member_infer(quote(value), index)$function_key, "value")
    body <- quote({ answer <- list(done = TRUE); answer })
    deferred <- member_value(binding_expr = body, binding_env = list())
    # Following a binding is an ordinary nested expression even when its
    # caller is examining initializer effects.
    result <- member_infer(quote(value), index, list(value = deferred),
        context = list(r6_initialize = TRUE))
    expect_identical(result, member_infer(body, index))
    expect_identical(names(result$fields), "done")
})
