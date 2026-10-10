test_that("shutdown responds with a null result", {
    self <- new.env(parent = baseenv())
    self$exit_flag <- FALSE
    self$deliveries <- list()
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }

    on_shutdown(self, id = 1L, params = NULL)

    expect_true(self$exit_flag)
    expect_length(self$deliveries, 1L)
    response <- self$deliveries[[1L]]
    expect_null(response$result)
    expect_match(response$to_json(), '"result":null', fixed = TRUE)
})
