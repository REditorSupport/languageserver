test_that("task timings separate queue delay from dispatched work", {
    task <- create_task(function() NULL, list())
    origin <- task$started_at
    expect_equal(task$timings(now = origin + 3), c(total = 3, queued = 3, running = 0))
    # Starting a task records dispatch separately from creation, even if a
    # session completes synchronously. Use an inert session rather than sleep.
    session <- list(call = function(...) NULL)
    task$start(session)
    dispatched <- task$execution_started_at
    timing <- task$timings(now = dispatched + 5)
    expect_equal(timing[["queued"]], dispatched - origin)
    expect_equal(timing[["running"]], 5)
    expect_equal(timing[["total"]], sum(timing[c("queued", "running")]))
    expect_equal(task$timings(now = origin - 1), c(total = 0, queued = 0, running = 0))
})
