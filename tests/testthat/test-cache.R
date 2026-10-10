test_that("ByteLruCache is byte bounded and refreshes recency", {
    cache <- ByteLruCache$new(max_bytes = 10000, max_entries = 2L)
    cache$set("first", 1L)
    cache$set("second", 2L)
    expect_equal(cache$get("first"), 1L)

    cache$set("third", 3L)
    expect_true(cache$has("first"))
    expect_false(cache$has("second"))
    expect_true(cache$has("third"))
    expect_lte(cache$bytes(), 10000)
})

test_that("ByteLruCache does not retain an oversized value", {
    cache <- ByteLruCache$new(max_bytes = 100, max_entries = 10L)
    cache$set("large", raw(1000))
    expect_false(cache$has("large"))
    expect_equal(cache$bytes(), 0)
})

test_that("ByteLruCache exposes safe collection operations", {
    cache <- ByteLruCache$new(max_bytes = 10000, max_entries = 2L)
    expect_equal(cache$get("missing", "fallback"), "fallback")
    expect_null(cache$remove("missing"))

    cache$set("first", 1L)
    cache$set("second", 2L)
    expect_equal(cache$size(), 2L)
    expect_setequal(cache$keys(), c("first", "second"))
    expect_true(cache$bytes() > 0)

    cache$clear()
    expect_equal(cache$size(), 0L)
    expect_length(cache$keys(), 0L)
    expect_equal(cache$bytes(), 0)
})

test_that("ByteLruCache prefers protected entries while retaining its bounds", {
    cache <- ByteLruCache$new(max_bytes = 2 * as.numeric(object.size(raw(100))), max_entries = 2L)
    cache$set("open", raw(100))
    cache$set("background", raw(100), protect = "open")
    cache$set("new", raw(100), protect = "open")
    expect_setequal(cache$keys(), c("open", "new"))

    # A background result too large to coexist with open metadata is dropped.
    cache$set("large", raw(150), protect = "open")
    expect_setequal(cache$keys(), "open")
    expect_lte(cache$bytes(), 2 * as.numeric(object.size(raw(100))))

    # Protected entries still compete in LRU order when they exceed the limit.
    cache$set("second", raw(100), protect = c("open", "second"))
    cache$set("third", raw(100), protect = c("open", "second", "third"))
    expect_setequal(cache$keys(), c("second", "third"))
    expect_lte(cache$size(), 2L)
    expect_lte(cache$bytes(), 2 * as.numeric(object.size(raw(100))))
})
