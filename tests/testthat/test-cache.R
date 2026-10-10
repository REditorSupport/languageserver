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

test_that("Member metadata restores decoded indexes from bounded inert snapshots", {
    index <- member_generic_index("run <- function(value = 1) list(done = value)")
    index$exports <- "run"
    index$padding <- raw(10000L)
    snapshot <- member_index_freeze(index)
    budget <- as.numeric(object.size(snapshot)) + 1024
    cache <- MemberMetadataCache$new(budget, max_entries = 2L)
    first <- as.list(member_index_thaw(snapshot))
    cache$set("first", first)
    cached <- cache$get("first")
    cached$cache$sentinel <- "decoded only"
    expect_identical(cache$get("first")$cache$sentinel, "decoded only")
    cache$set("second", as.list(member_index_thaw(snapshot)), protect = c("first", "second"))
    expect_setequal(cache$keys(), c("first", "second"))
    restored <- cache$get("first")
    expect_identical(restored$cache$sentinel, "decoded only")
    expect_identical(restored$definitions, first$definitions)
    expect_identical(member_infer(quote(run()$done), list2env(restored))$literal, 1)
    expect_lte(cache$bytes(), 3 * budget)

    # Reading selection metadata must not bring back an evicted decoded index.
    decoded <- cache$.__enclos_env__$private$indexes$keys()
    expect_false("second" %in% decoded)
    catalog <- cache$catalog("second")
    expect_identical(catalog$functions, "run")
    expect_null(catalog$definitions)
    expect_identical(cache$.__enclos_env__$private$indexes$keys(), decoded)
    expect_null(cache$catalog("missing"))

    cache$set("third", as.list(member_index_thaw(snapshot)))
    expect_false(cache$has("first"))
    expect_identical(cache$get("missing", "fallback"), "fallback")
    expect_null(cache$remove("missing"))
    cache$remove("third")
    expect_equal(cache$size(), 1L)
    cache$clear()
    expect_length(cache$keys(), 0L)
    expect_equal(cache$bytes(), 0)
})

test_that("Member metadata bounds summaries across decoded hits and replacements", {
    snapshot <- member_index_freeze(member_generic_index("run <- function(value = 1) list(done = value)"))
    cache <- MemberMetadataCache$new(10000)
    for (key in c("first", "second")) cache$set(key, snapshot)
    first <- cache$get("first")
    second <- cache$get("second")
    member_infer(quote(run()), list2env(first))
    member_infer(quote(run()), list2env(second))
    # Simulate nearly full summary environments without allocating megabytes.
    first$cache$.bytes <- 6000
    second$cache$.bytes <- 3000
    cache$get("second")
    member_infer(quote(run(2)), list2env(second))
    expect_equal(first$cache$.bytes, 0)
    expect_true(is.function(first$cache$.reserve))
    expect_lte(first$cache$.bytes + second$cache$.bytes, 10000)
    expect_gt(second$cache$.bytes, 3000)

    # Existing inference summaries must not survive changed definitions.
    first <- cache$get("first")
    expect_identical(member_infer(quote(run()$done), list2env(first))$literal, 1)
    first$return_definitions <- digest::digest(first$definitions, algo = "xxhash64")
    first$method_results <- list(run = member_literal(1))
    first$definitions$run[[3L]] <- quote(list(done = 2))
    cache$set("first", first)
    replaced <- cache$get("first")
    expect_length(replaced$method_results, 0L)
    expect_null(replaced$cache$.bytes)
    expect_identical(member_infer(quote(run()$done), list2env(replaced))$literal, 2)
    cache$set("first", list(schema = 2L))
    expect_identical(cache$get("first")$definitions, replaced$definitions)
})
