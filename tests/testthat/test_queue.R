context("Queue")

q <- languageserver:::Queue$new()
q$put(1)
q$put(2)
expect_equal(q$get(), 1)
expect_equal(q$get(), 2)


q <- languageserver:::NamedQueue$new()
q$put("a", 1)
q$put("b", 2)
q$put("c", 3)
expect_equal(q$get("b"), list(id = "b", item = 2))
expect_equal(q$get(), list(id = "a", item = 1))
