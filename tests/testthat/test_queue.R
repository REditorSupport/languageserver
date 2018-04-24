context("Queue")

q <- languageserver:::QueueL$new()
q$push(1)
q$push(2)
expect_equal(q$pop(), 1)
expect_equal(q$pop(), 2)


q <- languageserver:::OrderedDictL$new()
q$set("a", 1)
q$set("b", 2)
q$set("c", 3)
expect_equal(q$get("b"), 2)
expect_equal(q$pop("a"), 1)
expect_equal(q$size(), 2)
