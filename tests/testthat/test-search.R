context("Test Search")

test_that("enclosed_by_quotes works as expected", {
    enclosed <- function(text, col) {
          .Call("enclosed_by_quotes", PACKAGE = "languageserver", text, col)
      }
    expect_false(enclosed("hello_world", 4))
    expect_true(enclosed("'hello_world", 4))
    expect_true(enclosed("\"hello_world", 4))
    expect_true(enclosed("\"\\\"hello_world", 4))
    expect_false(enclosed("\"a\"hello_world", 4))
    expect_true(enclosed("'\\'hello_world", 4))
    expect_false(enclosed("'a'hello_world", 4))
})


test_that("find_unbalanced_paren works as expected", {
    bsearch <- function(content, row, column) {
        .Call("find_unbalanced_paren",
            PACKAGE = "languageserver",
            content, row, column, TRUE
        )
    }
    expect_equal(bsearch("foo(xy", 0, 5), c(0, 3))
    expect_equal(bsearch("foo(xy(abc)", 0, 5), c(0, 3))
    expect_equal(bsearch("foobar", 0, 3), c(-1, -1))
    expect_equal(bsearch("foo(bar)abc", 0, 7), c(-1, -1))
    expect_equal(bsearch("foo(bar)abc", 0, 8), c(-1, -1))
    expect_equal(bsearch("foo(xy(abc)", 0, 6), c(0, 6))
    expect_equal(bsearch("foo(xy(abc)", 0, 7), c(0, 6))
    expect_equal(bsearch("foo(xy(abc), param2", 0, 12), c(0, 3))
    expect_equal(bsearch("foo(#xyz(bar", 0, 10), c(0, 3))
    expect_equal(bsearch("foo('xyz(bar", 0, 10), c(0, 3))
    expect_equal(bsearch("foo(\"xyz(bar", 0, 10), c(0, 3))
    expect_equal(bsearch("foo('xyz', bar", 0, 10), c(0, 3))
    expect_equal(bsearch("foo(\"xyz\", bar", 0, 10), c(0, 3))
    expect_equal(bsearch("𐐀𐐀𐐀(𐐀𐐀𐐀", 0, 5), c(0, 3))

    # multiline
    expect_equal(bsearch(c("foo(", "xyz"), 1, 3), c(0, 3))
    expect_equal(bsearch(c("foo(  # (", "xyz"), 1, 1), c(0, 3))
    expect_equal(bsearch(c("foo(  # \"", "xyz"), 1, 2), c(0, 3))
    expect_equal(bsearch(c("foo(  # '", "xyz"), 1, 3), c(0, 3))
})
