context("Test Search")

test_that("enclosed_by_quotes works as expected", {
    enclosed <- function(text, col)
        .Call("enclosed_by_quotes", PACKAGE = "languageserver", text, col)
    expect_true(!enclosed("hello_world", 4))
    expect_true(enclosed("'hello_world", 4))
    expect_true(enclosed("\"hello_world", 4))
    expect_true(enclosed("\"\\\"hello_world", 4))
    expect_true(!enclosed("\"a\"hello_world", 4))
    expect_true(enclosed("'\\'hello_world", 4))
    expect_true(!enclosed("'a'hello_world", 4))
})


test_that("backward_search works as expected", {
    bsearch <- function(content, row, column) {
        .Call("backward_search",
            PACKAGE = "languageserver",
            content, row, column, "(", TRUE
        )
    }
    expect_equal(bsearch("foo(xy", 0, 5) , c(0, 3))
    expect_equal(bsearch("foo(xy'𐐀'", 0, 6) , c(-1, -1))
    expect_equal(bsearch("foo(xy'𐐀'", 0, 7) , c(-1, -1))
    expect_equal(bsearch("foo(xy'𐐀'", 0, 8) , c(0, 3))
    expect_equal(bsearch("foo(xy'\\'𐐀'", 0, 10) , c(0, 3))
    expect_equal(bsearch("𐐀𐐀𐐀(𐐀𐐀𐐀", 0, 5) , c(0, 3))

    # multiline
    expect_equal(bsearch(c("foo(", "xyz"), 1, 3) , c(0, 3))
    expect_equal(bsearch(c("foo(  # (", "xyz"), 1, 1) , c(0, 3))
    expect_equal(bsearch(c("foo(  # \"", "xyz"), 1, 2) , c(0, 3))
    expect_equal(bsearch(c("foo(  # '", "xyz"), 1, 3) , c(0, 3))
})
