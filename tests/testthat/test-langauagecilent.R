test_that("read_line and read_char works", {
    cilent <- LanguageClient$new()
    stub(cilent$read_char, "self$read_output", mock("hello", "abc", "def", "pqr\nxyz\n"))
    stub(cilent$read_line, "self$read_output_lines", mock("a line", "another line", "third line"))
    expect_equal(cilent$read_char(8), "hello")
    expect_equal(cilent$read_char(2), "ab")
    expect_equal(cilent$read_char(1), "c")
    expect_equal(cilent$read_line(), "a line")
    expect_equal(cilent$read_char(1), "d")
    expect_equal(cilent$read_line(), "efanother line")
    expect_equal(cilent$read_char(2), "pq")
    expect_equal(cilent$read_line(), "r")
    expect_equal(cilent$read_line(), "xyz")
    expect_equal(cilent$read_line(), "third line")
})
