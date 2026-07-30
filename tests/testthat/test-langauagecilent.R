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

test_that("LanguageClient reports dead servers and reads stderr", {
    client <- LanguageClient$new()
    dead_process <- new.env(parent = baseenv())
    dead_process$is_alive <- function() FALSE
    client$process <- dead_process
    expect_error(client$check_connection(), "Server is dead")

    live_process <- new.env(parent = baseenv())
    live_process$is_alive <- function() TRUE
    live_process$read_error_lines <- function() c("first", "second")
    client$process <- live_process
    expect_equal(client$read_error(), "first\nsecond")
    client$process <- NULL
})
