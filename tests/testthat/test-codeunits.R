context("Test Multi Bytes Characters")

test_that("Code units are correctly calculated", {
    skip_on_os("solaris")
    expect_equal(ncodeunit("a"), 1)
    expect_equal(ncodeunit("𐐀"), 2)
    expect_equal(ncodeunit("佢"), 1)
    expect_equal(ncodeunit("𠍲"), 2)
    expect_equal(ncodeunit(c("佢", "𠍲")), c(1, 2))
    expect_equal(code_point_from_unit("a𐐀c", c(0:5, Inf)), c(0, 1, NA, 2, 3, NA, 3))
    expect_equal(code_point_to_unit("a𐐀c", c(0:4, Inf)), c(0, 1, 3, 4, NA, 4))
})
