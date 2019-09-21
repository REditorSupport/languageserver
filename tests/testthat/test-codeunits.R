context("Test Multi Bytes Characters")

test_that("Code units are correctly calculated", {
    expect_equal(ncodeunit("a"), 1)
    expect_equal(ncodeunit("𐐀"), 2)
    expect_equal(ncodeunit("佢"), 1)
    expect_equal(ncodeunit("𠍲"), 2)
    expect_equal(ncodeunit(c("佢", "𠍲")), c(1, 2))
})
