
#
# Set up
#

# context("matrixInvertDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixInvertDS", {
    M1 <- matrix(c(1, 2, 3, 4), 2, 2)

    res <- matrixInvertDS("M1")

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 2)
    expect_equal(ncol(res), 2)
    expect_equal(res, solve(M1))
})

test_that("matrixInvertDS errors when serverside object does not exist", {
    expect_error(matrixInvertDS("nonexistent_object"), regexp = "does not exist")
})

test_that("matrixInvertDS errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    expect_error(matrixInvertDS("bad_input"), regexp = "must be of type")
})
