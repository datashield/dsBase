
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

test_that("matrixInvertDS gives the same result for a data.frame as for a matrix", {
    M1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3)
    M1.df <- as.data.frame(M1)

    res.matrix <- matrixInvertDS("M1")
    res.df <- matrixInvertDS("M1.df")

    expect_equal(unname(res.df), unname(res.matrix))
})
