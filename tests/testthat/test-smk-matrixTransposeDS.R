
#
# Set up
#

# context("matrixTransposeDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixTransposeDS", {
    M1 <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)

    res <- matrixTransposeDS("M1")

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 3)
    expect_equal(ncol(res), 2)
    expect_equal(res, t(M1))
})

test_that("matrixTransposeDS errors when serverside object does not exist", {
    expect_error(matrixTransposeDS("nonexistent_object"), regexp = "does not exist")
})

test_that("matrixTransposeDS errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    expect_error(matrixTransposeDS("bad_input"), regexp = "must be of type")
})

test_that("matrixTransposeDS gives the same result for a data.frame as for a matrix", {
    M1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3)
    M1.df <- as.data.frame(M1)

    res.matrix <- matrixTransposeDS("M1")
    res.df <- matrixTransposeDS("M1.df")

    expect_equal(unname(res.df), unname(res.matrix))
})
