
#
# Set up
#

# context("matrixMultDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixMultDS", {
    M1 <- matrix(c(1, 2, 3, 4), 2, 2)
    M2 <- matrix(c(5, 6, 7, 8), 2, 2)

    res <- matrixMultDS("M1", "M2")

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 2)
    expect_equal(ncol(res), 2)
    expect_equal(res, M1 %*% M2)
})

test_that("matrixMultDS errors when serverside object does not exist", {
    expect_error(matrixMultDS("nonexistent_object", "also_nonexistent"), regexp = "does not exist")
})

test_that("matrixMultDS errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    M2 <- matrix(c(1, 2, 3, 4), 2, 2)
    expect_error(matrixMultDS("bad_input", "M2"), regexp = "must be of type")
})
