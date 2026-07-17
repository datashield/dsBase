
#
# Set up
#

# context("matrixDetDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixDetDS1", {
    M1 <- matrix(c(1, 2, 3, 4), 2, 2)

    res <- matrixDetDS1("M1", logarithm=FALSE)

    expect_true(is.list(res))
    expect_equal(res$matrix.determinant, determinant(M1, logarithm=FALSE))
})

test_that("matrixDetDS1 errors when serverside object does not exist", {
    expect_error(matrixDetDS1("nonexistent_object", logarithm=FALSE), regexp = "does not exist")
})

test_that("matrixDetDS1 errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    expect_error(matrixDetDS1("bad_input", logarithm=FALSE), regexp = "must be of type")
})
