
#
# Set up
#

# context("matrixDetDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixDetDS2", {
    M1 <- matrix(c(1, 2, 3, 4), 2, 2)

    res <- matrixDetDS2("M1", logarithm=FALSE)

    expect_equal(res, determinant(M1, logarithm=FALSE))
})

test_that("matrixDetDS2 errors when serverside object does not exist", {
    expect_error(matrixDetDS2("nonexistent_object", logarithm=FALSE), regexp = "does not exist")
})

test_that("matrixDetDS2 errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    expect_error(matrixDetDS2("bad_input", logarithm=FALSE), regexp = "must be of type")
})
