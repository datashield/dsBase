
#
# Set up
#

# context("matrixDetDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixDetDS1 passes with matrix of sufficient dimensions", {
    M1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3)
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

test_that("matrixDetDS1 errors when matrix is too small (disclosure guard)", {
    M1x1 <- matrix(7, 1, 1)
    M2x2 <- matrix(c(1, 2, 3, 4), 2, 2)

    expect_error(matrixDetDS1("M1x1", logarithm=FALSE), regexp = "too small")
    expect_error(matrixDetDS1("M2x2", logarithm=FALSE), regexp = "too small")
})

test_that("matrixDetDS1 is blocked in non-permissive mode", {
    options(datashield.privacyControlLevel = "non-permissive")
    on.exit(options(datashield.privacyControlLevel = NULL), add = TRUE)
    M1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3)
    expect_error(matrixDetDS1("M1", logarithm=FALSE), regexp = "non-permissive")
})
