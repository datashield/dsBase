
#
# Set up
#

# context("matrixDet1DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixDet1DS passes with matrix of sufficient dimensions", {
    M1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3)
    res <- matrixDet1DS("M1", logarithm=FALSE)
    
    expect_true(is.list(res))
    expect_equal(res$matrix.determinant, determinant(M1, logarithm=FALSE))
})

test_that("matrixDet1DS errors when serverside object does not exist", {
    expect_error(matrixDet1DS("nonexistent_object", logarithm=FALSE), regexp = "does not exist")
})

test_that("matrixDet1DS errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    expect_error(matrixDet1DS("bad_input", logarithm=FALSE), regexp = "must be of type")
})

test_that("matrixDet1DS errors when matrix is too small (disclosure guard)", {
    M1x1 <- matrix(7, 1, 1)
    M2x2 <- matrix(c(1, 2, 3, 4), 2, 2)

    expect_error(matrixDet1DS("M1x1", logarithm=FALSE), regexp = "too small")
    expect_error(matrixDet1DS("M2x2", logarithm=FALSE), regexp = "too small")
})

test_that("matrixDet1DS is blocked in non-permissive mode", {
    options(datashield.privacyControlLevel = "non-permissive")
    on.exit(options(datashield.privacyControlLevel = NULL), add = TRUE)
    M1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3)
    expect_error(matrixDet1DS("M1", logarithm=FALSE), regexp = "non-permissive")
})

test_that("matrixDet1DS gives the same result for a data.frame as for a matrix", {
    M1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3)
    M1.df <- as.data.frame(M1)

    res.matrix <- matrixDet1DS("M1", logarithm=FALSE)
    res.df <- matrixDet1DS("M1.df", logarithm=FALSE)

    expect_equal(res.df, res.matrix)
})
