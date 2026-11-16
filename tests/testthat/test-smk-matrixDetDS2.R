
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

test_that("matrixDetDS2 gives the same result for a data.frame as for a matrix", {
    M1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3)
    M1.df <- as.data.frame(M1)

    res.matrix <- matrixDetDS2("M1", logarithm=FALSE)
    res.df <- matrixDetDS2("M1.df", logarithm=FALSE)

    expect_equal(res.df, res.matrix)
})
