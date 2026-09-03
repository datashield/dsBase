
#
# Set up
#

# context("matrixDiagDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixDiagDS, serverside.matrix.2.vector", {
    M1 <- matrix(c(1, 2, 3, 4), 2, 2)

    res <- matrixDiagDS("M1", aim = "serverside.matrix.2.vector", nrows.transmit = "-9")

    expect_equal(length(res), 2)
    expect_equal(res[1], 1)
    expect_equal(res[2], 4)
})

test_that("matrixDiagDS errors when serverside object does not exist", {
    expect_error(matrixDiagDS("nonexistent_object", aim = "serverside.matrix.2.vector", nrows.transmit = "-9"), regexp = "does not exist")
})
