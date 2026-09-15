
#
# Set up
#

# context("matrixDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixDS, serverside.vector", {
    mvec <- c(1, 2, 3, 4, 5, 6)

    res <- matrixDS("mvec", from = "serverside.vector", nrows.transmit = "2", ncols.transmit = "3", byrow = FALSE, dimnames = NULL)

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 2)
    expect_equal(ncol(res), 3)
    expect_equal(res[1, 1], 1)
    expect_equal(res[2, 1], 2)
    expect_equal(res[1, 2], 3)
})

test_that("matrixDS errors when serverside object does not exist", {
    expect_error(matrixDS("nonexistent_object", from = "serverside.vector", nrows.transmit = "2", ncols.transmit = "2", byrow = FALSE, dimnames = NULL), regexp = "does not exist")
})
