#
# Set up
#

# context("elsplineDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("elsplineDS::smk")
test_that("simple elsplineDS", {
    input <- seq(1, 30, by = 1)

    res <- elsplineDS("input", 3)

    expect_equal(class(res), c("lspline", "matrix"))
    expect_equal(dim(res), c(30, 3))
    expect_equal(unname(res[1, ]), c(1, 0, 0))
    expect_equal(unname(res[30, ]), c(10.66667, 9.666667, 9.666667), tolerance = 1e-6)
})

test_that("elsplineDS throws error when object does not exist", {
    expect_error(
        elsplineDS("nonexistent_object", 3),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("elsplineDS::smk::shutdown")

# context("elsplineDS::smk::done")
