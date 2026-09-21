#
# Set up
#

# context("lsplineDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("lsplineDS::smk")
test_that("simple lsplineDS", {
    input <- seq(1, 30, by = 1)

    res <- lsplineDS("input", knots = c(10, 20))

    expect_equal(class(res), c("lspline", "matrix"))
    expect_equal(dim(res), c(30, 3))
    expect_equal(unname(res[1, ]), c(1, 0, 0))
    expect_equal(unname(res[30, ]), c(10, 10, 10))
})

test_that("lsplineDS throws error when object does not exist", {
    expect_error(
        lsplineDS("nonexistent_object", knots = c(10, 20)),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("lsplineDS::smk::shutdown")

# context("lsplineDS::smk::done")
