#
# Set up
#

# context("qlsplineDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("qlsplineDS::smk")
test_that("simple qlsplineDS", {
    input <- seq(1, 30, by = 1)

    res <- qlsplineDS("input", q = 3, na.rm = TRUE, marginal = FALSE)

    expect_equal(class(res), c("lspline", "matrix"))
    expect_equal(dim(res), c(30, 3))
    expect_equal(unname(res[1, ]), c(1, 0, 0))
})

test_that("qlsplineDS throws error when object does not exist", {
    expect_error(
        qlsplineDS("nonexistent_object", q = 3, na.rm = TRUE, marginal = FALSE),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("qlsplineDS::smk::shutdown")

# context("qlsplineDS::smk::done")
