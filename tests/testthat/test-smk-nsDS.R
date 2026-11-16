#
# Set up
#

# context("nsDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("nsDS::smk")
test_that("simple nsDS", {
    input <- seq(1, 30, by = 1)

    res <- nsDS("input", df = 3, knots = NULL, intercept = FALSE, Boundary.knots = NULL)

    expect_equal(class(res), c("ns", "basis", "matrix"))
    expect_equal(dim(res), c(30, 3))
    expect_equal(unname(res[1, ]), c(0, 0, 0))
})

test_that("nsDS throws error when object does not exist", {
    expect_error(
        nsDS("nonexistent_object", df = 3, knots = NULL, intercept = FALSE, Boundary.knots = NULL),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("nsDS::smk::shutdown")

# context("nsDS::smk::done")
