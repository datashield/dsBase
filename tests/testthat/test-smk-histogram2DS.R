#
# Set up
#

# context("histogram2DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("histogram2DS::smk")
test_that("histogram2DS, smallCellsRule method", {
    xvect <- c(1:100)

    res <- histogram2DS("xvect", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25)

    expect_length(res, 2)
    expect_true(inherits(res$histobject, "histogram"))
    expect_equal(res$invalidcells, 0)
})

test_that("histogram2DS fails when x references nonexistent object", {
    expect_error(histogram2DS("nonexistent_obj", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25), "does not exist")
})

test_that("histogram2DS fails when x is not numeric or integer", {
    xvect <- c("a", "b", "c")
    expect_error(histogram2DS("xvect", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25), "must be of type numeric or integer")
})

test_that("histogram2DS gives the same result for numeric input as for integer", {
    x.integer <- c(1:100)
    x.numeric <- as.numeric(x.integer)

    expect_equal(histogram2DS("x.numeric", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25),
                 histogram2DS("x.integer", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25))
})

#
# Done
#

# context("histogram2DS::smk::shutdown")

# context("histogram2DS::smk::done")
