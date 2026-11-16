#
# Set up
#

# context("histogramDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("histogramDS2::smk")
test_that("histogramDS2, smallCellsRule method", {
    xvect <- c(1:100)

    res <- histogramDS2("xvect", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25)

    expect_length(res, 2)
    expect_true(inherits(res$histobject, "histogram"))
    expect_equal(res$invalidcells, 0)
})

test_that("histogramDS2 fails when x references nonexistent object", {
    expect_error(histogramDS2("nonexistent_obj", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25), "does not exist")
})

test_that("histogramDS2 fails when x is not numeric or integer", {
    xvect <- c("a", "b", "c")
    expect_error(histogramDS2("xvect", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25), "must be of type numeric or integer")
})

test_that("histogramDS2 gives the same result for numeric input as for integer", {
    x.integer <- c(1:100)
    x.numeric <- as.numeric(x.integer)

    expect_equal(histogramDS2("x.numeric", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25),
                 histogramDS2("x.integer", num.breaks=10, min=1, max=100, method.indicator=1, k=3, noise=0.25))
})

#
# Done
#

# context("histogramDS2::smk::shutdown")

# context("histogramDS2::smk::done")
