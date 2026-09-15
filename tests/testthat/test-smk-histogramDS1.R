#
# Set up
#

# context("histogramDS1::smk::setup")

set.standard.disclosure.settings()
set.random.seed.setting(1234)

#
# Tests
#

# context("histogramDS1::smk")
test_that("histogramDS1, smallCellsRule method", {
    xvect <- c(1:100)

    res <- histogramDS1("xvect", method.indicator=1, k=3, noise=0.25)

    expect_length(res, 2)
    expect_equal(res$class, "integer")
    expect_length(res$range, 2)
})

test_that("histogramDS1 fails when x references nonexistent object", {
    expect_error(histogramDS1("nonexistent_obj", method.indicator=1, k=3, noise=0.25), "does not exist")
})

test_that("histogramDS1 fails when x is not numeric or integer", {
    xvect <- c("a", "b", "c")
    expect_error(histogramDS1("xvect", method.indicator=1, k=3, noise=0.25), "must be of type numeric or integer")
})

#
# Done
#

# context("histogramDS1::smk::shutdown")

# context("histogramDS1::smk::done")
