#
# Set up
#

# context("scatterPlotDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("scatterPlotDS::smk")
test_that("scatterPlotDS, deterministic method", {
    xvect <- c(1:20)
    yvect <- c(20:1)

    res <- scatterPlotDS("xvect", "yvect", method.indicator=1, k=3, noise=0.25)

    expect_length(res, 4)
    expect_length(res[[1]], 20)
    expect_length(res[[2]], 20)
    expect_equal(res$class.x, "integer")
    expect_equal(res$class.y, "integer")
})

test_that("scatterPlotDS fails when x references nonexistent object", {
    yvect <- c(20:1)
    expect_error(scatterPlotDS("nonexistent_obj", "yvect", method.indicator=1, k=3, noise=0.25), "does not exist")
})

test_that("scatterPlotDS fails when y references nonexistent object", {
    xvect <- c(1:20)
    expect_error(scatterPlotDS("xvect", "nonexistent_obj", method.indicator=1, k=3, noise=0.25), "does not exist")
})

test_that("scatterPlotDS fails when x is not numeric or integer", {
    xvect <- c("a", "b", "c")
    yvect <- c(1, 2, 3)
    expect_error(scatterPlotDS("xvect", "yvect", method.indicator=1, k=3, noise=0.25), "must be of type numeric or integer")
})

test_that("scatterPlotDS fails when y is not numeric or integer", {
    xvect <- c(1, 2, 3)
    yvect <- c("a", "b", "c")
    expect_error(scatterPlotDS("xvect", "yvect", method.indicator=1, k=3, noise=0.25), "must be of type numeric or integer")
})

#
# Done
#

# context("scatterPlotDS::smk::shutdown")

# context("scatterPlotDS::smk::done")
