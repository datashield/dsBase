#
# Set up
#

# context("heatmapPlotDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("heatmapPlotDS::smk")
test_that("heatmapPlotDS, deterministic method", {
    xvect <- c(1:20)
    yvect <- c(20:1)

    res <- heatmapPlotDS("xvect", "yvect", k=3, noise=0.25, method.indicator=1)

    expect_length(res, 4)
    expect_length(res[[1]], 20)
    expect_length(res[[2]], 20)
    expect_equal(res$class.x, "integer")
    expect_equal(res$class.y, "integer")
})

test_that("heatmapPlotDS fails when x references nonexistent object", {
    yvect <- c(20:1)
    expect_error(heatmapPlotDS("nonexistent_obj", "yvect", k=3, noise=0.25, method.indicator=1), "does not exist")
})

test_that("heatmapPlotDS fails when y references nonexistent object", {
    xvect <- c(1:20)
    expect_error(heatmapPlotDS("xvect", "nonexistent_obj", k=3, noise=0.25, method.indicator=1), "does not exist")
})

test_that("heatmapPlotDS fails when x is not numeric or integer", {
    xvect <- c("a", "b", "c")
    yvect <- c(1, 2, 3)
    expect_error(heatmapPlotDS("xvect", "yvect", k=3, noise=0.25, method.indicator=1), "must be of type numeric or integer")
})

test_that("heatmapPlotDS fails when y is not numeric or integer", {
    xvect <- c(1, 2, 3)
    yvect <- c("a", "b", "c")
    expect_error(heatmapPlotDS("xvect", "yvect", k=3, noise=0.25, method.indicator=1), "must be of type numeric or integer")
})

#
# Done
#

# context("heatmapPlotDS::smk::shutdown")

# context("heatmapPlotDS::smk::done")
