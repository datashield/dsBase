#
# Set up
#

# context("boxPlotGG_data_Treatment_numericDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("boxPlotGG_data_Treatment_numericDS::smk")
test_that("boxPlotGG_data_Treatment_numericDS", {
    v1 <- c(1:20)

    res <- boxPlotGG_data_Treatment_numericDS("v1")

    expect_equal(unique(res$x), "v1")
    expect_equal(res$value, v1)
})

test_that("boxPlotGG_data_Treatment_numericDS fails when vector.name references nonexistent object", {
    expect_error(boxPlotGG_data_Treatment_numericDS("nonexistent_obj"), "does not exist")
})

test_that("boxPlotGG_data_Treatment_numericDS fails when vector is not numeric or integer", {
    v1 <- c("a", "b", "c")
    expect_error(boxPlotGG_data_Treatment_numericDS("v1"), "must be of type numeric or integer")
})

#
# Done
#

# context("boxPlotGG_data_Treatment_numericDS::smk::shutdown")

# context("boxPlotGG_data_Treatment_numericDS::smk::done")
