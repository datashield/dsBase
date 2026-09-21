#
# Set up
#

# context("boxPlotGG_data_TreatmentDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("boxPlotGG_data_TreatmentDS::smk")
test_that("boxPlotGG_data_TreatmentDS, no grouping", {
    D <- data.frame(v1 = c(1:20), v2 = c(20:1))

    res <- boxPlotGG_data_TreatmentDS("D", variables = c("v1", "v2"))

    expect_true(all(c("x", "value") %in% names(res)))
})

test_that("boxPlotGG_data_TreatmentDS, single grouping", {
    D <- data.frame(v1 = c(1:20), group = factor(rep(c("a", "b"), 10)))

    res <- boxPlotGG_data_TreatmentDS("D", variables = c("v1"), group = "group")

    expect_true(all(c("x", "value", "group") %in% names(res)))
})

test_that("boxPlotGG_data_TreatmentDS fails when table.name references nonexistent object", {
    expect_error(boxPlotGG_data_TreatmentDS("nonexistent_obj", variables = c("v1")), "does not exist")
})

test_that("boxPlotGG_data_TreatmentDS fails when a variable is not numeric or integer", {
    D <- data.frame(v1 = c("a", "b", "c"))
    expect_error(boxPlotGG_data_TreatmentDS("D", variables = c("v1")), "must be of type numeric or integer")
})

test_that("boxPlotGG_data_TreatmentDS fails when group is not a factor", {
    D <- data.frame(v1 = c(1:20), group = c(1:20))
    expect_error(boxPlotGG_data_TreatmentDS("D", variables = c("v1"), group = "group"), "must be of type factor")
})

#
# Done
#

# context("boxPlotGG_data_TreatmentDS::smk::shutdown")

# context("boxPlotGG_data_TreatmentDS::smk::done")
