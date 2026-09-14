#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

# context("boxPlotGGDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("boxPlotGGDS::smk")
test_that("boxPlotGGDS, no grouping", {
    boxPlotRawData <- data.frame(x = rep("v1", 20), value = c(1:20))

    res <- boxPlotGGDS("boxPlotRawData")

    expect_length(res, 2)
    expect_equal(res[[2]], "no_group")
})

test_that("boxPlotGGDS, single grouping", {
    boxPlotRawData <- data.frame(x = rep("v1", 20), value = c(1:20), group = factor(rep(c("a", "b"), 10)))

    res <- boxPlotGGDS("boxPlotRawData", group = "group")

    expect_length(res, 2)
    expect_equal(res[[2]], "single_group")
})

test_that("boxPlotGGDS fails when data_table.name references nonexistent object", {
    expect_error(boxPlotGGDS("nonexistent_obj"), "does not exist")
})

#
# Done
#

# context("boxPlotGGDS::smk::shutdown")

# context("boxPlotGGDS::smk::done")
