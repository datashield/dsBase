#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("subsetByClassDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("subsetByClassDS::smk")
test_that("simple subsetByClassDS, data.frame, unspecified variables", {
    data      <- data.frame(v1 = factor(c(0, 0, 0, 1, 1, 1, 2, 1, 2, 2)), v2 = c(4.0, 0.0, 3.0, 1.0, 2.0, 2.0, 1.0, 3.0, 0.0, 4.0), v3 = c(1:10), v4 = c(1:10))
    variables <- NULL

    res <- subsetByClassDS("data", variables)

    expect_equal(class(res), "list")
    expect_length(res, 3)

    expect_equal(class(res$v1.level_0), "data.frame")
    expect_length(res$v1.level_0, 4)

    expect_equal(class(res$v1.level_1), "data.frame")
    expect_length(res$v1.level_1, 4)

    expect_equal(class(res$v1.level_2), "data.frame")
    expect_length(res$v1.level_2, 4)
})

test_that("simple subsetByClassDS, data.frame, specified variables", {
    data      <- data.frame(v1 = factor(c(0, 0, 0, 1, 1, 1, 2, 1, 2, 2)), v2 = c(4.0, 0.0, 3.0, 1.0, 2.0, 2.0, 1.0, 3.0, 0.0, 4.0), v3 = c(1:10), v4 = c(1:10))
    variables <- c("v1")

    res <- subsetByClassDS("data", variables)

    expect_equal(class(res), "list")
    expect_length(res, 3)

    expect_equal(class(res$v1.level_0), "data.frame")
    expect_length(res$v1.level_0, 4)

    expect_equal(class(res$v1.level_1), "data.frame")
    expect_length(res$v1.level_1, 4)

    expect_equal(class(res$v1.level_2), "data.frame")
    expect_length(res$v1.level_2, 4)
})

test_that("simple subsetByClassDS, factor vector, specified variables", {
    data      <- factor(c(0, 0, 0, 1, 1, 1, 2, 1, 2, 2))
    variables <- NULL

    res <- subsetByClassDS("data", variables)

    expect_equal(class(res), "list")
    expect_length(res, 3)

    expect_equal(class(res$data.level_0), "factor")
    expect_length(res$data.level_0, 3)

    expect_equal(class(res$data.level_1), "factor")
    expect_length(res$data.level_1, 4)

    expect_equal(class(res$data.level_2), "factor")
    expect_length(res$data.level_2, 3)
})

#
# Done
#

context("subsetByClassDS::smk::shutdown")

context("subsetByClassDS::smk::done")
