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

context("lengthDS::smk::setup")

#
# Tests
#

context("lengthDS::smk::data.frame")
test_that("simple lengthDS, numeric data.frame", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- lengthDS("input")

    expect_equal(class(res), "integer")
    expect_equal(res, 2)
})

test_that("simple lengthDS, character data.frame", {
    input <- data.frame(v1 = c("0.0", "1.0", "2.0", "3.0", "4.0"), v2 = c("4.0", "3.0", "2.0", "1.0", "0.0"), stringsAsFactors = FALSE)

    res <- lengthDS("input")

    expect_equal(class(res), "integer")
    expect_equal(res, 2)
})

context("lengthDS::smk::vector")
test_that("simple lengthDS, numeric vector", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0)

    res <- lengthDS("input")

    expect_equal(class(res), "integer")
    expect_equal(res, 5)
})

test_that("simple lengthDS, character vector", {
    input <- c("0.0", "1.0", "2.0", "3.0", "4.0")

    res <- lengthDS("input")

    expect_equal(class(res), "integer")
    expect_equal(res, 5)
})

#
# Done
#

context("lengthDS::smk::shutdown")

context("lengthDS::smk::done")
