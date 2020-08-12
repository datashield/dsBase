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

context("rowColCalcDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("rowColCalcDS::smk")
test_that("simple rowColCalcDS, operation 1", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), ncol = 2)

    res <- rowColCalcDS(input, 1)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 4.0)
    expect_equal(res[2], 6.0)
    expect_equal(res[3], 8.0)
    expect_equal(res[4], 10.0)
})

test_that("simple rowColCalcDS, operation 2", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), ncol = 4)

    res <- rowColCalcDS(input, 2)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 1.0)
    expect_equal(res[2], 5.0)
    expect_equal(res[3], 9.0)
    expect_equal(res[4], 13.0)
})

test_that("simple rowColCalcDS, operation 3", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), ncol = 2)

    res <- rowColCalcDS(input, 3)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 2.0)
    expect_equal(res[2], 3.0)
    expect_equal(res[3], 4.0)
    expect_equal(res[4], 5.0)
})

test_that("simple rowColCalcDS, operation 4", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), ncol = 4)

    res <- rowColCalcDS(input, 4)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 0.5)
    expect_equal(res[2], 2.5)
    expect_equal(res[3], 4.5)
    expect_equal(res[4], 6.5)
})

#
# Done
#

context("rowColCalcDS::smk::shutdown")

context("rowColCalcDS::smk::done")
