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

# context("rowColCalcDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("rowColCalcDS::smk")
test_that("simple rowColCalcDS, operation 1", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), ncol = 2)

    res <- rowColCalcDS("input", 1)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 4.0)
    expect_equal(res[2], 6.0)
    expect_equal(res[3], 8.0)
    expect_equal(res[4], 10.0)
})

test_that("simple rowColCalcDS, operation 2", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), ncol = 4)

    res <- rowColCalcDS("input", 2)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 1.0)
    expect_equal(res[2], 5.0)
    expect_equal(res[3], 9.0)
    expect_equal(res[4], 13.0)
})

test_that("simple rowColCalcDS, operation 3", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), ncol = 2)

    res <- rowColCalcDS("input", 3)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 2.0)
    expect_equal(res[2], 3.0)
    expect_equal(res[3], 4.0)
    expect_equal(res[4], 5.0)
})

test_that("simple rowColCalcDS, operation 4", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), ncol = 4)

    res <- rowColCalcDS("input", 4)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 0.5)
    expect_equal(res[2], 2.5)
    expect_equal(res[3], 4.5)
    expect_equal(res[4], 6.5)
})

test_that("rowColCalcDS throws error when object does not exist", {
  expect_error(
    rowColCalcDS("nonexistent_object", 1),
    regexp = "does not exist"
  )
})

test_that("rowColCalcDS throws error when object is not data.frame or matrix", {
  bad_input <- list(a = 1:3, b = 4:6)
  expect_error(
    rowColCalcDS("bad_input", 1),
    regexp = "must be of type data.frame or matrix"
  )
})

test_that("rowColCalcDS throws error when a column is not numeric", {
    input <- data.frame(v1 = c(1.0, 2.0, 3.0, 4.0), v2 = c("a", "b", "c", "d"))

    expect_error(rowColCalcDS("input", 1), regexp = "are not of numeric type")
})

#
# Done
#

# context("rowColCalcDS::smk::shutdown")

# context("rowColCalcDS::smk::done")
