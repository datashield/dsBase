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

context("sqrtDS::smk::setup")

#
# Tests
#

context("sqrtDS::smk::special")
test_that("simple sqrtDS, NA", {
    input <- NA

    res <- sqrtDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.na(res))
})

test_that("simple sqrtDS, NaN", {
    input <- NaN

    res <- sqrtDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.nan(res))
})

test_that("simple sqrtDS, Inf", {
    input <- Inf

    res <- sqrtDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.infinite(res))
})

test_that("simple sqrtDS, -Inf", {
    input <- -Inf

    expect_warning(res <- sqrtDS("input"), "NaNs produced", fixed = TRUE)

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.nan(res))
})

context("sqrtDS::smk::numeric")
test_that("simple sqrtDS, numeric 0.0", {
    input <- 0.0

    res <- sqrtDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 0.0)
})

test_that("simple sqrtDS, numeric 10.0", {
    input <- 10.0

    res <- sqrtDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 3.16227766, tolerance = 1e-8)
})

test_that("simple sqrtDS, numeric -10.0", {
    input <- -10.0

    expect_warning(res <- sqrtDS("input"), "NaNs produced", fixed = TRUE)

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.nan(res))
})

context("sqrtDS::smk::integer")
test_that("simple sqrtDS, integer 0L", {
    input <- 0L

    res <- sqrtDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 0L)
})

test_that("simple sqrtDS, integer 10L", {
    input <- 10L

    res <- sqrtDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 3.16227766, tolerance = 1e-8)
})

test_that("simple sqrtDS, integer -10L", {
    input <- -10L

    expect_warning(res <- sqrtDS("input"), "NaNs produced", fixed = TRUE)

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.nan(res))
})

context("sqrtDS::smk::special vector")
test_that("simple sqrtDS", {
    input <- c(NA, NaN, Inf, -Inf)

    expect_warning(res <- sqrtDS("input"), "NaNs produced", fixed = TRUE)

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_true(is.na(res[1]))
    expect_true(is.infinite(res[3]))
    expect_true(is.nan(res[4]))
})

context("sqrtDS::smk::numeric vector")
test_that("simple sqrtDS", {
    input <- c(0.0, 4.0, 9.0, -10.0, -50.0, -20.0)

    expect_warning(res <- sqrtDS("input"), "NaNs produced", fixed = TRUE)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 0.0, tolerance = 1e-8)
    expect_equal(res[2], 2.0, tolerance = 1e-8)
    expect_equal(res[3], 3.0, tolerance = 1e-8)
    expect_true(is.nan(res[4]))
    expect_true(is.nan(res[5]))
    expect_true(is.nan(res[6]))
})

context("sqrtDS::smk::integer vector")
test_that("simple sqrtDS", {
    input <- c(0L, 4L, 9L, -10L, -50L, -20L)

    expect_warning(res <- sqrtDS("input"), "NaNs produced", fixed = TRUE)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 0.0, tolerance = 1e-8)
    expect_equal(res[2], 2.0, tolerance = 1e-8)
    expect_equal(res[3], 3.0, tolerance = 1e-8)
    expect_true(is.nan(res[4]))
    expect_true(is.nan(res[5]))
    expect_true(is.nan(res[6]))
})

#
# Done
#

context("sqrtDS::smk::shutdown")

context("sqrtDS::smk::done")
