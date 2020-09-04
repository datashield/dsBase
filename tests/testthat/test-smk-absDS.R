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

context("absDS::smk::setup")

#
# Tests
#

context("absDS::smk::special")
test_that("simple absDS, NA", {
    input <- NA

    res <- absDS("input")

    expect_equal(class(res), "integer")
    expect_length(res, 1)
    expect_true(is.na(res))
})

test_that("simple absDS, NaN", {
    input <- NaN

    res <- absDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.nan(res))
})

test_that("simple absDS, Inf", {
    input <- Inf

    res <- absDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.infinite(res))
})

test_that("simple absDS, -Inf", {
    input <- -Inf

    res <- absDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_true(is.infinite(res))
})

context("absDS::smk::numeric")
test_that("simple absDS, numeric 0.0", {
    input <- 0.0

    res <- absDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 0.0)
})

test_that("simple absDS, numeric 10.0", {
    input <- 10.0

    res <- absDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 10.0)
})

test_that("simple absDS, numeric -10.0", {
    input <- -10.0

    res <- absDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 10.0)
})

context("absDS::smk::integer")
test_that("simple absDS, integer 0L", {
    input <- 0L

    res <- absDS("input")

    expect_equal(class(res), "integer")
    expect_length(res, 1)
    expect_equal(res, 0L)
})

test_that("simple absDS, integer 10L", {
    input <- 10L

    res <- absDS("input")

    expect_equal(class(res), "integer")
    expect_length(res, 1)
    expect_equal(res, 10L)
})

test_that("simple absDS, integer -10L", {
    input <- -10L

    res <- absDS("input")

    expect_equal(class(res), "integer")
    expect_length(res, 1)
    expect_equal(res, 10L)
})

context("absDS::smk::special vector")
test_that("simple absDS", {
    input <- c(NA, NaN, Inf, -Inf)

    res <- absDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_true(is.na(res[1]))
    expect_true(is.nan(res[2]))
    expect_true(is.infinite(res[3]))
    expect_true(is.infinite(res[4]))
})

context("absDS::smk::numeric vector")
test_that("simple absDS", {
    input <- c(0.0, 4.0, 9.0, -10.0, -50.0, -20.0)

    res <- absDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 0.0)
    expect_equal(res[2], 4.0)
    expect_equal(res[3], 9.0)
    expect_equal(res[4], 10.0)
    expect_equal(res[5], 50.0)
    expect_equal(res[6], 20.0)
})

context("absDS::smk::integer vector")
test_that("simple absDS", {
    input        <- c(0L, 4L, 9L, -10L, -50L, -20L)

    res <- absDS("input")

    expect_equal(class(res), "integer")
    expect_length(res, 6)
    expect_equal(res[1], 0L)
    expect_equal(res[2], 4L)
    expect_equal(res[3], 9L)
    expect_equal(res[4], 10L)
    expect_equal(res[5], 50L)
    expect_equal(res[6], 20L)
})

#
# Done
#

context("absDS::smk::shutdown")

context("absDS::smk::done")
