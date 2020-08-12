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

context("subsetDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("subsetDS::smk")
test_that("simple subsetDS, no NAs", {
    data    <- data.frame(v1 = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4), v2 = c(4.0, 0.0, 3.0, 1.0, 2.0, 2.0, 1.0, 3.0, 0.0, 4.0))
    complt  <- FALSE
    rs      <- NULL
    cs      <- NULL
    lg      <- 2
    th      <- 2
    varname <- "v1"

    res <- subsetDS("data", complt, rs, cs, lg, th, varname)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    expect_equal(class(res$v1), "numeric")
    expect_length(res$v1, 6)
    expect_equal(res$v1[1], 2)
    expect_equal(res$v1[2], 2)
    expect_equal(res$v1[3], 3)
    expect_equal(res$v1[4], 3)
    expect_equal(res$v1[5], 4)
    expect_equal(res$v1[6], 4)

    expect_equal(class(res$v2), "numeric")
    expect_length(res$v2, 6)
    expect_equal(res$v2[1], 2.0)
    expect_equal(res$v2[2], 2.0)
    expect_equal(res$v2[3], 1.0)
    expect_equal(res$v2[4], 3.0)
    expect_equal(res$v2[5], 0.0)
    expect_equal(res$v2[6], 4.0)
})

test_that("simple subsetDS, NAs, complete.case FALSE", {
    data    <- data.frame(v1 = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4), v2 = c(4.0, 0.0, 3.0, 1.0, 2.0, 2.0, 1.0, NA, NA, 4.0))
    complt  <- FALSE
    rs      <- NULL
    cs      <- NULL
    lg      <- 2
    th      <- 2
    varname <- "v1"

    res <- subsetDS("data", complt, rs, cs, lg, th, varname)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    expect_equal(class(res$v1), "numeric")
    expect_length(res$v1, 6)
    expect_equal(res$v1[1], 2)
    expect_equal(res$v1[2], 2)
    expect_equal(res$v1[3], 3)
    expect_equal(res$v1[4], 3)
    expect_equal(res$v1[5], 4)
    expect_equal(res$v1[6], 4)

    expect_equal(class(res$v2), "numeric")
    expect_length(res$v2, 6)
    expect_equal(res$v2[1], 2.0)
    expect_equal(res$v2[2], 2.0)
    expect_equal(res$v2[3], 1.0)
    expect_true(is.na(res$v2[4]))
    expect_true(is.na(res$v2[5]))
    expect_equal(res$v2[6], 4.0)
})

test_that("simple subsetDS, NAs, complete.case TRUE", {
    data    <- data.frame(v1 = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4), v2 = c(4.0, 0.0, 3.0, 1.0, 2.0, 2.0, 1.0, NA, NA, 4.0))
    complt  <- TRUE
    rs      <- NULL
    cs      <- NULL
    lg      <- 2
    th      <- 2
    varname <- "v1"

    res <- subsetDS("data", complt, rs, cs, lg, th, varname)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    expect_equal(class(res$v1), "numeric")
    expect_length(res$v1, 4)
    expect_equal(res$v1[1], 2)
    expect_equal(res$v1[2], 2)
    expect_equal(res$v1[3], 3)
    expect_equal(res$v1[4], 4)

    expect_equal(class(res$v2), "numeric")
    expect_length(res$v2, 4)
    expect_equal(res$v2[1], 2.0)
    expect_equal(res$v2[2], 2.0)
    expect_equal(res$v2[3], 1.0)
    expect_equal(res$v2[4], 4.0)
})

#
# Done
#

context("subsetDS::smk::shutdown")

context("subsetDS::smk::done")
