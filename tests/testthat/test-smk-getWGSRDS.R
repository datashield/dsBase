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

context("getWGSRDS::smk::setup")

#
# Tests
#

context("getWGSRDS::smk::simple")

data <- data.frame(
    age    = c(6.0, 42.0, 23.0, 18.0, 52.0, 36.0, 30.0, NA, 29.0, 54.0),
    sex    = c(1, 2, 1, NA, 1, 2, 2, 1, 1, 3),
    weight = c(7.3, 12.5, 10.6, 12.8, 12.1, 16.9, 12.5, 16.3, 11.0, 14.2),
    height = c(65.0, 89.5, 78.1, 81.5, 87.3, NA, 82.2, 99.0, 0.0, 85.9),
    muac   = c(146, 156, 149, 160, 152, 190, 166, 154, 135, 151)
)

test_that("simple getWGSRDS - hfa", {
    sex        <- "data$sex"
    firstPart  <- "data$height"
    secondPart <- "data$age"
    index      <- "hfa"
    standing   <- NA
    thirdPart  <- NA

    res <- getWGSRDS(sex, firstPart, secondPart, index, standing, thirdPart)

    expect_equal(class(res), "numeric")
    expect_length(res, 10)
    expect_equal(res[1], 7.392301, tolerance = 1e-6)
    expect_equal(res[2], 17.32822, tolerance = 1e-6)
    expect_equal(res[3], 12.64677, tolerance = 1e-6)
    expect_true(is.na(res[4]))
    expect_equal(res[5], 15.03707, tolerance = 1e-6)
    expect_true(is.na(res[6]))
    expect_equal(res[7], 14.62922, tolerance = 1e-6)
    expect_true(is.na(res[8]))
    expect_equal(res[9], -28.05049, tolerance = 1e-6)
    expect_true(is.na(res[10]))
})

test_that("simple getWGSRDS - wfh", {
    sex        <- "data$sex"
    firstPart  <- "data$weight"
    secondPart <- "data$height"
    index      <- "wfh"
    standing   <- NA
    thirdPart  <- NA

    res <- getWGSRDS(sex, firstPart, secondPart, index, standing, thirdPart)

    expect_equal(class(res), "numeric")
    expect_length(res, 10)
    expect_equal(res[1], 0.05572347, tolerance = 1e-6)
    expect_equal(res[2], -0.01974903, tolerance = 1e-6)
    expect_equal(res[3], 0.5746911, tolerance = 1e-6)
    expect_true(is.na(res[4]))
    expect_equal(res[5], -0.1408004, tolerance = 1e-6)
    expect_true(is.na(res[6]))
    expect_equal(res[7], 1.833152, tolerance = 1e-6)
    expect_equal(res[8], 0.9361489, tolerance = 1e-6)
    expect_true(is.na(res[9]))
    expect_true(is.na(res[10]))
})

test_that("simple getWGSRDS - wfa", {
    sex        <- "data$sex"
    firstPart  <- "data$weight"
    secondPart <- "data$age"
    index      <- "wfa"
    standing   <- NA
    thirdPart  <- NA

    res <- getWGSRDS(sex, firstPart, secondPart, index, standing, thirdPart)

    expect_equal(class(res), "numeric")
    expect_length(res, 10)
    expect_equal(res[1], 6.245463, tolerance = 1e-6)
    expect_equal(res[2], 10.04803, tolerance = 1e-6)
    expect_equal(res[3], 9.099052, tolerance = 1e-6)
    expect_true(is.na(res[4]))
    expect_equal(res[5], 8.268653, tolerance = 1e-6)
    expect_equal(res[6], 16.24412, tolerance = 1e-6)
    expect_equal(res[7], 11.32272, tolerance = 1e-6)
    expect_true(is.na(res[8]))
    expect_equal(res[9], 8.928982, tolerance = 1e-6)
    expect_true(is.na(res[10]))
})

test_that("simple getWGSRDS - bfa", {
    sex        <- "data$sex"
    firstPart  <- "data$weight"
    secondPart <- "data$height"
    index      <- "bfa"
    standing   <- NA
    thirdPart  <- "data$age"

    res <- getWGSRDS(sex, firstPart, secondPart, index, standing, thirdPart)

    expect_equal(class(res), "numeric")
    expect_length(res, 10)
    expect_equal(res[1], 2.77812, tolerance = 1e-6)
    expect_equal(res[2], 0.3220866, tolerance = 1e-6)
    expect_equal(res[3], 2.122009, tolerance = 1e-6)
    expect_true(is.na(res[4]))
    expect_equal(res[5], -0.1105751, tolerance = 1e-6)
    expect_true(is.na(res[6]))
    expect_equal(res[7], 2.626587, tolerance = 1e-6)
    expect_true(is.na(res[8]))
    expect_true(is.na(res[9]))
    expect_true(is.na(res[10]))
})

test_that("simple getWGSRDS - mfa", {
    sex        <- "data$sex"
    firstPart  <- "data$age"
    secondPart <- NA
    index      <- "mfa"
    standing   <- NA
    thirdPart  <- NA

    res <- getWGSRDS(sex, firstPart, secondPart, index, standing, thirdPart)

    expect_equal(class(res), "logical")
    expect_length(res, 10)
    expect_true(is.na(res[1]))
    expect_true(is.na(res[2]))
    expect_true(is.na(res[3]))
    expect_true(is.na(res[4]))
    expect_true(is.na(res[5]))
    expect_true(is.na(res[6]))
    expect_true(is.na(res[7]))
    expect_true(is.na(res[8]))
    expect_true(is.na(res[9]))
    expect_true(is.na(res[10]))
})

#
# Done
#

context("getWGSRDS::smk::shutdown")

context("getWGSRDS::smk::done")
