#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("asNumericDS::smk::setup")

#
# Tests
#

context("asNumericDS::smk::character")
test_that("character asNumericDS - FALSE", {
    input <- "101"

    res <- asNumericDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "numeric")
    expect_equal(res, 101)
})

context("asNumericDS::smk::character vector")
test_that("character vector asNumericDS", {
    input <- c("101", "202", "303", "404", "505")

    res <- asNumericDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 101)
    expect_equal(res[2], 202)
    expect_equal(res[3], 303)
    expect_equal(res[4], 404)
    expect_equal(res[5], 505)
})

context("asNumericDS::smk::character 'non numeric' vector")
test_that("character 'non numeric' vector asNumericDS", {
    input <- c("aa", "bb", "cc", "dd", "ee")

    res <- expect_warning(asNumericDS("input"), "NAs introduced by coercion", fixed = TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_true(is.na(res[1]))
    expect_true(is.na(res[2]))
    expect_true(is.na(res[3]))
    expect_true(is.na(res[4]))
    expect_true(is.na(res[5]))
})

context("asNumericDS::smk::factor vector")
test_that("factor vector asNumericDS", {
    vec   <- c("101", "202", "303", "404", "505")
    input <- as.factor(vec)

    res <- asNumericDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 101)
    expect_equal(res[2], 202)
    expect_equal(res[3], 303)
    expect_equal(res[4], 404)
    expect_equal(res[5], 505)
})

context("asNumericDS::smk::factor rev vector")
test_that("factor vector asNumericDS", {
    vec   <- c("505", "404", "303", "202", "101")
    input <- as.factor(vec)

    res <- asNumericDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 505)
    expect_equal(res[2], 404)
    expect_equal(res[3], 303)
    expect_equal(res[4], 202)
    expect_equal(res[5], 101)
})

#
# Done
#

context("asNumericDS::smk::shutdown")

context("asNumericDS::smk::done")
