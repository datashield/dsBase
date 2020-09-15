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

context("isNaDS::smk::setup")

#
# Tests
#

context("isNaDS::smk::numeric vector")
test_that("numeric vector isNaDS", {
    input <- c(0.1, 1.1, 2.1, 3.1, 4.1)

    res <- isNaDS(input)

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, FALSE)
})

test_that("numeric vector isNaDS - with NA single", {
    input <- c(0.1, NA, 2.1, 3.1, 4.1)

    res <- isNaDS(input)

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, FALSE)
})

test_that("numeric vector isNaDS - with NA all", {
    input <- c(NA, NA, NA, NA, NA)

    res <- isNaDS(input)

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, TRUE)
})

context("isNaDS::smk::character vector")
test_that("character vector isNaDS", {
    input <- c("101", "202", "303", "404", "505")

    res <- isNaDS(input)

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, FALSE)
})

test_that("character vector isNaDS - with NA single", {
    input <- c("101", NA, "303", "404", "505")

    res <- isNaDS(input)

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, FALSE)
})

test_that("character vector isNaDS - with NA all", {
    input <- c(NA, NA, NA, NA, NA)

    res <- isNaDS(input)

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, TRUE)
})

#
# Done
#

context("isNaDS::smk::shutdown")

context("isNaDS::smk::done")
