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

context("asIntegerDS::smk::setup")

#
# Tests
#

context("asIntegerDS::smk::numeric")
test_that("numeric asIntegerDS", {
    input <- 3.141

    res <- asIntegerDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "integer")
    expect_equal(res, 3)
})

context("asIntegerDS::smk::numeric vector")
test_that("numeric vector asIntegerDS", {
    input <- c(0.1, 1.1, 2.1, 3.1, 4.1)

    res <- asIntegerDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "integer")
    expect_equal(res[1], 0)
    expect_equal(res[2], 1)
    expect_equal(res[3], 2)
    expect_equal(res[4], 3)
    expect_equal(res[5], 4)
})

context("asIntegerDS::smk::character")
test_that("character asIntegerDS - FALSE", {
    input <- "101"

    res <- asIntegerDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "integer")
    expect_equal(res, 101)
})

context("asIntegerDS::smk::character vector")
test_that("character vector asIntegerDS", {
    input <- c("101", "202", "303", "404", "505")

    res <- asIntegerDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "integer")
    expect_equal(res[1], 101)
    expect_equal(res[2], 202)
    expect_equal(res[3], 303)
    expect_equal(res[4], 404)
    expect_equal(res[5], 505)
})

#
# Done
#

context("asIntegerDS::smk::shutdown")

context("asIntegerDS::smk::done")
