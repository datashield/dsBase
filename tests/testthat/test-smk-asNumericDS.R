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

#
# Done
#

context("asNumericDS::smk::shutdown")

context("asNumericDS::smk::done")
