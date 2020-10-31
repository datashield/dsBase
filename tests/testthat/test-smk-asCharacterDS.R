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

context("asCharacterDS::smk::setup")

#
# Tests
#

context("asCharacterDS::smk::numeric")
test_that("numeric asCharacterDS", {
    input <- 3.141

    res <- asCharacterDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "3.141")
})

context("asCharacterDS::smk::numeric vector")
test_that("numeric vector asCharacterDS", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0)

    res <- asCharacterDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "character")
    expect_equal(res[1], "0")
    expect_equal(res[2], "1")
    expect_equal(res[3], "2")
    expect_equal(res[4], "3")
    expect_equal(res[5], "4")
})

context("asCharacterDS::smk::logical")
test_that("logical asCharacterDS - FALSE", {
    input <- FALSE

    res <- asCharacterDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "FALSE")
})

test_that("logical asCharacterDS - TRUE", {
    input <- TRUE

    res <- asCharacterDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "TRUE")
})

context("asCharacterDS::smk::logical vector")
test_that("logical vector asCharacterDS", {
    input <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

    res <- asCharacterDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "character")
    expect_equal(res[1], "TRUE")
    expect_equal(res[2], "FALSE")
    expect_equal(res[3], "TRUE")
    expect_equal(res[4], "FALSE")
    expect_equal(res[5], "TRUE")
})

#
# Done
#

context("asCharacterDS::smk::shutdown")

context("asCharacterDS::smk::done")
