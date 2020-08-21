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

context("asLogicalDS::smk::setup")

#
# Tests
#

context("asLogicalDS::smk::integer")
test_that("simple asLogicalDS integer - FALSE", {
    input <- 0L

    res <- asLogicalDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, FALSE)
})

test_that("simple asLogicalDS integer - TRUE", {
    input <- 1L

    res <- asLogicalDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, TRUE)
})

context("asLogicalDS::smk::integer vector")
test_that("simple asLogicalDS integer vector", {
    input <- c(1L, 0L, 1L, 0L, 1L)

    res <- asLogicalDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], TRUE)
    expect_equal(res[2], FALSE)
    expect_equal(res[3], TRUE)
    expect_equal(res[4], FALSE)
    expect_equal(res[5], TRUE)
})

context("asLogicalDS::smk::numeric")
test_that("simple asLogicalDS numeric - FALSE", {
    input <- 0.0

    res <- asLogicalDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, FALSE)
})

test_that("simple asLogicalDS numeric - TRUE", {
    input <- 1.0

    res <- asLogicalDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "logical")
    expect_equal(res, TRUE)
})

context("asLogicalDS::smk::numeric vector")
test_that("simple asLogicalDS numeric vector", {
    input <- c(1.0, 0.0, 1.0, 0.0, 1.0)

    res <- asLogicalDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], TRUE)
    expect_equal(res[2], FALSE)
    expect_equal(res[3], TRUE)
    expect_equal(res[4], FALSE)
    expect_equal(res[5], TRUE)
})

context("asLogicalDS::smk::character")
test_that("simple asLogicalDS, character - FALSE", {
    input <- "F"

    res <- asLogicalDS("input")

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple asLogicalDS, character - FALSE", {
    input <- "False"

    res <- asLogicalDS("input")

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple asLogicalDS, character - FALSE", {
    input <- "FALSE"

    res <- asLogicalDS("input")

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple asLogicalDS, character - TRUE", {
    input <- "T"

    res <- asLogicalDS("input")

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

test_that("simple asLogicalDS, character - TRUE", {
    input <- "True"

    res <- asLogicalDS("input")

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

test_that("simple asLogicalDS, character - TRUE", {
    input <- "TRUE"

    res <- asLogicalDS("input")

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

test_that("simple asLogicalDS, character vector", {
     input <- c("T", "True", "TRUE", "F", "False", "FALSE")

     res <- asLogicalDS("input")

     expect_equal(class(res), "logical")
     expect_length(res, 6)
     expect_equal(res[1], TRUE)
     expect_equal(res[2], TRUE)
     expect_equal(res[3], TRUE)
     expect_equal(res[4], FALSE)
     expect_equal(res[5], FALSE)
     expect_equal(res[6], FALSE)
})

#
# Done
#

context("asLogicalDS::smk::shutdown")

context("asLogicalDS::smk::done")
