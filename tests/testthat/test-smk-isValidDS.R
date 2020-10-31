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

context("isValidDS::smk::setup")

#
# Tests
#

context("isValidDS::smk::character")
test_that("simple isValidDS, character", {
    input <- "value"

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple isValidDS, character vector", {
    input <- c("value1", "value2", "value3", "value4")

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

context("isValidDS::smk::integer")
test_that("simple isValidDS, integer", {
    input <- 1L

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple isValidDS, integer vector", {
    input <- c(1L, 2L, 3L, 4L)

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res,  TRUE)
})

context("isValidDS::smk::numeric")
test_that("simple isValidDS, numeric", {
    input <- 1.1

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple isValidDS, numeric vector", {
    input <- c(1.1, 2.1, 3.1, 4.1)

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

context("isValidDS::smk::logical")
test_that("simple isValidDS, logical, FALSE", {
    input <- FALSE

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple isValidDS, logical, TRUE", {
    input <- TRUE

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple isValidDS, logical vector", {
    input <- c(FALSE, TRUE, FALSE, TRUE)

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

context("isValidDS::smk::data.frame")
test_that("simple isValidDS, data.frame", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

context("isValidDS::smk::array")
test_that("simple isValidDS, array", {
    input <- array(c(0.0, 1.0, 2.0, 3.0, 4.0))

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

context("isValidDS::smk::matrix")
test_that("simple isValidDS, matrix", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0))

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

context("isValidDS::smk::data.matrix")
test_that("simple isValidDS, data.matrix", {
    input <- data.matrix(data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0)))

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

context("isValidDS::smk::date")
test_that("simple isValidDS, date", {
    input <- Sys.Date()

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

context("isValidDS::smk::formula")
test_that("simple isValidDS, formula", {
    input <- X ~ A + B

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

context("isValidDS::smk::environment")
test_that("simple isValidDS, environment", {
    input <- environment()

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

context("isValidDS::smk::NA")
test_that("special isValidDS, NA", {
    input <- NA

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

context("isValidDS::smk::NULL")
test_that("special isValidDS, NULL", {
    input <- NULL

    res <- isValidDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

#
# Done
#

context("isValidDS::smk::shutdown")

context("isValidDS::smk::done")
