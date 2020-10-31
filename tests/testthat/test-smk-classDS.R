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

context("classDS::smk::setup")

#
# Tests
#

context("classDS::smk::character")
test_that("simple classDS, character", {
    input <- "value"

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "character")
})

test_that("simple classDS, character vector", {
    input <- c("value1", "value2", "value3", "value4")

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "character")
})

context("classDS::smk::integer")
test_that("simple classDS, integer", {
    input <- 1L

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "integer")
})

test_that("simple classDS, integer vector", {
    input <- c(1L, 2L, 3L, 4L)

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "integer")
})

context("classDS::smk::numeric")
test_that("simple classDS, numeric", {
    input <- 1.1

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "numeric")
})

test_that("simple classDS, numeric vector", {
    input <- c(1.1, 2.1, 3.1, 4.1)

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "numeric")
})

context("classDS::smk::logical")
test_that("simple classDS, logical, FALSE", {
    input <- FALSE

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "logical")
})

test_that("simple classDS, logical, TRUE", {
    input <- TRUE

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "logical")
})

test_that("simple classDS, logical vector", {
    input <- c(FALSE, TRUE, FALSE, TRUE)

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "logical")
})

context("classDS::smk::data.frame")
test_that("simple classDS, data.frame", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "data.frame")
})

context("classDS::smk::array")
test_that("simple classDS, array", {
    input <- array(c(0.0, 1.0, 2.0, 3.0, 4.0))

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "array")
})

context("classDS::smk::matrix")
test_that("simple classDS, matrix", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0))

    res <- classDS("input")

    expect_equal(class(res), "character")
    if (base::getRversion() < 4.0)
    {
        expect_length(res, 1)
        expect_true("matrix" %in% res)
    }
    else
    {
        expect_length(res, 2)
        expect_true("matrix" %in% res)
        expect_true("array" %in% res)
    }
})

context("classDS::smk::data.matrix")
test_that("simple classDS, data.matrix", {
    input <- data.matrix(data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0)))

    res <- classDS("input")

    expect_equal(class(res), "character")
    if (base::getRversion() < 4.0)
    {
        expect_length(res, 1)
        expect_true("matrix" %in% res)
    }
    else
    {
        expect_length(res, 2)
        expect_true("matrix" %in% res)
        expect_true("array" %in% res)
    }
})

context("classDS::smk::date")
test_that("simple classDS, date", {
    input <- Sys.Date()

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "Date")
})

context("classDS::smk::formula")
test_that("simple classDS, formula", {
    input <- X ~ A + B

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "formula")
})

context("classDS::smk::environment")
test_that("simple classDS, environment", {
    input <- environment()

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "environment")
})

context("classDS::smk::NA")
test_that("special classDS, NA", {
    input <- NA

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "logical")
})

context("classDS::smk::NULL")
test_that("special classDS, NULL", {
    input <- NULL

    res <- classDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "NULL")
})

#
# Done
#

context("classDS::smk::shutdown")

context("classDS::smk::done")
