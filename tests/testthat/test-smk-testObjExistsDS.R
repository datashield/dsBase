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

context("testObjExistsDS::smk::setup")

#
# Tests
#

context("testObjExistsDS::smk::character")
test_that("simple testObjExistsDS, character", {
    input <- "value"

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "character")
})

test_that("simple testObjExistsDS, character vector", {
    input <- c("value1", "value2", "value3", "value4")

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "character")
})

context("testObjExistsDS::smk::integer")
test_that("simple testObjExistsDS, integer", {
    input <- 1L

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "integer")
})

test_that("simple testObjExistsDS, integer vector", {
    input <- c(1L, 2L, 3L, 4L)

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "integer")
})

context("testObjExistsDS::smk::numeric")
test_that("simple testObjExistsDS, numeric", {
    input <- 1.1

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "numeric")
})

test_that("simple testObjExistsDS, numeric vector", {
    input <- c(1.1, 2.1, 3.1, 4.1)

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "numeric")
})

context("testObjExistsDS::smk::logical")
test_that("simple testObjExistsDS, logical, FALSE", {
    input <- FALSE

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "logical")
})

test_that("simple testObjExistsDS, logical, TRUE", {
    input <- TRUE

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "logical")
})

test_that("simple testObjExistsDS, logical vector", {
    input <- c(FALSE, TRUE, FALSE, TRUE)

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "logical")
})

context("testObjExistsDS::smk::data.frame")
test_that("simple testObjExistsDS, data.frame", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "data.frame")
})

context("testObjExistsDS::smk::array")
test_that("simple testObjExistsDS, array", {
    input <- array(c(0.0, 1.0, 2.0, 3.0, 4.0))

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "array")
})

context("testObjExistsDS::smk::matrix")
test_that("simple testObjExistsDS, matrix", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0))

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    if (base::getRversion() < 4.0)
    {
        expect_length(res$test.obj.class, 1)
        expect_true("matrix" %in% res$test.obj.class)
    }
    else
    {
        expect_length(res$test.obj.class, 2)
        expect_true("matrix" %in% res$test.obj.class)
        expect_true("array" %in% res$test.obj.class)
    }
})

context("testObjExistsDS::smk::data.matrix")
test_that("simple testObjExistsDS, data.matrix", {
    input <- data.matrix(data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0)))

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)

    if (base::getRversion() < 4.0)
    {
        expect_length(res$test.obj.class, 1)
        expect_true("matrix" %in% res$test.obj.class)
    }
    else
    {
        expect_length(res$test.obj.class, 2)
        expect_true("matrix" %in% res$test.obj.class)
        expect_true("array" %in% res$test.obj.class)
    }
})

context("testObjExistsDS::smk::date")
test_that("simple testObjExistsDS, date", {
    input <- Sys.Date()

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "Date")
})

context("testObjExistsDS::smk::formula")
test_that("simple testObjExistsDS, formula", {
    input <- X ~ A + B

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "formula")
})

context("testObjExistsDS::smk::environment")
test_that("simple testObjExistsDS, environment", {
    input <- environment()

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "environment")
})

context("testObjExistsDS::smk::NA")
test_that("special testObjExistsDS, NA", {
    input <- NA

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "logical")
})

context("testObjExistsDS::smk::NULL")
test_that("special testObjExistsDS, NULL", {
    input <- NULL

    res <- testObjExistsDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, TRUE)
    expect_equal(res$test.obj.class, "NULL")
})

context("testObjExistsDS::smk::not exists")
test_that("special testObjExistsDS, not exists", {
    res <- testObjExistsDS("XXXinputXXX")

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$test.obj.exists, FALSE)
    expect_equal(res$test.obj.class, NULL)
})

#
# Done
#

context("testObjExistsDS::smk::shutdown")

context("testObjExistsDS::smk::done")
