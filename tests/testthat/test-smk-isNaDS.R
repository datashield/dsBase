#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("isNaDS::smk::setup")

#
# Tests
#

# context("isNaDS::smk::numeric vector")
test_that("numeric vector isNaDS", {
    input <- c(0.1, 1.1, 2.1, 3.1, 4.1)

    res <- isNaDS("input")

    expect_length(res$is.na, 1)
    expect_equal(class(res$is.na), "logical")
    expect_equal(res$is.na, FALSE)
})

test_that("numeric vector isNaDS - with NA single", {
    input <- c(0.1, NA, 2.1, 3.1, 4.1)

    res <- isNaDS("input")

    expect_length(res$is.na, 1)
    expect_equal(class(res$is.na), "logical")
    expect_equal(res$is.na, FALSE)
})

test_that("numeric vector isNaDS - with NA all", {
    input <- c(NA, NA, NA, NA, NA)

    res <- isNaDS("input")

    expect_length(res$is.na, 1)
    expect_equal(class(res$is.na), "logical")
    expect_equal(res$is.na, TRUE)
})

# context("isNaDS::smk::character vector")
test_that("character vector isNaDS", {
    input <- c("101", "202", "303", "404", "505")

    res <- isNaDS("input")

    expect_length(res$is.na, 1)
    expect_equal(class(res$is.na), "logical")
    expect_equal(res$is.na, FALSE)
})

test_that("character vector isNaDS - with NA single", {
    input <- c("101", NA, "303", "404", "505")

    res <- isNaDS("input")

    expect_length(res$is.na, 1)
    expect_equal(class(res$is.na), "logical")
    expect_equal(res$is.na, FALSE)
})

test_that("character vector isNaDS - with NA all", {
    input <- c(NA, NA, NA, NA, NA)

    res <- isNaDS("input")

    expect_length(res$is.na, 1)
    expect_equal(class(res$is.na), "logical")
    expect_equal(res$is.na, TRUE)
})

test_that("isNaDS throws error when object does not exist", {
    expect_error(
        isNaDS("nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("isNaDS throws error when object is the wrong type", {
    bad_input <- list(1, 2, 3)
    expect_error(isNaDS("bad_input"), regexp = "must be of type character, factor, integer, logical, numeric, data.frame or matrix")
})

test_that("isNaDS accepts a factor", {
    input <- as.factor(c("a", "b", NA))

    res <- isNaDS("input")

    expect_equal(res$class, "factor")
    expect_false(res$is.na)
})

test_that("isNaDS accepts an integer vector", {
    input <- c(1L, NA, 3L)

    res <- isNaDS("input")

    expect_equal(res$class, "integer")
    expect_false(res$is.na)
})

test_that("isNaDS accepts a data.frame", {
    input <- data.frame(v1 = c(1, NA), v2 = c("x", "y"))

    res <- isNaDS("input")

    expect_equal(res$class, "data.frame")
    expect_false(res$is.na)
})

test_that("isNaDS accepts a matrix", {
    input <- matrix(c(1, NA, 3, 4), nrow = 2)

    res <- isNaDS("input")

    expect_true("matrix" %in% res$class)
    expect_false(res$is.na)
})

#
# Done
#

# context("isNaDS::smk::shutdown")

# context("isNaDS::smk::done")
