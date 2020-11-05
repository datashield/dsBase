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

context("completeCasesDS::smk::setup")

#
# Tests
#

context("completeCasesDS::smk::vector")
test_that("simple completeCasesDS, vector, with no NAs", {
    input <- c(1.1, 2.1, 3.1, 4.1)

    res <- completeCasesDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[1], 1.1)
    expect_equal(res[2], 2.1)
    expect_equal(res[3], 3.1)
    expect_equal(res[4], 4.1)
})

test_that("simple completeCasesDS, vector, with NAs", {
    input <- c(1.1, NA, 3.1, 4.1)

    res <- completeCasesDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 3)
    expect_equal(res[1], 1.1)
    expect_equal(res[2], 3.1)
    expect_equal(res[3], 4.1)
})

context("completeCasesDS::smk::data.frame")
test_that("simple completeCasesDS, data.frame, with no NAs", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- completeCasesDS("input")

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)
    expect_length(res$v1, 5)
    expect_equal(res$v1[1], 0.0)
    expect_equal(res$v1[2], 1.0)
    expect_equal(res$v1[3], 2.0)
    expect_equal(res$v1[4], 3.0)
    expect_equal(res$v1[5], 4.0)
    expect_length(res$v2, 5)
    expect_equal(res$v2[1], 4.0)
    expect_equal(res$v2[2], 3.0)
    expect_equal(res$v2[3], 2.0)
    expect_equal(res$v2[4], 1.0)
    expect_equal(res$v2[5], 0.0)

    res.colnames <- colnames(res)

    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], "v1")
    expect_equal(res.colnames[2], "v2")
})

test_that("simple completeCasesDS, data.frame, with NAs", {
    input <- data.frame(v1 = c(NA, 1.0, 2.0, NA, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, NA))

    res <- completeCasesDS("input")

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)
    expect_length(res$v1, 2)
    expect_equal(res$v1[1], 1.0)
    expect_equal(res$v1[2], 2.0)
    expect_length(res$v2, 2)
    expect_equal(res$v2[1], 3.0)
    expect_equal(res$v2[2], 2.0)

    res.colnames <- colnames(res)

    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], "v1")
    expect_equal(res.colnames[2], "v2")
})

context("completeCasesDS::smk::matrix")
test_that("simple completeCasesDS, matrix, with no NAs", {
    input <- matrix(c(0.0, 1.0, 2.0, 3.0, 4.0))

    res <- completeCasesDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 5)
    expect_equal(res[1], 0.0)
    expect_equal(res[2], 1.0)
    expect_equal(res[3], 2.0)
    expect_equal(res[4], 3.0)
    expect_equal(res[5], 4.0)
})

test_that("simple completeCasesDS, matrix, with NAs", {
    input <- matrix(c(0.0, NA, 2.0, NA, 4.0))

    res <- completeCasesDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 3)
    expect_equal(res[1], 0.0)
    expect_equal(res[2], 2.0)
    expect_equal(res[3], 4.0)
})

context("completeCasesDS::smk::data.matrix")
test_that("simple completeCasesDS, data.matrix, with no NAs", {
    input <- data.matrix(data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0)))

    res <- completeCasesDS("input")

    res.class <- class(res)
    if (base::getRversion() < 4.0)
    {
        expect_length(res.class, 1)
        expect_true("matrix" %in% res.class)
    }
    else
    {
        expect_length(res.class, 2)
        expect_true("matrix" %in% res.class)
        expect_true("array" %in% res.class)
    }

    expect_length(res, 10)
    expect_equal(res[1], 0.0)
    expect_equal(res[2], 1.0)
    expect_equal(res[3], 2.0)
    expect_equal(res[4], 3.0)
    expect_equal(res[5], 4.0)
    expect_equal(res[6], 4.0)
    expect_equal(res[7], 3.0)
    expect_equal(res[8], 2.0)
    expect_equal(res[9], 1.0)
    expect_equal(res[10], 0.0)

    res.colnames <- colnames(res)

    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], "v1")
    expect_equal(res.colnames[2], "v2")
})

test_that("simple completeCasesDS, data.matrix, with NAs", {
    input <- data.matrix(data.frame(v1 = c(0.0, NA, 2.0, 3.0, NA), v2 = c(NA, 3.0, 2.0, 1.0, 0.0)))

    res <- completeCasesDS("input")

    res.class <- class(res)
    if (base::getRversion() < 4.0)
    {
        expect_length(res.class, 1)
        expect_true("matrix" %in% res.class)
    }
    else
    {
        expect_length(res.class, 2)
        expect_true("matrix" %in% res.class)
        expect_true("array" %in% res.class)
    }

    expect_length(res, 4)
    expect_equal(res[1], 2.0)
    expect_equal(res[2], 3.0)
    expect_equal(res[3], 2.0)
    expect_equal(res[4], 1.0)

    res.colnames <- colnames(res)

    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], "v1")
    expect_equal(res.colnames[2], "v2")
})

#
# Done
#

context("completeCasesDS::smk::shutdown")

context("completeCasesDS::smk::done")
