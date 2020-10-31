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

context("BooleDS::smk::setup")

#
# Tests
#

context("BooleDS::smk::simple equal")
test_that("simple BooleDS, equal numeric", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 1, "NA", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 0)
    expect_equal(res[2], 0)
    expect_equal(res[3], 1)
    expect_equal(res[4], 0)
    expect_equal(res[5], 0)
})

test_that("simple BooleDS, equal logical", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 1, "NA", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], FALSE)
    expect_equal(res[2], FALSE)
    expect_equal(res[3], TRUE)
    expect_equal(res[4], FALSE)
    expect_equal(res[5], FALSE)
})

context("BooleDS::smk::simple not-equal")
test_that("simple BooleDS, not-equal numeric", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 2, "NA", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 0)
    expect_equal(res[4], 1)
    expect_equal(res[5], 1)
})

test_that("simple BooleDS, not-equal logical", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 2, "NA", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], TRUE)
    expect_equal(res[2], TRUE)
    expect_equal(res[3], FALSE)
    expect_equal(res[4], TRUE)
    expect_equal(res[5], TRUE)
})

context("BooleDS::smk::simple less-than")
test_that("simple BooleDS, less-than numeric", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 3, "NA", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 0)
    expect_equal(res[4], 0)
    expect_equal(res[5], 0)
})

test_that("simple BooleDS, less-than logical", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 3, "NA", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], TRUE)
    expect_equal(res[2], TRUE)
    expect_equal(res[3], FALSE)
    expect_equal(res[4], FALSE)
    expect_equal(res[5], FALSE)
})

context("BooleDS::smk::simple less-than-equal")
test_that("simple BooleDS, less-than-equal numeric", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 4, "NA", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 1)
    expect_equal(res[4], 0)
    expect_equal(res[5], 0)
})

test_that("simple BooleDS, less-than-equal logical", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 4, "NA", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], TRUE)
    expect_equal(res[2], TRUE)
    expect_equal(res[3], TRUE)
    expect_equal(res[4], FALSE)
    expect_equal(res[5], FALSE)
})

context("BooleDS::smk::simple greater-than")
test_that("simple BooleDS, greater-than numeric", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 5, "NA", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 0)
    expect_equal(res[2], 0)
    expect_equal(res[3], 0)
    expect_equal(res[4], 1)
    expect_equal(res[5], 1)
})

test_that("simple BooleDS, greater-than logical", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 5, "NA", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], FALSE)
    expect_equal(res[2], FALSE)
    expect_equal(res[3], FALSE)
    expect_equal(res[4], TRUE)
    expect_equal(res[5], TRUE)
})

context("BooleDS::smk::simple greater-than-equal")
test_that("simple BooleDS, greater-than-equal numeric", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 6, "NA", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 0)
    expect_equal(res[2], 0)
    expect_equal(res[3], 1)
    expect_equal(res[4], 1)
    expect_equal(res[5], 1)
})

test_that("simple BooleDS, greater-than-equal logical", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- BooleDS("input$v1", "input$v2", 6, "NA", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], FALSE)
    expect_equal(res[2], FALSE)
    expect_equal(res[3], TRUE)
    expect_equal(res[4], TRUE)
    expect_equal(res[5], TRUE)
})

context("BooleDS::smk::na-check numeric")
test_that("na-check BooleDS, numeric, NA=NA", {
    input <- data.frame(v1 = c(0.0, NA, 2.0, 3.0, NA), v2 = c(NA, 3.0, 2.0, 1.0, NA))

    res <- BooleDS("input$v1", "input$v2", 1, "NA", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_true(is.na(res[1]))
    expect_true(is.na(res[2]))
    expect_equal(res[3], 1)
    expect_equal(res[4], 0)
    expect_true(is.na(res[5]))
})

test_that("na-check BooleDS, numeric, NA=0", {
    input <- data.frame(v1 = c(0.0, NA, 2.0, 3.0, NA), v2 = c(NA, 3.0, 2.0, 1.0, NA))

    res <- BooleDS("input$v1", "input$v2", 1, "0", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 0)
    expect_equal(res[2], 0)
    expect_equal(res[3], 1)
    expect_equal(res[4], 0)
    expect_equal(res[5], 0)
})

test_that("na-check BooleDS, numeric, NA=1", {
    input <- data.frame(v1 = c(0.0, NA, 2.0, 3.0, NA), v2 = c(NA, 3.0, 2.0, 1.0, NA))

    res <- BooleDS("input$v1", "input$v2", 1, "1", TRUE)

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 1)
    expect_equal(res[4], 0)
    expect_equal(res[5], 1)
})

context("BooleDS::smk::na-check logical")
test_that("na-check BooleDS, logical, NA=NA", {
    input <- data.frame(v1 = c(0.0, NA, 2.0, 3.0, NA), v2 = c(NA, 3.0, 2.0, 1.0, NA))

    res <- BooleDS("input$v1", "input$v2", 1, "NA", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_true(is.na(res[1]))
    expect_true(is.na(res[2]))
    expect_equal(res[3], TRUE)
    expect_equal(res[4], FALSE)
    expect_true(is.na(res[5]))
})

test_that("na-check BooleDS, logical, NA=0", {
    input <- data.frame(v1 = c(0.0, NA, 2.0, 3.0, NA), v2 = c(NA, 3.0, 2.0, 1.0, NA))

    res <- BooleDS("input$v1", "input$v2", 1, "0", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], FALSE)
    expect_equal(res[2], FALSE)
    expect_equal(res[3], TRUE)
    expect_equal(res[4], FALSE)
    expect_equal(res[5], FALSE)
})

test_that("na-check BooleDS, logical, NA=1", {
    input <- data.frame(v1 = c(0.0, NA, 2.0, 3.0, NA), v2 = c(NA, 3.0, 2.0, 1.0, NA))

    res <- BooleDS("input$v1", "input$v2", 1, "1", FALSE)

    expect_length(res, 5)
    expect_equal(class(res), "logical")
    expect_equal(res[1], TRUE)
    expect_equal(res[2], TRUE)
    expect_equal(res[3], TRUE)
    expect_equal(res[4], FALSE)
    expect_equal(res[5], TRUE)
})

#
# Done
#

context("BooleDS::smk::shutdown")

context("BooleDS::smk::done")
