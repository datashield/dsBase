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

context("dataFrameSortDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("dataFrameSortDS::smk::numeric input")
test_that("simple dataFrameSortDS, ascending, default", {
    df              <- data.frame(v1 = c(-2.0, -3.0, 4.0, 2.0, 1.0, 0.0, -1.0, 3.0), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    sort.key.name   <- "df$v1"
    sort.descending <- FALSE
    sort.method     <- "default"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], -3.0)
    expect_equal(res$v2[1], 1.0)
    expect_equal(res$v1[2], -2.0)
    expect_equal(res$v2[2], 0.0)
    expect_equal(res$v1[3], -1.0)
    expect_equal(res$v2[3], 6.0)
    expect_equal(res$v1[4], 0.0)
    expect_equal(res$v2[4], 5.0)
    expect_equal(res$v1[5], 1.0)
    expect_equal(res$v2[5], 4.0)
    expect_equal(res$v1[6], 2.0)
    expect_equal(res$v2[6], 3.0)
    expect_equal(res$v1[7], 3.0)
    expect_equal(res$v2[7], 7.0)
    expect_equal(res$v1[8], 4.0)
    expect_equal(res$v2[8], 2.0)
})

test_that("simple dataFrameSortDS, descending, default", {
    df              <- data.frame(v1 = c(-2.0, -3.0, 4.0, 2.0, 1.0, 0.0, -1.0, 3.0), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    sort.key.name   <- "df$v1"
    sort.descending <- TRUE
    sort.method     <- "default"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], 4.0)
    expect_equal(res$v2[1], 2.0)
    expect_equal(res$v1[2], 3.0)
    expect_equal(res$v2[2], 7.0)
    expect_equal(res$v1[3], 2.0)
    expect_equal(res$v2[3], 3.0)
    expect_equal(res$v1[4], 1.0)
    expect_equal(res$v2[4], 4.0)
    expect_equal(res$v1[5], 0.0)
    expect_equal(res$v2[5], 5.0)
    expect_equal(res$v1[6], -1.0)
    expect_equal(res$v2[6], 6.0)
    expect_equal(res$v1[7], -2.0)
    expect_equal(res$v2[7], 0.0)
    expect_equal(res$v1[8], -3.0)
    expect_equal(res$v2[8], 1.0)
})

test_that("simple dataFrameSortDS, ascending, numeric", {
    df              <- data.frame(v1 = c(-2.0, -3.0, 4.0, 2.0, 1.0, 0.0, -1.0, 3.0), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    sort.key.name   <- "df$v1"
    sort.descending <- FALSE
    sort.method     <- "numeric"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], -3.0)
    expect_equal(res$v2[1], 1.0)
    expect_equal(res$v1[2], -2.0)
    expect_equal(res$v2[2], 0.0)
    expect_equal(res$v1[3], -1.0)
    expect_equal(res$v2[3], 6.0)
    expect_equal(res$v1[4], 0.0)
    expect_equal(res$v2[4], 5.0)
    expect_equal(res$v1[5], 1.0)
    expect_equal(res$v2[5], 4.0)
    expect_equal(res$v1[6], 2.0)
    expect_equal(res$v2[6], 3.0)
    expect_equal(res$v1[7], 3.0)
    expect_equal(res$v2[7], 7.0)
    expect_equal(res$v1[8], 4.0)
    expect_equal(res$v2[8], 2.0)
})

test_that("simple dataFrameSortDS, descending, numeric", {
    df              <- data.frame(v1 = c(-2.0, -3.0, 4.0, 2.0, 1.0, 0.0, -1.0, 3.0), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    sort.key.name   <- "df$v1"
    sort.descending <- TRUE
    sort.method     <- "numeric"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], 4.0)
    expect_equal(res$v2[1], 2.0)
    expect_equal(res$v1[2], 3.0)
    expect_equal(res$v2[2], 7.0)
    expect_equal(res$v1[3], 2.0)
    expect_equal(res$v2[3], 3.0)
    expect_equal(res$v1[4], 1.0)
    expect_equal(res$v2[4], 4.0)
    expect_equal(res$v1[5], 0.0)
    expect_equal(res$v2[5], 5.0)
    expect_equal(res$v1[6], -1.0)
    expect_equal(res$v2[6], 6.0)
    expect_equal(res$v1[7], -2.0)
    expect_equal(res$v2[7], 0.0)
    expect_equal(res$v1[8], -3.0)
    expect_equal(res$v2[8], 1.0)
})

test_that("simple dataFrameSortDS, ascending, alphabetic", {
    df              <- data.frame(v1 = c(-2.0, -3.0, 4.0, 2.0, 1.0, 0.0, -1.0, 3.0), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    sort.key.name   <- "df$v1"
    sort.descending <- FALSE
    sort.method     <- "alphabetic"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], -1.0)
    expect_equal(res$v2[1], 6.0)
    expect_equal(res$v1[2], -2.0)
    expect_equal(res$v2[2], 0.0)
    expect_equal(res$v1[3], -3.0)
    expect_equal(res$v2[3], 1.0)
    expect_equal(res$v1[4], 0.0)
    expect_equal(res$v2[4], 5.0)
    expect_equal(res$v1[5], 1.0)
    expect_equal(res$v2[5], 4.0)
    expect_equal(res$v1[6], 2.0)
    expect_equal(res$v2[6], 3.0)
    expect_equal(res$v1[7], 3.0)
    expect_equal(res$v2[7], 7.0)
    expect_equal(res$v1[8], 4.0)
    expect_equal(res$v2[8], 2.0)
})

test_that("simple dataFrameSortDS, descending, alphabetic", {
    df              <- data.frame(v1 = c(-2.0, -3.0, 4.0, 2.0, 1.0, 0.0, -1.0, 3.0), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    sort.key.name   <- "df$v1"
    sort.descending <- TRUE
    sort.method     <- "alphabetic"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], 4.0)
    expect_equal(res$v2[1], 2.0)
    expect_equal(res$v1[2], 3.0)
    expect_equal(res$v2[2], 7.0)
    expect_equal(res$v1[3], 2.0)
    expect_equal(res$v2[3], 3.0)
    expect_equal(res$v1[4], 1.0)
    expect_equal(res$v2[4], 4.0)
    expect_equal(res$v1[5], 0.0)
    expect_equal(res$v2[5], 5.0)
    expect_equal(res$v1[6], -3.0)
    expect_equal(res$v2[6], 1.0)
    expect_equal(res$v1[7], -2.0)
    expect_equal(res$v2[7], 0.0)
    expect_equal(res$v1[8], -1.0)
    expect_equal(res$v2[8], 6.0)
})

context("dataFrameSortDS::smk::string input")
test_that("simple dataFrameSortDS, ascending, default", {
    df              <- data.frame(v1 = c("-2.0", "-3.0", "4.0", "2.0", "1.0", "0.0", "-1.0", "3.0"), v2 = c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0"), stringsAsFactors = FALSE)
    sort.key.name   <- "df$v1"
    sort.descending <- FALSE
    sort.method     <- "default"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], "-1.0")
    expect_equal(res$v2[1], "6.0")
    expect_equal(res$v1[2], "-2.0")
    expect_equal(res$v2[2], "0.0")
    expect_equal(res$v1[3], "-3.0")
    expect_equal(res$v2[3], "1.0")
    expect_equal(res$v1[4], "0.0")
    expect_equal(res$v2[4], "5.0")
    expect_equal(res$v1[5], "1.0")
    expect_equal(res$v2[5], "4.0")
    expect_equal(res$v1[6], "2.0")
    expect_equal(res$v2[6], "3.0")
    expect_equal(res$v1[7], "3.0")
    expect_equal(res$v2[7], "7.0")
    expect_equal(res$v1[8], "4.0")
    expect_equal(res$v2[8], "2.0")
})

test_that("simple dataFrameSortDS, descending, default", {
    df              <- data.frame(v1 = c("-2.0", "-3.0", "4.0", "2.0", "1.0", "0.0", "-1.0", "3.0"), v2 = c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0"), stringsAsFactors = FALSE)
    sort.key.name   <- "df$v1"
    sort.descending <- TRUE
    sort.method     <- "default"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], "4.0")
    expect_equal(res$v2[1], "2.0")
    expect_equal(res$v1[2], "3.0")
    expect_equal(res$v2[2], "7.0")
    expect_equal(res$v1[3], "2.0")
    expect_equal(res$v2[3], "3.0")
    expect_equal(res$v1[4], "1.0")
    expect_equal(res$v2[4], "4.0")
    expect_equal(res$v1[5], "0.0")
    expect_equal(res$v2[5], "5.0")
    expect_equal(res$v1[6], "-3.0")
    expect_equal(res$v2[6], "1.0")
    expect_equal(res$v1[7], "-2.0")
    expect_equal(res$v2[7], "0.0")
    expect_equal(res$v1[8], "-1.0")
    expect_equal(res$v2[8], "6.0")
})

test_that("simple dataFrameSortDS, ascending, numeric", {
    df              <- data.frame(v1 = c("-2.0", "-3.0", "4.0", "2.0", "1.0", "0.0", "-1.0", "3.0"), v2 = c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0"), stringsAsFactors = FALSE)

    sort.key.name   <- "df$v1"
    sort.descending <- FALSE
    sort.method     <- "numeric"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], "-3.0")
    expect_equal(res$v2[1], "1.0")
    expect_equal(res$v1[2], "-2.0")
    expect_equal(res$v2[2], "0.0")
    expect_equal(res$v1[3], "-1.0")
    expect_equal(res$v2[3], "6.0")
    expect_equal(res$v1[4], "0.0")
    expect_equal(res$v2[4], "5.0")
    expect_equal(res$v1[5], "1.0")
    expect_equal(res$v2[5], "4.0")
    expect_equal(res$v1[6], "2.0")
    expect_equal(res$v2[6], "3.0")
    expect_equal(res$v1[7], "3.0")
    expect_equal(res$v2[7], "7.0")
    expect_equal(res$v1[8], "4.0")
    expect_equal(res$v2[8], "2.0")
})

test_that("simple dataFrameSortDS, descending, numeric", {
    df              <- data.frame(v1 = c("-2.0", "-3.0", "4.0", "2.0", "1.0", "0.0", "-1.0", "3.0"), v2 = c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0"), stringsAsFactors = FALSE)
    sort.key.name   <- "df$v1"
    sort.descending <- TRUE
    sort.method     <- "numeric"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], "4.0")
    expect_equal(res$v2[1], "2.0")
    expect_equal(res$v1[2], "3.0")
    expect_equal(res$v2[2], "7.0")
    expect_equal(res$v1[3], "2.0")
    expect_equal(res$v2[3], "3.0")
    expect_equal(res$v1[4], "1.0")
    expect_equal(res$v2[4], "4.0")
    expect_equal(res$v1[5], "0.0")
    expect_equal(res$v2[5], "5.0")
    expect_equal(res$v1[6], "-1.0")
    expect_equal(res$v2[6], "6.0")
    expect_equal(res$v1[7], "-2.0")
    expect_equal(res$v2[7], "0.0")
    expect_equal(res$v1[8], "-3.0")
    expect_equal(res$v2[8], "1.0")
})

test_that("simple dataFrameSortDS, ascending, alphabetic", {
    df              <- data.frame(v1 = c("-2.0", "-3.0", "4.0", "2.0", "1.0", "0.0", "-1.0", "3.0"), v2 = c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0"), stringsAsFactors = FALSE)
    sort.key.name   <- "df$v1"
    sort.descending <- FALSE
    sort.method     <- "alphabetic"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], "-1.0")
    expect_equal(res$v2[1], "6.0")
    expect_equal(res$v1[2], "-2.0")
    expect_equal(res$v2[2], "0.0")
    expect_equal(res$v1[3], "-3.0")
    expect_equal(res$v2[3], "1.0")
    expect_equal(res$v1[4], "0.0")
    expect_equal(res$v2[4], "5.0")
    expect_equal(res$v1[5], "1.0")
    expect_equal(res$v2[5], "4.0")
    expect_equal(res$v1[6], "2.0")
    expect_equal(res$v2[6], "3.0")
    expect_equal(res$v1[7], "3.0")
    expect_equal(res$v2[7], "7.0")
    expect_equal(res$v1[8], "4.0")
    expect_equal(res$v2[8], "2.0")
})

test_that("simple dataFrameSortDS, descending, alphabetic", {
    df              <- data.frame(v1 = c("-2.0", "-3.0", "4.0", "2.0", "1.0", "0.0", "-1.0", "3.0"), v2 = c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0"), stringsAsFactors = FALSE)
    sort.key.name   <- "df$v1"
    sort.descending <- TRUE
    sort.method     <- "alphabetic"

    res <- dataFrameSortDS("df", sort.key.name, sort.descending, sort.method)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)

    expect_equal(res$v1[1], "4.0")
    expect_equal(res$v2[1], "2.0")
    expect_equal(res$v1[2], "3.0")
    expect_equal(res$v2[2], "7.0")
    expect_equal(res$v1[3], "2.0")
    expect_equal(res$v2[3], "3.0")
    expect_equal(res$v1[4], "1.0")
    expect_equal(res$v2[4], "4.0")
    expect_equal(res$v1[5], "0.0")
    expect_equal(res$v2[5], "5.0")
    expect_equal(res$v1[6], "-3.0")
    expect_equal(res$v2[6], "1.0")
    expect_equal(res$v1[7], "-2.0")
    expect_equal(res$v2[7], "0.0")
    expect_equal(res$v1[8], "-1.0")
    expect_equal(res$v2[8], "6.0")
})

#
# Done
#

context("dataFrameSortDS::smk::shutdown")

context("dataFrameSortDS::smk::done")
