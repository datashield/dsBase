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

context("seqDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("seqDS::smk")
test_that("simple seqDS", {
    FROM.value.char       <- "1"
    TO.value.char         <- "12"
    BY.value.char         <- "2"
    LENGTH.OUT.value.char <- NULL
    ALONG.WITH.name       <- NULL

    res <- seqDS(FROM.value.char, TO.value.char, BY.value.char, LENGTH.OUT.value.char, ALONG.WITH.name)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 1.0)
    expect_equal(res[2], 3.0)
    expect_equal(res[3], 5.0)
    expect_equal(res[4], 7.0)
    expect_equal(res[5], 9.0)
    expect_equal(res[6], 11.0)
})

test_that("simple seqDS", {
    FROM.value.char       <- "-5"
    TO.value.char         <- "6"
    BY.value.char         <- "2"
    LENGTH.OUT.value.char <- NULL
    ALONG.WITH.name       <- NULL

    res <- seqDS(FROM.value.char, TO.value.char, BY.value.char, LENGTH.OUT.value.char, ALONG.WITH.name)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], -5.0)
    expect_equal(res[2], -3.0)
    expect_equal(res[3], -1.0)
    expect_equal(res[4], 1.0)
    expect_equal(res[5], 3.0)
    expect_equal(res[6], 5.0)
})

test_that("simple seqDS", {
    FROM.value.char       <- "12"
    TO.value.char         <- "1"
    BY.value.char         <- "-2"
    LENGTH.OUT.value.char <- NULL
    ALONG.WITH.name       <- NULL

    res <- seqDS(FROM.value.char, TO.value.char, BY.value.char, LENGTH.OUT.value.char, ALONG.WITH.name)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 12.0)
    expect_equal(res[2], 10.0)
    expect_equal(res[3], 8.0)
    expect_equal(res[4], 6.0)
    expect_equal(res[5], 4.0)
    expect_equal(res[6], 2.0)
})

test_that("simple seqDS", {
    FROM.value.char       <- "6"
    TO.value.char         <- "-5"
    BY.value.char         <- "-2"
    LENGTH.OUT.value.char <- NULL
    ALONG.WITH.name       <- NULL

    res <- seqDS(FROM.value.char, TO.value.char, BY.value.char, LENGTH.OUT.value.char, ALONG.WITH.name)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 6.0)
    expect_equal(res[2], 4.0)
    expect_equal(res[3], 2.0)
    expect_equal(res[4], 0.0)
    expect_equal(res[5], -2.0)
    expect_equal(res[6], -4.0)
})

test_that("simple seqDS", {
    FROM.value.char       <- "-1.5"
    TO.value.char         <- "1.5"
    BY.value.char         <- "0.5"
    LENGTH.OUT.value.char <- NULL
    ALONG.WITH.name       <- NULL

    res <- seqDS(FROM.value.char, TO.value.char, BY.value.char, LENGTH.OUT.value.char, ALONG.WITH.name)

    expect_equal(class(res), "numeric")
    expect_length(res, 7)
    expect_equal(res[1], -1.5)
    expect_equal(res[2], -1.0)
    expect_equal(res[3], -0.5)
    expect_equal(res[4], 0.0)
    expect_equal(res[5], 0.5)
    expect_equal(res[6], 1.0)
    expect_equal(res[7], 1.5)
})

#
# Done
#

context("seqDS::smk::shutdown")

context("seqDS::smk::done")
