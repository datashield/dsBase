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

context("extract::smk::setup")

#
# Tests
#

context("extract::smk::simple")
test_that("simple extract no holder", {
    input <- "variable"

    res <- extract(input)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_length(res$holders, 1)
    expect_true(is.na(res$holders))
    expect_length(res$elements, 1)
    expect_equal(res$elements, "variable")
})

test_that("simple extract", {
    input <- "holder$variable"

    res <- extract(input)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_length(res$holders, 1)
    expect_equal(res$holders, "holder")
    expect_length(res$elements, 1)
    expect_equal(res$elements, "variable")
})

context("extract::smk::simple vector")
test_that("simple extract no holder, vector", {
    input <- c("v1", "v2", "v3", "v4")

    res <- extract(input)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_length(res$holders, 4)
    expect_true(is.na(res$holders[1]))
    expect_true(is.na(res$holders[2]))
    expect_true(is.na(res$holders[3]))
    expect_true(is.na(res$holders[4]))
    expect_length(res$elements, 4)
    expect_equal(res$elements[1], "v1")
    expect_equal(res$elements[2], "v2")
    expect_equal(res$elements[3], "v3")
    expect_equal(res$elements[4], "v4")
})

test_that("simple extract, holder, vector", {
    input <- c("h1$v1", "h2$v2", "h3$v3", "h4$v4")

    res <- extract(input)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_length(res$holders, 4)
    expect_equal(res$holders[1], "h1")
    expect_equal(res$holders[2], "h2")
    expect_equal(res$holders[3], "h3")
    expect_equal(res$holders[4], "h4")
    expect_length(res$elements, 4)
    expect_equal(res$elements[1], "v1")
    expect_equal(res$elements[2], "v2")
    expect_equal(res$elements[3], "v3")
    expect_equal(res$elements[4], "v4")
})

test_that("simple extract, mixed, vector", {
    input <- c("v1", "h2$v2", "v3", "h4$v4")

    res <- extract(input)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_length(res$holders, 4)
    expect_true(is.na(res$holders[1]))
    expect_equal(res$holders[2], "h2")
    expect_true(is.na(res$holders[3]))
    expect_equal(res$holders[4], "h4")
    expect_length(res$elements, 4)
    expect_equal(res$elements[1], "v1")
    expect_equal(res$elements[2], "v2")
    expect_equal(res$elements[3], "v3")
    expect_equal(res$elements[4], "v4")
})

#
# Done
#

context("extract::smk::shutdown")

context("extract::smk::done")
