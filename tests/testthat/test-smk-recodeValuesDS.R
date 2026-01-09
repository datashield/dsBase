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

# context("recodeValuesDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple recodeValuesDS, numeric input, with no missings", {
    input          <- c(1, 2, 3, 4, 1, 3)
    values2replace <- "1,3"
    new.values     <- "10,30"

    res <- recodeValuesDS("input", values2replace, new.values)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 10)
    expect_equal(res[2], 2)
    expect_equal(res[3], 30)
    expect_equal(res[4], 4)
    expect_equal(res[5], 10)
    expect_equal(res[6], 30)
})

test_that("simple recodeValuesDS, numeric input, with missings", {
  input          <- c(1, 2, 3, 4, 1, 3, NA)
  values2replace <- "1,3"
  new.values     <- "10,30"
  missing        <- NULL
  
  res <- recodeValuesDS("input", values2replace, new.values, missing)
  
  expect_equal(class(res), "numeric")
  expect_length(res, 7)
  expect_equal(res[1], 10)
  expect_equal(res[2], 2)
  expect_equal(res[3], 30)
  expect_equal(res[4], 4)
  expect_equal(res[5], 10)
  expect_equal(res[6], 30)
  expect_true(is.na(res[7]))
})

test_that("simple recodeValuesDS, numeric input with missings, replace missings", {
  input          <- c(1, 2, 3, 4, 1, 3, NA)
  values2replace <- "1,3"
  new.values     <- "10,30"
  missing        <- "999"
  
  expect_warning(res <- recodeValuesDS("input", values2replace, new.values, missing))
  
  expect_equal(class(res), "numeric")
  expect_length(res, 7)
  expect_equal(res[1], 10)
  expect_equal(res[2], 2)
  expect_equal(res[3], 30)
  expect_equal(res[4], 4)
  expect_equal(res[5], 10)
  expect_equal(res[6], 30)
  expect_equal(res[7], 999)
  expect_true(length(which(is.na(res)))==0)
})

test_that("simple recodeValuesDS, factor input, with no missings", {
  input          <- as.factor(c(1, 2, 3, 4, 1, 3))
  values2replace <- "1,3"
  new.values     <- "10,30"
  expected_output <- as.factor(c(10, 2, 30, 4, 10, 30))

  res <- recodeValuesDS("input", values2replace, new.values)
  
  expect_equal(class(res), "factor")
  expect_length(res, 6)
  expect_true(setequal(levels(res), levels(expected_output)))
})

test_that("simple recodeValuesDS, factor input with missings, replace missings", {
  input          <- as.factor(c(1, 2, 3, 4, 1, 3, NA))
  values2replace <- "1,3"
  new.values     <- "10,30"
  missing        <- "999"
  expected_output <- as.factor(c(10, 2, 30, 4, 10, 30, 999))
  
  expect_warning(res <- recodeValuesDS("input", values2replace, new.values, missing))
  
  expect_equal(class(res), "factor")
  expect_length(res, 7)
  expect_true(setequal(levels(res), levels(expected_output)))
  expect_true(length(which(is.na(res)))==0)
})

test_that("simple recodeValuesDS, character input with missings", {
  input          <- c('ab', 'ab', 'ba', 'ba', 'bb', 'aa', NA)
  values2replace <- "ab,ba"
  new.values     <- "cc,dd"

  res <- recodeValuesDS("input", values2replace, new.values)
  
  expect_equal(class(res), "character")
  expect_length(res, 7)
  expect_equal(res[1], 'cc')
  expect_equal(res[2], 'cc')
  expect_equal(res[3], 'dd')
  expect_equal(res[4], 'dd')
  expect_equal(res[5], 'bb')
  expect_equal(res[6], 'aa')
  expect_true(is.na(res[7]))
})

#
# Done
#

# context("recodeValuesDS::smk::shutdown")

# context("recodeValuesDS::smk::done")
