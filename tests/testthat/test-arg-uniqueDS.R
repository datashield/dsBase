#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("uniqueDS::arg::setup")

#
# Tests
#

context("uniqueDS::arg::simple null argument")
test_that("simple uniqueDS for NULL", {
    expect_error(uniqueDS(NULL), "Variable's name can't be NULL", fixed = TRUE)
})

context("uniqueDS::arg::null value")
test_that("simple uniqueDS for NULL", {
    input <- NULL
    expect_error(uniqueDS("input"), "Variable can't be NULL", fixed = TRUE)
})

context("uniqueDS::arg::not character value")
test_that("simple uniqueDS for NULL", {
  expect_error(uniqueDS(17), "Variable's name isn't a single character vector", fixed = TRUE)
})

context("uniqueDS::arg::missing value")
test_that("simple uniqueDS for NULL", {
    expect_error(uniqueDS("input"), "object 'input' not found", fixed = TRUE)
})

#
# Done
#

context("uniqueDS::arg::shutdown")

context("uniqueDS::arg::done")
