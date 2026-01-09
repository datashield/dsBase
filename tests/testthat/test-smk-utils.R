
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

## When .loadServersideObject is called, the actual data exists two levels below the function,
## i.e. data in global env --> ds function --> .loadServersideObject. We recreate this in
## the test environment with a wrapper function.
.dsFunctionWrapper <- function(x) {
  .loadServersideObject(x)
}

# context("utils::smk::setup")
test_that(".loadServersideObject() returns existing object", {
  test_df <- data.frame(a = 1:3)
  result <- .dsFunctionWrapper("test_df")
  expect_identical(result, test_df)
})

test_that(".loadServersideObject() throws error for missing object", {
  expect_error(
    .dsFunctionWrapper("test_df"),
    regexp = "does not exist"
  )
})

test_that(".checkClass() passes for correct class", {
  df <- data.frame(a = 1)
  expect_invisible(
    .checkClass(df, "df", c("data.frame", "matrix"))
  )
})

test_that(".checkClass() throws informative error for wrong class", {
  x <- list(a = 1)
  expect_error(
    .checkClass(x, "x", c("data.frame", "matrix")),
    regexp = "must be of type data.frame or matrix"
  )
})

# context("utils::smk::shutdown")
# context("utils::smk::done")
