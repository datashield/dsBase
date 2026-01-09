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

# context("colnamesDS::smk::setup")

#
# Tests
#

# context("colnamesDS::smk::data.frame")
test_that("simple colnamesDS, data.frame", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- colnamesDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 2)
    expect_true("v1" %in% res)
    expect_true("v2" %in% res)
})

# context("colnamesDS::smk::data.matrix")
test_that("simple colnamesDS, data.matrix", {
    input <- data.matrix(data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0)))

    res <- colnamesDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 2)
    expect_true("v1" %in% res)
    expect_true("v2" %in% res)
})

test_that("colnamesDS throws error when object does not exist", {
  expect_error(
    colnamesDS("nonexistent_object"),
    regexp = "does not exist"
  )
})

test_that("colnamesDS throws error when object is not data.frame or matrix", {
  bad_input <- list(a = 1:3, b = 4:6)
  expect_error(
    colnamesDS("bad_input"),
    regexp = "must be of type data.frame or matrix"
  )
})

#
# Done
#

# context("colnamesDS::smk::shutdown")

# context("colnamesDS::smk::done")