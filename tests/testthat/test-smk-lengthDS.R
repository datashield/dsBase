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

# context("lengthDS::smk::setup")

#
# Tests
#

# context("lengthDS::smk::vector")
test_that("simple lengthDS, numeric vector", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0)

    res <- lengthDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$length, 5)
    expect_equal(res$class, "numeric")
})

test_that("simple lengthDS, character vector", {
    input <- c("0.0", "1.0", "2.0", "3.0", "4.0")

    res <- lengthDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$length, 5)
    expect_equal(res$class, "character")
})

test_that("simple lengthDS, list", {
    input <- list(a = 1, b = 2, c = 3)

    res <- lengthDS("input")

    expect_equal(res$length, 3)
    expect_equal(res$class, "list")
})

test_that("lengthDS throws error when object does not exist", {
    expect_error(
        lengthDS("nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("lengthDS throws error when object is not a permitted type", {
    bad_input <- data.frame(a = 1:3)
    expect_error(
        lengthDS("bad_input"),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("lengthDS::smk::shutdown")

# context("lengthDS::smk::done")
