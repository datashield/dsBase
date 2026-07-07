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

test_that("simple lengthDS, numeric data.frame", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- lengthDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$length, 2)
    expect_equal(res$class, "data.frame")
})

test_that("simple lengthDS, character data.frame", {
    input <- data.frame(v1 = c("0.0", "1.0", "2.0", "3.0", "4.0"), v2 = c("4.0", "3.0", "2.0", "1.0", "0.0"), stringsAsFactors = FALSE)

    res <- lengthDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$length, 2)
    expect_equal(res$class, "data.frame")
})

test_that("simple lengthDS, matrix", {
    input <- matrix(1:6, nrow = 2, ncol = 3)

    res <- lengthDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$length, 6)
    expect_equal(res$class, c("matrix", "array"))
})

test_that("simple lengthDS, array", {
    input <- array(1:24, dim = c(2, 3, 4))

    res <- lengthDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$length, 24)
    expect_equal(res$class, "array")
})

#
# Done
#

# context("lengthDS::smk::shutdown")

# context("lengthDS::smk::done")
