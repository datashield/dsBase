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

# context("dimDS::smk::setup")

#
# Tests
#

# context("dimDS::smk::numeric")
test_that("numeric dimDS", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- dimDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$dim[1], 5)
    expect_equal(res$dim[2], 2)
    expect_equal(res$class, "data.frame")
})

# context("dimDS::smk::character")
test_that("character dimDS", {
    input <- data.frame(v1 = c("0.0", "1.0", "2.0", "3.0", "4.0"), v2 = c("4.0", "3.0", "2.0", "1.0", "0.0"), stringsAsFactors = FALSE)

    res <- dimDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$dim[1], 5)
    expect_equal(res$dim[2], 2)
    expect_equal(res$class, "data.frame")
})

test_that("dimDS with matrix", {
    input <- matrix(1:6, nrow = 2, ncol = 3)

    res <- dimDS("input")

    expect_equal(res$dim[1], 2)
    expect_equal(res$dim[2], 3)
    expect_true("matrix" %in% res$class)
})

test_that("dimDS throws error when object does not exist", {
    expect_error(
        dimDS("nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("dimDS throws error when object is not data.frame or matrix", {
    bad_input <- c(1, 2, 3)
    expect_error(
        dimDS("bad_input"),
        regexp = "must be of type data.frame or matrix"
    )
})

#
# Done
#

# context("dimDS::smk::shutdown")

# context("dimDS::smk::done")
