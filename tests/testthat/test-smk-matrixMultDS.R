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

# context("matrixMultDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixMultDS", {
    M1 <- matrix(c(1, 2, 3, 4), 2, 2)
    M2 <- matrix(c(5, 6, 7, 8), 2, 2)

    res <- matrixMultDS("M1", "M2")

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 2)
    expect_equal(ncol(res), 2)
    expect_equal(res, M1 %*% M2)
})

test_that("matrixMultDS errors when serverside object does not exist", {
    expect_error(matrixMultDS("nonexistent_object", "also_nonexistent"), regexp = "does not exist")
})

test_that("matrixMultDS errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    M2 <- matrix(c(1, 2, 3, 4), 2, 2)
    expect_error(matrixMultDS("bad_input", "M2"), regexp = "must be of type")
})
