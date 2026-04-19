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

# context("matrixTransposeDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixTransposeDS", {
    M1 <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)

    res <- matrixTransposeDS("M1")

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 3)
    expect_equal(ncol(res), 2)
    expect_equal(res, t(M1))
})

test_that("matrixTransposeDS errors when serverside object does not exist", {
    expect_error(matrixTransposeDS("nonexistent_object"), regexp = "does not exist")
})

test_that("matrixTransposeDS errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    expect_error(matrixTransposeDS("bad_input"), regexp = "must be of type")
})
