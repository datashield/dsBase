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

# context("matrixDimnamesDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixDimnamesDS", {
    M1 <- matrix(c(1, 2, 3, 4), 2, 2)
    new.dimnames <- list(c("r1", "r2"), c("c1", "c2"))

    res <- matrixDimnamesDS("M1", dimnames = new.dimnames)

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 2)
    expect_equal(ncol(res), 2)
    expect_equal(rownames(res), c("r1", "r2"))
    expect_equal(colnames(res), c("c1", "c2"))
})

test_that("matrixDimnamesDS errors when serverside object does not exist", {
    expect_error(matrixDimnamesDS("nonexistent_object", dimnames = list(c("r1"), c("c1"))), regexp = "does not exist")
})

test_that("matrixDimnamesDS errors when input is wrong type", {
    bad_input <- c("a", "b", "c")
    expect_error(matrixDimnamesDS("bad_input", dimnames = list(c("r1"), c("c1"))), regexp = "must be of type")
})
