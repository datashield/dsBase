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

# context("matrixDiagDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple matrixDiagDS, serverside.matrix.2.vector", {
    M1 <- matrix(c(1, 2, 3, 4), 2, 2)

    res <- matrixDiagDS("M1", aim = "serverside.matrix.2.vector", nrows.transmit = "-9")

    expect_equal(length(res), 2)
    expect_equal(res[1], 1)
    expect_equal(res[2], 4)
})

test_that("matrixDiagDS errors when serverside object does not exist", {
    expect_error(matrixDiagDS("nonexistent_object", aim = "serverside.matrix.2.vector", nrows.transmit = "-9"), regexp = "does not exist")
})
