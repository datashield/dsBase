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

# context("asFactorDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple asFactorDS2", {
    input <- c(2, 1, 3, 3, 3, 1, 2, 2, 1, 2)

    res <- asFactorDS2("input", "1,2,3,4", FALSE, NULL)

    expect_equal(class(res), "factor")
    expect_length(res, 10)
    expect_equal(levels(res), c("1", "2", "3", "4"))
})

test_that("asFactorDS2 with fixed dummy variables", {
    input <- c(2, 1, 3, 3, 3, 1, 2, 2, 1, 2)

    res <- asFactorDS2("input", "1,2,3,4", TRUE, 1)

    expect_true(is.matrix(res))
    expect_equal(dim(res), c(10L, 3L))
    expect_equal(colnames(res), c("DV2", "DV3", "DV4"))
    expect_equal(unname(res[, "DV2"]), c(1, 0, 0, 0, 0, 0, 1, 1, 0, 1))
    expect_equal(unname(res[, "DV3"]), c(0, 0, 1, 1, 1, 0, 0, 0, 0, 0))
    expect_equal(unname(res[, "DV4"]), rep(0, 10))
})

test_that("asFactorDS2 errors when serverside object does not exist", {
    expect_error(asFactorDS2("nonexistent_object", "1,2", FALSE, NULL), regexp = "does not exist")
})
