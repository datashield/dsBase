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

# context("changeRefGroupDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple changeRefGroupDS", {
    xf <- as.factor(c(8, 1, 6, 1, 4, 1, 2, 1))

    res <- changeRefGroupDS("xf", ref = 2, reorderByRef = FALSE)

    expect_equal(class(res), "factor")
    expect_length(res, 8)
    expect_equal(levels(res), c("2", "1", "4", "6", "8"))
    expect_equal(as.character(res), c("8", "1", "6", "1", "4", "1", "2", "1"))
})

test_that("changeRefGroupDS with reorderByRef", {
    xf <- as.factor(c(8, 1, 6, 1, 4, 1, 2, 1))

    res <- changeRefGroupDS("xf", ref = 2, reorderByRef = TRUE)

    expect_equal(class(res), "factor")
    expect_length(res, 8)
    expect_equal(levels(res), c("2", "1", "4", "6", "8"))
    expect_equal(as.character(res), c("2", "8", "1", "6", "1", "4", "1", "1"))
})

test_that("changeRefGroupDS errors when serverside object does not exist", {
    expect_error(changeRefGroupDS("nonexistent_object", ref = 1, reorderByRef = FALSE),
                 regexp = "does not exist")
})

test_that("changeRefGroupDS errors when object is not a factor", {
    bad_input <- c(1, 2, 3)
    expect_error(changeRefGroupDS("bad_input", ref = 1, reorderByRef = FALSE),
                 regexp = "must be of type")
})
