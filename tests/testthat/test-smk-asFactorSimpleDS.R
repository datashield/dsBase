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

# context("asFactorSimpleDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple asFactorSimpleDS", {
    input <- c(2, 1, 3, 3, 3, 1, 2, 2, 1, 2)

    res <- asFactorSimpleDS("input")

    expect_equal(class(res), "factor")
    expect_length(res, 10)
    expect_equal(levels(res), c("1", "2", "3"))
    expect_equal(as.character(res), as.character(input))
})

test_that("asFactorSimpleDS errors when serverside object does not exist", {
    expect_error(asFactorSimpleDS("nonexistent_object"), regexp = "does not exist")
})
