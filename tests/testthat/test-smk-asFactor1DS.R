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

# context("asFactor1DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple asFactor1DS", {
    input <- c(2.0, 1.0, 3.0, 3.0, 3.0, 1.0, 2.0, 2.0, 1.0, 2.0)

    res <- asFactor1DS("input")

    expect_equal(class(res), "character")
    expect_length(res, 3)
    expect_equal(res[1], "1")
    expect_equal(res[2], "2")
    expect_equal(res[3], "3")
})

test_that("asFactor1DS errors when serverside object does not exist", {
    expect_error(asFactor1DS("nonexistent_object"), regexp = "does not exist")
})
