#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("recodeLevelsDS::smk::setup")

#
# Tests
#

test_that("simple recodeLevelsDS", {
    input <- c(1, 2, 3, 4, 1, 3)
    classes <- c("one", "two", "three", "four")

    res <- recodeLevelsDS(input, classes)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)

    res.levels <- levels(res)

    expect_equal(class(res.levels), "character")
    expect_length(res.levels, 4)
    expect_equal(res.levels[1], "one")
    expect_equal(res.levels[2], "two")
    expect_equal(res.levels[3], "three")
    expect_equal(res.levels[4], "four")
})

#
# Done
#

context("recodeLevelsDS::smk::shutdown")

context("recodeLevelsDS::smk::done")
