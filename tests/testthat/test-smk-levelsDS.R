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

context("levelsDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("levelsDS::smk::numeric vector")
test_that("numeric vector levelsDS", {
    input <- as.factor(c(0, 1, 2, 1, 2, 3, 1, 2, 1, 0, 1, 2, 0))

    res <- levelsDS(input)

    expect_length(res, 2)
    expect_equal(class(res), "list")
    expect_equal(class(res$Levels), "character")
    expect_length(res$Levels, 4)
    expect_equal(res$Levels[1], "0")
    expect_equal(res$Levels[2], "1")
    expect_equal(res$Levels[3], "2")
    expect_equal(res$Levels[4], "3")
    expect_equal(class(res$ValidityMessage), "character")
    expect_equal(res$ValidityMessage, "VALID ANALYSIS")
})

#
# Done
#

context("levelsDS::smk::shutdown")

context("levelsDS::smk::done")
