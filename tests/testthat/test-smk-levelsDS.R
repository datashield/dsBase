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

# context("levelsDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("levelsDS::smk::numeric vector")
test_that("numeric vector levelsDS", {
    input <- as.factor(c(0, 1, 2, 1, 2, 3, 1, 2, 1, 0, 1, 2, 0))

    res <- levelsDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "list")
    expect_equal(class(res$Levels), "character")
    expect_length(res$Levels, 4)
    expect_equal(res$Levels[1], "0")
    expect_equal(res$Levels[2], "1")
    expect_equal(res$Levels[3], "2")
    expect_equal(res$Levels[4], "3")
})

test_that("levelsDS throws error when object does not exist", {
    expect_error(
        levelsDS("nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("levelsDS throws error when object is not a factor", {
    bad_input <- c(1, 2, 3)
    expect_error(
        levelsDS("bad_input"),
        regexp = "must be of type factor"
    )
})

test_that("levelsDS blocks when levels density exceeds threshold", {
    input <- factor(1:10, levels = 1:10)

    expect_error(
        levelsDS("input"),
        regexp = "nfilter.levels.density"
    )
})

#
# Done
#

# context("levelsDS::smk::shutdown")

# context("levelsDS::smk::done")
