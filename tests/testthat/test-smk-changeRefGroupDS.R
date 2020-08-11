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

context("changeRefGroupDS::smk::setup")

#
# Tests
#

context("changeRefGroupDS::smk")
test_that("simple changeRefGroupDS, reorderByRef is FALSE", {
    x            <- c(8.0, 1.0, 6.0, 1.0, 4.0, 1.0, 2.0, 1.0)
    xf           <- factor(x)
    ref          <- 3
    reorderByRef <- FALSE

    res <- changeRefGroupDS(xf, ref, reorderByRef)

    expect_equal(class(res), "factor")
    expect_length(res, 8)

    res.num <- as.numeric(res)

    expect_equal(class(res.num), "numeric")
    expect_length(res.num, 8)

    expect_equal(res.num[1], 5)
    expect_equal(res.num[2], 2)
    expect_equal(res.num[3], 4)
    expect_equal(res.num[4], 2)
    expect_equal(res.num[5], 1)
    expect_equal(res.num[6], 2)
    expect_equal(res.num[7], 3)
    expect_equal(res.num[8], 2)

    res.levels <- levels(res)

    expect_equal(class(res.levels), "character")
    expect_length(res.levels, 5)
    expect_equal(res.levels[1], "4")
    expect_equal(res.levels[2], "1")
    expect_equal(res.levels[3], "2")
    expect_equal(res.levels[4], "6")
    expect_equal(res.levels[5], "8")
})

test_that("simple changeRefGroupDS, reorderByRef is TRUE", {
    x            <- c(8.0, 1.0, 6.0, 1.0, 4.0, 1.0, 2.0, 1.0)
    xf           <- factor(x)
    ref          <- 3
    reorderByRef <- TRUE

    res <- changeRefGroupDS(xf, ref, reorderByRef)

    expect_equal(class(res), "integer")
    expect_length(res, 8)
    expect_equal(res[1], 5)
    expect_equal(res[2], 2)
    expect_equal(res[3], 4)
    expect_equal(res[4], 2)
    expect_equal(res[5], 1)
    expect_equal(res[6], 2)
    expect_equal(res[7], 3)
    expect_equal(res[8], 2)

    res.levels <- levels(res)

    expect_true(is.null(res.levels))
})

#
# Done
#

context("changeRefGroupDS::smk::shutdown")

context("changeRefGroupDS::smk::done")
