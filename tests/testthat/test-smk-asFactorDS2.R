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

context("asFactorDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("asFactorDS2::smk::simple")
test_that("simple asFactorDS2, fixed.dummy.vars is FALSE", {
    input                      <- c(2, 1, 3, 3, 3, 1, 2, 2, 1, 2)
    all.unique.levels.transmit <- "1,2,3,4"
    fixed.dummy.vars           <- FALSE
    baseline.level             <- NULL

    res <- asFactorDS2("input", all.unique.levels.transmit, fixed.dummy.vars, baseline.level)

    expect_equal(class(res), "factor")
    expect_length(res, 10)

    res.levels <- levels(res)

    expect_equal(class(res.levels), "character")
    expect_length(res.levels, 4)

    expect_equal(res.levels[1], "1")
    expect_equal(res.levels[2], "2")
    expect_equal(res.levels[3], "3")
    expect_equal(res.levels[4], "4")
})

test_that("simple asFactorDS2, fixed.dummy.vars is TRUE", {
    input                      <- c(2, 1, 3, 3, 3, 1, 2, 2, 1, 2)
    all.unique.levels.transmit <- "1,2,3,4"
    fixed.dummy.vars           <- TRUE
    baseline.level             <- 1.0

    res <- asFactorDS2("input", all.unique.levels.transmit, fixed.dummy.vars, baseline.level)

    expect_length(res, 30)

    res.class <- class(res)

    if (base::getRversion() < 4.0)
    {
        expect_length(res.class, 1)
        expect_true("matrix" %in% res.class)
    }
    else
    {
        expect_length(res.class, 2)
        expect_true("matrix" %in% res.class)
        expect_true("array" %in% res.class)
    }

    expect_equal(res[1], 1)
    expect_equal(res[2], 0)
    expect_equal(res[3], 0)
    expect_equal(res[4], 0)
    expect_equal(res[5], 0)
    expect_equal(res[6], 0)
    expect_equal(res[7], 1)
    expect_equal(res[8], 1)
    expect_equal(res[9], 0)
    expect_equal(res[10], 1)
    expect_equal(res[11], 0)
    expect_equal(res[12], 0)
    expect_equal(res[13], 1)
    expect_equal(res[14], 1)
    expect_equal(res[15], 1)
    expect_equal(res[16], 0)
    expect_equal(res[17], 0)
    expect_equal(res[18], 0)
    expect_equal(res[19], 0)
    expect_equal(res[20], 0)
    expect_equal(res[21], 0)
    expect_equal(res[22], 0)
    expect_equal(res[23], 0)
    expect_equal(res[24], 0)
    expect_equal(res[25], 0)
    expect_equal(res[26], 0)
    expect_equal(res[27], 0)
    expect_equal(res[28], 0)
    expect_equal(res[29], 0)
    expect_equal(res[30], 0)


    res.colnames <- colnames(res)

    expect_equal(class(res.colnames), "character")
    expect_length(res.colnames, 3)

    expect_equal(res.colnames[1], "DV2")
    expect_equal(res.colnames[2], "DV3")
    expect_equal(res.colnames[3], "DV4")
})

#
# Done
#

context("asFactorDS2::smk::shutdown")

context("asFactorDS2::smk::done")
