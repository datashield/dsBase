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

context("rUnifDS::smk::setup")

#
# Tests
#

context("rUnifDS::smk::simple")
test_that("simple rUnifDS, by name", {
    n   <- 8
    min <- 2
    max <- 6

    res <- rUnifDS(n, "min", "max", 8)

    expect_equal(class(res), "numeric")
    expect_length(res, 8)
    expect_true(res[1] >= 0)
    expect_true(res[2] >= 0)
    expect_true(res[3] >= 0)
    expect_true(res[4] >= 0)
    expect_true(res[5] >= 0)
    expect_true(res[6] >= 0)
    expect_true(res[7] >= 0)
    expect_true(res[8] >= 0)
})

test_that("simple rUnifDS, direct", {
    n   <- 8
    min <- 2
    max <- 6

    res <- rUnifDS(n, min, max, 8)

    expect_equal(class(res), "numeric")
    expect_length(res, 8)
    expect_true(res[1] >= 0)
    expect_true(res[2] >= 0)
    expect_true(res[3] >= 0)
    expect_true(res[4] >= 0)
    expect_true(res[5] >= 0)
    expect_true(res[6] >= 0)
    expect_true(res[7] >= 0)
    expect_true(res[8] >= 0)
})

#
# Done
#

context("rUnifDS::smk::shutdown")

context("rUnifDS::smk::done")
