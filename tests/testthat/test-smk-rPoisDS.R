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

context("rPoisDS::smk::setup")

#
# Tests
#

context("rPoisDS::smk::simple")
test_that("simple rPoisDS, by name", {
    n      <- 8
    lambda <- 32

    res <- rPoisDS(n, "lambda")

    expect_equal(class(res), "integer")
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

test_that("simple rPoisDS, direct", {
    n      <- 8
    lambda <- 32

    res <- rPoisDS(n, lambda)

    expect_equal(class(res), "integer")
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

context("rPoisDS::smk::shutdown")

context("rPoisDS::smk::done")
