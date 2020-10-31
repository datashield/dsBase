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

context("rNormDS::smk::setup")

#
# Tests
#

context("rNormDS::smk::simple")
test_that("simple rNormDS, by name", {
    n    <- 8
    mean <- 32.0
    sd   <- 8.0

    res <- rNormDS(n, "mean", "sd", 8)

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

test_that("simple rNormDS, direct", {
    n    <- 8
    mean <- 32.0
    sd   <- 8.0

    res <- rNormDS(n, mean, sd, 8)

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

context("rNormDS::smk::shutdown")

context("rNormDS::smk::done")
