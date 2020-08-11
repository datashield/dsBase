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

context("rBinomDS::smk::setup")

#
# Tests
#

context("rBinomDS::smk::simple")
test_that("simple rBinomDS, by name", {
    n    <- 8
    size <- 32
    prob <- 0.5

    res <- rBinomDS(n, "size", "prob")

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

test_that("simple rBinomDS, direct", {
    n    <- 8
    size <- 32
    prob <- 0.5

    res <- rBinomDS(n, size, prob)

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

context("rBinomDS::smk::shutdown")

context("rBinomDS::smk::done")
