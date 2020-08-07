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

context("rangeDS::smk::setup")

set.random.seed.setting(1234)

#
# Tests
#

context("rangeDS::smk::without NAs")
test_that("numeric rangeDS", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0, 4.0, 3.0, 2.0, 1.0, 0.0)

    res <- rangeDS(input)

    expect_length(res, 2)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 0.0, tolerance = 1e-6)
    expect_equal(res[2], 4.12446, tolerance = 1e-6)
})

context("rangeDS::smk::with NAs")
test_that("character rangeDS", {
    input <- c(0.0, NA, 2.0, NA, 4.0, NA, 3.0, NA, 1.0, NA)

    res <- rangeDS(input)

    expect_length(res, 2)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 0.0, tolerance = 1e-6)
    expect_equal(res[2], 4.12446, tolerance = 1e-6)
})

#
# Done
#

context("rangeDS::smk::shutdown")

context("rangeDS::smk::done")
