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

context("numNaDS::smk::setup")

#
# Tests
#

context("numNaDS::smk::simple")
test_that("simple numNaDS", {
    input <- c(NA, 1, NA, 2, NA)

    res <- numNaDS(input)

    expect_equal(class(res), "integer")
    expect_length(res, 1)
    expect_equal(res, 3)
})

test_that("simple numNaDS", {
    input <- NA

    res <- numNaDS(input)

    expect_equal(class(res), "integer")
    expect_length(res, 1)
    expect_equal(res, 1)
})

#
# Done
#

context("numNaDS::smk::shutdown")

context("numNaDS::smk::done")
