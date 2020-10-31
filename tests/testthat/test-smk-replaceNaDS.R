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

context("replaceNaDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("replaceNaDS::smk")
test_that("simple replaceNaDS", {
    input        <- c(0.0, NA, 2.0, NA, 4.0, NA, 6.0, NA)
    replacements <- c(1.1, 3.3, 5.5, 7.7)

    res <- replaceNaDS(input, replacements)

    expect_equal(class(res), "numeric")
    expect_length(res, 8)
    expect_equal(res[1], 0.0)
    expect_equal(res[2], 1.1)
    expect_equal(res[3], 2.0)
    expect_equal(res[4], 3.3)
    expect_equal(res[5], 4.0)
    expect_equal(res[6], 5.5)
    expect_equal(res[7], 6.0)
    expect_equal(res[8], 7.7)
})

#
# Done
#

context("replaceNaDS::smk::shutdown")

context("replaceNaDS::smk::done")
