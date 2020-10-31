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

context("dimDS::smk::setup")

#
# Tests
#

context("dimDS::smk::numeric")
test_that("numeric dimDS", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- dimDS("input")

    expect_length(res, 2)
    expect_equal(class(res), "integer")
    expect_equal(res[1], 5)
    expect_equal(res[2], 2)
})

context("dimDS::smk::character")
test_that("character dimDS", {
    input <- data.frame(v1 = c("0.0", "1.0", "2.0", "3.0", "4.0"), v2 = c("4.0", "3.0", "2.0", "1.0", "0.0"), stringsAsFactors = FALSE)

    res <- dimDS("input")

    expect_length(res, 2)
    expect_equal(class(res), "integer")
    expect_equal(res[1], 5)
    expect_equal(res[2], 2)
})

#
# Done
#

context("dimDS::smk::shutdown")

context("dimDS::smk::done")
