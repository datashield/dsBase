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

context("unListDS::smk::setup")

#
# Tests
#

context("unListDS::smk::simple")
test_that("simple unListDS", {
    input <- list(v1 = c(1, 2, 3), v2 = c(4, 5, 6))

    res <- unListDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[[1]], 1)
    expect_equal(res[[2]], 2)
    expect_equal(res[[3]], 3)
    expect_equal(res[[4]], 4)
    expect_equal(res[[5]], 5)
    expect_equal(res[[6]], 6)

    res.names <- names(res)

    expect_equal(class(res.names), "character")
    expect_length(res.names, 6)
    expect_equal(res.names[1], 'v11')
    expect_equal(res.names[2], 'v12')
    expect_equal(res.names[3], 'v13')
    expect_equal(res.names[4], 'v21')
    expect_equal(res.names[5], 'v22')
    expect_equal(res.names[6], 'v23')
})

#
# Done
#

context("unListDS::smk::shutdown")

context("unListDS::smk::done")
