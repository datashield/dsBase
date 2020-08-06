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

context("listDS::smk::setup")

#
# Tests
#

context("listDS::smk::simple")
test_that("simple listDS", {
    input    <- list(v1 = c(1, 2, 3), v2 = c(4, 5, 6))
    eltnames <- c('n1', 'n2')

    res <- listDS(input, eltnames)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_length(res[[1]], 3)
    expect_length(res[[2]], 3)

    res.names <- names(res)

    expect_equal(class(res.names), "character")
    expect_length(res.names, 2)
    expect_equal(res.names[1], 'n1')
    expect_equal(res.names[2], 'n2')
})

#
# Done
#

context("listDS::smk::shutdown")

context("listDS::smk::done")
