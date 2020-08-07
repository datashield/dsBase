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

context("cbindDS::smk::setup")

#
# Tests
#

context("cbindDS::smk::simple")
test_that("simple cbindDS", {
    inputs   <- 'input1,input2'
    input1   <- c(0.0, 1.0, 2.0, 3.0)
    input2   <- c(3.0, 2.0, 1.0, 0.0)
    colnames <- 'v1,v2'

    res <- cbindDS(inputs, colnames)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)
    expect_equal(class(res[1]), "data.frame")
    expect_equal(class(res[2]), "data.frame")

    res.names <- names(res)

    expect_equal(class(res.names), "character")
    expect_length(res.names, 2)
    expect_equal(res.names[1], 'v1')
    expect_equal(res.names[2], 'v2')
})

#
# Done
#

context("cbindDS::smk::shutdown")

context("cbindDS::smk::done")
