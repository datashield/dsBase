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

context("asLogicalDS::arg::setup")

#
# Tests
#

context("asLogicalDS::arg::direct input numeric")
test_that("simple asLogicalDS non-input", {
    res <- asLogicalDS(1.0)

    expect_length(res, 1)
    expect_equal(class(res), "list")
    expect_equal(res[[1]], "ERROR: x.name must be specified as a character string")
    expect_equal(res$studysideMessage, "ERROR: x.name must be specified as a character string")
})

context("asLogicalDS::arg::input NULL")
test_that("simple asLogicalDS NULL", {
    input <- NULL

    res <- asLogicalDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "list")
    expect_equal(res[[1]], "ERROR: for ds.asLogical function, x.name must specify an input object of class numeric, integer, character or matrix")
    expect_equal(res$studysideMessage, "ERROR: for ds.asLogical function, x.name must specify an input object of class numeric, integer, character or matrix")
})

context("asLogicalDS::arg::input NA")
test_that("simple asLogicalDS NA", {
    input <- NA

    res <- asLogicalDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "list")
    expect_equal(res[[1]], "ERROR: for ds.asLogical function, x.name must specify an input object of class numeric, integer, character or matrix")
    expect_equal(res$studysideMessage, "ERROR: for ds.asLogical function, x.name must specify an input object of class numeric, integer, character or matrix")
})

#
# Done
#

context("asLogicalDS::arg::shutdown")

context("asLogicalDS::arg::done")
