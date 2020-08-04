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

context("varDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("varDS::smk::numeric")
test_that("numeric varDS", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0)

    res <- varDS(input)

    expect_length(res, 6)
    expect_equal(class(res), "list")
    expect_equal(class(res$Sum), "numeric")
    expect_equal(res$Sum, 10.0)
    expect_equal(class(res$SumOfSquares), "numeric")
    expect_equal(res$SumOfSquares, 30.0)
    expect_equal(class(res$Nmissing), "integer")
    expect_equal(res$Nmissing, 0)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid, 5)
    expect_equal(class(res$Ntotal), "integer")
    expect_equal(res$Ntotal, 5)
    expect_equal(class(res$ValidityMessage), "character")
    expect_equal(res$ValidityMessage, "VALID ANALYSIS")
})

context("varDS::smk::numeric with NA")
test_that("numeric varDS, with NA", {
    input <- c(0.0, NA, 2.0, NA, 4.0)

    res <- varDS(input)

    expect_length(res, 6)
    expect_equal(class(res), "list")
    expect_equal(class(res$Sum), "numeric")
    expect_equal(res$Sum, 6.0)
    expect_equal(class(res$SumOfSquares), "numeric")
    expect_equal(res$SumOfSquares, 20.0)
    expect_equal(class(res$Nmissing), "integer")
    expect_equal(res$Nmissing, 2)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid, 3)
    expect_equal(class(res$Ntotal), "integer")
    expect_equal(res$Ntotal, 5)
    expect_equal(class(res$ValidityMessage), "character")
    expect_equal(res$ValidityMessage, "VALID ANALYSIS")
})

#
# Done
#

context("varDS::smk::shutdown")

context("varDS::smk::done")
