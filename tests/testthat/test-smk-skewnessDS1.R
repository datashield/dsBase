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

context("skewnessDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("skewnessDS1::smk::method 1")
test_that("simple skewnessDS1, method 1", {
    input <- c(0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 4.0)

    res <- skewnessDS1("input", 1)

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_equal(class(res$Skewness), "numeric")
    expect_equal(res$Skewness, 0.443147, tolerance = 1e-6)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid,9)
    expect_equal(class(res$ValidityMessage), "character")
    expect_equal(res$ValidityMessage, "VALID ANALYSIS")
})

context("skewnessDS1::smk::method 2")
test_that("simple skewnessDS1, method 2", {
    input <- c(0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 4.0)

    res <- skewnessDS1("input", 2)

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_equal(class(res$Skewness), "numeric")
    expect_equal(res$Skewness, 0.537175, tolerance = 1e-6)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid,9)
    expect_equal(class(res$ValidityMessage), "character")
    expect_equal(res$ValidityMessage, "VALID ANALYSIS")
})

context("skewnessDS1::smk::method 3")
test_that("simple skewnessDS1, method 3", {
    input <- c(0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 4.0)

    res <- skewnessDS1("input", 3)

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_equal(class(res$Skewness), "numeric")
    expect_equal(res$Skewness, 0.371380, tolerance = 1e-6)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid,9)
    expect_equal(class(res$ValidityMessage), "character")
    expect_equal(res$ValidityMessage, "VALID ANALYSIS")
})

#
# Done
#

context("skewnessDS1::smk::shutdown")

context("skewnessDS1::smk::done")
