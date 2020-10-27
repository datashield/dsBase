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

context("recodeValuesDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("recodeValuesDS2::smk::numeric")
test_that("simple recodeValuesDS2", {
    input                          <- c(1, 2, 3, 4, 1, 3)
    values2replace                 <- "1,3"
    new.values                     <- "10,30"
    numeric.output.format.possible <- TRUE
    force.output.format            <- "no"
    v2r.numeric                    <- TRUE

    res <- recodeValuesDS2("input", values2replace, new.values, numeric.output.format.possible, force.output.format, v2r.numeric)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 10)
    expect_equal(res[2], 2)
    expect_equal(res[3], 30)
    expect_equal(res[4], 4)
    expect_equal(res[5], 10)
    expect_equal(res[6], 30)
})

context("recodeValuesDS2::smk::character")
test_that("simple recodeValuesDS2", {
    input                          <- c(1, 2, 3, 4, 1, 3)
    values2replace                 <- "1,3"
    new.values                     <- "10,30"
    numeric.output.format.possible <- FALSE
    force.output.format            <- "no"
    v2r.numeric                    <- FALSE

    res <- recodeValuesDS2("input", values2replace, new.values, numeric.output.format.possible, force.output.format, v2r.numeric)

    expect_equal(class(res), "character")
    expect_length(res, 6)
    expect_equal(res[1], "10")
    expect_equal(res[2], "2")
    expect_equal(res[3], "30")
    expect_equal(res[4], "4")
    expect_equal(res[5], "10")
    expect_equal(res[6], "30")
})

#
# Done
#

context("recodeValuesDS2::smk::shutdown")

context("recodeValuesDS2::smk::done")
