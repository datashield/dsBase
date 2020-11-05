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

context("checkNegValueDS::smk::setup")

#
# Tests
#

context("checkNegValueDS::smk::with no neg")
test_that("simple checkNegValueDS, with no neg and no NA", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)

    res <- checkNegValueDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

test_that("simple checkNegValueDS, with no neg and NA", {
    input <- c(0.0, NA, 2.0, NA, 4.0, NA, 6.0, NA)

    res <- checkNegValueDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, FALSE)
})

context("checkNegValueDS::smk::with neg")
test_that("simple checkNegValueDS, with neg and no NA", {
    input <- c(0.0, -1.0, -2.0, 3.0, -4.0, 5.0, -6.0, 7.0)

    res <- checkNegValueDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

test_that("simple checkNegValueDS, with neg and NA", {
    input <- c(0.0, -1.0, -2.0, NA, -4.0, NA, NA, 7.0)

    res <- checkNegValueDS(input)

    expect_equal(class(res), "logical")
    expect_length(res, 1)
    expect_equal(res, TRUE)
})

#
# Done
#

context("checkNegValueDS::smk::shutdown")

context("checkNegValueDS::smk::done")
