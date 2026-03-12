#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("expDS::smk::setup")

#
# Tests
#

# context("expDS::smk::numeric")
test_that("expDS computes exponential for numeric vector", {
    input <- c(0.0, 1.0, 2.0, -1.0)

    res <- expDS("input")

    expect_equal(res, exp(input))
    expect_true(is.numeric(res))
})

# context("expDS::smk::integer")
test_that("expDS computes exponential for integer vector", {
    input <- as.integer(c(0, 1, 2, 3))

    res <- expDS("input")

    expect_equal(res, exp(input))
})

# context("expDS::smk::error")
test_that("expDS throws error when object does not exist", {
    expect_error(
        expDS("nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("expDS throws error when object is not numeric or integer", {
    bad_input <- c("a", "b", "c")
    expect_error(
        expDS("bad_input"),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("expDS::smk::shutdown")

# context("expDS::smk::done")