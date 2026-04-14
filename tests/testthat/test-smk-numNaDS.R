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

# context("numNaDS::smk::setup")

#
# Tests
#

# context("numNaDS::smk::simple")
test_that("simple numNaDS", {
    input <- c(NA, 1, NA, 2, NA)

    res <- numNaDS("input")

    expect_equal(class(res$numNA), "integer")
    expect_length(res$numNA, 1)
    expect_equal(res$numNA, 3)
})

test_that("simple numNaDS, single NA", {
    input <- NA

    res <- numNaDS("input")

    expect_equal(class(res$numNA), "integer")
    expect_length(res$numNA, 1)
    expect_equal(res$numNA, 1)
})

test_that("numNaDS throws error when object does not exist", {
    expect_error(
        numNaDS("nonexistent_object"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("numNaDS::smk::shutdown")

# context("numNaDS::smk::done")
