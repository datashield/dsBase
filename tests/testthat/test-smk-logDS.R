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

# context("logDS::smk::setup")

#
# Tests
#

# context("logDS::smk::numeric")
test_that("logDS computes natural log for numeric vector", {
    input <- c(1.0, exp(1), exp(2))

    res <- logDS("input")

    expect_equal(res, log(input))
    expect_true(is.numeric(res))
})

test_that("logDS computes log with custom base", {
    input <- c(1.0, 10.0, 100.0)

    res <- logDS("input", base = 10)

    expect_equal(res, log(input, base = 10))
})

# context("logDS::smk::integer")
test_that("logDS computes log for integer vector", {
    input <- as.integer(c(1, 2, 3, 4))

    res <- logDS("input")

    expect_equal(res, log(input))
})
#
# Done
#

# context("logDS::smk::shutdown")

# context("logDS::smk::done")