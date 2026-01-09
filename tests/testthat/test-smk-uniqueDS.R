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

# context("uniqueDS::smk::setup")

#
# Tests
#

# context("uniqueDS::smk::simple for vector")
test_that("simple uniqueDS for vector", {
    input <- c(1, 2, 3, 2, 3, 6)

    res <- uniqueDS("input")

    expect_equal(class(res), "numeric")
    expect_length(res, 4)
    expect_equal(res[[1]], 1)
    expect_equal(res[[2]], 2)
    expect_equal(res[[3]], 3)
    expect_equal(res[[4]], 6)
})

# context("uniqueDS::smk::simple for vector")
test_that("simple uniqueDS for list", {
    input <- list(a=1, b=2, c=3, d=2, e=3, f=6)

    res <- uniqueDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 4)
    expect_equal(res[[1]], 1)
    expect_equal(res[[2]], 2)
    expect_equal(res[[3]], 3)
    expect_equal(res[[4]], 6)

    expect_length(names(res), 0)
})

#
# Done
#

# context("uniqueDS::smk::shutdown")

# context("uniqueDS::smk::done")
