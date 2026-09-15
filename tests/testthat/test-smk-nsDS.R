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

# context("nsDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("nsDS::smk")
test_that("simple nsDS", {
    input <- seq(1, 30, by = 1)

    res <- nsDS("input", df = 3, knots = NULL, intercept = FALSE, Boundary.knots = NULL)

    expect_equal(class(res), c("ns", "basis", "matrix"))
    expect_equal(dim(res), c(30, 3))
    expect_equal(unname(res[1, ]), c(0, 0, 0))
})

test_that("nsDS throws error when object does not exist", {
    expect_error(
        nsDS("nonexistent_object", df = 3, knots = NULL, intercept = FALSE, Boundary.knots = NULL),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("nsDS::smk::shutdown")

# context("nsDS::smk::done")
