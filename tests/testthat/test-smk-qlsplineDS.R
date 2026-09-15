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

# context("qlsplineDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("qlsplineDS::smk")
test_that("simple qlsplineDS", {
    input <- seq(1, 30, by = 1)

    res <- qlsplineDS("input", q = 3, na.rm = TRUE, marginal = FALSE)

    expect_equal(class(res), c("lspline", "matrix"))
    expect_equal(dim(res), c(30, 3))
    expect_equal(unname(res[1, ]), c(1, 0, 0))
})

test_that("qlsplineDS throws error when object does not exist", {
    expect_error(
        qlsplineDS("nonexistent_object", q = 3, na.rm = TRUE, marginal = FALSE),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("qlsplineDS::smk::shutdown")

# context("qlsplineDS::smk::done")
