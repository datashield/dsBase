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

# context("meanSdGpDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("meanSdGpDS::smk::numeric by factor")
test_that("simple meanSdGpDS, numeric by factor", {
    x_var <- c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    index_var <- as.factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"))

    res <- meanSdGpDS("x_var", "index_var")

    expect_equal(class(res), "list")
    expect_true(res$Table_valid)
    expect_equal(res$Nvalid, 10)
    expect_equal(res$Nmissing, 0)
    expect_equal(res$Ntotal, 10)
    expect_equal(as.numeric(res$Mean_gp)[1], 3.0)
    expect_equal(as.numeric(res$Mean_gp)[2], 8.0)
})

test_that("meanSdGpDS throws error when X does not exist", {
    index_var <- as.factor(c("A", "A", "B", "B"))
    expect_error(meanSdGpDS("nonexistent_x", "index_var"), regexp = "does not exist")
})

test_that("meanSdGpDS throws error when INDEX does not exist", {
    x_var <- c(1.0, 2.0, 3.0, 4.0)
    expect_error(meanSdGpDS("x_var", "nonexistent_index"), regexp = "does not exist")
})

test_that("meanSdGpDS throws error when X is not numeric or integer", {
    bad_x <- c("a", "b", "c", "d")
    index_var <- as.factor(c("A", "A", "B", "B"))
    expect_error(meanSdGpDS("bad_x", "index_var"), regexp = "must be of type numeric or integer")
})

#
# Done
#

# context("meanSdGpDS::smk::shutdown")

# context("meanSdGpDS::smk::done")