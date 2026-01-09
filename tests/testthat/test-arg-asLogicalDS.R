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

# context("asLogicalDS::arg::setup")

#
# Tests
#

# context("asLogicalDS::arg::direct input numeric")
test_that("simple asLogicalDS non-input", {
    expect_error(asLogicalDS(1.0), "ERROR: x.name must be specified as a character string", fixed = TRUE)
})

# context("asLogicalDS::arg::input NULL")
test_that("simple asLogicalDS NULL", {
    input <- NULL

    expect_error(asLogicalDS("input"), "ERROR: for ds.asLogical function, x.name must specify an input object of class numeric, integer, character or matrix", fixed = TRUE)
})

# context("asLogicalDS::arg::input NA")
test_that("simple asLogicalDS NA", {
    input <- NA

    expect_error(asLogicalDS("input"), "ERROR: for ds.asLogical function, x.name must specify an input object of class numeric, integer, character or matrix", fixed = TRUE)
})

#
# Done
#

# context("asLogicalDS::arg::shutdown")

# context("asLogicalDS::arg::done")
