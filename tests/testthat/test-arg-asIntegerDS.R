#-------------------------------------------------------------------------------
# Copyright (c) 2024-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("asIntegerDS::arg::setup")

#
# Tests
#

# context("asIntegerDS::arg::direct input numeric")
test_that("simple asIntegerDS non-input", {
    expect_error(asIntegerDS(1.0), "ERROR: x.name must be specified as a character string", fixed = TRUE)
})

#
# Done
#

# context("asIntegerDS::arg::shutdown")

# context("asIntegerDS::arg::done")
