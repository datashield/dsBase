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

# context("uniqueDS::arg::setup")

#
# Tests
#

# context("uniqueDS::arg::null argument")
test_that("uniqueDS errors for NULL argument", {
    expect_error(uniqueDS(NULL), "must be a single character string", fixed = TRUE)
})

# context("uniqueDS::arg::not character value")
test_that("uniqueDS errors for non-character argument", {
    expect_error(uniqueDS(17), "must be a single character string", fixed = TRUE)
})

# context("uniqueDS::arg::missing value")
test_that("uniqueDS errors for nonexistent object", {
    expect_error(uniqueDS("nonexistent_object"), "does not exist")
})

#
# Done
#

# context("uniqueDS::arg::shutdown")

# context("uniqueDS::arg::done")
