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

# context("meanDS::disc::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("meanDS::disc::numeric with below nfilter.tab values")
test_that("numeric meanDS, with below nfilter.tab values", {
    input <- c(NA, NA, 2.0, NA, 4.0)
    
    expect_error(meanDS(input), "FAILED: Nvalid less than nfilter.tab", fixed = TRUE)
})

#
# Done
#

# context("meanDS::disc::shutdown")

# context("meanDS::disc::done")
