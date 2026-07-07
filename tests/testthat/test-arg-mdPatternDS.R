#-------------------------------------------------------------------------------
# Copyright (c) 2025 ProPASS Consortium. All rights reserved.
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

# context("mdPatternDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("mdPatternDS::arg::x NULL")
test_that("mdPatternDS x NULL", {
    x <- NULL

    expect_error(mdPatternDS(x), "The input object must be of type 'data.frame' or 'matrix'. Current type: NULL")
})

# context("mdPatternDS::arg::x not valid variable")
test_that("mdPatternDS x not variable", {
    x <- "not a variable"

    expect_error(mdPatternDS(x), "Object 'not a variable' does not exist on the server")
})

#
# Done
#

# context("mdPatternDS::arg::shutdown")

# context("mdPatternDS::arg::done")
