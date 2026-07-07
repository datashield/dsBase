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

# context("dataFrameSortDS::arg::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("dataFrameSortDS::arg")
test_that("simple dataFrameSortDS, factor error check", {
    df              <- data.frame(v1 = as.factor(c("a", "b", "c", "d", "b", "e", "f", "f")), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    sort.key.name   <- "df$v1"
    sort.descending <- FALSE
    sort.method     <- "default"

    expect_error(dataFrameSortDS("df", sort.key.name, sort.descending, sort.method), "specified sort.key variable is of type 'factor'", fixed = TRUE)
})

#
# Shutdown
#

# context("dataFrameSortDS::arg::shutdown")

#
# Done
#

# context("dataFrameSortDS::arg::done")
