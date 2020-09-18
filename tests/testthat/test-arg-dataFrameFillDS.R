#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("dataFrameFillDS::arg::setup")

#
# Tests
#

context("dataFrameFillDS::arg")
test_that("simple dataFrameFillDS, ascending, numeric", {
    df                  <- data.frame(v1 = c(-2.0, -3.0, 4.0, 2.0, 1.0, 0.0, -1.0, 3.0), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    allNames.transmit   <- "v1,v2,v3,v4,v5,v6,v7"
    class.vect.transmit <- "numeric,numeric,numeric,foo,bar,factor,logical"

    expect_error(res <- dataFrameFillDS("df", allNames.transmit, class.vect.transmit), "Unexpected missing class specified: 'foo'", fixed = TRUE)

    expect_true(! exists("res"))
})

#
# Shutdown
#

context("dataFrameFillDS::arg::shutdown")

#
# Done
#

context("dataFrameFillDS::arg::done")
