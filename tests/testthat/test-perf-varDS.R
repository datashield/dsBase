#-------------------------------------------------------------------------------
# Copyright (c) 2024 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

context("varDS::perf::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("varDS::perf::numeric")
test_that("numeric varDS - performance", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0)

    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        varDS(input)

        .count <- .count + 1
        .current.time <- Sys.time()
    }
    expect_true(TRUE)

    print(paste("varDS::perf::number::0:", format(.count / (difftime(.current.time, .start.time, units = "secs")[[1]]), digits = 8)))
})

context("varDS::perf::numeric with NA")
test_that("numeric varDS, with NA - performance", {
    input <- c(0.0, NA, 2.0, NA, 4.0)

    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        varDS(input)

        .count <- .count + 1
        .current.time <- Sys.time()
    }
    expect_true(TRUE)

    print(paste("varDS::perf::numberAndNA::0:", format(.count / (difftime(.current.time, .start.time, units = "secs")[[1]]), digits = 8)))
})

#
# Done
#

context("varDS::perf::shutdown")

context("varDS::perf::done")
