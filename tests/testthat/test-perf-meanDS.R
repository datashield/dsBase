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

context("meanDS::perf::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("meanDS::perf::numeric")
test_that("numeric meanDS - performance", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0)

    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        meanDS(input)

        .count <- .count + 1
        .current.time <- Sys.time()
    }
    expect_true(TRUE)

    print(paste("meanDS::perf::number::0:", format(.count / (difftime(.current.time, .start.time, units = "secs")[[1]]), digits = 8)))
})

context("meanDS::perf::numeric with NA")
test_that("numeric meanDS, with NA - performance", {
    input <- c(0.0, NA, 2.0, NA, 4.0)

    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        meanDS(input)

        .count <- .count + 1
        .current.time <- Sys.time()
    }
    expect_true(TRUE)

    print(paste("meanDS::perf::numberAndNA::0:", format(.count / (difftime(.current.time, .start.time, units = "secs")[[1]]), digits = 8)))
})

#
# Done
#

context("meanDS::perf::shutdown")

context("meanDS::perf::done")
