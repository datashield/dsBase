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

# avoid performance tests on CRAN and GitHub Actions
testthat::skip_on_cran()
testthat::skip_on_ci()

context("varDS::perf::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("varDS::perf::numeric")
test_that("numeric varDS - performance", {
    skip_on_cran()

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

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("varDS::perf::numeric::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("varDS::perf::numeric::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("varDS::perf::numeric::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("varDS::perf::numeric::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("varDS::perf::numeric::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("varDS::perf::numeric::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("varDS::perf::numeric::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

context("varDS::perf::numeric with NA")
test_that("numeric varDS, with NA - performance", {
    skip_on_cran()

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

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("varDS::perf::numberAndNA::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("varDS::perf::numberAndNA::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("varDS::perf::numberAndNA::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("varDS::perf::numberAndNA::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("varDS::perf::numberAndNA::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("varDS::perf::numberAndNA::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("varDS::perf::numberAndNA::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

context("varDS::perf::shutdown")

context("varDS::perf::done")
