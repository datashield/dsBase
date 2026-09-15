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

# context("setSeedDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("setSeedDS::smk::simple")
test_that("simple setSeedDS", {
    seedtext    <- "19"
    kind        <- NULL
    normal.kind <- NULL

    res <- setSeedDS(seedtext, kind, normal.kind)

    expect_equal(class(res), "list")
    expect_length(res, 1)
    expect_length(res$seed.as.set, 626)
})

test_that("setSeedDS with \"NULL\" seedtext", {
    res <- setSeedDS("NULL", NULL, NULL)

    expect_equal(class(res), "list")
    expect_length(res, 1)
    expect_length(res$seed.as.set, 626)
})

test_that("setSeedDS fails with non-numeric seedtext", {
    expect_error(suppressWarnings(setSeedDS("abc", NULL, NULL)), "supplied seed is not a valid integer")
})

#
# Done
#

# context("setSeedDS::smk::shutdown")

# context("setSeedDS::smk::done")
