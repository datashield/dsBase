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

context("kurtosisDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("kurtosisDS2::smk")
test_that("simple kurtosisDS2", {
    input       <- c(1.0, 2.0, 2.0, 3.0, 3.0)
    global.mean <- 2.5

    res <- kurtosisDS2("input", global.mean)

    expect_length(res, 4)
    expect_equal(class(res), "list")
    expect_equal(class(res$Sum.quartics), "numeric")
    expect_equal(res$Sum.quartics, 5.3125, tolerance = 1e-6)
    expect_equal(class(res$Sum.squares), "numeric")
    expect_equal(res$Sum.squares, 3.25, tolerance = 1e-6)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid, 5)
    expect_equal(class(res$ValidityMessage), "character")
    expect_equal(res$ValidityMessage, "VALID ANALYSIS")
})

#
# Done
#

context("kurtosisDS2::smk::shutdown")

context("kurtosisDS2::smk::done")
