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

context("asFactorDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("asFactorDS1::smk::simple")
test_that("simple asFactorDS1", {
    input <- c(2.0, 1.0, 3.0, 3.0, 3.0, 1.0, 2.0, 2.0, 1.0, 2.0)

    res <- asFactorDS1("input")

    expect_equal(class(res), "character")
    expect_length(res, 3)
    expect_equal(res[1], "1")
    expect_equal(res[2], "2")
    expect_equal(res[3], "3")
})

#
# Done
#

context("asFactorDS1::smk::shutdown")

context("asFactorDS1::smk::done")
