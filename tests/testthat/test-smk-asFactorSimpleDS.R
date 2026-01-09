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

# context("asFactorSimpleDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("asFactorSimpleDS::smk::simple")
test_that("simple asFactorSimpleDS", {
    input <- c(2.0, 1.0, 3.0, 3.0, 3.0, 1.0, 2.0, 2.0, 1.0, 2.0)

    res <- asFactorSimpleDS("input")

    expect_equal(class(res), "factor")
    expect_length(res, 10)
    expect_true(res[1] == "2")
    expect_true(res[2] == "1")
    expect_true(res[3] == "3")
    expect_true(res[4] == "3")
    expect_true(res[5] == "3")
    expect_true(res[6] == "1")
    expect_true(res[7] == "2")
    expect_true(res[8] == "2")
    expect_true(res[9] == "1")
    expect_true(res[10] == "2")

    res.levels <- levels(res)

    expect_equal(class(res.levels), "character")
    expect_length(res.levels, 3)
    expect_equal(res.levels[1], "1")
    expect_equal(res.levels[2], "2")
    expect_equal(res.levels[3], "3")
})

#
# Done
#

# context("asFactorSimpleDS::smk::shutdown")

# context("asFactorSimpleDS::smk::done")
