#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("recodeValuesDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple recodeValuesDS", {
    input          <- c(1, 2, 3, 4, 1, 3)
    values2replace <- "1,3"
    new.values     <- "10,30"

    res <- recodeValuesDS("input", values2replace, new.values)

    expect_equal(class(res), "numeric")
    expect_length(res, 6)
    expect_equal(res[1], 10)
    expect_equal(res[2], 2)
    expect_equal(res[3], 30)
    expect_equal(res[4], 4)
    expect_equal(res[5], 10)
    expect_equal(res[6], 30)
})

#
# Done
#

context("recodeValuesDS::smk::shutdown")

context("recodeValuesDS::smk::done")
