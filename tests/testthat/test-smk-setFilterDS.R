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

context("setFilterDS::smk::setup")

set.standard.disclosure.settings

#
# Tests
#

context("setFilterDS::smk::simple")
test_that("simple setFilterDS", {
    res <- setFilterDS()

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 5)
})

test_that("simple setFilterDS", {
    x <- 2

    res <- setFilterDS(x)

    expect_equal(class(res), "numeric")
    expect_length(res, 1)
    expect_equal(res, 2)
})

#
# Done
#

context("setFilterDS::smk::shutdown")

context("setFilterDS::smk::done")
