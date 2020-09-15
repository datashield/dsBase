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

context("asListDS::smk::setup")

#
# Tests
#

context("asListDS::smk::simple")
test_that("simple asListDS", {
    input       <- list(v1 = c(1, 2, 3), v2 = c(4, 5, 6))
    newobj.name <- 'newobj'

    expect_false(exists("newobj"))

    res <- asListDS("input", newobj.name)

    expect_true(exists("newobj"))

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res[[1]], "New object <newobj> created")
    expect_equal(res[[2]], "Class of <newobj> is 'list'")
    expect_equal(res$return.message, "New object <newobj> created")
    expect_equal(res$class.of.newobj, "Class of <newobj> is 'list'")
})

#
# Done
#

context("asListDS::smk::shutdown")

context("asListDS::smk::done")
