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

context("rmDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("rmDS::smk::simple")
test_that("simple rmDS", {
    expect_false(exists("input"))

    input <- "value"

    expect_true(exists("input"))

    res <- rmDS("input")

    expect_false(exists("input"))

    expect_equal(class(res), "list")
    expect_length(res, 1)
    expect_equal(res$return.message, "Object <input> successfully deleted")
})

#
# Done
#

context("rmDS::smk::shutdown")

context("rmDS::smk::done")
