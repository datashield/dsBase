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

context("namesDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("namesDS::smk::list of atoms")
test_that("simple namesDS, data.frame", {
    input <- list(v1 = 0.0, v2 = 1.0)

    res <- namesDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 2)
    expect_true("v1" %in% res)
    expect_true("v2" %in% res)
})

context("namesDS::smk::list of vectors")
test_that("simple namesDS, data.matrix", {
    input <- list(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- namesDS("input")

    expect_equal(class(res), "character")
    expect_length(res, 2)
    expect_true("v1" %in% res)
    expect_true("v2" %in% res)
})

#
# Done
#

context("namesDS::smk::shutdown")

context("namesDS::smk::done")
