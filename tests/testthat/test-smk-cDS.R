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

context("cDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("cDS::smk::numeric")
test_that("numeric cDS", {
    input <- list(a=0.0, b=1.0, c=2.0, d=3.0)

    res <- cDS(input)

    expect_length(res, 4)
    expect_equal(class(res), "numeric")
    expect_equal(res[[1]], 0.0)
    expect_equal(res[[2]], 1.0)
    expect_equal(res[[3]], 2.0)
    expect_equal(res[[4]], 3.0)
})

context("cDS::smk::character")
test_that("character cDS", {
    input <- list(a="0.0", b="1.0", c="2.0", d="3.0")

    res <- cDS(input)

    expect_length(res, 4)
    expect_equal(class(res), "character")
    expect_equal(res[[1]], "0.0")
    expect_equal(res[[2]], "1.0")
    expect_equal(res[[3]], "2.0")
    expect_equal(res[[4]], "3.0")
})

#
# Done
#

context("cDS::smk::shutdown")

context("cDS::smk::done")
