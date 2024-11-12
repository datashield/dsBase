#-------------------------------------------------------------------------------
# Copyright (c) 2024 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

context("vectorDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("vectorDS::smk::numeric list")
test_that("numeric list vectorDS", {
    input <- list(a=0.0, b=1.0, c=2.0, d=3.0)

    res <- vectorDS(input)

    expect_length(res, 4)
    expect_equal(class(res), "list")
    expect_equal(res[[1]], 0.0)
    expect_equal(res[[2]], 1.0)
    expect_equal(res[[3]], 2.0)
    expect_equal(res[[4]], 3.0)
})

context("vectorDS::smk::character list")
test_that("character list vectorDS", {
    input <- list(a="0.0", b="1.0", c="2.0", d="3.0")

    res <- vectorDS(input)

    expect_length(res, 4)
    expect_equal(class(res), "list")
    expect_equal(res[[1]], "0.0")
    expect_equal(res[[2]], "1.0")
    expect_equal(res[[3]], "2.0")
    expect_equal(res[[4]], "3.0")
})

context("vectorDS::smk::numeric list small")
test_that("single numeric list small vectorDS", {
    input <- list(a=0, b=1)

    res <- vectorDS(input)

    expect_length(res, 2)
    expect_equal(class(res), "list")
    expect_equal(res[[1]], 0)
    expect_equal(res[[2]], 1)
})

context("vectorDS::smk::empty list")
test_that("empty list vectorDS", {
    input <- list()

    res <- vectorDS(input)

    expect_length(res, 0)
    expect_equal(class(res), "list")
})

#
# Done
#

context("vectorDS::smk::shutdown")

context("vectorDS::smk::done")
