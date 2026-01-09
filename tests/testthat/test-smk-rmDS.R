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

# context("rmDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("rmDS::smk::single")
test_that("single rmDS", {
    expect_false(exists("input"))

    input <- "value"

    expect_true(exists("input"))

    res <- rmDS("input")

    expect_false(exists("input"))

    expect_equal(class(res), "list")
    expect_length(res, 4)
    expect_equal(res$return.message, "Object(s) 'input' was deleted.")
    expect_equal(res$deleted.objects, "input")
    expect_equal(res$missing.objects, "")
    expect_equal(res$problem.objects, "")
})

# context("rmDS::smk::multiple")
test_that("multiple rmDS", {
    expect_false(exists("input1"))
    expect_false(exists("input2"))

    input1 <- "value"
    input2 <- "value"

    expect_true(exists("input1"))
    expect_true(exists("input2"))

    res <- rmDS("input1,input2")

    expect_false(exists("input1"))
    expect_false(exists("input2"))

    expect_equal(class(res), "list")
    expect_length(res, 4)

    expect_equal(res$return.message, "Object(s) 'input1,input2' was deleted.")
    expect_equal(res$deleted.objects, "input1,input2")
    expect_equal(res$missing.objects, "")
    expect_equal(res$problem.objects, "")
})

# context("rmDS::smk::single missing")
test_that("single missing rmDS", {
    expect_false(exists("input"))

    res <- rmDS("input")

    expect_false(exists("input"))

    expect_equal(class(res), "list")
    expect_length(res, 4)
    expect_equal(res$return.message, "Object(s) 'input' which are missing.")
    expect_equal(res$deleted.objects, "")
    expect_equal(res$missing.objects, "input")
    expect_equal(res$problem.objects, "")
})


# context("rmDS::smk::multiple missing")
test_that("multiple missing rmDS", {
    expect_false(exists("input1"))
    expect_false(exists("input2"))

    res <- rmDS("input1,input2")

    expect_false(exists("input1"))
    expect_false(exists("input2"))

    expect_equal(class(res), "list")
    expect_length(res, 4)
    expect_equal(res$return.message, "Object(s) 'input1,input2' which are missing.")
    expect_equal(res$deleted.objects, "")
    expect_equal(res$missing.objects, "input1,input2")
    expect_equal(res$problem.objects, "")
})

# context("rmDS::smk::multiple mixed")
test_that("multiple mixed rmDS", {
    expect_false(exists("input1"))
    expect_false(exists("input2"))

    input1 <- "value"

    res <- rmDS("input1,input2")

    expect_false(exists("input1"))
    expect_false(exists("input2"))

    expect_equal(class(res), "list")
    expect_length(res, 4)
    expect_equal(res$return.message, "Object(s) 'input1' was deleted. 'input2' which are missing.")
    expect_equal(res$deleted.objects, "input1")
    expect_equal(res$missing.objects, "input2")
    expect_equal(res$problem.objects, "")
})

#
# Done
#

# context("rmDS::smk::shutdown")

# context("rmDS::smk::done")
