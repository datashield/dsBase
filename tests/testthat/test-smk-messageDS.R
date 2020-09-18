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

context("messageDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("messageDS::smk")
test_that("simple messageDS", {
    expect_warning(base::rm("object"), "object 'object' not found", fixed = TRUE)

    res <- messageDS("object")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "Error: the object <message.object.name> does not exist in this datasource", fixed = TRUE)
})

test_that("simple messageDS", {
    object <- NULL

    res <- messageDS("object")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "ALL OK: there are no studysideMessage(s) on this datasource", fixed = TRUE)
})

test_that("simple messageDS", {
    object <- list()

    res <- messageDS("object")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "Outcome object is a list without names. So a studysideMessage may be hidden. Please check output is OK", fixed = TRUE)
})


test_that("simple messageDS", {
    object <- list("a", "b", "c", "d")

    res <- messageDS("object")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "Outcome object is a list without names. So a studysideMessage may be hidden. Please check output is OK", fixed = TRUE)
})

test_that("simple messageDS", {
    object <- list(v1 = "a", v2 = "b", v3 = "c", v4 = "d")

    res <- messageDS("object")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "ALL OK: there are no studysideMessage(s) on this datasource", fixed = TRUE)
})

test_that("simple messageDS", {
    object <- list(v1 = "a", v2 = "b", studysideMessage = "message", v3 = "c", v4 = "d")

    res <- messageDS("object")

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res, "NOT ALL OK: there are studysideMessage(s) on this datasource", fixed = TRUE)
})

#
# Done
#

context("messageDS::smk::shutdown")

context("messageDS::smk::done")
