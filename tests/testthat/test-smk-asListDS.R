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

# context("asListDS::smk::setup")

#
# Tests
#

# context("asListDS::smk::list")
test_that("asListDS returns a list unchanged", {
    input <- list(v1 = c(1, 2, 3), v2 = c(4, 5, 6))

    res <- asListDS("input")

    expect_equal(class(res), "list")
    expect_identical(res, input)
})

# context("asListDS::smk::numeric")
test_that("asListDS converts a numeric vector to a list of its elements", {
    input <- c(1.5, 2.5, 3.5)

    res <- asListDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 3)
    expect_equal(unlist(res), input)
})

# context("asListDS::smk::data.frame")
test_that("asListDS converts a data.frame to a named list of its columns", {
    input <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"))

    res <- asListDS("input")

    expect_equal(class(res), "list")
    expect_named(res, c("a", "b"))
    expect_equal(res$a, input$a)
    expect_equal(res$b, input$b)
})

# context("asListDS::smk::column")
test_that("asListDS converts a data.frame column given with $", {
    input <- data.frame(a = c(1, 2, 3))

    res <- asListDS("input$a")

    expect_equal(class(res), "list")
    expect_equal(unlist(res), c(1, 2, 3))
})

test_that("asListDS throws error when object does not exist", {
    expect_error(asListDS("nonexistent_object"), regexp = "does not exist")
})

test_that("asListDS throws error when object is NULL", {
    input <- NULL

    expect_error(asListDS("input"), "The server-side object 'input' is NULL", fixed = TRUE)
})

#
# Done
#

# context("asListDS::smk::shutdown")

# context("asListDS::smk::done")
