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

# context("kurtosis1DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("kurtosis1DS::smk::method 1")
test_that("simple kurtosis1DS, method 1", {
    input <- c(0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 4.0)

    res <- kurtosis1DS("input", 1)

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_equal(class(res$Kurtosis), "numeric")
    expect_equal(res$Kurtosis, -0.458210, tolerance = 1e-6)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid,9)
    expect_equal(res$class, "numeric")
})

# context("kurtosis1DS::smk::method 2")
test_that("simple kurtosis1DS, method 2", {
    input <- c(0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 4.0)

    res <- kurtosis1DS("input", 2)

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_equal(class(res$Kurtosis), "numeric")
    expect_equal(res$Kurtosis, 0.270076, tolerance = 1e-6)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid,9)
    expect_equal(res$class, "numeric")
})

# context("kurtosis1DS::smk::method 3")
test_that("simple kurtosis1DS, method 3", {
    input <- c(0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 4.0)

    res <- kurtosis1DS("input", 3)

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_equal(class(res$Kurtosis), "numeric")
    expect_equal(res$Kurtosis, -0.991672, tolerance = 1e-6)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid,9)
    expect_equal(res$class, "numeric")
})

test_that("kurtosis1DS throws error when object does not exist", {
    expect_error(kurtosis1DS("nonexistent_object", 1), regexp = "does not exist")
})

test_that("kurtosis1DS throws error when object is not numeric or integer", {
    bad_input <- c("a", "b", "c")
    expect_error(kurtosis1DS("bad_input", 1), regexp = "must be of type numeric or integer")
})

test_that("kurtosis1DS gives the same result for integer input as for numeric", {
    input.numeric <- c(0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 4.0)
    input.integer <- as.integer(input.numeric)

    res.numeric <- kurtosis1DS("input.numeric", 1)
    res.integer <- kurtosis1DS("input.integer", 1)

    expect_equal(res.integer$class, "integer")
    expect_equal(res.integer[names(res.integer) != "class"], res.numeric[names(res.numeric) != "class"])
})

#
# Done
#

# context("kurtosis1DS::smk::shutdown")

# context("kurtosis1DS::smk::done")
