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

# context("kurtosisDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("kurtosisDS2::smk")
test_that("simple kurtosisDS2", {
    input       <- c(1.0, 2.0, 2.0, 3.0, 3.0)
    global.mean <- 2.5

    res <- kurtosisDS2("input", global.mean)

    expect_length(res, 4)
    expect_equal(class(res), "list")
    expect_equal(class(res$Sum.quartics), "numeric")
    expect_equal(res$Sum.quartics, 5.3125, tolerance = 1e-6)
    expect_equal(class(res$Sum.squares), "numeric")
    expect_equal(res$Sum.squares, 3.25, tolerance = 1e-6)
    expect_equal(class(res$Nvalid), "integer")
    expect_equal(res$Nvalid, 5)
    expect_equal(res$class, "numeric")
})

test_that("kurtosisDS2 throws error when object does not exist", {
    expect_error(kurtosisDS2("nonexistent_object", 2.5), regexp = "does not exist")
})

test_that("kurtosisDS2 throws error when object is not numeric or integer", {
    bad_input <- c("a", "b", "c")
    expect_error(kurtosisDS2("bad_input", 2.5), regexp = "must be of type numeric or integer")
})

test_that("kurtosisDS2 gives the same result for integer input as for numeric", {
    input.numeric <- c(0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 4.0)
    input.integer <- as.integer(input.numeric)

    res.numeric <- kurtosisDS2("input.numeric", 1.8)
    res.integer <- kurtosisDS2("input.integer", 1.8)

    expect_equal(res.integer$class, "integer")
    expect_equal(res.integer[names(res.integer) != "class"], res.numeric[names(res.numeric) != "class"])
})

#
# Done
#

# context("kurtosisDS2::smk::shutdown")

# context("kurtosisDS2::smk::done")
