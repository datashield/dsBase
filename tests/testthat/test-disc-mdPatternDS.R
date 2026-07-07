#-------------------------------------------------------------------------------
# Copyright (c) 2025 ProPASS Consortium. All rights reserved.
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

# context("mdPatternDS::disc::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("mdPatternDS::disc::sample incomplete data.frame")
test_that("mdPatternDS: sample incomplete data.frame", {
    x_val <- data.frame(v1 = c(0.0, NA, 2.0, 3.0, 4.0, 5.0, 6.0), v2 = c(6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0))
    x     <- "x_val"

    res <- mdPatternDS(x)

    expect_length(res, 3)
    expect_length(class(res), 1)
    expect_true(all(class(res) %in% c("list")))
    expect_length(class(res$pattern), 2)
    expect_true(all(class(res$pattern) %in% c("matrix", "array")))

    expect_length(colnames(res$pattern), 3)
    expect_equal(colnames(res$pattern)[1], "v2")
    expect_equal(colnames(res$pattern)[2], "v1")
    expect_equal(colnames(res$pattern)[3], "")
    expect_length(rownames(res$pattern), 3)
    expect_equal(rownames(res$pattern)[1], "6")
    expect_equal(rownames(res$pattern)[2], "suppressed(<3)")
    expect_equal(rownames(res$pattern)[3], "")

    expect_equal(res$pattern[1, 1], 1)
    expect_equal(res$pattern[1, 2], 1)
    expect_equal(res$pattern[1, 3], 0)
    expect_true(is.na(res$pattern[2, 1]))
    expect_true(is.na(res$pattern[2, 2]))
    expect_true(is.na(res$pattern[2, 3]))
    expect_true(is.na(res$pattern[3, 1]))
    expect_true(is.na(res$pattern[3, 2]))
    expect_true(is.na(res$pattern[3, 3]))

    expect_length(class(res$valid), 1)
    expect_true(all(class(res$valid) %in% c("logical")))
    expect_false(res$valid)
    expect_length(class(res$message), 1)
    expect_true(all(class(res$message) %in% c("character")))
    expect_equal(res$message, "Invalid: some pattern counts below threshold (3) have been suppressed")
})

#
# Done
#

# context("mdPatternDS::disc::shutdown")

# context("mdPatternDS::disc::done")
