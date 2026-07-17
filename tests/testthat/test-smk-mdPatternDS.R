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

# context("mdPatternDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("mdPatternDS::smk::sample 1 complete data.frame")
test_that("mdPatternDS: sample 1 complete data.frame", {
    x_val <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))
    x     <- "x_val"

    res <- mdPatternDS(x)

    expect_length(res, 3)
    expect_length(class(res), 1)
    expect_true(all(class(res) %in% c("list")))
    expect_length(class(res$pattern), 2)
    expect_true(all(class(res$pattern) %in% c("matrix", "array")))

    expect_length(colnames(res$pattern), 3)
    expect_equal(colnames(res$pattern)[1], "v1")
    expect_equal(colnames(res$pattern)[2], "v2")
    expect_equal(colnames(res$pattern)[3], "")
    expect_length(rownames(res$pattern), 2)
    expect_equal(rownames(res$pattern)[1], "5")
    expect_equal(rownames(res$pattern)[2], "")

    expect_equal(res$pattern[1, 1], 1)
    expect_equal(res$pattern[1, 2], 1)
    expect_equal(res$pattern[1, 3], 0)
    expect_equal(res$pattern[2, 1], 0)
    expect_equal(res$pattern[2, 2], 0)
    expect_equal(res$pattern[2, 3], 0)
	
    expect_length(class(res$valid), 1)
    expect_true(all(class(res$valid) %in% c("logical")))
    expect_true(res$valid)
    expect_length(class(res$message), 1)
    expect_true(all(class(res$message) %in% c("character")))
    expect_equal(res$message, "Valid: all pattern counts meet disclosure requirements")
})


# context("mdPatternDS::smk::sample 2 complete data.frame")
test_that("mdPatternDS: sample 2 complete data.frame", {
    x_val <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0), v2 = c(9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0))
    x     <- "x_val"

    res <- mdPatternDS(x)

    expect_length(res, 3)
    expect_length(class(res), 1)
    expect_true(all(class(res) %in% c("list")))
    expect_length(class(res$pattern), 2)
    expect_true(all(class(res$pattern) %in% c("matrix", "array")))

    expect_length(colnames(res$pattern), 3)
    expect_equal(colnames(res$pattern)[1], "v1")
    expect_equal(colnames(res$pattern)[2], "v2")
    expect_equal(colnames(res$pattern)[3], "")
    expect_length(rownames(res$pattern), 2)
    expect_equal(rownames(res$pattern)[1], "10")
    expect_equal(rownames(res$pattern)[2], "")
    expect_true(is.na(rownames(res$pattern)[3]))

    expect_equal(res$pattern[1, 1], 1)
    expect_equal(res$pattern[1, 2], 1)
    expect_equal(res$pattern[1, 3], 0)
    expect_equal(res$pattern[2, 1], 0)
    expect_equal(res$pattern[2, 2], 0)
    expect_equal(res$pattern[2, 3], 0)
	
    expect_length(class(res$valid), 1)
    expect_true(all(class(res$valid) %in% c("logical")))
    expect_true(res$valid)
    expect_length(class(res$message), 1)
    expect_true(all(class(res$message) %in% c("character")))
    expect_equal(res$message, "Valid: all pattern counts meet disclosure requirements")
})

#
# Done
#

# context("mdPatternDS::smk::shutdown")

# context("mdPatternDS::smk::done")
