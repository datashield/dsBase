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

    expect_length(res, 4)
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

    expect_length(res, 4)
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

test_that("mdPatternDS throws error when object does not exist", {
    expect_error(mdPatternDS("nonexistent_object"), regexp = "does not exist")
})

test_that("mdPatternDS throws error when object is the wrong type", {
    bad_input <- c("a", "b", "c")
    expect_error(mdPatternDS("bad_input"), regexp = "must be of type data.frame or matrix")
})

test_that("mdPatternDS gives the same result for a matrix as for a data.frame", {
    x.df <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))
    x.matrix <- as.matrix(x.df)

    res.df <- mdPatternDS("x.df")
    res.matrix <- mdPatternDS("x.matrix")

    expect_true("matrix" %in% res.matrix$class)
    expect_equal(res.matrix[names(res.matrix) != "class"], res.df[names(res.df) != "class"])
})

#
# Done
#

# context("mdPatternDS::smk::shutdown")

# context("mdPatternDS::smk::done")
