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

context("corDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("corDS::smk::pairwise without na")
test_that("simple corDS, pairwise, full", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    use <- 'pairwise.complete'

    res <- corDS("x", "y", use)

    expect_equal(class(res), "list")
    expect_length(res, 7)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.products), 1)
        expect_true("matrix" %in% class(res$sums.of.products))
    }
    else
    {
        expect_length(class(res$sums.of.products), 2)
        expect_true("matrix" %in% class(res$sums.of.products))
        expect_true("array" %in% class(res$sums.of.products))
    }
    expect_length(res$sums.of.products, 4)

    expect_equal(res$sums.of.products[1], 140.0)
    expect_equal(res$sums.of.products[2], 140.0)
    expect_equal(res$sums.of.products[3], 140.0)
    expect_equal(res$sums.of.products[4], 140.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums), 1)
        expect_true("matrix" %in% class(res$sums))
    }
    else
    {
        expect_length(class(res$sums), 2)
        expect_true("matrix" %in% class(res$sums))
        expect_true("array" %in% class(res$sums))
    }
    expect_length(res$sums, 4)

    expect_equal(res$sums[1], 28.0)
    expect_equal(res$sums[2], 28.0)
    expect_equal(res$sums[3], 28.0)
    expect_equal(res$sums[4], 28.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$complete.counts), 1)
        expect_true("matrix" %in% class(res$complete.counts))
    }
    else
    {
        expect_length(class(res$complete.counts), 2)
        expect_true("matrix" %in% class(res$complete.counts))
        expect_true("array" %in% class(res$complete.counts))
    }
    expect_length(res$complete.counts, 4)

    expect_equal(res$complete.counts[1], 8.0)
    expect_equal(res$complete.counts[2], 8.0)
    expect_equal(res$complete.counts[3], 8.0)
    expect_equal(res$complete.counts[4], 8.0)

    expect_equal(class(res$na.counts), "list")
    expect_length(res$na.counts, 2)

    expect_equal(class(res$errorMessage), "logical")
    expect_length(res$errorMessage, 1)

    expect_true(is.na(res$errorMessage))

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$vars), 1)
        expect_true("matrix" %in% class(res$vars))
    }
    else
    {
        expect_length(class(res$vars), 2)
        expect_true("matrix" %in% class(res$vars))
        expect_true("array" %in% class(res$vars))
    }
    expect_length(res$vars, 4)

    expect_equal(res$vars[1], 6.0)
    expect_equal(res$vars[2], 6.0)
    expect_equal(res$vars[3], 6.0)
    expect_equal(res$vars[4], 6.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.squares), 1)
        expect_true("matrix" %in% class(res$sums.of.squares))
    }
    else
    {
        expect_length(class(res$sums.of.squares), 2)
        expect_true("matrix" %in% class(res$sums.of.squares))
        expect_true("array" %in% class(res$sums.of.squares))
    }
    expect_length(res$sums.of.squares, 4)

    expect_equal(res$sums.of.squares[1], 140.0)
    expect_equal(res$sums.of.squares[2], 140.0)
    expect_equal(res$sums.of.squares[3], 140.0)
    expect_equal(res$sums.of.squares[4], 140.0)
})

test_that("simple corDS, pairwise, neg. full", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0)
    use <- 'pairwise.complete'

    res <- corDS("x", "y", use)

    expect_equal(class(res), "list")
    expect_length(res, 7)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.products), 1)
        expect_true("matrix" %in% class(res$sums.of.products))
    }
    else
    {
        expect_length(class(res$sums.of.products), 2)
        expect_true("matrix" %in% class(res$sums.of.products))
        expect_true("array" %in% class(res$sums.of.products))
    }
    expect_length(res$sums.of.products, 4)

    expect_equal(res$sums.of.products[1], 140.0)
    expect_equal(res$sums.of.products[2], 56.0)
    expect_equal(res$sums.of.products[3], 56.0)
    expect_equal(res$sums.of.products[4], 140.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums), 1)
        expect_true("matrix" %in% class(res$sums))
    }
    else
    {
        expect_length(class(res$sums), 2)
        expect_true("matrix" %in% class(res$sums))
        expect_true("array" %in% class(res$sums))
    }
    expect_length(res$sums, 4)

    expect_equal(res$sums[1], 28.0)
    expect_equal(res$sums[2], 28.0)
    expect_equal(res$sums[3], 28.0)
    expect_equal(res$sums[4], 28.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$complete.counts), 1)
        expect_true("matrix" %in% class(res$complete.counts))
    }
    else
    {
        expect_length(class(res$complete.counts), 2)
        expect_true("matrix" %in% class(res$complete.counts))
        expect_true("array" %in% class(res$complete.counts))
    }
    expect_length(res$complete.counts, 4)

    expect_equal(res$complete.counts[1], 8.0)
    expect_equal(res$complete.counts[2], 8.0)
    expect_equal(res$complete.counts[3], 8.0)
    expect_equal(res$complete.counts[4], 8.0)

    expect_equal(class(res$na.counts), "list")
    expect_length(res$na.counts, 2)

    expect_equal(class(res$errorMessage), "logical")
    expect_length(res$errorMessage, 1)

    expect_true(is.na(res$errorMessage))

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$vars), 1)
        expect_true("matrix" %in% class(res$vars))
    }
    else
    {
        expect_length(class(res$vars), 2)
        expect_true("matrix" %in% class(res$vars))
        expect_true("array" %in% class(res$vars))
    }
    expect_length(res$vars, 4)

    expect_equal(res$vars[1], 6.0)
    expect_equal(res$vars[2], 6.0)
    expect_equal(res$vars[3], 6.0)
    expect_equal(res$vars[4], 6.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.squares), 1)
        expect_true("matrix" %in% class(res$sums.of.squares))
    }
    else
    {
        expect_length(class(res$sums.of.squares), 2)
        expect_true("matrix" %in% class(res$sums.of.squares))
        expect_true("array" %in% class(res$sums.of.squares))
    }
    expect_length(res$sums.of.squares, 4)

    expect_equal(res$sums.of.squares[1], 140.0)
    expect_equal(res$sums.of.squares[2], 140.0)
    expect_equal(res$sums.of.squares[3], 140.0)
    expect_equal(res$sums.of.squares[4], 140.0)
})

test_that("simple corDS, pairwise, some", {
    x   <- c(0.1, 1.0, 1.9, 3.0, 4.0, 5.1, 6.0, 7.0)
    y   <- c(0.0, 1.2, 2.0, 2.9, 4.0, 5.0, 6.1, 7.0)
    use <- 'pairwise.complete'

    res <- corDS("x", "y", use)

    expect_equal(class(res), "list")
    expect_length(res, 7)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.products), 1)
        expect_true("matrix" %in% class(res$sums.of.products))
    }
    else
    {
        expect_length(class(res$sums.of.products), 2)
        expect_true("matrix" %in% class(res$sums.of.products))
        expect_true("array" %in% class(res$sums.of.products))
    }
    expect_length(res$sums.of.products, 4)

    expect_equal(res$sums.of.products[1], 140.63)
    expect_equal(res$sums.of.products[2], 140.8)
    expect_equal(res$sums.of.products[3], 140.8)
    expect_equal(res$sums.of.products[4], 141.06)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums), 1)
        expect_true("matrix" %in% class(res$sums))
    }
    else
    {
        expect_length(class(res$sums), 2)
        expect_true("matrix" %in% class(res$sums))
        expect_true("array" %in% class(res$sums))
    }

    expect_length(res$sums, 4)

    expect_equal(res$sums[1], 28.1)
    expect_equal(res$sums[2], 28.2)
    expect_equal(res$sums[3], 28.1)
    expect_equal(res$sums[4], 28.2)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$complete.counts), 1)
        expect_true("matrix" %in% class(res$complete.counts))
    }
    else
    {
        expect_length(class(res$complete.counts), 2)
        expect_true("matrix" %in% class(res$complete.counts))
        expect_true("array" %in% class(res$complete.counts))
    }
    expect_length(res$complete.counts, 4)

    expect_equal(res$complete.counts[1], 8.0)
    expect_equal(res$complete.counts[2], 8.0)
    expect_equal(res$complete.counts[3], 8.0)
    expect_equal(res$complete.counts[4], 8.0)

    expect_equal(class(res$na.counts), "list")
    expect_length(res$na.counts, 2)

    expect_equal(class(res$errorMessage), "logical")
    expect_length(res$errorMessage, 1)

    expect_true(is.na(res$errorMessage))

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$vars), 1)
        expect_true("matrix" %in% class(res$vars))
    }
    else
    {
        expect_length(class(res$vars), 2)
        expect_true("matrix" %in% class(res$vars))
        expect_true("array" %in% class(res$vars))
    }
    expect_length(res$vars, 4)

    expect_equal(res$vars[1], 5.989820,  tolerance = 1e-6)
    expect_equal(res$vars[2], 5.950714, tolerance = 1e-6)
    expect_equal(res$vars[3], 5.989820,  tolerance = 1e-6)
    expect_equal(res$vars[4], 5.950714, tolerance = 1e-6)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.squares), 1)
        expect_true("matrix" %in% class(res$sums.of.squares))
    }
    else
    {
        expect_length(class(res$sums.of.squares), 2)
        expect_true("matrix" %in% class(res$sums.of.squares))
        expect_true("array" %in% class(res$sums.of.squares))
    }
    expect_length(res$sums.of.squares, 4)

    expect_equal(res$sums.of.squares[1], 140.63)
    expect_equal(res$sums.of.squares[2], 141.06)
    expect_equal(res$sums.of.squares[3], 140.63)
    expect_equal(res$sums.of.squares[4], 141.06)
})

context("corDS::smk::pairwise with na")
test_that("simple corDS, pairwise, some", {
    x   <- c(0.0, NA, 2.0, 3.0, NA, 5.0, NA, 7.0)
    y   <- c(0.0, 1.0, NA, 3.0, 4.0, NA, NA, 7.0)
    use <- 'pairwise.complete'

    res <- corDS("x", "y", use)

    expect_equal(class(res), "list")
    expect_length(res, 7)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.products), 1)
        expect_true("matrix" %in% class(res$sums.of.products))
    }
    else
    {
        expect_length(class(res$sums.of.products), 2)
        expect_true("matrix" %in% class(res$sums.of.products))
        expect_true("array" %in% class(res$sums.of.products))
    }
    expect_length(res$sums.of.products, 4)

    expect_equal(res$sums.of.products[1], 87.0)
    expect_equal(res$sums.of.products[2], 58.0)
    expect_equal(res$sums.of.products[3], 58.0)
    expect_equal(res$sums.of.products[4], 75.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums), 1)
        expect_true("matrix" %in% class(res$sums))
    }
    else
    {
        expect_length(class(res$sums), 2)
        expect_true("matrix" %in% class(res$sums))
        expect_true("array" %in% class(res$sums))
    }
    expect_length(res$sums, 4)

    expect_equal(res$sums[1], 17.0)
    expect_equal(res$sums[2], 10.0)
    expect_equal(res$sums[3], 10.0)
    expect_equal(res$sums[4], 15.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$complete.counts), 1)
        expect_true("matrix" %in% class(res$complete.counts))
    }
    else
    {
        expect_length(class(res$complete.counts), 2)
        expect_true("matrix" %in% class(res$complete.counts))
        expect_true("array" %in% class(res$complete.counts))
    }
    expect_length(res$complete.counts, 4)

    expect_equal(res$complete.counts[1], 5.0)
    expect_equal(res$complete.counts[2], 3.0)
    expect_equal(res$complete.counts[3], 3.0)
    expect_equal(res$complete.counts[4], 5.0)

    expect_equal(class(res$na.counts), "list")
    expect_length(res$na.counts, 2)

    expect_equal(class(res$errorMessage), "logical")
    expect_length(res$errorMessage, 1)

    expect_true(is.na(res$errorMessage))

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$vars), 1)
        expect_true("matrix" %in% class(res$vars))
    }
    else
    {
        expect_length(class(res$vars), 2)
        expect_true("matrix" %in% class(res$vars))
        expect_true("array" %in% class(res$vars))
    }
    expect_length(res$vars, 4)

    expect_equal(res$vars[1], 7.3,       tolerance = 1e-6)
    expect_equal(res$vars[2], 12.333333, tolerance = 1e-6)
    expect_equal(res$vars[3], 12.333333, tolerance = 1e-6)
    expect_equal(res$vars[4], 7.5,       tolerance = 1e-6)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.squares), 1)
        expect_true("matrix" %in% class(res$sums.of.squares))
    }
    else
    {
        expect_length(class(res$sums.of.squares), 2)
        expect_true("matrix" %in% class(res$sums.of.squares))
        expect_true("array" %in% class(res$sums.of.squares))
    }
    expect_length(res$sums.of.squares, 4)

    expect_equal(res$sums.of.squares[1], 87.0)
    expect_equal(res$sums.of.squares[2], 58.0)
    expect_equal(res$sums.of.squares[3], 58.0)
    expect_equal(res$sums.of.squares[4], 75.0)
})

context("corDS::smk::casewise without na")
test_that("simple corDS, casewise, full", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    use <- 'casewise.complete'

    res <- corDS("x", "y", use)

    expect_equal(class(res), "list")
    expect_length(res, 7)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.products), 1)
        expect_true("matrix" %in% class(res$sums.of.products))
    }
    else
    {
        expect_length(class(res$sums.of.products), 2)
        expect_true("matrix" %in% class(res$sums.of.products))
        expect_true("array" %in% class(res$sums.of.products))
    }
    expect_length(res$sums.of.products, 4)

    expect_equal(res$sums.of.products[1], 140.0)
    expect_equal(res$sums.of.products[2], 140.0)
    expect_equal(res$sums.of.products[3], 140.0)
    expect_equal(res$sums.of.products[4], 140.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums), 1)
        expect_true("matrix" %in% class(res$sums))
    }
    else
    {
        expect_length(class(res$sums), 2)
        expect_true("matrix" %in% class(res$sums))
        expect_true("array" %in% class(res$sums))
    }
    expect_length(res$sums, 2)

    expect_equal(res$sums[1], 28.0)
    expect_equal(res$sums[2], 28.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$complete.counts), 1)
        expect_true("matrix" %in% class(res$complete.counts))
    }
    else
    {
        expect_length(class(res$complete.counts), 2)
        expect_true("matrix" %in% class(res$complete.counts))
        expect_true("array" %in% class(res$complete.counts))
    }
    expect_length(res$complete.counts, 4)

    expect_equal(res$complete.counts[1], 8.0)
    expect_equal(res$complete.counts[2], 8.0)
    expect_equal(res$complete.counts[3], 8.0)
    expect_equal(res$complete.counts[4], 8.0)

    expect_equal(class(res$na.counts), "list")
    expect_length(res$na.counts, 2)

    expect_equal(class(res$errorMessage), "logical")
    expect_length(res$errorMessage, 1)

    expect_true(is.na(res$errorMessage))

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$vars), 1)
        expect_true("matrix" %in% class(res$vars))
    }
    else
    {
        expect_length(class(res$vars), 2)
        expect_true("matrix" %in% class(res$vars))
        expect_true("array" %in% class(res$vars))
    }
    expect_length(res$vars, 2)

    expect_equal(res$vars[1], 6.0)
    expect_equal(res$vars[2], 6.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.squares), 1)
        expect_true("matrix" %in% class(res$sums.of.squares))
    }
    else
    {
        expect_length(class(res$sums.of.squares), 2)
        expect_true("matrix" %in% class(res$sums.of.squares))
        expect_true("array" %in% class(res$sums.of.squares))
    }
    expect_length(res$sums.of.squares, 4)

    expect_equal(res$sums.of.squares[1], 140.0)
    expect_equal(res$sums.of.squares[2], 140.0)
    expect_equal(res$sums.of.squares[3], 140.0)
    expect_equal(res$sums.of.squares[4], 140.0)
})

test_that("simple corDS, casewise, neg. full", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0)
    use <- 'casewise.complete'

    res <- corDS("x", "y", use)

    expect_equal(class(res), "list")
    expect_length(res, 7)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.products), 1)
        expect_true("matrix" %in% class(res$sums.of.products))
    }
    else
    {
        expect_length(class(res$sums.of.products), 2)
        expect_true("matrix" %in% class(res$sums.of.products))
        expect_true("array" %in% class(res$sums.of.products))
    }
    expect_length(res$sums.of.products, 4)

    expect_equal(res$sums.of.products[1], 140.0)
    expect_equal(res$sums.of.products[2], 56.0)
    expect_equal(res$sums.of.products[3], 56.0)
    expect_equal(res$sums.of.products[4], 140.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums), 1)
        expect_true("matrix" %in% class(res$sums))
    }
    else
    {
        expect_length(class(res$sums), 2)
        expect_true("matrix" %in% class(res$sums))
        expect_true("array" %in% class(res$sums))
    }
    expect_length(res$sums, 2)

    expect_equal(res$sums[1], 28.0)
    expect_equal(res$sums[2], 28.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$complete.counts), 1)
        expect_true("matrix" %in% class(res$complete.counts))
    }
    else
    {
        expect_length(class(res$complete.counts), 2)
        expect_true("matrix" %in% class(res$complete.counts))
        expect_true("array" %in% class(res$complete.counts))
    }
    expect_length(res$complete.counts, 4)

    expect_equal(res$complete.counts[1], 8.0)
    expect_equal(res$complete.counts[2], 8.0)
    expect_equal(res$complete.counts[3], 8.0)
    expect_equal(res$complete.counts[4], 8.0)

    expect_equal(class(res$na.counts), "list")
    expect_length(res$na.counts, 2)

    expect_equal(class(res$errorMessage), "logical")
    expect_length(res$errorMessage, 1)

    expect_true(is.na(res$errorMessage))

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$vars), 1)
        expect_true("matrix" %in% class(res$vars))
    }
    else
    {
        expect_length(class(res$vars), 2)
        expect_true("matrix" %in% class(res$vars))
        expect_true("array" %in% class(res$vars))
    }
    expect_length(res$vars, 2)

    expect_equal(res$vars[1], 6.0)
    expect_equal(res$vars[2], 6.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.squares), 1)
        expect_true("matrix" %in% class(res$sums.of.squares))
    }
    else
    {
        expect_length(class(res$sums.of.squares), 2)
        expect_true("matrix" %in% class(res$sums.of.squares))
        expect_true("array" %in% class(res$sums.of.squares))
    }
    expect_length(res$sums.of.squares, 4)

    expect_equal(res$sums.of.squares[1], 140.0)
    expect_equal(res$sums.of.squares[2], 140.0)
    expect_equal(res$sums.of.squares[3], 140.0)
    expect_equal(res$sums.of.squares[4], 140.0)
})

test_that("simple corDS, casewise, some", {
    x   <- c(0.1, 1.0, 1.9, 3.0, 4.0, 5.1, 6.0, 7.0)
    y   <- c(0.0, 1.2, 2.0, 2.9, 4.0, 5.0, 6.1, 7.0)
    use <- 'casewise.complete'

    res <- corDS("x", "y", use)

    expect_equal(class(res), "list")
    expect_length(res, 7)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.products), 1)
        expect_true("matrix" %in% class(res$sums.of.products))
    }
    else
    {
        expect_length(class(res$sums.of.products), 2)
        expect_true("matrix" %in% class(res$sums.of.products))
        expect_true("array" %in% class(res$sums.of.products))
    }
    expect_length(res$sums.of.products, 4)

    expect_equal(res$sums.of.products[1], 140.63)
    expect_equal(res$sums.of.products[2], 140.8)
    expect_equal(res$sums.of.products[3], 140.8)
    expect_equal(res$sums.of.products[4], 141.06)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums), 1)
        expect_true("matrix" %in% class(res$sums))
    }
    else
    {
        expect_length(class(res$sums), 2)
        expect_true("matrix" %in% class(res$sums))
        expect_true("array" %in% class(res$sums))
    }
    expect_length(res$sums, 2)

    expect_equal(res$sums[1], 28.1)
    expect_equal(res$sums[2], 28.2)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$complete.counts), 1)
        expect_true("matrix" %in% class(res$complete.counts))
    }
    else
    {
        expect_length(class(res$complete.counts), 2)
        expect_true("matrix" %in% class(res$complete.counts))
        expect_true("array" %in% class(res$complete.counts))
    }
    expect_length(res$complete.counts, 4)

    expect_equal(res$complete.counts[1], 8.0)
    expect_equal(res$complete.counts[2], 8.0)
    expect_equal(res$complete.counts[3], 8.0)
    expect_equal(res$complete.counts[4], 8.0)

    expect_equal(class(res$na.counts), "list")
    expect_length(res$na.counts, 2)

    expect_equal(class(res$errorMessage), "logical")
    expect_length(res$errorMessage, 1)

    expect_true(is.na(res$errorMessage))

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$vars), 1)
        expect_true("matrix" %in% class(res$vars))
    }
    else
    {
        expect_length(class(res$vars), 2)
        expect_true("matrix" %in% class(res$vars))
        expect_true("array" %in% class(res$vars))
    }
    expect_length(res$vars, 2)

    expect_equal(res$vars[1], 5.989820, tolerance = 1e-6)
    expect_equal(res$vars[2], 5.950714, tolerance = 1e-6)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.squares), 1)
        expect_true("matrix" %in% class(res$sums.of.squares))
    }
    else
    {
        expect_length(class(res$sums.of.squares), 2)
        expect_true("matrix" %in% class(res$sums.of.squares))
        expect_true("array" %in% class(res$sums.of.squares))
    }
    expect_length(res$sums.of.squares, 4)

    expect_equal(res$sums.of.squares[1], 140.63)
    expect_equal(res$sums.of.squares[2], 141.06)
    expect_equal(res$sums.of.squares[3], 140.63)
    expect_equal(res$sums.of.squares[4], 141.06)
})

context("corDS::smk::casewise with na")
test_that("simple corDS, casewise, some", {
    x   <- c(0.0, NA, 2.0, 3.0, NA, 5.0, NA, 7.0)
    y   <- c(0.0, 1.0, NA, 3.0, 4.0, NA, NA, 7.0)
    use <- 'casewise.complete'

    res <- corDS("x", "y", use)

    expect_equal(class(res), "list")
    expect_length(res, 7)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.products), 1)
        expect_true("matrix" %in% class(res$sums.of.products))
    }
    else
    {
        expect_length(class(res$sums.of.products), 2)
        expect_true("matrix" %in% class(res$sums.of.products))
        expect_true("array" %in% class(res$sums.of.products))
    }
    expect_length(res$sums.of.products, 4)

    expect_equal(res$sums.of.products[1], 58.0)
    expect_equal(res$sums.of.products[2], 58.0)
    expect_equal(res$sums.of.products[3], 58.0)
    expect_equal(res$sums.of.products[4], 58.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums), 1)
        expect_true("matrix" %in% class(res$sums))
    }
    else
    {
        expect_length(class(res$sums), 2)
        expect_true("matrix" %in% class(res$sums))
        expect_true("array" %in% class(res$sums))
    }
    expect_length(res$sums, 2)

    expect_equal(res$sums[1], 10.0)
    expect_equal(res$sums[2], 10.0)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$complete.counts), 1)
        expect_true("matrix" %in% class(res$complete.counts))
    }
    else
    {
        expect_length(class(res$complete.counts), 2)
        expect_true("matrix" %in% class(res$complete.counts))
        expect_true("array" %in% class(res$complete.counts))
    }
    expect_length(res$complete.counts, 4)

    expect_equal(res$complete.counts[1], 3.0)
    expect_equal(res$complete.counts[2], 3.0)
    expect_equal(res$complete.counts[3], 3.0)
    expect_equal(res$complete.counts[4], 3.0)

    expect_equal(class(res$na.counts), "list")
    expect_length(res$na.counts, 2)

    expect_equal(class(res$errorMessage), "logical")
    expect_length(res$errorMessage, 1)

    expect_true(is.na(res$errorMessage))

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$vars), 1)
        expect_true("matrix" %in% class(res$vars))
    }
    else
    {
        expect_length(class(res$vars), 2)
        expect_true("matrix" %in% class(res$vars))
        expect_true("array" %in% class(res$vars))
    }
    expect_length(res$vars, 2)

    expect_equal(res$vars[1], 12.333333, tolerance = 1e-6)
    expect_equal(res$vars[2], 12.333333, tolerance = 1e-6)

    if (base::getRversion() < 4.0)
    {
        expect_length(class(res$sums.of.squares), 1)
        expect_true("matrix" %in% class(res$sums.of.squares))
    }
    else
    {
        expect_length(class(res$sums.of.squares), 2)
        expect_true("matrix" %in% class(res$sums.of.squares))
        expect_true("array" %in% class(res$sums.of.squares))
    }
    expect_length(res$sums.of.squares, 4)

    expect_equal(res$sums.of.squares[1], 58.0)
    expect_equal(res$sums.of.squares[2], 58.0)
    expect_equal(res$sums.of.squares[3], 58.0)
    expect_equal(res$sums.of.squares[4], 58.0)
})

#
# Done
#

context("corDS::smk::shutdown")

context("corDS::smk::done")
