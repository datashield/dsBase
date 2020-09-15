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

context("covDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("covDS::smk::casewise.complete")
test_that("numeric covDS, casewise.complete", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), v2 = c(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0))

    res <- covDS("input$v1", "input$v2", "casewise.complete")

    expect_length(res, 5)
    expect_equal(class(res), "list")

    res.sums.of.products.class <- class(res$sums.of.products)

    if (base::getRversion() < 4.0)
    {
        expect_length(res.sums.of.products.class, 1)
        expect_true("matrix" %in% res.sums.of.products.class)
    }
    else
    {
        expect_length(res.sums.of.products.class, 2)
        expect_true("matrix" %in% res.sums.of.products.class)
        expect_true("array" %in% res.sums.of.products.class)
    }

    expect_length(res$sums.of.products, 4)
    expect_equal(res$sums.of.products[1], 140.0)
    expect_equal(res$sums.of.products[2], 56.0)
    expect_equal(res$sums.of.products[3], 56.0)
    expect_equal(res$sums.of.products[4], 140.0)

    res.sums.class <- class(res$sums)

    if (base::getRversion() < 4.0)
    {
        expect_length(res.sums.class, 1)
        expect_true("matrix" %in% res.sums.class)
    }
    else
    {
        expect_length(res.sums.class, 2)
        expect_true("matrix" %in% res.sums.class)
        expect_true("array" %in% res.sums.class)
    }

    expect_length(res$sums, 2)
    expect_equal(res$sums[1], 28.0)
    expect_equal(res$sums[2], 28.0)

    res.complete.counts.class <- class(res$complete.counts)

    if (base::getRversion() < 4.0)
    {
        expect_length(res.complete.counts.class, 1)
        expect_true("matrix" %in% res.complete.counts.class)
    }
    else
    {
        expect_length(res.complete.counts.class, 2)
        expect_true("matrix" %in% res.complete.counts.class)
        expect_true("array" %in% res.complete.counts.class)
    }

    expect_length(res$complete.counts, 4)
    expect_equal(res$complete.counts[1], 8.0)
    expect_equal(res$complete.counts[2], 8.0)
    expect_equal(res$complete.counts[3], 8.0)
    expect_equal(res$complete.counts[4], 8.0)

    res.na.counts.class <- class(res$na.counts)

    expect_length(res.na.counts.class, 1)
    expect_equal(res.na.counts.class, "list")

    expect_length(res$na.counts, 2)

    res$na.counts.names <- names(res$na.counts)
    expect_length(res$na.counts.names, 2)
    expect_equal(res$na.counts.names[1], "Number of NAs in each column")
    expect_equal(res$na.counts.names[2], "Number of NAs casewise")

    res.na.counts.1.class <- class(res$na.counts[1])

    expect_length(res.na.counts.1.class, 1)
    expect_equal(res.na.counts.1.class, "list")

    expect_length(res$na.counts[[1]], 2)
    expect_equal(res$na.counts[[1]][1], 0)

    res.na.counts.2.class <- class(res$na.counts[2])

    expect_length(res.na.counts.2.class, 1)
    expect_equal(res.na.counts.2.class, "list")

    expect_length(res$na.counts[[2]], 1)
    expect_equal(res$na.counts[[2]][1], 0.0)

    res.errorMessage.class <- class(res$errorMessage)

    expect_length(res.errorMessage.class, 1)
    expect_equal(res.errorMessage.class, "logical")

    expect_true(is.na(res$errorMessage))
})

context("covDS::smk::pairwise.complete")
test_that("numeric covDS, pairwise.complete", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), v2 = c(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0))

    res <- covDS("input$v1", "input$v2", "pairwise.complete")

    expect_length(res, 5)
    expect_equal(class(res), "list")

    res.sums.of.products.class <- class(res$sums.of.products)

    if (base::getRversion() < 4.0)
    {
        expect_length(res.sums.of.products.class, 1)
        expect_true("matrix" %in% res.sums.of.products.class)
    }
    else
    {
        expect_length(res.sums.of.products.class, 2)
        expect_true("matrix" %in% res.sums.of.products.class)
        expect_true("array" %in% res.sums.of.products.class)
    }

    expect_length(res$sums.of.products, 4)
    expect_equal(res$sums.of.products[1], 140.0)
    expect_equal(res$sums.of.products[2], 56.0)
    expect_equal(res$sums.of.products[3], 56.0)
    expect_equal(res$sums.of.products[4], 140.0)

    res.sums.class <- class(res$sums)

    if (base::getRversion() < 4.0)
    {
        expect_length(res.sums.class, 1)
        expect_true("matrix" %in% res.sums.class)
    }
    else
    {
        expect_length(res.sums.class, 2)
        expect_true("matrix" %in% res.sums.class)
        expect_true("array" %in% res.sums.class)
    }

    expect_length(res$sums, 4)
    expect_equal(res$sums[1], 28.0)
    expect_equal(res$sums[2], 28.0)
    expect_equal(res$sums[3], 28.0)
    expect_equal(res$sums[4], 28.0)

    res.complete.counts.class <- class(res$complete.counts)

    if (base::getRversion() < 4.0)
    {
        expect_length(res.complete.counts.class, 1)
        expect_true("matrix" %in% res.complete.counts.class)
    }
    else
    {
        expect_length(res.complete.counts.class, 2)
        expect_true("matrix" %in% res.complete.counts.class)
        expect_true("array" %in% res.complete.counts.class)
    }

    expect_length(res$complete.counts, 4)
    expect_equal(res$complete.counts[1], 8.0)
    expect_equal(res$complete.counts[2], 8.0)
    expect_equal(res$complete.counts[3], 8.0)
    expect_equal(res$complete.counts[4], 8.0)

    res.na.counts.class <- class(res$na.counts)

    expect_length(res.na.counts.class, 1)
    expect_equal(res.na.counts.class, "list")

    expect_length(res$na.counts, 2)

    res$na.counts.names <- names(res$na.counts)
    expect_length(res$na.counts.names, 2)
    expect_equal(res$na.counts.names[1], "Number of NAs in each column")
    expect_equal(res$na.counts.names[2], "Number of NAs pairwise")

    res.na.counts.1.class <- class(res$na.counts[1])

    expect_length(res.na.counts.1.class, 1)
    expect_equal(res.na.counts.1.class, "list")

    expect_length(res$na.counts[[1]], 2)
    expect_equal(res$na.counts[[1]][1], 0)

    res.na.counts.2.class <- class(res$na.counts[2])

    expect_length(res.na.counts.2.class, 1)
    expect_equal(res.na.counts.2.class, "list")

    expect_length(res$na.counts[[2]], 4)
    expect_equal(res$na.counts[[2]][1], 0.0)
    expect_equal(res$na.counts[[2]][2], 0.0)
    expect_equal(res$na.counts[[2]][3], 0.0)
    expect_equal(res$na.counts[[2]][4], 0.0)

    res.errorMessage.class <- class(res$errorMessage)

    expect_length(res.errorMessage.class, 1)
    expect_equal(res.errorMessage.class, "logical")

    expect_true(is.na(res$errorMessage))
})

#
# Done
#

context("covDS::smk::shutdown")

context("covDS::smk::done")
