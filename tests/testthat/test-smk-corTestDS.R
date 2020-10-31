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

context("corTestDS::smk::setup")

#
# Tests
#

context("corTestDS::smk::without na")
test_that("simple corTestDS, full", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)

    res <- corTestDS("x", "y")

    expect_equal(class(res), "htest")
    expect_length(res, 9)

    expect_equal(class(res$statistic), "numeric")
    expect_length(res$statistic, 1)
    expect_true(is.infinite(res$statistic[[1]]))

    expect_equal(class(res$parameter), "integer")
    expect_length(res$parameter, 1)
    expect_equal(res$parameter[[1]], 6L)

    expect_equal(class(res$p.value), "numeric")
    expect_length(res$p.value, 1)
    expect_equal(res$p.value[[1]], 0.0)

    expect_equal(class(res$estimate), "numeric")
    expect_length(res$estimate, 1)
    expect_equal(res$estimate[[1]], 1.0)

    expect_equal(class(res$null.value), "numeric")
    expect_length(res$null.value, 1)
    expect_equal(res$null.value[[1]], 0.0)

    expect_equal(class(res$alternative), "character")
    expect_length(res$alternative, 1)
    expect_equal(res$alternative[[1]], "two.sided")

    expect_equal(class(res$method), "character")
    expect_length(res$method, 1)
    expect_equal(res$method[[1]], "Pearson's product-moment correlation")

    expect_equal(class(res$data.name), "character")
    expect_length(res$data.name, 1)
    expect_equal(res$data.name[[1]], "x.var and y.var")

    expect_equal(class(res$conf.int), "numeric")
    expect_length(res$conf.int, 2)
    expect_equal(res$conf.int[[1]], 1.0)
    expect_equal(res$conf.int[[2]], 1.0)
})

test_that("simple corTestDS, neg. full", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0)

    res <- corTestDS("x", "y")

    expect_equal(class(res), "htest")
    expect_length(res, 9)

    expect_equal(class(res$statistic), "numeric")
    expect_length(res$statistic, 1)
    expect_true(is.infinite(res$statistic[[1]]))

    expect_equal(class(res$parameter), "integer")
    expect_length(res$parameter, 1)
    expect_equal(res$parameter[[1]], 6L)

    expect_equal(class(res$p.value), "numeric")
    expect_length(res$p.value, 1)
    expect_equal(res$p.value[[1]], 0.0)

    expect_equal(class(res$estimate), "numeric")
    expect_length(res$estimate, 1)
    expect_equal(res$estimate[[1]], -1.0)

    expect_equal(class(res$null.value), "numeric")
    expect_length(res$null.value, 1)
    expect_equal(res$null.value[[1]], 0.0)

    expect_equal(class(res$alternative), "character")
    expect_length(res$alternative, 1)
    expect_equal(res$alternative[[1]], "two.sided")

    expect_equal(class(res$method), "character")
    expect_length(res$method, 1)
    expect_equal(res$method[[1]], "Pearson's product-moment correlation")

    expect_equal(class(res$data.name), "character")
    expect_length(res$data.name, 1)
    expect_equal(res$data.name[[1]], "x.var and y.var")

    expect_equal(class(res$conf.int), "numeric")
    expect_length(res$conf.int, 2)
    expect_equal(res$conf.int[[1]], -1.0)
    expect_equal(res$conf.int[[2]], -1.0)
})

test_that("simple corTestDS, some", {
    x   <- c(0.1, 1.0, 1.9, 3.0, 4.0, 5.1, 6.0, 7.0)
    y   <- c(0.0, 1.2, 2.0, 2.9, 4.0, 5.0, 6.1, 7.0)

    res <- corTestDS("x", "y")

    expect_equal(class(res), "htest")
    expect_length(res, 9)

    expect_equal(class(res$statistic), "numeric")
    expect_length(res$statistic, 1)
    expect_equal(res$statistic[[1]], 53.2466, tolerance = 1e-6)

    expect_equal(class(res$parameter), "integer")
    expect_length(res$parameter, 1)
    expect_equal(res$parameter[[1]], 6L)

    expect_equal(class(res$p.value), "numeric")
    expect_length(res$p.value, 1)
    expect_equal(res$p.value[[1]], 0.0)

    expect_equal(class(res$estimate), "numeric")
    expect_length(res$estimate, 1)
    expect_equal(res$estimate[[1]], 0.998943, tolerance = 1e-6)

    expect_equal(class(res$null.value), "numeric")
    expect_length(res$null.value, 1)
    expect_equal(res$null.value[[1]], 0.0)

    expect_equal(class(res$alternative), "character")
    expect_length(res$alternative, 1)
    expect_equal(res$alternative[[1]], "two.sided")

    expect_equal(class(res$method), "character")
    expect_length(res$method, 1)
    expect_equal(res$method[[1]], "Pearson's product-moment correlation")

    expect_equal(class(res$data.name), "character")
    expect_length(res$data.name, 1)
    expect_equal(res$data.name[[1]], "x.var and y.var")

    expect_equal(class(res$conf.int), "numeric")
    expect_length(res$conf.int, 2)
    expect_equal(res$conf.int[[1]], 0.9939173, tolerance = 1e-6)
    expect_equal(res$conf.int[[2]], 0.9998169, tolerance = 1e-6)
})

context("corTestDS::smk::with na")
test_that("simple corTestDS, some", {
    x   <- c(0.0, NA, 2.0, 3.0, NA, 5.0, NA, 7.0)
    y   <- c(0.0, 1.0, NA, 3.0, 4.0, NA, NA, 7.0)

    res <- corTestDS("x", "y")

    expect_equal(class(res), "htest")
    expect_length(res, 8)

    expect_equal(class(res$statistic), "numeric")
    expect_length(res$statistic, 1)
    expect_true(is.infinite(res$statistic[[1]]))

    expect_equal(class(res$parameter), "integer")
    expect_length(res$parameter, 1)
    expect_equal(res$parameter[[1]], 1L)

    expect_equal(class(res$p.value), "numeric")
    expect_length(res$p.value, 1)
    expect_equal(res$p.value[[1]], 0.0)

    expect_equal(class(res$estimate), "numeric")
    expect_length(res$estimate, 1)
    expect_equal(res$estimate[[1]], 1.0)

    expect_equal(class(res$null.value), "numeric")
    expect_length(res$null.value, 1)
    expect_equal(res$null.value[[1]], 0.0)

    expect_equal(class(res$alternative), "character")
    expect_length(res$alternative, 1)
    expect_equal(res$alternative[[1]], "two.sided")

    expect_equal(class(res$method), "character")
    expect_length(res$method, 1)
    expect_equal(res$method[[1]], "Pearson's product-moment correlation")

    expect_equal(class(res$data.name), "character")
    expect_length(res$data.name, 1)
    expect_equal(res$data.name[[1]], "x.var and y.var")
})

#
# Done
#

context("corTestDS::smk::shutdown")

context("corTestDS::smk::done")
