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

# context("corTestDS::smk::setup")

#
# Tests
#

###########

# context("corTestDS::smk::without na, pearson")
test_that("simple corTestDS, full, without na, pearson", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)

    res <- corTestDS("x", "y", "pearson", NULL, 0.95)

    expect_equal(class(res), "list")
    expect_length(res, 2)

    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 9)

    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_true(is.infinite(res$`Correlation test`$statistic[[1]]))

    expect_equal(class(res$`Correlation test`$parameter), "integer")
    expect_length(res$`Correlation test`$parameter, 1)
    expect_equal(res$`Correlation test`$parameter[[1]], 6L)

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 0.0)

    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 1.0)

    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)

    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")

    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Pearson's product-moment correlation")

    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")

    expect_equal(class(res$`Correlation test`$conf.int), "numeric")
    expect_length(res$`Correlation test`$conf.int, 2)
    expect_equal(res$`Correlation test`$conf.int[[1]], 1.0)
    expect_equal(res$`Correlation test`$conf.int[[2]], 1.0)
})

test_that("simple corTestDS, neg. full, without na, pearson", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0)

    res <- corTestDS("x", "y", "pearson", NULL, 0.95)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 9)

    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_true(is.infinite(res$`Correlation test`$statistic[[1]]))

    expect_equal(class(res$`Correlation test`$parameter), "integer")
    expect_length(res$`Correlation test`$parameter, 1)
    expect_equal(res$`Correlation test`$parameter[[1]], 6L)

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 0.0)

    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], -1.0)

    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)

    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")

    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Pearson's product-moment correlation")

    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")

    expect_equal(class(res$`Correlation test`$conf.int), "numeric")
    expect_length(res$`Correlation test`$conf.int, 2)
    expect_equal(res$`Correlation test`$conf.int[[1]], -1.0)
    expect_equal(res$`Correlation test`$conf.int[[2]], -1.0)
})

test_that("simple corTestDS, some, pearson, without na, pearson", {
    x   <- c(0.1, 1.0, 1.9, 3.0, 4.0, 5.1, 6.0, 7.0)
    y   <- c(0.0, 1.2, 2.0, 2.9, 4.0, 5.0, 6.1, 7.0)

    res <- corTestDS("x", "y", "pearson", NULL, 0.95)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 9)

    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 53.2466, tolerance = 1e-6)

    expect_equal(class(res$`Correlation test`$parameter), "integer")
    expect_length(res$`Correlation test`$parameter, 1)
    expect_equal(res$`Correlation test`$parameter[[1]], 6L)

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 0.0)

    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 0.998943, tolerance = 1e-6)

    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)

    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")

    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Pearson's product-moment correlation")

    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")

    expect_equal(class(res$`Correlation test`$conf.int), "numeric")
    expect_length(res$`Correlation test`$conf.int, 2)
    expect_equal(res$`Correlation test`$conf.int[[1]], 0.9939173, tolerance = 1e-6)
    expect_equal(res$`Correlation test`$conf.int[[2]], 0.9998169, tolerance = 1e-6)
})

# context("corTestDS::smk::with na, pearson")
test_that("simple corTestDS, some, with na, pearson", {
    x   <- c(NA, 1.0, 2.0, 3.0, NA, 5.0, NA, 7.0)
    y   <- c(0.0, 1.0, NA, 3.0, 4.0, NA, NA, 7.0)

    res <- corTestDS("x", "y", "pearson", NULL, 0.95)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 3)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)

    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_true(is.infinite(res$`Correlation test`$statistic[[1]]))

    expect_equal(class(res$`Correlation test`$parameter), "integer")
    expect_length(res$`Correlation test`$parameter, 1)
    expect_equal(res$`Correlation test`$parameter[[1]], 1L)

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 0.0)

    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 1.0)

    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)

    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")

    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Pearson's product-moment correlation")

    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

# context("corTestDS::smk::without na, kendall")
test_that("simple corTestDS, full, without na, kendall", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    
    res <- corTestDS("x", "y", "kendall", NULL, 0.95)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)
    
    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 28)

    expect_true(is.null(res$`Correlation test`$parameter))
    
    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 4.960317e-05, tolerance = 1e-6)

    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 1.0)
    
    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)
    
    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")
    
    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Kendall's rank correlation tau")
    
    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

test_that("simple corTestDS, neg. full, without na, kendall", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0)
    
    res <- corTestDS("x", "y", "kendall", NULL, 0.95)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)
    
    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 0)
    
    expect_true(is.null(res$`Correlation test`$parameter))
    
    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 0)
    
    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], -1.0)
    
    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)
    
    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")
    
    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Kendall's rank correlation tau")
    
    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

test_that("simple corTestDS, some, kendall, without na, kendall", {
    x   <- c(0.1, 1.0, 1.9, 3.0, 4.0, 5.1, 6.0, 7.0)
    y   <- c(0.0, 1.2, 2.0, 2.9, 4.0, 5.0, 6.1, 7.0)
    
    res <- corTestDS("x", "y", "kendall", NULL, 0.95)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)
    
    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 28)

    expect_true(is.null(res$`Correlation test`$parameter))

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 4.960317e-05, tolerance = 1e-6)
    
    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 1.0, tolerance = 1e-6)
    
    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)
    
    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")
    
    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Kendall's rank correlation tau")
    
    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

# context("corTestDS::smk::with na, kendall")
test_that("simple corTestDS, some, with na, kendall", {
    x   <- c(0.0, NA, 2.0, 3.0, NA, 5.0, NA, 7.0)
    y   <- c(0.0, 1.0, NA, 3.0, 4.0, NA, NA, 7.0)
    
    res <- corTestDS("x", "y", "kendall", NULL, 0.95)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 3)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)
    
    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 3)

    expect_true(is.null(res$`Correlation test`$parameter))

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 0.33333333)
    
    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 1.0)
    
    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)
    
    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")
    
    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Kendall's rank correlation tau")
    
    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

# context("corTestDS::smk::without na, spearman")
test_that("simple corTestDS, full, without na, spearman", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    
    res <- corTestDS("x", "y", "spearman", NULL, 0.95)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)
    
    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 0)

    expect_true(is.null(res$`Correlation test`$parameter))

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 4.960317e-05, tolerance = 1e-6)
    
    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 1.0)
    
    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)
    
    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")
    
    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Spearman's rank correlation rho")
    
    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

test_that("simple corTestDS, neg. full, without na, spearman", {
    x   <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    y   <- c(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0)
    
    res <- corTestDS("x", "y", "spearman", NULL, 0.95)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)
    
    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 168)

    expect_true(is.null(res$`Correlation test`$parameter))

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 4.960317e-05, tolerance = 1e-6)
    
    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], -1.0)
    
    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)
    
    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")
    
    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Spearman's rank correlation rho")
    
    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

test_that("simple corTestDS, some, spearman, without na, spearman", {
    x   <- c(0.1, 1.0, 1.9, 3.0, 4.0, 5.1, 6.0, 7.0)
    y   <- c(0.0, 1.2, 2.0, 2.9, 4.0, 5.0, 6.1, 7.0)
    
    res <- corTestDS("x", "y", "spearman", NULL, 0.95)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 8)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)
    
    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 0.0, tolerance = 1e-6)

    expect_true(is.null(res$`Correlation test`$parameter))

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 4.960317e-05, tolerance = 1e-6)
    
    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 1.0, tolerance = 1e-6)
    
    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)
    
    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")
    
    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Spearman's rank correlation rho")
    
    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

# context("corTestDS::smk::with na, spearman")
test_that("simple corTestDS, some, with na, spearman", {
    x   <- c(0.0, NA, 2.0, 3.0, NA, 5.0, NA, 7.0)
    y   <- c(0.0, 1.0, NA, 3.0, 4.0, NA, NA, 7.0)
    
    res <- corTestDS("x", "y", "spearman", NULL, 0.95)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    expect_length(res$`Number of pairwise complete cases`, 1)
    expect_equal(res$`Number of pairwise complete cases`, 3)
    
    expect_equal(class(res$`Correlation test`), "htest")
    expect_length(res$`Correlation test`, 8)

    expect_equal(class(res$`Correlation test`$statistic), "numeric")
    expect_length(res$`Correlation test`$statistic, 1)
    expect_equal(res$`Correlation test`$statistic[[1]], 0)

    expect_true(is.null(res$`Correlation test`$parameter))

    expect_equal(class(res$`Correlation test`$p.value), "numeric")
    expect_length(res$`Correlation test`$p.value, 1)
    expect_equal(res$`Correlation test`$p.value[[1]], 0.33333333)
    
    expect_equal(class(res$`Correlation test`$estimate), "numeric")
    expect_length(res$`Correlation test`$estimate, 1)
    expect_equal(res$`Correlation test`$estimate[[1]], 1.0)
    
    expect_equal(class(res$`Correlation test`$null.value), "numeric")
    expect_length(res$`Correlation test`$null.value, 1)
    expect_equal(res$`Correlation test`$null.value[[1]], 0.0)
    
    expect_equal(class(res$`Correlation test`$alternative), "character")
    expect_length(res$`Correlation test`$alternative, 1)
    expect_equal(res$`Correlation test`$alternative[[1]], "two.sided")
    
    expect_equal(class(res$`Correlation test`$method), "character")
    expect_length(res$`Correlation test`$method, 1)
    expect_equal(res$`Correlation test`$method[[1]], "Spearman's rank correlation rho")
    
    expect_equal(class(res$`Correlation test`$data.name), "character")
    expect_length(res$`Correlation test`$data.name, 1)
    expect_equal(res$`Correlation test`$data.name[[1]], "x.var and y.var")
})

#
# Done
#

# context("corTestDS::smk::shutdown")

# context("corTestDS::smk::done")

