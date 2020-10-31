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

context("tapplyDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple tapplyDS, mean", {
    x           <- c(1, 2, 1, 2, 1, 2, 1, 2)
    index       <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2))
    INDEX.names <- c('index')
    FUN.name    <- "mean"
    
    res <- tapplyDS("x", INDEX.names, FUN.name)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    res.names <- names(res)
    
    expect_equal(class(res.names), "character")
    expect_length(res.names, 2)
    expect_equal(res.names[1], "Mean")
    expect_equal(res.names[2], "N")
    
    expect_equal(class(res$Mean), "array")
    expect_length(res$Mean, 2)
    expect_equal(res$Mean[[1]], 1)
    expect_equal(res$Mean[[2]], 2)
    
    expect_equal(class(res$N), "array")
    expect_length(res$N, 2)
    expect_equal(res$N[[1]], 4)
    expect_equal(res$N[[2]], 4)
})

test_that("simple tapplyDS, sd", {
    x           <- c(1, 2, 1, 2, 1, 2, 1, 2)
    index       <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2))
    INDEX.names <- c('index')
    FUN.name    <- "sd"
    
    res <- tapplyDS("x", INDEX.names, FUN.name)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    res.names <- names(res)
    
    expect_equal(class(res.names), "character")
    expect_length(res.names, 2)
    expect_equal(res.names[1], "SD")
    expect_equal(res.names[2], "N")
    
    expect_equal(class(res$SD), "array")
    expect_length(res$SD, 2)
    expect_equal(res$SD[[1]], 0)
    expect_equal(res$SD[[2]], 0)
    
    expect_equal(class(res$N), "array")
    expect_length(res$N, 2)
    expect_equal(res$N[[1]], 4)
    expect_equal(res$N[[2]], 4)
})

test_that("simple tapplyDS, sum", {
    x           <- c(1, 2, 1, 2, 1, 2, 1, 2)
    index       <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2))
    INDEX.names <- c('index')
    FUN.name    <- "sum"
    
    res <- tapplyDS("x", INDEX.names, FUN.name)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    
    res.names <- names(res)
    
    expect_equal(class(res.names), "character")
    expect_length(res.names, 2)
    expect_equal(res.names[1], "Sum")
    expect_equal(res.names[2], "N")
    
    expect_equal(class(res$Sum), "array")
    expect_length(res$Sum, 2)
    expect_equal(res$Sum[[1]], 4)
    expect_equal(res$Sum[[2]], 8)
    
    expect_equal(class(res$N), "array")
    expect_length(res$N, 2)
    expect_equal(res$N[[1]], 4)
    expect_equal(res$N[[2]], 4)
})

test_that("simple tapplyDS, quantile", {
    x           <- c(1, 2, 1, 2, 1, 2, 1, 2)
    index       <- as.factor(c(1, 2, 1, 2, 1, 2, 1, 2))
    INDEX.names <- c('index')
    FUN.name    <- "quantile"
    
    res <- tapplyDS("x", INDEX.names, FUN.name)

    expect_equal(class(res), "array")
    expect_length(res, 2)

    expect_equal(class(res[[1]]), "numeric")
    expect_length(res[[1]], 15)

    expect_equal(res[[1]][[1]], 1.0)
    expect_equal(res[[1]][[2]], 1.0)
    expect_equal(res[[1]][[3]], 1.0)
    expect_equal(res[[1]][[4]], 1.0)
    expect_equal(res[[1]][[5]], 1.0)
    expect_equal(res[[1]][[6]], 1.0)
    expect_equal(res[[1]][[7]], 1.0)
    expect_equal(res[[1]][[8]], 1.0)
    expect_equal(res[[1]][[9]], 1.0)
    expect_equal(res[[1]][[10]], 1.0)
    expect_equal(res[[1]][[11]], 1.0)
    expect_equal(res[[1]][[12]], 1.0)
    expect_equal(res[[1]][[13]], 1.0)
    expect_equal(res[[1]][[14]], 1.0)
    expect_equal(res[[1]][[15]], 1.0)

    expect_equal(class(res[[2]]), "numeric")
    expect_length(res[[2]], 15)

    expect_equal(res[[2]][[1]], 2.0)
    expect_equal(res[[2]][[2]], 2.0)
    expect_equal(res[[2]][[3]], 2.0)
    expect_equal(res[[2]][[4]], 2.0)
    expect_equal(res[[2]][[5]], 2.0)
    expect_equal(res[[2]][[6]], 2.0)
    expect_equal(res[[2]][[7]], 2.0)
    expect_equal(res[[2]][[8]], 2.0)
    expect_equal(res[[2]][[9]], 2.0)
    expect_equal(res[[2]][[10]], 2.0)
    expect_equal(res[[2]][[11]], 2.0)
    expect_equal(res[[2]][[12]], 2.0)
    expect_equal(res[[2]][[13]], 2.0)
    expect_equal(res[[2]][[14]], 2.0)
    expect_equal(res[[2]][[15]], 2.0)
})

#
# Done
#

context("tapplyDS::smk::shutdown")

context("tapplyDS::smk::done")
