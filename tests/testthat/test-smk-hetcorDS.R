#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("hetcorDS::smk::setup")

set.standard.disclosure.settings()


#
# Tests
#

context("hetcorDS::smk")
test_that("hetcorDS", {
  
  D <- as.data.frame(matrix(NA, nrow=20, ncol=3))
  colnames(D) <- c('x','y','z')
  D$x <- c(4100, 4158, 4110, 2900, 3580, 3780, 4220, 4210, 2460, 3510,
           2800, 3730, 3300, 3310, 2390, 3480, 3040, 3050, 3500, 3420) 
  D$y <- c(41.00, 41.00, 39.00, 38.86, 40.57, 41.00, 40.71, 41.43, 36.00, 39.00,
           37.57, 40.00, 40.29, 40.71, 37.00, 40.00, 41.57, 36.29, 39.29, 40.00)
  D$z <- as.factor(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1))
  data <- "D"
  ML <- FALSE
  std.err <- TRUE
  bins <- 2
  pd <- TRUE
  use <- "complete.obs"
  
  res <- hetcorDS(data = data, ML = ML, std.err = std.err, bins = bins, pd = pd, use = use)
  
  expect_equal(class(res), "hetcor")
  expect_length(res, 7)
  expect_equal(class(res[[1]]), c("matrix","array"))
  expect_equal(class(res[[2]]), c("matrix","array"))
  expect_equal(class(res[[3]]), c("character"))
  expect_equal(class(res[[4]]), c("logical"))
  expect_equal(class(res[[5]]), c("matrix","array"))
  expect_equal(class(res[[6]]), c("integer"))
  expect_equal(class(res[[7]]), c("matrix","array"))
  
  res.names <- names(res)
  
  expect_equal(class(res.names), "character")
  expect_length(res.names, 7)
  expect_equal(res.names[1], 'correlations')
  expect_equal(res.names[2], 'type')
  expect_equal(res.names[3], 'NA.method')
  expect_equal(res.names[4], 'ML')
  expect_equal(res.names[5], 'std.errors')
  expect_equal(res.names[6], 'n')
  expect_equal(res.names[7], 'tests')
  
  expect_equal(round(res$correlations['x','x'], digits = 3), 1.000)
  expect_equal(round(res$correlations['x','y'], digits = 3), 0.712)
  expect_equal(round(res$correlations['x','z'], digits = 3), 0.242)
  expect_equal(round(res$correlations['y','z'], digits = 3), 0.207)
  
  expect_equal(res$type[1,1], "")
  expect_equal(res$type[1,2], "Pearson")
  expect_equal(res$type[1,3], "Polyserial")
  expect_equal(res$type[2,3], "Polyserial")
  
  expect_equal(res$NA.method, "complete.obs")
  
  expect_equal(round(res$std.errors['x','x'], digits = 3), 0.000)
  expect_equal(round(res$std.errors['x','y'], digits = 3), 0.110)
  expect_equal(round(res$std.errors['x','z'], digits = 3), 0.272)
  expect_equal(round(res$std.errors['y','z'], digits = 3), 0.275)
  
})


#
# Done
#

context("hetcorDS::smk::shutdown")

context("hetcorDS::smk::done")