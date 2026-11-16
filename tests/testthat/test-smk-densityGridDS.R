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

# context("densityGridDS::smk::setup")

set.standard.disclosure.settings()


#
# Tests
#

# context("densityGridDS::smk")
test_that("densityGridDS", {
  
  xvect <- c(11.95, 10.06, 9.98,  9.50, 12.26,  9.66, 11.08, 12.29, 11.00,  9.91,  
           9.98, 11.78,  9.34,  9.75, 10.29, 10.18,  8.10,  9.74,  9.14,  9.38) 
  yvect <- c(14.53, 21.37, 16.66, 16.46, 17.73, 16.66, 15.32, 19.25, 17.84, 12.44, 
           15.29, 15.33, 18.78, 15.16, 17.06, 15.99, 15.01, 12.37, 15.32, 14.23)
  limits <- FALSE
  x.min <- NULL
  x.max <- NULL
  y.min <- NULL
  y.max <- NULL
  numints <- 3
  
  res <- densityGridDS(x="xvect", y="yvect", limits=FALSE, x.min=NULL,
                        x.max=NULL, y.min=NULL, y.max=NULL, numints=numints)
  
    expect_equal(class(res$grid), c("matrix","array"))
    expect_equal(colnames(res$grid), c("", "", "", "x.mids", "y.mids"))
    expect_equal(as.numeric(round(res$grid[1,], digits=3)), c(3.000, 0.000, 0.000, 8.798, 13.870))
    expect_equal(as.numeric(round(res$grid[2,], digits=3)), c(4.000, 5.000, 0.000, 10.195, 16.870))
    expect_equal(as.numeric(round(res$grid[3,], digits=3)), c(3.000, 0.000, 0.000, 11.592, 19.870))

    expect_equal(names(dimnames(res$grid))[2], "Number of invalid cells (cells with counts >0 and < nfilter.tab ) is 4")
    expect_equal(res$class.x, "numeric")
    expect_equal(res$class.y, "numeric")
    
})


test_that("densityGridDS fails when x references nonexistent object", {
    expect_error(densityGridDS(x="nonexistent_obj", y="yvect", numints=3), "does not exist")
})

test_that("densityGridDS fails when y references nonexistent object", {
    yvect <- c(1:20)
    expect_error(densityGridDS(x="yvect", y="nonexistent_obj", numints=3), "does not exist")
})

test_that("densityGridDS fails when x is not numeric or integer", {
    xvect <- c("a", "b", "c")
    yvect <- c(1, 2, 3)
    expect_error(densityGridDS(x="xvect", y="yvect", numints=3), "must be of type numeric or integer")
})

test_that("densityGridDS fails when y is not numeric or integer", {
    xvect <- c(1, 2, 3)
    yvect <- c("a", "b", "c")
    expect_error(densityGridDS(x="xvect", y="yvect", numints=3), "must be of type numeric or integer")
})

test_that("densityGridDS gives the same result for integer input as for numeric", {
  x.numeric <- c(119, 101, 100, 95, 123, 97, 111, 123, 110, 99,
                 100, 118, 93, 98, 103, 102, 81, 97, 91, 94)
  y.numeric <- rev(x.numeric) + c(1:20)
  x.integer <- as.integer(x.numeric)
  y.integer <- as.integer(y.numeric)

  res.numeric <- densityGridDS(x="x.numeric", y="y.numeric", limits=FALSE, x.min=NULL,
                               x.max=NULL, y.min=NULL, y.max=NULL, numints=3)
  res.integer <- densityGridDS(x="x.integer", y="y.integer", limits=FALSE, x.min=NULL,
                               x.max=NULL, y.min=NULL, y.max=NULL, numints=3)

  expect_equal(res.integer$class.x, "integer")
  expect_equal(res.integer$class.y, "integer")
  expect_equal(res.integer$grid, res.numeric$grid)
})

#
# Done
#

# context("densityGridDS::smk::shutdown")

# context("densityGridDS::smk::done")
