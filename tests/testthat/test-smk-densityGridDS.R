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

context("densityGridDS::smk::setup")

set.standard.disclosure.settings()


#
# Tests
#

context("densityGridDS::smk")
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
  
  res <- densityGridDS(xvect=xvect, yvect=yvect, limits=FALSE, x.min=NULL, 
                        x.max=NULL, y.min=NULL, y.max=NULL, numints=numints)
  
    expect_equal(class(res), c("matrix","array"))
    expect_equal(colnames(res), c("", "", "", "x.mids", "y.mids"))
    expect_equal(as.numeric(round(res[1,], digits=3)), c(3.000, 0.000, 0.000, 8.798, 13.870))
    expect_equal(as.numeric(round(res[2,], digits=3)), c(4.000, 5.000, 0.000, 10.195, 16.870))
    expect_equal(as.numeric(round(res[3,], digits=3)), c(3.000, 0.000, 0.000, 11.592, 19.870))
    
    expect_equal(names(dimnames(res))[2], "Number of invalid cells (cells with counts >0 and < nfilter.tab ) is 4")
    
})


#
# Done
#

context("densityGridDS::smk::shutdown")

context("densityGridDS::smk::done")
