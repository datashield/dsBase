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

# contect("gamlssDS::smk::setup")

set.standard.disclosure.settings()


#
# Tests
#

# contect("dataFrameSubsetDS2::smk::test1")
test_that("test1 dataFrameSubsetDS2", {
  
  D <- as.data.frame(matrix(NA, nrow=20, ncol=3))
  colnames(D) <- c('integer','numeric','binary')
  D$integer <- c(4100, 4158, 4110, 2900, 3580, 3780, 4220, 4210, 2460, 3510,
                  2800, 3730, 3300, 3310, 2390, 3480, 3040, 3050, 3500, 3420) 
  D$numeric <- c(41.00, 41.00, 39.00, 38.86, 40.57, 41.00, 40.71, 41.43, 36.00, 39.00,
                 37.57, 40.00, 40.29, 40.71, 37.00, 40.00, 41.57, 36.29, 39.29, 40.00)
  D$binary <- c(1,1,1,1,1,1,1,2,2,2,2,2,1,1,2,2,1,2,1,2)
  Boolean.operator.n <- 1
  keep.NAs <- FALSE

  res <- dataFrameSubsetDS2(df.name="D", V1.name="D$binary", V2.name="2",
                            Boolean.operator.n=Boolean.operator.n, keep.NAs = keep.NAs)
  
    expect_equal(class(res), "data.frame")
    expect_equal(nrow(res), 9)
    expect_equal(ncol(res), 3)
    expect_equal(colnames(res), c("integer", "numeric", "binary"))
    expect_equal(res$integer, c(4210, 2460, 3510, 2800, 3730, 2390, 3480, 3050, 3420))
    expect_equal(res$numeric, c(41.43, 36.00, 39.00, 37.57, 40.00, 37.00, 40.00, 36.29, 40.00))
    expect_equal(res$binary, c(2, 2, 2, 2, 2, 2, 2, 2, 2))
    
})

# contect("dataFrameSubsetDS2::smk::test2")
test_that("test2 dataFrameSubsetDS2", {
  
  D <- as.data.frame(matrix(NA, nrow=20, ncol=3))
  colnames(D) <- c('integer','numeric','binary')
  D$integer <- c(4100, 4158, 4110, 2900, 3580, 3780, 4220, 4210, 2460, 3510,
                 2800, 3730, 3300, 3310, 2390, 3480, 3040, 3050, 3500, 3420) 
  D$numeric <- c(41.00, 41.00, 39.00, 38.86, 40.57, 41.00, 40.71, 41.43, 36.00, 39.00,
                 37.57, 40.00, 40.29, 40.71, 37.00, 40.00, 41.57, 36.29, 39.29, 40.00)
  D$binary <- c(1,1,1,1,1,1,1,2,2,2,2,2,1,1,2,2,1,2,1,2)
  Boolean.operator.n <- 3
  keep.NAs <- FALSE
  
  res <- dataFrameSubsetDS2(df.name="D", V1.name="D$numeric", V2.name="41.10",
                            Boolean.operator.n=Boolean.operator.n, keep.NAs = keep.NAs)
  
  expect_equal(class(res), "data.frame")
  expect_equal(nrow(res), 18)
  expect_equal(ncol(res), 3)
  expect_equal(colnames(res), c("integer", "numeric", "binary"))
  
})

#
# Done
#

# contect("dataFrameSubsetDS2::smk::shutdown")

# contect("dataFrameSubsetDS2::smk::done")
