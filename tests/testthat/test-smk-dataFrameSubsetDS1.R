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

# context("gamlssDS::smk::setup")

set.standard.disclosure.settings()


#
# Tests
#

# context("dataFrameSubsetDS1::smk::test1")
test_that("test1 dataFrameSubsetDS1", {
  
  D <- as.data.frame(matrix(NA, nrow=20, ncol=3))
  colnames(D) <- c('integer','numeric','binary')
  D$integer <- c(4100, 4158, 4110, 2900, 3580, 3780, 4220, 4210, 2460, 3510,
                  2800, 3730, 3300, 3310, 2390, 3480, 3040, 3050, 3500, 3420) 
  D$numeric <- c(41.00, 41.00, 39.00, 38.86, 40.57, 41.00, 40.71, 41.43, 36.00, 39.00,
                 37.57, 40.00, 40.29, 40.71, 37.00, 40.00, 41.57, 36.29, 39.29, 40.00)
  D$binary <- c(1,1,1,1,1,1,1,2,2,2,2,2,1,1,2,2,1,2,1,2)
  Boolean.operator.n <- 1
  keep.NAs <- FALSE

  res <- dataFrameSubsetDS1(df.name="D", V1.name="D$binary", V2.name="2",
                            Boolean.operator.n=Boolean.operator.n, keep.NAs = keep.NAs)
  
    expect_equal(class(res), "character")
    expect_equal(res, "Subsetting undertaken without problems")
    
})

# context("dataFrameSubsetDS1::smk::test2")
test_that("test2 dataFrameSubsetDS1", {
  
  D <- as.data.frame(matrix(NA, nrow=20, ncol=3))
  colnames(D) <- c('integer','numeric','binary')
  D$integer <- c(4100, 4158, 4110, 2900, 3580, 3780, 4220, 4210, 2460, 3510,
                 2800, 3730, 3300, 3310, 2390, 3480, 3040, 3050, 3500, 3420) 
  D$numeric <- c(41.00, 41.00, 39.00, 38.86, 40.57, 41.00, 40.71, 41.43, 36.00, 39.00,
                 37.57, 40.00, 40.29, 40.71, 37.00, 40.00, 41.57, 36.29, 39.29, 40.00)
  D$binary <- c(1,1,1,1,1,1,1,2,2,2,2,2,1,1,2,2,1,2,1,2)
  Boolean.operator.n <- 3
  keep.NAs <- FALSE
  
  res <- dataFrameSubsetDS1(df.name="D", V1.name="D$numeric", V2.name="41.10",
                            Boolean.operator.n=Boolean.operator.n, keep.NAs = keep.NAs)
  
  expect_equal(class(res), "list")
  expect_length(res, 10)
  expect_equal(res[[1]], "Warning: DataSHIELD monitors every session for potentially disclosive analytic requests.")
  expect_equal(res[[2]], "The analysis you just submitted has generated a subset in which the number of elements")
  expect_equal(res[[3]], "differs - but only very slightly so - from the original data frame. This is most likely to be")
  expect_equal(res[[4]], "an innocent consequence of your subsetting needs. However, it could in theory be one step")
  expect_equal(res[[5]], "in a difference-based attack aimed at identifying individuals. This analytic request has")
  expect_equal(res[[6]], "therefore been highlighted in the session log file. Please be reassured, if you do not try")
  expect_equal(res[[7]], "to identify individuals this will cause you no difficulty. However, if you do plan a ")
  expect_equal(res[[8]], "malicious attempt to identify individuals by differencing, this will become obvious in the")
  expect_equal(res[[9]], "session log and you will be sanctioned. Possible consequences include loss of future access")
  expect_equal(res[[10]], "to DataSHIELD and/or legal penalties.")
  
})

#
# Done
#

# context("dataFrameSubsetDS1::smk::shutdown")

# context("dataFrameSubsetDS1::smk::done")
