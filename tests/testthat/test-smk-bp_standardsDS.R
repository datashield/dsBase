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

# context("bp_standardsDS::smk::setup")

#
# Tests
#

# context("bp_standardsDS::smk::systolic")
test_that("systolic bp_standardsDS", {
  
  sex <- c(2, 2, 2, 2, 2, 1, 2, 1, 2, 1)
  age <- c(5.26, 7.73, 4.59, 9.81, 7.99, 9.24, 3.93, 9.86, 4.44, 5.42)
  height   <- c(113.57, 92.30, 50.28, 134.84, 123.12, 76.94, 148.17, 68.92, 65.75, 120.99)
  bp <- c(118.54, 87.62, 72.13, 91.18, 88.47, 73.28, 100.27, 86.84, 103.51, 115.28)
  systolic <- TRUE
  
  res <- bp_standardsDS(sex=sex, age=age, height=height, bp=bp, systolic=systolic)
  
    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(class(res[[1]]), "numeric")
    expect_equal(class(res[[2]]), "numeric")

    res.names <- names(res)

    expect_equal(class(res.names), "character")
    expect_length(res.names, 2)
    expect_equal(res.names[1], 'Zbp')
    expect_equal(res.names[2], 'perc')
    
    expect_equal(round(res$Zbp, digits=2), c(2.23, -0.60, -15.57, -0.93, -0.78, -14.74, NaN, -24.74, -0.34, 1.42))
    expect_equal(round(res$perc, digits=2), c(98.72, 27.46, 0.00, 17.51, 21.79, 0.00, NaN, 0.00, 36.77, 92.19))
    
})

# context("bp_standardsDS::smk::diastolic")
test_that("diastolic bp_standardsDS", {
  
  sex <- c(2, 2, 2, 2, 2, 1, 2, 1, 2, 1)
  age <- c(5.26, 7.73, 4.59, 9.81, 7.99, 9.24, 3.93, 9.86, 4.44, 5.42)
  height   <- c(113.57, 92.30, 50.28, 134.84, 123.12, 76.94, 148.17, 68.92, 65.75, 120.99)
  bp <- c(118.54, 87.62, 72.13, 91.18, 88.47, 73.28, 100.27, 86.84, 103.51, 115.28)
  systolic <- FALSE
  
  res <- bp_standardsDS(sex=sex, age=age, height=height, bp=bp, systolic=systolic)
  
  expect_equal(class(res), "list")
  expect_length(res, 2)
  expect_equal(class(res[[1]]), "numeric")
  expect_equal(class(res[[2]]), "numeric")
  
  res.names <- names(res)
  
  expect_equal(class(res.names), "character")
  expect_length(res.names, 2)
  expect_equal(res.names[1], 'Zbp')
  expect_equal(res.names[2], 'perc')
  
  expect_equal(round(res$Zbp, digits=2), c(5.72, 1.85, -10.00, 2.85, 2.81, -11.07, NaN, -20.13, 1.51, 5.03))
  expect_equal(round(res$perc, digits=2), c(100.00, 96.79, 0.00, 99.78, 99.75, 0.00, NaN, 0.00, 93.39, 100.00))
  
})


#
# Done
#

# context("bp_standardsDS::smk::shutdown")

# context("bp_standardsDS::smk::done")
