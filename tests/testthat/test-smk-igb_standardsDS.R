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

# context("igb_standardsDS::smk::setup")

#
# Tests
#

# context("igb_standardsDS::smk::simple")

data <- data.frame(
    gagebrth = c(287, 287, 287, 280, 280, 280, 280, 266, 266, 259, 
                 273, 287, 287, 287, 287, 287, 287, 287, 287, 287,
                 287, 287, 287, 287, 287, 287, 287, 266, 266, 266),
    birthlen = c(55, 55, 55, 51, 51, 51, 51, 54, 49, 50, 
                 50, 53, 53, 53, 53, 53, 53, 53, 53, 53,
                 55, 55, 55, 55, 55, NA, NA, 51, 51, 51),
    sex = c("Male","Male","Male","Male","Male","Male","Male","Male","Female","Female",
            "Male","Male","Male","Male","Male","Male","Male","Male","Male","Male",
            "Female","Female","Female","Female","Female","Male","Male","Male","Male","Male")
)

test_that("igb_standardsDS - igb_value2zscore", {

    res <- igb_standardsDS(gagebrth="data$gagebrth", z=0, p=50, val="data$birthlen", 
                           var="lencm", sex='data$sex', fun='igb_value2zscore')

    expect_equal(class(res), "numeric")
    expect_length(res, 30)
    expect_equal(res[1], 2.5700886, tolerance = 1e-6)
    expect_equal(res[2], 2.5700886, tolerance = 1e-6)
    expect_equal(res[3], 2.5700886, tolerance = 1e-6)
    expect_equal(res[4], 0.6676939, tolerance = 1e-6)
    expect_equal(res[5], 0.6676939, tolerance = 1e-6)
    expect_equal(res[6], 0.6676939, tolerance = 1e-6)
    expect_equal(res[7], 0.6676939, tolerance = 1e-6)
    expect_equal(res[8], 2.7755141, tolerance = 1e-6)
    expect_equal(res[9], 0.6216118, tolerance = 1e-6)
    expect_equal(res[10], 1.6357256, tolerance = 1e-6)
    expect_equal(res[11], 0.4300711, tolerance = 1e-6)
    expect_equal(res[12], 1.5392060, tolerance = 1e-6)
    expect_equal(res[13], 1.5392060, tolerance = 1e-6)
    expect_equal(res[14], 1.5392060, tolerance = 1e-6)
    expect_equal(res[15], 1.5392060, tolerance = 1e-6)
    expect_equal(res[16], 1.5392060, tolerance = 1e-6)
    expect_equal(res[17], 1.5392060, tolerance = 1e-6)
    expect_equal(res[18], 1.5392060, tolerance = 1e-6)
    expect_equal(res[19], 1.5392060, tolerance = 1e-6)
    expect_equal(res[20], 1.5392060, tolerance = 1e-6)
    expect_equal(res[21], 3.1033261, tolerance = 1e-6)
    expect_equal(res[22], 3.1033261, tolerance = 1e-6)
    expect_equal(res[23], 3.1033261, tolerance = 1e-6)
    expect_equal(res[24], 3.1033261, tolerance = 1e-6)
    expect_equal(res[25], 3.1033261, tolerance = 1e-6)
    expect_true(is.na(res[26]))
    expect_true(is.na(res[27]))
    expect_equal(res[28], 1.3708047, tolerance = 1e-6)
    expect_equal(res[29], 1.3708047, tolerance = 1e-6)
    expect_equal(res[30], 1.3708047, tolerance = 1e-6)
    
})

test_that("igb_standardsDS - igb_value2centile", {

    res <- igb_standardsDS(gagebrth="data$gagebrth", z=0, p=50, val="data$birthlen", 
                           var="lencm", sex='data$sex', fun='igb_value2centile')
    
    expect_equal(class(res), "numeric")
    expect_length(res, 30)
    expect_equal(res[1], 99.49164, tolerance = 1e-6)
    expect_equal(res[2], 99.49164, tolerance = 1e-6)
    expect_equal(res[3], 99.49164, tolerance = 1e-6)
    expect_equal(res[4], 74.78355, tolerance = 1e-6)
    expect_equal(res[5], 74.78355, tolerance = 1e-6)
    expect_equal(res[6], 74.78355, tolerance = 1e-6)
    expect_equal(res[7], 74.78355, tolerance = 1e-6)
    expect_equal(res[8], 99.72443, tolerance = 1e-6)
    expect_equal(res[9], 73.29014, tolerance = 1e-6)
    expect_equal(res[10], 94.90515, tolerance = 1e-6)
    expect_equal(res[11], 66.64280, tolerance = 1e-6)
    expect_equal(res[12], 93.81230, tolerance = 1e-6)
    expect_equal(res[13], 93.81230, tolerance = 1e-6)
    expect_equal(res[14], 93.81230, tolerance = 1e-6)
    expect_equal(res[15], 93.81230, tolerance = 1e-6)
    expect_equal(res[16], 93.81230, tolerance = 1e-6)
    expect_equal(res[17], 93.81230, tolerance = 1e-6)
    expect_equal(res[18], 93.81230, tolerance = 1e-6)
    expect_equal(res[19], 93.81230, tolerance = 1e-6)
    expect_equal(res[20], 93.81230, tolerance = 1e-6)
    expect_equal(res[21], 99.90432, tolerance = 1e-6)
    expect_equal(res[22], 99.90432, tolerance = 1e-6)
    expect_equal(res[23], 99.90432, tolerance = 1e-6)
    expect_equal(res[24], 99.90432, tolerance = 1e-6)
    expect_equal(res[25], 99.90432, tolerance = 1e-6)
    expect_true(is.na(res[26]))
    expect_true(is.na(res[27]))
    expect_equal(res[28], 91.47821, tolerance = 1e-6)
    expect_equal(res[29], 91.47821, tolerance = 1e-6)
    expect_equal(res[30], 91.47821, tolerance = 1e-6)

})

test_that("igb_standardsDS - igb_centile2value", {

  res <- igb_standardsDS(gagebrth="data$gagebrth", z=0, p=99, val=NULL, 
                         var="wtkg", sex='data$sex', fun='igb_centile2value')
  
  expect_equal(class(res), "numeric")
  expect_length(res, 30)
  expect_equal(res[1], 4.585547, tolerance = 1e-6)
  expect_equal(res[2], 4.585547, tolerance = 1e-6)
  expect_equal(res[3], 4.585547, tolerance = 1e-6)
  expect_equal(res[4], 4.460156, tolerance = 1e-6)
  expect_equal(res[5], 4.460156, tolerance = 1e-6)
  expect_equal(res[6], 4.460156, tolerance = 1e-6)
  expect_equal(res[7], 4.460156, tolerance = 1e-6)
  expect_equal(res[8], 4.157728, tolerance = 1e-6)
  expect_equal(res[9], 4.005680, tolerance = 1e-6)
  expect_equal(res[10], 3.818300, tolerance = 1e-6)
  expect_equal(res[11], 4.317829, tolerance = 1e-6)
  expect_equal(res[12], 4.585547, tolerance = 1e-6)
  expect_equal(res[13], 4.585547, tolerance = 1e-6)
  expect_equal(res[14], 4.585547, tolerance = 1e-6)
  expect_equal(res[15], 4.585547, tolerance = 1e-6)
  expect_equal(res[16], 4.585547, tolerance = 1e-6)
  expect_equal(res[17], 4.585547, tolerance = 1e-6)
  expect_equal(res[18], 4.585547, tolerance = 1e-6)
  expect_equal(res[19], 4.585547, tolerance = 1e-6)
  expect_equal(res[20], 4.585547, tolerance = 1e-6)
  expect_equal(res[21], 4.433851, tolerance = 1e-6)
  expect_equal(res[22], 4.433851, tolerance = 1e-6)
  expect_equal(res[23], 4.433851, tolerance = 1e-6)
  expect_equal(res[24], 4.433851, tolerance = 1e-6)
  expect_equal(res[25], 4.433851, tolerance = 1e-6)
  expect_equal(res[26], 4.585547, tolerance = 1e-6)
  expect_equal(res[27], 4.585547, tolerance = 1e-6)
  expect_equal(res[28], 4.157728, tolerance = 1e-6)
  expect_equal(res[29], 4.157728, tolerance = 1e-6)
  expect_equal(res[30], 4.157728, tolerance = 1e-6)

})


#
# Done
#

# context("igb_standardsDS::smk::shutdown")

# context("igb_standardsDS::smk::done")
