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

context("asFactorDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("asFactorDS1::smk::simple")
test_that("simple asFactorDS1", {
    input <- c(2.0, 1.0, 3.0, 3.0, 3.0, 1.0, 2.0, 2.0, 1.0, 2.0)

    res <- asFactorDS1("input")

    expect_equal(class(res), "character")
    expect_length(res, 3)
    expect_equal(res[1], "1")
    expect_equal(res[2], "2")
    expect_equal(res[3], "3")
})

context("asFactorDS1::smk::make errors")
test_that("make errors, vector with more unique values than nfilter.levels.max", {
  input <- c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
  
  factor.levels.present.in.source <- levels(factor(input))
  num.levels <- length(factor.levels.present.in.source)
  
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.levels.density <- as.numeric(thr$nfilter.levels.density)
  nfilter.levels.max <- as.numeric(thr$nfilter.levels.max)
  max.levels.by.density <- nfilter.levels.density*length(input)
  
  error.message <- paste0("FAILED: this variable has too many levels and may be disclosive. 
                            The number of factor levels must not exceed ", (nfilter.levels.density*100), 
                          "% of the length of the variable being converted to a factor. The max number 
                            of levels in this study is therefore ",max.levels.by.density," but this 
                            variable has ", num.levels, " factor levels")
  
  expect_error(asFactorDS1("input"), error.message, fixed=TRUE)
})

test_that("make errors, vector with more levels than nfilter.levels.max", {
  input <- c(2.0, 1.0, 3.0, 3.0, 3.0, 1.0, 2.0, 2.0, 1.0, 2.0)
  
  factor.levels.present.in.source <- levels(factor(input))
  num.levels <- length(factor.levels.present.in.source)
  
  set.specific.disclosure.settings(nfilter.levels.max='2')
  
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.levels.density <- as.numeric(thr$nfilter.levels.density)
  nfilter.levels.max <- as.numeric(thr$nfilter.levels.max)
  max.levels.by.density <- nfilter.levels.density*length(input)
  
  error.message <- paste0("FAILED: this variable has too many levels and may be disclosive. 
                            It exceeds the max number of levels allowed by nfilter.levels.max: 
                            that is ", nfilter.levels.max, ". In this study this variable has ", 
                          num.levels," factor levels")
  
  expect_error(asFactorDS1("input"), error.message, fixed=TRUE)
})


#
# Done
#

context("asFactorDS1::smk::shutdown")

context("asFactorDS1::smk::done")
