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

# context("aucDS::smk::setup")

#
# Tests
#

test_that("aucDS", {
  
    D <- read.csv(file = 'data_files/DASIM/DASIM1.csv')
    
    model <- glm(formula = D$DIS_DIAB~D$GENDER+D$PM_BMI_CONTINUOUS, family = 'binomial')
    pred <- predict(object = model, type = "link")
    res <- aucDS(pred = pred, y = model$y)
    
    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(class(res$AUC), "numeric")
    expect_equal(res$AUC, 0.6767515, tolerance=1e-07)
    expect_equal(class(res$se), "numeric")
    expect_equal(res$se, 0.02065186, tolerance=1e-07)
    
})

#
# Done
#

# context("aucDS::smk::shutdown")

# context("aucDS::smk::done")
