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

# context("gamlssDS::smk::birthweight")
test_that("birthweight gamlssDS", {
  
  D <- as.data.frame(matrix(NA, nrow=20, ncol=2))
  colnames(D) <- c('e3_bw','e3_gac_None')
  D$e3_bw <- c(4100, 4158, 4110, 2900, 3580, 3780, 4220, 4210, 2460, 3510,
                  2800, 3730, 3300, 3310, 2390, 3480, 3040, 3050, 3500, 3420) 
  D$e3_gac_None <- c(41.00, 41.00, 39.00, 38.86, 40.57, 41.00, 40.71, 41.43, 36.00, 39.00,
                        37.57, 40.00, 40.29, 40.71, 37.00, 40.00, 41.57, 36.29, 39.29, 40.00)
  family <- "NOleft_parenthesisright_parenthesis"
  centiles = TRUE
  formula <- "e3_bwtilde_symbole3_gac_None"
  sigma.formula <- "e3_bwtilde_symbole3_gac_None"
  nu.formula <- "tilde_symbol1"
  tau.formula <- "tilde_symbol1"
  method <- 'RS'
  mu.fix <- FALSE
  sigma.fix <- FALSE 
  nu.fix <- FALSE
  tau.fix <- FALSE
  xvar <- "D$e3_gac_None"
  newobj <- 'z_scores_e3_bw'
  control = "0.001,20,1,1,1,1,Inf"
  i.control = "0.001,50,30,0.001"
  data="D"
  
  res <- gamlssDS(formula=formula, sigma.formula=sigma.formula, 
                  nu.formula=nu.formula, tau.formula=tau.formula,
                  family=family, data=data, method=method, mu.fix=mu.fix,
                  sigma.fix=sigma.fix, nu.fix=nu.fix, tau.fix=tau.fix,
                  control=control, i.control=i.control, centiles=centiles,
                  xvar=xvar, newobj=newobj)
  
    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(class(res[[1]]), c("gamlss","gam","glm","lm"))
    expect_equal(class(res[[2]]), c("matrix","array"))

    res.names <- names(res)

    expect_equal(class(res.names), "character")
    expect_length(res.names, 2)
    expect_equal(res.names[1], 'results')
    expect_equal(res.names[2], 'centiles')
    
    expect_equal(res$results$family, c("NO", "Normal"))
    expect_equal(res$results$parameters, c("mu", "sigma"))
    expect_equal(class(res$results$call), "call")
    expect_equal(res$results$y, "The response variable is not disclosed!")
    expect_equal(class(res$results$control), "list")
    expect_length(res$results$control, 11)
    expect_equal(round(res$results$G.deviance, digits=4), 294.1581)
    expect_equal(res$results$N, 20)
    expect_equal(class(res$results$rqres), "expression")
    expect_equal(res$results$iter, 3)
    
    expect_equal(colnames(res$centiles), c("cent","per"))
    expect_equal(res$centiles[,'cent'], c(0.4, 2.0, 10.0, 25.0, 50.0, 75.0, 90.0, 98.0, 99.6))
    expect_equal(res$centiles[,'per'], c(0, 5, 10, 25, 55, 70, 95, 95, 100))
    
})


#
# Done
#

# context("gamlssDS::smk::shutdown")

# context("gamlssDS::smk::done")
