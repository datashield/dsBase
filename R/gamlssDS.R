#'
#' @title gamlssDS an aggregate function called by ds.galmss
#' @description This function calls the gamlssDS that is a wrapper function from 
#' the gamlss R package. The function returns an object of class "gamlss", which 
#' is a generalized additive model for location, scale and shape (GAMLSS). The 
#' function also saves the residuals as an object on the server-side with a name 
#' specified by the newobj argument. In addition, if the argument centiles is set 
#' to TRUE, the function calls the centiles function from the gamlss package and 
#' returns the sample percentages below each centile curve.
#' @details For additional details see the help header of gamlss and centiles
#' functions in native R gamlss package.
#' @param formula a formula object, with the response on the left of an ~ operator, 
#' and the terms, separated by + operators, on the right. Nonparametric smoothing
#' terms are indicated by pb() for penalised beta splines, cs for smoothing splines, 
#' lo for loess smooth terms and random or ra for random terms, 
#' e.g. y~cs(x,df=5)+x1+x2*x3. 
#' @param sigma.formula a formula object for fitting a model to the sigma parameter,
#' as in the formula above, e.g. sigma.formula=~cs(x,df=5).
#' @param nu.formula a formula object for fitting a model to the nu parameter, 
#' e.g. nu.formula=~x
#' @param tau.formula a formula object for fitting a model to the tau parameter, 
#' e.g. tau.formula=~cs(x,df=2)
#' @param family a gamlss.family object, which is used to define the distribution 
#' and the link functions of the various parameters. The distribution families 
#' supported by gamlss() can be found in gamlss.family. Functions such as BI() 
#' (binomial) produce a family object. Also can be given without the parentheses
#' i.e. BI. Family functions can take arguments, as in BI(mu.link=probit).
#' @param data a data frame containing the variables occurring in the formula. 
#' If this is missing, the variables should be on the parent environment.
#' @param method a character indicating the algorithm for GAMLSS. Can be either
#' 'RS', 'CG' or 'mixed'. If method='RS' the function will use the Rigby and 
#' Stasinopoulos algorithm, if method='CG' the function will use the Cole and 
#' Green algorithm, and if method='mixed' the function will use the RS algorithm
#' twice before switching to the Cole and Green algorithm for up to 10 extra
#' iterations.
#' @param mu.fix logical, indicate whether the mu parameter should be kept fixed
#' in the fitting processes.
#' @param sigma.fix logical, indicate whether the sigma parameter should be kept
#' fixed in the fitting processes.
#' @param nu.fix logical, indicate whether the nu parameter should be kept fixed 
#' in the fitting processes.
#' @param tau.fix logical, indicate whether the tau parameter should be kept fixed
#' in the fitting processes.
#' @param control this sets the control parameters of the outer iterations algorithm 
#' using the gamlss.control function. This is a vector of 7 numeric values: (i) c.crit 
#' (the convergence criterion for the algorithm), (ii) n.cyc (the number of cycles of 
#' the algorithm), (iii) mu.step (the step length for the parameter mu), (iv) sigma.step 
#' (the step length for the parameter sigma), (v) nu.step (the step length for the
#' parameter nu), (vi) tau.step (the step length for the parameter tau), (vii) gd.tol
#' (global deviance tolerance level). The default values for these 7 parameters are 
#' set to c(0.001, 20, 1, 1, 1, 1, Inf).
#' @param i.control this sets the control parameters of the inner iterations of the 
#' RS algorithm using the glim.control function. This is a vector of 4 numeric values: 
#' (i) cc (the convergence criterion for the algorithm), (ii) cyc (the number of 
#' cycles of the algorithm), (iii) bf.cyc (the number of cycles of the backfitting 
#' algorithm), (iv) bf.tol (the convergence criterion (tolerance level) for the 
#' backfitting algorithm). The default values for these 4 parameters are set to 
#' c(0.001, 50, 30, 0.001).
#' @param centiles logical, indicating whether the function centiles() will be used to 
#' tabulate the sample percentages below each centile curve. Default is set to FALSE.
#' @param xvar the unique explanatory variable used in the centiles() function. This 
#' variable is used only if the centiles argument is set to TRUE. A restriction in
#' the centiles function is that it applies to models with one explanatory variable
#' only.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{gamlss_residuals}. 
#' @return a gamlss object with all components as in the native R gamlss function. 
#' Individual-level information like the components y (the response response) and 
#' residuals (the normalised quantile residuals of the model) are not disclosed to 
#' the client-side.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @import gamlss
#' @import gamlss.dist
#' @export
#'
gamlssDS <- function(formula=formula, sigma.formula=sigma.formula, nu.formula=nu.formula,
                     tau.formula=tau.formula, family=family, data=data, method=method,
                     mu.fix=mu.fix, sigma.fix=sigma.fix, nu.fix=nu.fix, tau.fix=tau.fix,
                     control=control, i.control=i.control, centiles=centiles, xvar=xvar, 
                     newobj=newobj){
  
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.glm <- as.numeric(thr$nfilter.glm)
  
  data <- eval(parse(text = data), envir = parent.frame())
  
  family <- gsub("left_parenthesis", "(", family, fixed = TRUE)
  family <- gsub("right_parenthesis", ")", family, fixed = TRUE)
  family <- gsub("equal_symbol", "=", family, fixed = TRUE)
  family <- gamlss.dist::as.family(eval(parse(text=family)))
  
  formula <- gsub("left_parenthesis", "(", formula, fixed = TRUE)
  formula <- gsub("right_parenthesis", ")", formula, fixed = TRUE)
  formula <- gsub("tilde_symbol", "~", formula, fixed = TRUE)
  formula <- gsub("equal_symbol", "=", formula, fixed = TRUE)
  formula <- gsub("comma_symbol", ",", formula, fixed = TRUE)
  formula <- stats::as.formula(formula)
  
  sigma.formula <- gsub("left_parenthesis", "(", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub("right_parenthesis", ")", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub("tilde_symbol", "~", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub("equal_symbol", "=", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub("comma_symbol", ",", sigma.formula, fixed = TRUE)
  sigma.formula <- stats::as.formula(sigma.formula)
  
  nu.formula <- gsub("left_parenthesis", "(", nu.formula, fixed = TRUE)
  nu.formula <- gsub("right_parenthesis", ")", nu.formula, fixed = TRUE)
  nu.formula <- gsub("tilde_symbol", "~", nu.formula, fixed = TRUE)
  nu.formula <- gsub("equal_symbol", "=", nu.formula, fixed = TRUE)
  nu.formula <- gsub("comma_symbol", ",", nu.formula, fixed = TRUE)
  nu.formula <- stats::as.formula(nu.formula)
  
  tau.formula <- gsub("left_parenthesis", "(", tau.formula, fixed = TRUE)
  tau.formula <- gsub("right_parenthesis", ")", tau.formula, fixed = TRUE)
  tau.formula <- gsub("tilde_symbol", "~", tau.formula, fixed = TRUE)
  tau.formula <- gsub("equal_symbol", "=", tau.formula, fixed = TRUE)
  tau.formula <- gsub("comma_symbol", ",", tau.formula, fixed = TRUE)
  tau.formula <- stats::as.formula(tau.formula)
  
  c1 <- as.numeric(unlist(strsplit(control, split=",")))
  
  c2 <- as.numeric(unlist(strsplit(i.control, split=",")))
  
  # if(source.weights == 'clientside'){
  #   weights <- as.numeric(unlist(strsplit(weights, split=",")))
  # }
  # if(source.weights == 'serverside'){
  #   weights <- eval(parse(text = weights), envir = parent.frame())
  # }
  # if(is.null(source.weights)){
  #   weights <- NULL
  # }
  
  if(method=='RS'){
    results <- gamlss::gamlss(formula = formula, sigma.formula = sigma.formula, 
                              nu.formula = nu.formula, tau.formula = tau.formula, 
                              family = family, data = data, method = RS(),
                              mu.fix = mu.fix, sigma.fix = sigma.fix, nu.fix = nu.fix, 
                              tau.fix = tau.fix,
                              control = gamlss.control(c.crit=c1[1], n.cyc=c1[2], 
                                                       mu.step=c1[3], sigma.step=c1[4], 
                                                       nu.step=c1[5], tau.step=c1[6],
                                                       gd.tol=c1[7]),
                              i.control = glim.control(cc=c2[1], cyc=c2[2], 
                                                       bf.cyc=c2[3], bf.tol=c2[4]))
  }
  if(method=='CG'){
    results <- gamlss::gamlss(formula = formula, sigma.formula = sigma.formula, 
                              nu.formula = nu.formula, tau.formula = tau.formula, 
                              family = family, data = data, method = CG(),
                              mu.fix = mu.fix, sigma.fix = sigma.fix, nu.fix = nu.fix, 
                              tau.fix = tau.fix,
                              control = gamlss.control(c.crit=c1[1], n.cyc=c1[2], 
                                                       mu.step=c1[3], sigma.step=c1[4], 
                                                       nu.step=c1[5], tau.step=c1[6],
                                                       gd.tol=c1[7]),
                              i.control = glim.control(cc=c2[1], cyc=c2[2], 
                                                       bf.cyc=c2[3], bf.tol=c2[4]))
  } 
  if(method=='mixed'){
    results <- gamlss::gamlss(formula = formula, sigma.formula = sigma.formula, 
                              nu.formula = nu.formula, tau.formula = tau.formula, 
                              family = family, data = data, method = mixed(),
                              mu.fix = mu.fix, sigma.fix = sigma.fix, nu.fix = nu.fix, 
                              tau.fix = tau.fix, 
                              control = gamlss.control(c.crit=c1[1], n.cyc=c1[2], 
                                                       mu.step=c1[3], sigma.step=c1[4], 
                                                       nu.step=c1[5], tau.step=c1[6],
                                                       gd.tol=c1[7]),
                              i.control = glim.control(cc=c2[1], cyc=c2[2], 
                                                       bf.cyc=c2[3], bf.tol=c2[4]))
  }
  
  # checks for oversaturated models
  if(results$df.fit > nfilter.glm * results$N){
    stop("ERROR: Model has too many parameters, there is a possible risk of disclosure - please simplify model", fixed=TRUE)
  }
  if(length(results$sigma.terms) > nfilter.glm * results$N){
    stop("ERROR: Model has too many parameters, there is a possible risk of disclosure - please simplify model", fixed=TRUE)
  }
  if(length(results$mu.terms) > nfilter.glm * results$N){
    stop("ERROR: Model has too many parameters, there is a possible risk of disclosure - please simplify model", fixed=TRUE)
  }
  
  # save the residuals on the server-side
  base::assign(newobj, results$residuals, envir = parent.frame())
  
  if(centiles==TRUE){
    xvar <- eval(parse(text=xvar), envir = parent.frame())
    centiles_out <- gamlss::centiles(obj = results, xvar = xvar, points = FALSE, 
                                     save = TRUE)
  }else{
    centiles_out <- NA
  }
  
  # Don't disclose any individual-level information
  results$y <- "The response variable is not disclosed!"
  results$residuals <- "The residuals of the model are not disclosed!"
  results$mu.fv <- "The fitted values of the mu model are not disclosed!"
  results$sigma.fv <- "The fitted values of the sigma model are not disclosed!"
  results$tau.fv <- "The fitted values of the tau model are not disclosed!"
  results$mu.lp <- "The linear predictors of the mu model are not disclosed!"
  results$sigma.lp <- "The linear predictors of the sigma model are not disclosed!"
  results$tau.lp <- "The linear predictors of the tau model are not disclosed!"
  results$mu.wv <- "The working variable of the mu model are not disclosed!"
  results$sigma.wv <- "The working variable of the sigma model are not disclosed!"
  results$tau.wv <- "The working variable of the tau model are not disclosed!"
  results$mu.wt <- "The working weights of the mu model are not disclosed!"
  results$sigma.wt <- "The working weights of the sigma model are not disclosed!"
  results$tau.wt <- "The working weights of the tau model are not disclosed!"
  results$mu.x <- "The designed matrix of the mu model is not disclosed!"
  results$sigma.x <- "The designed matrix of the sigma model is not disclosed!"
  results$tau.x <- "The designed matrix of the tau model is not disclosed!"
  results$mu.qr <- "The QR decomposition of the mu model is not disclosed!"
  results$sigma.qr <- "The QR decomposition of the sigma model is not disclosed!"
  results$tau.qr <- "The QR decomposition of the tau model is not disclosed!"
  
  return(list(results = results, centiles = centiles_out))
  
}
