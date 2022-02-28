#' 
#' @title Heterogeneous Correlation Matrix
#' @description This function is based on the hetcor function from the R package \code{polycor}.
#' @details Computes a heterogenous correlation matrix, consisting of Pearson product-moment
#' correlations between numeric variables, polyserial correlations between numeric and ordinal
#' variables, and polychoric correlations between ordinal variables.
#' @param data the name of a data frame consisting of factors, ordered factors, logical variables,
#' character variables, and/or numeric variables, or the first of several variables.
#' @param ML if TRUE, compute maximum-likelihood estimates; if FALSE (default), compute quick
#' two-step estimates.
#' @param std.err	if TRUE (default), compute standard errors.
#' @param bins number of bins to use for continuous variables in testing bivariate normality;
#' the default is 4.
#' @param pd if TRUE (default) and if the correlation matrix is not positive-definite, an attempt
#' will be made to adjust it to a positive-definite matrix, using the nearPD function in the Matrix
#' package. Note that default arguments to nearPD are used (except corr=TRUE); for more control call
#' nearPD directly.
#' @param use	if "complete.obs", remove observations with any missing data; if "pairwise.complete.obs",
#' compute each correlation using all observations with valid data for that pair of variables.
#' @return Returns an object of class "hetcor" with the following components: the correlation matrix;
#' the type of each correlation: "Pearson", "Polychoric", or "Polyserial"; the standard errors of the
#' correlations, if requested; the number (or numbers) of observations on which the correlations are
#' based; p-values for tests of bivariate normality for each pair of variables; the method by which
#' any missing data were handled: "complete.obs" or "pairwise.complete.obs"; TRUE for ML estimates,
#' FALSE for two-step estimates.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
hetcorDS <- function(data, ML, std.err, bins, pd, use){
  
  data <- eval(parse(text=data), envir = parent.frame())

  out <- polycor::hetcor(data = data, ML = ML, std.err = std.err, bins = bins, pd = pd, use = use)
  
  return(out)
  
}
# AGGREGATE FUNCTION
# hetcorDS

