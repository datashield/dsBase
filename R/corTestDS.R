#'
#' @title Tests for correlation between paired samples
#' @description This function is similar to R function \code{cor.test}.
#' @details The function runs a two-sided correlation test
#' @param x a character string providing  the name of a numerical vector. 
#' @param y a character string providing  the name of a numerical vector.
#' @param method a character string indicating which correlation coefficient is to be
#' used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated.
#' @param exact a logical indicating whether an exact p-value should be computed. Used for
#' Kendall's tau and Spearman's rho.  
#' @param conf.level confidence level for the returned confidence interval. Currently
#' only used for the Pearson product moment correlation coefficient if there are at least
#' 4 complete pairs of observations.
#' @return the results of the correlation test.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
corTestDS <- function(x, y, method, exact, conf.level){

  x.var <- eval(parse(text=x), envir = parent.frame())
  y.var <- eval(parse(text=y), envir = parent.frame())
  
  # get the number of pairwise complete cases
  n <- sum(stats::complete.cases(x.var, y.var))
  
  # runs a two-sided correlation test
  corTest <- stats::cor.test(x=x.var, y=y.var, method=method, exact=exact, conf.level=conf.level)

  out <- list(n, corTest)
  names(out) <- c("Number of pairwise complete cases", "Correlation test")
  
  # return the results
  return(out)

}
# AGGREGATE FUNCTION
# corTestDS
