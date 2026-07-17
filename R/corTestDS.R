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
#' @return a list with the results of the correlation test and \code{class}, the class of the
#' input object for client-side consistency checking.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
corTestDS <- function(x, y, method, exact, conf.level){

  x.var <- .loadServersideObject(x)
  .checkClass(obj = x.var, obj_name = x, permitted_classes = c("numeric", "integer"))
  y.var <- .loadServersideObject(y)
  .checkClass(obj = y.var, obj_name = y, permitted_classes = c("numeric", "integer"))
  
  # get the number of pairwise complete cases
  n <- sum(stats::complete.cases(x.var, y.var))
  
  # runs a two-sided correlation test
  corTest <- stats::cor.test(x=x.var, y=y.var, method=method, exact=exact, conf.level=conf.level)

  out <- list(n, corTest, class = class(x.var))
  names(out)[1:2] <- c("Number of pairwise complete cases", "Correlation test")

  # return the results
  return(out)

}
# AGGREGATE FUNCTION
# corTestDS
