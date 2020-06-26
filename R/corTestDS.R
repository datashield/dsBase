#'
#' @title Tests for correlation between paired samples
#' @description This function is similar to R function \code{cor.test}.
#' @details The function runs a two-sided Pearson test with a 0.95 confidence level.
#' @param x a character string providing  the name of a numerical vector. 
#' @param y a character string providing  the name of a numerical vector.
#' @return the results of the Pearson test.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
corTestDS <- function(x, y){

  x.var <- eval(parse(text=x), envir = parent.frame())
  y.var <- eval(parse(text=y), envir = parent.frame())
  
  # runs a two-sided Pearson test
  out <- stats::cor.test(x.var, y.var)

  # return the dimension
  return(out)

}
#AGGREGATE FUNCTION
# corTestDS
