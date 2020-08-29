#'
#' @title Computes the square root values of the input variable
#' @description This function is similar to R function \code{sqrt}.
#' @details The function computes the square root values of an input numeric
#' or integer vector.
#' @param x a string character, the name of a numeric or integer vector
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.sqrt} (or default name \code{sqrt.newobj})
#' which is written to the server-side. The output object is of class numeric 
#' or integer.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
sqrtDS <- function(x){

  x.var <- eval(parse(text=x), envir = parent.frame())

  # compute the square root values of x
  out <- sqrt(x.var)
  
  # assign the outcome to the data servers
  return(out)

}
# ASSIGN FUNCTION
# sqrtDS
