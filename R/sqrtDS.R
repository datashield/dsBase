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
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
sqrtDS <- function(x){
  x.var <- .loadServersideObject(x)
  .checkClass(obj = x.var, obj_name = x, permitted_classes = c("numeric", "integer"))

  out <- sqrt(x.var)
  return(out)
}
# ASSIGN FUNCTION
# sqrtDS
