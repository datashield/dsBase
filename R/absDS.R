#'
#' @title Computes the absolute values of the input variable
#' @description This function is similar to R function \code{abs}.
#' @details The function computes the  absolute values of an input numeric
#' or integer vector.
#' @param x a string character, the name of a numeric or integer vector
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.abs} (or default name \code{abs.newobj})
#' which is written to the serverside. The output object is of class numeric
#' or integer.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
absDS <- function(x) {
  x.var <- .loadServersideObject(x)
  .checkClass(obj = x.var, obj_name = x, permitted_classes = c("numeric", "integer"))

  out <- abs(x.var)
  return(out)
}
# ASSIGN FUNCTION
# absDS
