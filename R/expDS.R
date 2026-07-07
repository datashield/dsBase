#'
#' @title Computes the exponential values of the input variable
#' @description This function is similar to R function \code{exp}.
#' @details The function computes the exponential values of an input numeric
#' or integer vector.
#' @param x a string character, the name of a numeric or integer vector
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.exp} (or default name \code{exp.newobj})
#' which is written to the serverside. The output object is of class numeric.
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
expDS <- function(x) {
  x.var <- .loadServersideObject(x)
  .checkClass(obj = x.var, obj_name = x, permitted_classes = c("numeric", "integer"))

  out <- exp(x.var)
  return(out)
}
# ASSIGN FUNCTION
# expDS
