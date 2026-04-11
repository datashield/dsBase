#'
#' @title Computes the logarithm values of the input variable
#' @description This function is similar to R function \code{log}.
#' @details The function computes the logarithm values of an input numeric
#' or integer vector. By default natural logarithms are computed.
#' @param x a string character, the name of a numeric or integer vector
#' @param base a positive number, the base for which logarithms are computed.
#' Default \code{exp(1)}.
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.log} (or default name \code{log.newobj})
#' which is written to the serverside. The output object is of class numeric.
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
logDS <- function(x, base=exp(1)) {
  x.var <- .loadServersideObject(x)
  .checkClass(obj = x.var, obj_name = x, permitted_classes = c("numeric", "integer"))

  out <- log(x.var, base = base)
  return(out)
}
# ASSIGN FUNCTION
# logDS
