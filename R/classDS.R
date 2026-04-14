#'
#' @title Returns the class of an object
#' @description This function is similar to R function \code{class}.
#' @details The function returns the class of an object
#' @param x a string character, the name of an object
#' @return the class of the input object
#' @author Stuart Wheater, for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
classDS <- function(x){
  x.val <- .loadServersideObject(x)
  out <- class(x.val)
  return(out)
}
#AGGREGATE FUNCTION
# classDS
