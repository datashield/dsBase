#'
#' @title Returns the length of a vector or list
#' @description This function is similar to R function \code{length}.
#' @details The function returns the length of the input vector or list.
#' @param x a string character, the name of a vector or list
#' @return a numeric, the number of elements of the input vector or list.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
lengthDS <- function(x){

  x.var <- eval(parse(text=x), envir = parent.frame())

  # find the length of the input vector or list
  out <- length(x.var)

  # return output length
  return(out)

}
#AGGREGATE FUNCTION
# lengthDS
