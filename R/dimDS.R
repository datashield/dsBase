#'
#' @title Returns the dimension of a data frame or matrix
#' @description This function is similar to R function \code{dim}.
#' @details The function returns the dimension of the input dataframe or matrix
#' @param x a string character, the name of a dataframe or matrix
#' @return the dimension of the input object
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
dimDS <- function(x){

  x.var <- eval(parse(text=x), envir = parent.frame())

  # find the dim of the input dataframe or matrix
  out <- dim(x.var)

  # return the dimension
  return(out)

}
#AGGREGATE FUNCTION
# dimDS
