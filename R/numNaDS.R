#'
#' @title Counts the number of missing values
#' @description this function just counts the number of missing entries
#' in a vector.
#' @param x a character string, the name of a server-side vector
#' @return an integer, the number of missing values
#' @author Gaye, A.
#' @export
#'
numNaDS <- function(x){
  xvect <- .loadServersideObject(x)
  out <- length(which(is.na(xvect)))
  return(out)
}
