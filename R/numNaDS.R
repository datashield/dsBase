#'
#' @title Counts the number of missing values
#' @description this function just counts the number of missing entries
#' in a vector.
#' @param x a character string, the name of a server-side vector
#' @return a list with two elements: \code{numNA} (an integer, the number of
#'   missing values) and \code{class} (the class of the input object, for
#'   client-side consistency checking)
#' @author Gaye, A.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
numNaDS <- function(x){
  xvect <- .loadServersideObject(x)
  out <- length(which(is.na(xvect)))
  list(numNA = out, class = class(xvect))
}
