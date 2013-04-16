#' Variance of a vector or a matrix with NA values stripped before computation proceeds.
#'
#' @param a vector or matrix
#' @export
#' 
var <- function (a) {
  stats::var(a,na.rm=TRUE)
}

