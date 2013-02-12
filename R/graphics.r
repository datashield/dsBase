#' Computes a histogram of the given data values without plotting.
#'
#' @param a a vector of values for which the histogram is desired.
#' @export
#' 
histogram.1 <- function (a) {
  graphics::hist(a,plot=FALSE)
}