#' Replaces missing values by '999'
#' 
#' @param a a numerical vector
#' @export
#' 

replace.na.999 <- function (a) {
  replace(a,a==NA,999)
}
