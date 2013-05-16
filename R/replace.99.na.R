#' Replaces entries with value '99' by missing data
#' 
#' @param a a numerical vector
#' @export
#' 

replace.99.na <- function (a) {
  replace(a,a==99,NA)
}