#' Replaces entries with value '9' by missing data
#' 
#' @param a a numerical vector
#' @export
#' 

replace.9.na <- function (a) {
  replace(a,a==9,NA)
}