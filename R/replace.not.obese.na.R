#' Replaces missing values by '999'
#' 
#' @param a a numerical vector
#' @export
#' 

replace.not.obese.na <- function (a) {
  replace(a,bmi.c!=3,NA)
}
