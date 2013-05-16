#' Replaces entries with value '9999' by missing data
#' 
#' @param a a numerical vector
#' @export
#' 

replace.9999.na <- function (a) {
  replace(a,a==9999,NA)
}