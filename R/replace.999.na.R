#' Replaces entries with value '999' by missing data
#' 
#' @param a a numerical vector
#' @export
#' 

replace.999.na <- function (a) {
  replace(a,a==999,NA)
}