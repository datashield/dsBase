#' 
#' @title Sets the privacy level
#' @description This is an internal function that set the number of observation
#' that are allowed in any table or vector. The function gets the value from the data repository
#' and if no value was set it uses the default value 5.
#' @details The function is called by server side functios
#' @param x a dummy argument
#' @return an integer between 1 and 5
#' @keywords internal
#' @noRd
#' @author Gaye, A.
#'
setFilterDS <- function(x=getOption("datashield.privacyLevel", default=5)){
  a <- as.numeric(as.character(x))
  return(a)
}