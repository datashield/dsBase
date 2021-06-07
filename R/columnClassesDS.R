#' @title Get classes of data frame columns
#' 
#'
#' @param x \code {character} Name of a data.frame on the server
#'
#' @return \code{named character vector} with the class of each column, the name 
#' corresponds to the column name
#' @export
#'

columnClassesDS <- function(x){
  x.val <- eval(parse(text=x), envir = parent.frame())
  return(sapply(x.val, class))
}