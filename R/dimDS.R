#' 
#' @title Returns the dimensions of a table structure
#' @description Returns the number of rows and the number of columns.
#' @details This is similar to R base function \code{dim} but here the input 
#' is a character.
#' @param x a character, the name of a matrix or a dataframe.
#' @return the number of rows and the number of columns
#' @author Gaye, A.
#' @export
#' 
dimDS <- function(x){
  
  D <- eval(parse(text=x))
  output <- dim(D)
  
  return(output)
  
}