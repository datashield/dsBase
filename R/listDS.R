#' 
#' @title Coerce objects into a list
#' @description this function is similar to R function 'list'
#' @details Unlike the R function 'list' it takes also a vector of characters,
#' the names of the elements in the output list.
#' @param input a list of objects to coerce into a list
#' @param eltnames a character vector, the names of the elements in the list.
#' @return a list
#' @author Gaye, A.
#' @export
#' 
listDS <-function (input=NULL, eltnames=NULL){
  
  mylist <- input
  names(mylist) <- eltnames

  return(mylist)
  
}