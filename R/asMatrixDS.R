#' 
#' @title Attempts to convert the input into a matrix
#' @description this function is similar to R function 'as.matrix'
#' @details Unlike the r function 'as.matrix' some restriction are applied.
#' @param input the object to turn into list.
#' @return a valid list or an empty list
#' @author Gaye, A.
#' @export
#' 
asMatrixDS <-function (input){
  
  # check if the input is valid (i.e. meets DataSHIELD criteria)
  check <- isValidDS(input)
  if(check){
    output <- as.matrix(input) 
  }else{
    output <- as.matrix(input) 
    output[] <- NA
  }
  
  return(output)
  
}