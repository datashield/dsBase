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
  # check if the input is a dataframe or a matrix
  # this should be checked on the client side
  # but just in case an '***' changes the client code
  # to 'hack'. He/she will not be able to reach here
  cl <- class(input)
  if(typ != 'data.frame' & typ != 'factor' & typ != 'character' & typ != 'numeric' & typ != 'integer'){
    output <- as.list(rep(NA, 4))
  }else{
    # check if the input vector is valid (i.e. meets DataSHIELD criteria)
    check <- dsbase:::.isValidDS(input)
    if(check){
      output <- as.matrix(input) 
    }else{
      output <- as.list(rep(NA, 4))
    }
  }
  
  return(output)
  
}