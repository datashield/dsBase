#' 
#' @title Converts a data.frame or matrix into a list
#' @description this function is similar to R function 'as.list'
#' @details Unlike the R function 'as.list' some restriction are applied.
#' @param input the object to turn into list.
#' @return a valid list or an empty list
#' @author Gaye, A.
#' @export
#' 
asListDS <-function (input){
  
  # check if the input is valid (i.e. meets DataSHIELD criteria)
  check <- isValidDS(input)
  if(check){
    if(class(input) == 'matrix'){ 
      input2 <- as.data.frame(input) 
    }else{
      input2 <- input
    }
    output <- as.list(input2)
  }else{
    input2[] <- NA
    output <- input2
  }
  
  return(output)
}