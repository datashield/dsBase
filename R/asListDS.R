#' 
#' @title Converts a data.frame or matrix into a list
#' @description this function is similar to R function 'as.list'.
#' @param input the object to turn into list.
#' @return a valid list or an empty list
#' @author Gaye, A.
#' @export
#' 
asListDS <-function (input){
  # check if the input is a dataframe or a matrix
  # this should be checked on the client side
  # but just in case a '***' changes the client code
  # to 'hack' he/she will not be able to reach here
  cl <- class(input)
  if(cl != 'data.frame' & cl != 'matrix'){
    output <- as.list(rep(NA, 4))
  }else{
    # check if the input vector is valid (i.e. meets DataSHIELD criteria)
    check <- dsbase:::.isValidDS(input)
    if(check){
      output <- as.list(input)
      names(output) <- colnames(input)
    }else{
      output <- as.list(rep(NA, 4))
    }
  }
  return(output)
}