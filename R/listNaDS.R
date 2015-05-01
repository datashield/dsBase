#' 
#' @title Indicates the position of missing values in a vector
#' @description This function identifies the position of missing values
#' @details This function is used when the analyst needs to know where missing values
#' are located. This is useful when using methods such as Missing Indicator. 
#' @param xvect a character, the name of the vector to process.
#' @return a new vector indicating where missing values are found
#' @author Bishop, T.
#' @export
#' 
listNaDS <- function(xvect){
  
  # check if the input vector is valid (i.e. meets DataSHIELD criteria)
  check <- isValidDS(xvect)
  
  if(check){
    # if the input vector is valid get the indices of the missing values
    indx <- as.numeric(is.na(xvect))
  }else{
    # if the input vector is not valid and is of size > 0
    indx[1:length(xvect)] <- NA
  }
  
  # return the new vector
  return(indx) 
  
}