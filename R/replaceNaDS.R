#' 
#' @title Replaces the missing values in a vector
#' @description This function identifies missing values and replaces them by a value or 
#' values specified by the analyst.
#' @details This function is used when the analyst prefer or requires complete vectors. 
#' It is then possible the specify one value for each missing value by first returning
#' the number of missing values using the function \code{numNaDS} but in most cases
#' it might be more sensible to replace all missing values by one specific value e.g. 
#' replace all missing values in a vector by the mean or median value. Once the missing
#' values have been replaced a new vector is created.
#' @param xvect a character, the name of the vector to process.
#' @param replacements a vector which contains the replacement value(s), a vector one or 
#' more values for each study. 
#' @return a new vector without missing values
#' @author Gaye, A.
#' @export
#' 
replaceNaDS <- function(xvect, replacements){
  
  # check if the input vector is valid (i.e. meets DataSHIELD criteria)
  check <- isValidDS(xvect)
  
  # get the indices of the missing values
  indx <- which(is.na(xvect))
  
  if(check){
    # if the inpout vector is valid replace missing values
    xvect[indx] <- replacements
  }else{
    # if the inpout vector is not valid and is of size > 0
    xvect[1:length(xvect)] <- NA
  }
  
  # return the new vector
  return(xvect) 
  
}