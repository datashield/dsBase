#' 
#' @title Computes sums and means of rows or columns of numeric arrays
#' @description The function is similar to R base functions 'rowSums', 'colSums',
#' 'rowMeans' and 'colMeans'.
#' @details the output is returned to the user only the number of entries in the 
#' output vector is greater or equal to the allowed size. 
#' @param dataset an array of two or more dimensions.
#' @param operation an integer that indicates the operation to carry out:
#' 1 for 'rowSums', 2 for 'colSums', 3 for 'rowMeans' or 4 for 'colMeans'
#' @return a numeric vector
#' @export
#' @author Gaye, A.
#' 
rowColCalcDS <- function (dataset, operation) {
  
  if(operation == 1){
    result <- rowSums(dataset, na.rm=TRUE)
  }
  if(operation == 2){
    result <- colSums(dataset, na.rm=TRUE)
  }
  if(operation == 3){
    result <- rowMeans(dataset, na.rm=TRUE)
  }
  if(operation == 4){
    result <- colMeans(dataset, na.rm=TRUE)
  }
  
  # check if the output is valid (i.e. meets DataSHIELD criteria)
  check <- isValidDS(result)
  if(check){
    return(result)
  }else{
    resultNA <- rep(NA, length(result))
    return(resultNA)
  }
  
  return(result)
  
}