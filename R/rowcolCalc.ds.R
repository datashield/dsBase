#' 
#' @title Computes sums and means of rows or columns of numeric arrays
#' @details The function is similar to R base functions 'rowSums', 'colSums',
#' 'rowMeans' and 'colMeans' but its outcome cannot be returned to the user as it might
#' be revealing if the array contains only one row or one column. 
#' @param dataset an array of two or more dimensions.
#' @param operation an integer that indicates the operation to carry out:
#' 1 for 'rowSums', 2 for 'colSums', 3 for 'rowMeans' or 4 for 'colMeans'
#' @return a list that contains the vector of results.
#' @export
#' @author Gaye, A.
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' library(opal)
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#  myvar <- list("LAB_TSC", "LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a histogram object without plotting
#' call.object <-  call("rowcolCalc.ds", quote(D), as.character("rowSums"))
#' datashield.assign(opals, "rowsums", call.object)
#' }
#' 
rowcolCalc.ds <- function (dataset, operation) {
  
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
  
  return(result)
  
}