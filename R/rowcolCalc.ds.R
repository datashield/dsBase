#' 
#' @title Computes sums and means of rows or columns of numeric arrays
#' @details The function is similar to R base functions 'rowSums', 'colSums',
#' 'rowMeans' and 'colMeans' but its outcome cannot be returned to the user as it might
#' be revealing if the array contains only one row or one column. 
#' @param dataset an array of two or more dimensions.
#' @param operation a string character that indicates the operation to carry out:
#' 'rowSums', 'colSums', 'rowMeans' or 'colMeans'
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
#' datashield.assign(opals, dataset=quote(D), operation=as.character("rowSums"))
#' }
#' 
rowcolCalc.ds <- function (dataset, operation) {
  
  # contruct the operation to carry out as a call
  cally <- call(operation, dataset, na.rm=TRUE)
  
  # evaluate the call and return it
  results <- eval(cally)
  return(results)
  
}
