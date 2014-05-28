#'
#' @title Computes statistical mean of vector with more than 4 entries
#' @description Calculates the mean value.
#' @details if the length of input vector is less than the set filter
#' a missing value is returned. 
#' @param xvect a vector
#' @return a numeric, the statistical mean
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' library(opal)
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#  myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute the statistical mean
#' stat.mean <- datashield.aggregate(opals, quote(meanDS(D$LAB_TSC)))
#' }
#'
meanDS <- function (xvect) {
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  if(length(xvect) < nfilter){
    result <- NA
  }else{
    result <- mean(xvect, na.rm=TRUE) 
  }
  return(result)
}
