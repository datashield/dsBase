#'
#' @title Computes the variance of vector with more than 4 entries
#' @description Calculates the variance.
#' @details if the length of input vector is less than the set filter
#' a missing value is returned. 
#' @param xvect a vector
#' @return a numeric, the variance
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
#' # compute the variance
#' variance <- datashield.aggregate(opals, quote(varDS(D$LAB_TSC)))
#' }
#'
varDS <- function (xvect) {
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- dsbase:::.setFilterDS()
  
  if(length(xvect) < nfilter){
    result <- NA
  }else{
    result <- var(xvect, na.rm=TRUE) 
  }
  
  return(result)
}