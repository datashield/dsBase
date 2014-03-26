#'
#' @title Computes statistical mean of vector with more than 4 entries
#' @description Calculates the mean value.
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
  if(length(xvect) > 0 & length(xvect) < 5){
    stop("Operation not allowed: argument contains between 1 and 4 observations only!\n")
  }else{
    result <- mean(xvect, na.rm=TRUE) 
  }
  return(result)
}
