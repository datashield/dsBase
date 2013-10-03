#'
#' @title Calculates statistical mean of vector with more than 4 entries
#' @param xvect a vector
#' @return a numeric, the statistical mean
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#' library(opal)
#  myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute the statistical mean
#' stat.mean <- datashield.aggregate(opals, quote(mean.ds(D$LAB_TSC)))
#' }
#'
mean.ds <- function (xvect) {
  if(length(xvect) > 0 & length(xvect) < 5){
    stop("Operation not allowed: argument contains between 1 and 4 observations only!\n")
  }else{
    result <- mean(xvect, na.rm=TRUE) 
  }
  return(result)
}
