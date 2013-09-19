#'
#' @title Calculates a statistical mean for a vector with more than 4 entries
#' @param xvect a numerical vector
#' @return a numerical
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#' library(dsbaseclient)
#  myvar <- list("LAB_TSC")
#' opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute the statistical mean
#' stat.mean <- datashield.aggregate(opals, quote(mean.ds(D$LAB_TSC)))
#' }
#'
mean.ds <- function (xvect) {
  if(length(xvect) > 0 & length(xvect) < 5){
    stop("Operation not allowed!\n")
  }else{
    mean(xvect,na.rm=TRUE) 
  }
}
