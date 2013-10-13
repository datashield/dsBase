#'
#' @title Computes the variance of a numeric vector
#' @param xvect a numeric vector
#' @return a numeric, the variance
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
#' # compute the variance
#' stat.mean <- datashield.aggregate(opals, quote(mean.ds(D$LAB_TSC)))
#' }
#'
var.ds <- function (xvect) {
  if(is.numeric(xvect)){
    result <- var(xvect, na.rm=TRUE)
    return(result)
  }else{
    stop("\n\'xvect muxt be a numeric vector!\n\n")
  }
}
