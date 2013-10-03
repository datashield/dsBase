#'
#' @title Calculates statistical mean for a numeric 1D array with more than 4 entries
#' @param xvect a numerical vector
#' @return a numerical array, matrix or dataframe
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
#' stat.mean <- datashield.aggregate(opals, quote(colMeans.ds(D$LAB_TSC)))
#' }
#'
colMeans.ds <- function (xvect) {
  if(is.array(xvect) || is.data.frame(xvect) || is.matrix(xvect)){
    if(length(xvect) > 0 & length(xvect) < 5){
      stop("Operation not allowed: argument contains between 1 and 4 observations!\n")
    }else{
      colMeans(xvect, na.rm=TRUE) 
    }
  }else{
    stop("'xvect' must be an array, a matrix or a dataframe!\n")
  }
}
