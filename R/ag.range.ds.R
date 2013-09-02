#' 
#' @title returns the minimum and maximum of a numeric vector
#' @description this function is similar to R function \code{range} but instead to not return 
#' the real minimum and maximum, the computed values are multiplied by a very small random number. 
#' @param xvect a numerical 
#' @return  a numeric vector which contains the minimum and the maximum
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_HDL")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # get the range of the variable 'LAB_HDL'
#' datashield.aggregate(opals, quote(range.ds(D$LAB_HDL)))
#' }
#'
ag.range.ds <- function(xvect){
  
  # print an error message if the input vector is not a numeric
  if(!(is.numeric(xvect))){
    stop("The input vector is not a numeric!")
  }else{
    rr <- c(min(xvect, na.rm=TRUE), max(xvect, na.rm=TRUE))
    random1 <- runif(1, 0.875, 1)
    random2 <- runif(1, 1, 1.125)
    output <- c(rr[1]*random1, rr[2]*random2)
  }
  return (output)
}