#' 
#' @title Computes a product
#' @description calculates the product of two or more objects.
#' @param xlist a list of numerical to compute a product for.
#' @return a numerical
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' library(opal)
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("LAB_TSC","LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute the of 'LAB_TSC' by 'LAB_HDL' and assign it to 'P'
#' prodinput <- list(D$LAB_TSC, D$LAB_HDL)
#' cally <- call("productDS", prodinput)
#' datashield.assign(opals, "P", cally)
#' }
#' 
productDS <- function (xlist){
  a <- 1
  for(i in 1:length(xlist)){
    a <- a * xlist[[i]]
  }
  return(a)
}
