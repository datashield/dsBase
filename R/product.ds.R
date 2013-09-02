#' 
#' @title Computes a product
#' @param a a numerical 
#' @param b a numerical 
#' @param c a numerical 
#' @param d a numerical 
#' @param e a numerical
#' @return a numerical
#' @author Burton, P.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("LAB_TSC","LAB_HDL")
#' opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute the of 'LAB_TSC' by 'LAB_HDL' and assign it to 'P'
#' datashield.assign(opals, "P", quote(product.ds(D$LAB_TSC, D$LAB_HDL)))
#' }
#' 
product.ds <- function (a=1,b=1,c=1,d=1,e=1){
  a*b*c*d*e
}
