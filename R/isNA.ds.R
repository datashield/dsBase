#' 
#' @title Checks if a vector is empty 
#' @description this function is similar to R function \code{is.na} but instead of a vector 
#' of booleans it returns just one boolean to tell if all the element are missing values.
#' @param xvect a numerical or character vector
#' @return the integer '1' if the vector contains on NAs and '0'  otherwise
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' library(opal)
#' myvar <- list("LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # check if the variable 'LAB_HDL' is empty
#' datashield.aggregate(opals, quote(isNA.ds(D$LAB_HDL)))
#' }
#'
isNA.ds <- function(xvect){
  
  out <- is.na(xvect)
  total <- sum(out, na.rm=TRUE)
  if(total==(1*length(out))){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
