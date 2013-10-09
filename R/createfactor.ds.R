#' 
#' @title Generates a factor variable
#' @description The functions uses the input vector to generate a factor vector.
#' The generate factor is checked and if valid (no category with count > 0 and < 5)
#' returned; if not valid an empty of the same length is returned. 
#' @param xvect a numerical or character vector
#' @param numobs the number of observations/missing values to add to 'xvect'
#' @return a 'valid' factor vector or an empty vector of the same length.
#' @author Gaye, A.; Burton, P.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' library(opal)
#' myvar <- list("GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a factor vector using the variable 'GENDER'
#' datashield.assign(opals, "sex", quote(createfactor.ds(D$GENDER)))
#' 
#' # display the number of observation in each vector
#' datashield.aggregate(opals, quote(length(sex)))
#'  }
#'
createfactor.ds <- function (xvect=NULL, numobs=NULL){
  
  if(is.null(xvect)){
    Stop("\n\nPlease provide a valid numeric or factor vector!\n\n")
  }
  if(is.null(numobs)){
    Stop("\n\nNumber of observations must provided!\n\n")
  }
  
  # add the required number of observations as missing values
  newxvect <- factor(c(as.character(xvect), rep(NA,numobs)))

  return(newxvect)
}
