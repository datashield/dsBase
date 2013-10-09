#' 
#' @title Generates a factor variable
#' @description The functions uses the input factor and generates a new factor 
#' with additional levels.
#' @param xvect a numerical or character vector
#' @param addLevels the additional levels to add to the input factor
#' @return a factor vector that has its original levels plus the added levels
#' @author Gaye, A.
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
createfactor.ds <- function (xvect=NULL, addLevels=NULL){
  
  if(is.null(xvect)){
    Stop("\n\nPlease provide a valid numeric or factor vector!\n\n")
  }
  if(is.null(addLevels)){
    Stop("\n\nNumber of observations must provided!\n\n")
  }
  
  # get the current levels of the input vector
  old.levels <- levels(xvect)
  levels(xvect) <- c(old.levels, addLevels)

  return(xvect)
}
