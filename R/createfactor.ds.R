#' 
#' @title Generates a factor variable
#' @description The functions uses the input vector to generate a factor vector.
#' The generate factor is checked and if valid (no category with count > 0 and < 5)
#' returned; if not valid an empty of the same length is returned. 
#' @param xvect a numerical or character vector
#' @param levels optional vector of values that 'xvect' might have taken
#' @param labels labels for the different levels
#' @param exclude values to be excluded
#' @param ordered tells if the variable 'x' is ordered
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
createfactor.ds <- function (xvect, levels=NULL, labels=levels, exclude=NA, ordered=is.ordered(xvect)){
  
  if(is.null(levels)){
    tt <- table(xvect)
    levels <- names(tt)
    labels <- levels
  }
  # use the R function to turn the input vector into a factor
  # if it is not already a factor
  if(!(is.factor(xvect))){
    xvector <- factor(xvect, levels, labels, exclude, ordered)
  }else{
    xvector <- xvect
  }
  
  # call the function that checks of the vector is a valid factor
  out <- checkfactor.ds(xvector) 
  status <- out$status
  
  # if the factor is valid (i.e. no categories with between 0 and 5 counts)
  # return the created factor otherwise print a failure message
  if(status == 0){
    fvect <- xvector 
  }else{
    fvect <- rep(NA, length(xvector))
  }
  cat(length(fvect),"\n")
  return(fvect)
}
