#' 
#' @title Verifies that a factor vector is valid
#' @description This function checks if a numeric or character factor vector contains
#' levels with counts > 0 and < 5.
#' @param xvect a numeric of character vector of type \code{factor}
#' @return a list which contains two elements:
#' \code{summary table}, the results of the tabulation of the input vector
#' \code{status}, tells if the factor is valid, 1 if one or more levels have counts 
#' of between 0 and 5 and 0 otherwise.
#' @author Gaye, A.; Newby, C.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#' library(dsbaseclient)
#  myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # check that the factor does not contain categories with count > 0 and < 5
#' output <- datashield.aggregate(opals, quote(checkfactor.ds(D$LAB_TSC)))
#' }
#' 
checkfactor.ds  <-  function(xvect){
  
  # check if the input vector is of factor type
  # if it is not display a message and stop the process
  if(!(is.factor(xvect))){
    stop("The argument of the function must be of type factor!")
  }else{
    # remove duplicates 
    unics <- unique(xvect)
    
    # tabulate the input vector
    b <- table(xvect)
    n <- length(unics)
    
    # assign the symbol '*' to levels that have counts between 0 and 5
    # number of levels
    numlevels <- length(b)
    
    # counts in the levels
    level.counts <- b[][1:numlevels]
    
    # assign the symbol '*' to levels that have counts between 0 and 5
    idx <- which(level.counts > 0 & level.counts < 5)
    if(length(idx) > 0){
      status <-  1
      b[][idx] <- "*"
    }else{
      status <-  0
    }
  } 
  # return the table and the status 
  return(list("summary table"=b, "status"=status))
}
