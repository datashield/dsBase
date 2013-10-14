#' 
#' @title Derives a valid binary variable from a continuous variable
#' @details This function runs only if the set threshold (i.e. >=)
#' is met or not met by more than one observation.
#' @param xvect a continuous numeric variable
#' @param operator a string character that represents the logical operator.
#' Six logical operators are possible: '>', '<', '>=', '<=', '==' and '!='.
#' @param threshold a numeric value that sets the threshold
#' @return a vector where the entries that met the condition are 
#' assigned the value 1 or 0 otherwise
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("PM_BMI_CONTINUOUS")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # run the function with for the condition '>=70'
#' var <- quote(D$PM_BMI_CONTINUOUS) 
#' operator <- ">="
#' threshold <- 70
#' cally <-  call("subsetvar.ds", var, as.character(operator), as.numeric(70))
#' datashield.assign(datasources=opals, "subset", cally)
#' }
#' 
subsetvar.ds <- function(xvect=NULL, operator=NULL, threshold=NULL){
  
  if(is.null(xvect)){
    stop("\n\nNo input variable provided!\n\n")
  }
  
  if(is.null(operator)){
    stop("\n\nNo logical operator provided!\n\n")
  }
  
  if(is.null(threshold)){
    stop("\n\nNo input variable provided!\n\n")
  }
  
  # get te indices of the observation where the condition is true
  cally <- call(operator, xvect, threshold)
  indx1 <- which(eval(cally))
  
  # turn all the the value of all the observation that met the condition to 1
  xvect_out <- xvect
  xvect_out[indx1] <- 1
  
  # get the indices of the observations that did not meet the condition and turn 
  # them to 0
  indx2 <- which(xvect_out != 1)
  xvect_out[indx2] <- 0

  if(length(indx1) < 2 | length(indx2) < 2){
    stop("\n\nThe subset variable is not valid!\n\n")
  }else{
    return(xvect_out)
  }
}
