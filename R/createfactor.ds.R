#' 
#' @title Generates a factor variable
#' @description The functions uses the input factor and generates a new factor 
#' with new levels.
#' @param xvect a factor vector
#' @param categories the levels of the  factor to generate
#' @return a factor vector that has the levels of the original vector 
#' and some additional levels
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' library(opal)
#' myvar <- list("PM_BMI_CATEGORICAL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # get the levels of variable PM_BMI_CATEGORICAL in study1
#' current.levels <- datashield.aggregate(opals[1], quote(levels(D$PM_BMI_CATEGORICAL)))
#' 
#' #' generate a new factor vector which has an additonal level '4' 
#' newlevels <- c(current.levels[[1]], "4")
#' datashield.assign(opals, "bmi_f", quote(createfactor.ds(D$PM_BMI_CATEGORICAL, newlevels)))
#'
#'  }
#'
createfactor.ds <- function (xvect=NULL, categories=NULL){
  
  if(is.null(xvect)){
    stop("\n\nPlease provide a valid numeric or factor vector!\n\n")
  }
  if(is.null(categories)){
    stop("\n\nthe levels of the variable must provided!\n\n")
  }
  
  # get the current levels of the input vector
  levels(xvect) <- categories

  return(xvect)
}
