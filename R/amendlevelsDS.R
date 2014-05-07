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
#' library(opal)
#' data(logindata)
#' 
#' # login and assign specific variable(s)

#' myvar <- list("PM_BMI_CATEGORICAL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # get the levels of variable PM_BMI_CATEGORICAL in study1
#' current.levels <- datashield.aggregate(opals[1], quote(levels(D$PM_BMI_CATEGORICAL)))
#' 
#' #' generate a new factor vector which has an additonal level '4' 
#' call.object <- call("createfactor.ds", quote(D$PM_BMI_CATEGORICAL), as.list(current.levels[[1]], "4"))
#' datashield.assign(opals, "bmi_f", call.object)
#'
#'  }
#'
amendlevelsDS <- function (xvect=NULL, categories=NULL){
  
  if(is.null(xvect)){
    stop("\n\nPlease provide a valid factor vector!\n\n")
  }

  if(is.null(categories)){
    y = unique(xvect)
    ind = sort.list(y)
    y = as.character(y)
    categories = unique(y[ind])
    # stop("\n\nthe levels of the new variable must provided!\n\n")
  }
  
  # check if the input vector is valid (i.e. respect DataSHIELD conditions)
  check <- isValid.ds(xvect)
  
  if(check){
    # generate the new variable with the specified levels
    ll <- unlist(categories)
    xvect_f <- factor(xvect, levels=ll)
    return(xvect_f)
  }else{
    return(NULL)
  }
}
