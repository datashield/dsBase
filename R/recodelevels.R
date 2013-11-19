#' 
#' @title recodes a categorical variable
#' @description this function recodes levels of a categorical variable with new given labels. 
#' @param xvect a factor
#' @param newlabels a string vector with new labels for the levels
#' @return  a factor vector with new labels for levels
#' @author Isaeva, J.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("PM_BMI_CATEGORICAL")
#' library(opal)
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # get the range of the variable 'LAB_HDL'
#' datashield.assign(opals, 'bmi_new', quote(recodelevels.ds(D$PM_BMI_CATEGORICAL, newlabels=c('normal', 'overweight', 'obesity'))))
#' }
#'
recodelevels.ds <- function(xvect, newlabels){
  
  # print an error message if the input vector is not a factor
  if(!(is.factor(xvect)))
    stop("The input vector is not a factor!")
  
  
  oldlabels = levels(xvect)
  newlabels = factor(newlabels, levels=newlabels)
  xvect_newlabels = newlabels[match(xvect, oldlabels)]
  return(xvect_newlabels)
  
  
}