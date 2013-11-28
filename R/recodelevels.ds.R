#' 
#' @title recodes a categorical variable
#' @description this function recodes levels of a categorical variable with new given labels. 
#' @param xvect a factor
#' @param newlabels a string vector with new labels for the levels
#' If \code{newlabels} is not specified, the naming of original levels is amended - numbering is added (0_..., 1_..., 2_... etc.)
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
#' # rename the levels of PM_BMI_CATEGORICAL
#' datashield.assign(opals, 'bmi_new', quote(recodelevels.ds(D$PM_BMI_CATEGORICAL, newlabels=c('normal', 'overweight', 'obesity'))))
#' }
#'
recodelevels.ds <- function(xvect, newlabels=NULL){
  
  # print an error message if the input vector is not a factor
  if(!(is.factor(xvect)))
    stop("The input vector is not a factor!")
  
  
  oldlabels = levels(xvect)
  
  if (!is.null(newlabels)) {
    if (length(oldlabels)!=length(newlabels)) {
      stop('The number of new levels provided is not the same as in the original vector!')
    } else
      for (i in 1:length(newlabels)) {
        dummy = as.character(i-1)
        newlabels[i] = paste0(dummy, "_", newlabels[i])
      }
    
  } else {
    newlabels = character(length(oldlabels))
    for (i in 1:length(newlabels)) {
      dummy = as.character(i-1)
      newlabels[i] = paste0(dummy, "_", oldlabels[i])
    }
  }
  
  
  newlabels = factor(newlabels, levels=newlabels)
  xvect_newlabels = newlabels[match(xvect, oldlabels)]
  return(xvect_newlabels)
  
  
}