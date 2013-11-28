#' 
#' @title changes a reference level of a factor
#' @description this function is similar to R function \code{relevel}, but in addition addes numbering to the levels
#' so that they are displayed in the right order when creating cross-tables.
#' @param xvect a factor
#' @param ref the reference level
#' @return  a factor of the same length as xvect
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
#' # rename the levels of PM_BMI_CATEGORICAL and make "obesity" as a reference level
#' datashield.assign(opals, 'bmi_new', quote(recodelevels.ds(D$PM_BMI_CATEGORICAL, newlabels=c('normal', 'overweight', 'obesity'))))
#' datashield.assign(opals, 'bmi_ob', quote(changerefgroup.ds(bmi_new, ref='2_obesity')))
#' ds.levels(opals, quote(bmi_ob))
#' 
#' # or without renaming the levels (group "3" as a reference level)
#' datashield.assign(opals, 'bmi_ob', quote(changerefgroup.ds(D$PM_BMI_CATEGORICAL, ref='3')))
#' ds.levels(opals, quote(bmi_ob))
#' }
#'
changerefgroup.ds <- function(xvect, ref=NULL){
  
  # print an error message if the input vector is not a factor
  if(!(is.factor(xvect)))
    stop("The input vector is not a factor!")
  
  xvect_reordered = relevel(xvect, ref)
  xvect_levels = levels(xvect_reordered)
  for (i in 1:length(xvect_levels)) {
    dummy = as.character(i-1)
    # xvect_levels[i] = paste0(dummy, "_", xvect_levels[i])
    levels(xvect_reordered)[i] = paste0(dummy, "_", levels(xvect_reordered)[i])
  }
  
  #   levels(xvect_reordered) = xvect_levels
  return(xvect_reordered)
  
  
}